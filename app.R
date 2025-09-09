library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(RSQLite)
library(openxlsx)
library(rhandsontable)

# 1. Cargar datos geoespaciales ------------------------------------------------
shapefile <- st_read("Lotes_geo.geojson", quiet = TRUE)
shapefile$id <- 1:nrow(shapefile)

# 2. Funciones auxiliares ------------------------------------------------------
db_query <- function(query, params = NULL) {
  con <- dbConnect(RSQLite::SQLite(), "lotes.db")
  if (!is.null(params) && length(params) > 0) {
    stmt <- dbSendQuery(con, query)
    dbBind(stmt, params)
    result <- dbFetch(stmt)
    dbClearResult(stmt)
  } else {
    result <- dbGetQuery(con, query)
  }
  dbDisconnect(con)
  return(result)
}

db_execute <- function(query, params = list()) {
  con <- dbConnect(RSQLite::SQLite(), "lotes.db")
  if (length(params) == 0) {
    dbExecute(con, query)
  } else {
    stmt <- dbSendStatement(con, query)
    dbBind(stmt, params)
    dbClearResult(stmt)
  }
  dbDisconnect(con)
}

formatear_fecha <- function(fecha) {
  format(as.Date(fecha, format = "%Y-%m-%d"), "%d/%m/%Y")
}

# 3. Configurar base de datos --------------------------------------------------
db_execute("CREATE TABLE IF NOT EXISTS lotes (
              id INTEGER PRIMARY KEY AUTOINCREMENT,
              poligono_id INTEGER,
              Sector TEXT,
              Lote TEXT,
              OCUPACION TEXT,
              Sup REAL,
              fecha TEXT
            )")

# 4. Interfaz de usuario -------------------------------------------------------
ui <- navbarPage(
  title = "Gestión de Riegos Agrícolas",
  
  tabPanel("Registro",
           sidebarLayout(
             sidebarPanel(
               dateInput("fecha_riego", "Fecha de Riego:", value = Sys.Date()),
               actionButton("guardar", "Guardar", class = "btn-primary"),
               actionButton("limpiar", "Limpiar", class = "btn-warning"),
               width = 3
             ),
             mainPanel(
               leafletOutput("mapa", height = "600px"),
               tableOutput("tabla_seleccion")
             )
           )),
  
  tabPanel("Histórico",
           sidebarLayout(
             sidebarPanel(
               dateRangeInput("fecha_filtro", "Filtrar por fecha:",
                              start = Sys.Date() - 30, end = Sys.Date()),
               downloadButton("descargar_excel", "Descargar Excel"),
               width = 3
             ),
             mainPanel(
               leafletOutput("mapa_visualizacion", height = "500px"),
               tableOutput("tabla_resumen"),
               h4("Hectáreas por cultivo"),
               tableOutput("tabla_cultivos"),
               verbatimTextOutput("kpi_hectareas")
             )
           )),
  
  tabPanel("Editar Riegos",
           sidebarLayout(
             sidebarPanel(
               helpText("Modificar registros de riego:"),
               actionButton("guardar_ediciones", "Guardar cambios", class = "btn-success"),
               br(), br(),
               fileInput("nuevo_geojson", "Actualizar archivo de lotes (.geojson)",
                         accept = c(".geojson")),
               helpText("Al cargar, se actualizarán los polígonos del sistema.")
             ),
             mainPanel(
               rHandsontableOutput("tabla_editable")
             )
           ))
)

# 5. Lógica del servidor -------------------------------------------------------
server <- function(input, output, session) {
  
  shapefile_reactivo <- reactiveVal(shapefile)
  selected_polygons <- reactiveVal(vector())
  
  output$mapa <- renderLeaflet({
    leaflet(shapefile_reactivo()) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~id,
        fillColor = "#4A89DC",
        fillOpacity = 0.6,
        weight = 1,
        color = "#34495E",
        highlightOptions = highlightOptions(
          color = "#E9573F",
          weight = 3,
          bringToFront = TRUE
        )
      )
  })
  
  observeEvent(input$mapa_shape_click, {
    click <- input$mapa_shape_click
    if (!is.null(click$id)) {
      seleccion_actual <- selected_polygons()
      if (click$id %in% seleccion_actual) {
        seleccion_actual <- setdiff(seleccion_actual, click$id)
      } else {
        seleccion_actual <- c(seleccion_actual, click$id)
      }
      selected_polygons(seleccion_actual)
      
      leafletProxy("mapa") %>%
        clearGroup("seleccionados") %>%
        addPolygons(
          data = shapefile_reactivo() %>% filter(id %in% seleccion_actual),
          fillColor = "#FF8000",
          fillOpacity = 0.7,
          color = "#FF4000",
          weight = 2,
          group = "seleccionados"
        )
    }
  })
  
  output$tabla_seleccion <- renderTable({
    ids <- selected_polygons()
    if (length(ids) == 0) return(NULL)
    
    seleccion <- shapefile_reactivo() %>% filter(id %in% ids)
    
    data.frame(
      Sector = as.character(seleccion$Sector),
      Lote = as.character(seleccion$Lote),
      OCUPACION = as.character(seleccion$OCUPACION),
      Sup = as.numeric(seleccion$Sup)
    )
  })
  
  observeEvent(input$guardar, {
    ids <- selected_polygons()
    if (length(ids) == 0) {
      showNotification("⚠️ No se seleccionó ningún lote.", type = "error", duration = 5)
      return()
    }
    
    fecha <- format(input$fecha_riego, "%Y-%m-%d")
    seleccion <- shapefile_reactivo() %>% filter(id %in% ids)
    
    for (i in 1:nrow(seleccion)) {
      db_execute("INSERT INTO lotes (poligono_id, Sector, Lote, OCUPACION, Sup, fecha)
                  VALUES (?, ?, ?, ?, ?, ?)",
                 params = list(
                   seleccion$id[i],
                   seleccion$Sector[i],
                   seleccion$Lote[i],
                   seleccion$OCUPACION[i],
                   seleccion$Sup[i],
                   fecha
                 ))
    }
    
    showNotification("✔️ Registro guardado correctamente", type = "message", duration = 5)
    selected_polygons(vector())
    leafletProxy("mapa") %>% clearGroup("seleccionados")
  })
  
  observeEvent(input$limpiar, {
    selected_polygons(vector())
    leafletProxy("mapa") %>% clearGroup("seleccionados")
  })
  
  output$mapa_visualizacion <- renderLeaflet({
    registros <- db_query(
      "SELECT poligono_id, fecha FROM lotes WHERE fecha BETWEEN ? AND ?",
      params = list(
        format(input$fecha_filtro[1], "%Y-%m-%d"),
        format(input$fecha_filtro[2], "%Y-%m-%d")
      )
    )
    
    datos_mapa <- shapefile_reactivo() %>%
      left_join(registros, by = c("id" = "poligono_id")) %>%
      mutate(
        fecha_numeric = as.numeric(as.Date(fecha)),
        label = ifelse(!is.na(fecha),
                       sprintf("Sector: %s<br>Lote: %s<br>Fecha: %s",
                               Sector, Lote, formatear_fecha(fecha)),
                       NA)
      )
    
    pal <- colorNumeric("RdYlGn", domain = datos_mapa$fecha_numeric, na.color = "#CCCCCC")
    
    leaflet(datos_mapa) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ifelse(is.na(fecha_numeric), "#CCCCCC", pal(fecha_numeric)),
        fillOpacity = ~ifelse(is.na(fecha_numeric), 0.2, 0.7),
        weight = 1,
        color = "#2C3E50",
        label = ~lapply(label, HTML)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = pal(sort(unique(na.omit(datos_mapa$fecha_numeric)))),
        labels = format(as.Date(sort(unique(na.omit(datos_mapa$fecha))), "%Y-%m-%d"), "%d/%m/%Y"),
        title = "Fecha de Riego"
      )
  })
  
  output$tabla_resumen <- renderTable({
    req(input$fecha_filtro)
    data <- db_query(
      "SELECT fecha, Sector, Lote, OCUPACION, Sup FROM lotes WHERE fecha BETWEEN ? AND ?",
      params = list(format(input$fecha_filtro[1], "%Y-%m-%d"),
                    format(input$fecha_filtro[2], "%Y-%m-%d"))
    )
    
    validate(need(nrow(data) > 0, "No hay datos en este rango de fechas."))
    
    data %>%
      mutate(
        Fecha = format(as.Date(fecha), "%d/%m/%Y"),
        Hectáreas = round(Sup, 2)
      ) %>%
      select(Fecha, Sector, Lote, Cultivo = OCUPACION, Hectáreas)
  })
  
  output$tabla_cultivos <- renderTable({
    req(input$fecha_filtro)
    data <- db_query(
      "SELECT OCUPACION, SUM(Sup) as Hectareas FROM lotes WHERE fecha BETWEEN ? AND ? GROUP BY OCUPACION",
      params = list(format(input$fecha_filtro[1], "%Y-%m-%d"),
                    format(input$fecha_filtro[2], "%Y-%m-%d"))
    )
    
    validate(need(nrow(data) > 0, "No hay datos en este rango de fechas."))
    
    data %>%
      mutate(
        Cultivo = as.character(OCUPACION),
        Hectáreas = round(Hectareas, 2)
      ) %>%
      select(Cultivo, Hectáreas)
  })
  
  output$descargar_excel <- downloadHandler(
    filename = function() paste0("reporte_riegos_", Sys.Date(), ".xlsx"),
    content = function(file) {
      datos <- db_query(
        "SELECT * FROM lotes WHERE fecha BETWEEN ? AND ?",
        params = list(format(input$fecha_filtro[1], "%Y-%m-%d"),
                      format(input$fecha_filtro[2], "%Y-%m-%d"))
      )
      write.xlsx(datos, file, asTable = TRUE)
    }
  )
  
  # ✅ Edición completa ordenada por fecha descendente
  datos_edicion <- reactiveVal()
  
  observe({
    data <- db_query("SELECT id, fecha, Lote, OCUPACION, Sup FROM lotes ORDER BY fecha DESC")
    datos_edicion(data)
  })
  
  output$tabla_editable <- renderRHandsontable({
    req(datos_edicion())
    rhandsontable(datos_edicion(), rowHeaders = NULL) %>%
      hot_col("fecha", type = "date") %>%
      hot_col("Lote", type = "text") %>%
      hot_col("OCUPACION", type = "text") %>%
      hot_col("Sup", type = "numeric")
  })
  
  observeEvent(input$guardar_ediciones, {
    data <- hot_to_r(input$tabla_editable)
    for (i in 1:nrow(data)) {
      db_execute("UPDATE lotes SET fecha = ?, Lote = ?, OCUPACION = ?, Sup = ? WHERE id = ?",
                 params = list(data$fecha[i], data$Lote[i], data$OCUPACION[i], data$Sup[i], data$id[i]))
    }
    showNotification("✔️ Registros actualizados correctamente", type = "message", duration = 4)
    datos_edicion(data)
  })
  
  observeEvent(input$nuevo_geojson, {
    req(input$nuevo_geojson)
    nuevo <- st_read(input$nuevo_geojson$datapath, quiet = TRUE)
    nuevo$id <- 1:nrow(nuevo)
    shapefile_reactivo(nuevo)
    showNotification("✔️ Lotes actualizados correctamente", type = "message", duration = 5)
  })
}

# 6. Ejecutar aplicación -------------------------------------------------------
shinyApp(ui, server)
