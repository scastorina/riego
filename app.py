import os
import io
import sqlite3
from pathlib import Path
from datetime import datetime

import pandas as pd
import geopandas as gpd
import streamlit as st
from streamlit_folium import st_folium
import folium

APP_DIR = Path(__file__).parent
DATA_GEOJSON = APP_DIR / "Lotes_geo.geojson"
DB_PATH = APP_DIR / "lotes.db"
MAX_AGE = 0

st.set_page_config(page_title="Lotes - Python App", layout="wide")


@st.cache_data
def load_geojson(path: Path, _mtime: float) -> gpd.GeoDataFrame:
    gdf = gpd.read_file(path)
    if "id" not in gdf.columns:
        gdf["id"] = range(1, len(gdf) + 1)
    return gdf


def ensure_db_schema(db_path: Path):
    con = sqlite3.connect(db_path)
    cur = con.cursor()
    cur.execute(
        """
        CREATE TABLE IF NOT EXISTS lotes (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            poligono_id INTEGER,
            Sector TEXT,
            Lote TEXT,
            OCUPACION TEXT,
            Sup REAL,
            fecha TEXT
        )
        """
    )
    con.commit()
    con.close()


def fetch_table() -> pd.DataFrame:
    con = sqlite3.connect(DB_PATH)
    df = pd.read_sql_query("SELECT * FROM lotes", con)
    con.close()
    df["fecha"] = pd.to_datetime(df["fecha"], errors="coerce").dt.date
    return df


def upsert_rows(df: pd.DataFrame):
    con = sqlite3.connect(DB_PATH)
    cur = con.cursor()
    cur.execute("DELETE FROM lotes")
    con.commit()
    df.to_sql("lotes", con, if_exists="append", index=False)
    con.close()


def export_excel(df: pd.DataFrame) -> bytes:
    output = io.BytesIO()
    with pd.ExcelWriter(output, engine="xlsxwriter") as writer:
        df.to_excel(writer, index=False, sheet_name="lotes")
    return output.getvalue()


def semaforo_color(fecha: pd.Timestamp | None):
    """Return fill color and opacity according to irrigation age."""
    if pd.isna(fecha):
        return "#cccccc", 0.2
    days = (datetime.today().date() - fecha).days
    ratio = min(days / MAX_AGE, 1.0) if MAX_AGE else 0
    brown = (165, 42, 42)
    blue = (0, 0, 255)
    r = int(brown[0] + (blue[0] - brown[0]) * ratio)
    g = int(brown[1] + (blue[1] - brown[1]) * ratio)
    b = int(brown[2] + (blue[2] - brown[2]) * ratio)
    color = f"#{r:02x}{g:02x}{b:02x}"
    opacity = 0.3 + 0.5 * (1 - ratio)
    return color, opacity


# --- Sidebar ---
st.sidebar.header("Opciones")
uploaded_geo = st.sidebar.file_uploader("Subir nuevo GeoJSON de Lotes", type=["geojson", "json"])
if uploaded_geo:
    DATA_GEOJSON.write_bytes(uploaded_geo.getbuffer())
    st.sidebar.success("GeoJSON actualizado. Recarga el mapa si no se ve.")

ensure_db_schema(DB_PATH)
gdf = load_geojson(DATA_GEOJSON, DATA_GEOJSON.stat().st_mtime)

df = fetch_table()
today = datetime.today().date()
df["antiguedad"] = df["fecha"].apply(lambda f: (today - f).days if pd.notnull(f) else None)
MAX_AGE = max((a for a in df["antiguedad"] if a is not None), default=1)
fecha_dict = df.set_index("poligono_id")["fecha"].to_dict()

st.title("Gestión de Lotes (Python + Streamlit)")
st.caption("Replica simple de la versión Shiny: mapa, edición de tabla y exportación.")

tab_gestion, tab_riego = st.tabs(["Lotes", "Estado de riego"])

with tab_gestion:
    col_map, col_table = st.columns([1.3, 1.0], gap="large")

    with col_map:
        st.subheader("Mapa de Lotes")
        bounds = gdf.to_crs(4326).total_bounds
        m = folium.Map(zoom_start=13, control_scale=True)
        m.fit_bounds([[bounds[1], bounds[0]], [bounds[3], bounds[2]]])
        folium.GeoJson(
            data=gdf.to_json(),
            name="Lotes",
            tooltip=folium.GeoJsonTooltip(fields=[c for c in gdf.columns if c != "geometry"][:8]),
            popup=folium.GeoJsonPopup(fields=[c for c in gdf.columns if c != "geometry"][:8]),
            style_function=lambda feat: {"fillColor": "#4A89DC", "color": "#34495E", "weight": 1, "fillOpacity": 0.6},
            highlight_function=lambda feat: {"color": "#E9573F", "weight": 3},
        ).add_to(m)
        folium.LayerControl().add_to(m)
        st_folium(m, width=None, height=600)

    with col_table:
        st.subheader("Edición de datos")
        if df.empty:
            df = pd.DataFrame([
                {
                    "id": None,
                    "poligono_id": 1,
                    "Sector": "",
                    "Lote": "",
                    "OCUPACION": "",
                    "Sup": None,
                    "fecha": datetime.today().date(),
                }
            ])

        edited = st.data_editor(
            df,
            num_rows="dynamic",
            use_container_width=True,
            hide_index=True,
            column_config={"fecha": st.column_config.DateColumn(format="YYYY-MM-DD")},
        )
        col1, col2, col3 = st.columns(3)
        with col1:
            if st.button("💾 Guardar cambios", type="primary"):
                edited["fecha"] = pd.to_datetime(edited["fecha"]).dt.strftime("%Y-%m-%d")
                upsert_rows(edited.fillna(""))
                st.success("Cambios guardados en la base SQLite.")
        with col2:
            if st.button("⬇️ Exportar a Excel"):
                xlsx = export_excel(edited)
                st.download_button(
                    "Descargar lotes.xlsx",
                    data=xlsx,
                    file_name="lotes.xlsx",
                    mime="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                )

        st.caption(
            "Consejo: use 'poligono_id' para relacionar filas con polígonos por su 'id' en el GeoJSON."
        )

with tab_riego:
    st.subheader("Estado de riego")
    bounds = gdf.to_crs(4326).total_bounds
    m_riego = folium.Map(zoom_start=13, control_scale=True)
    m_riego.fit_bounds([[bounds[1], bounds[0]], [bounds[3], bounds[2]]])

    def style_func(feat):
        pol_id = feat["properties"].get("id")
        fecha = fecha_dict.get(pol_id)
        color, opacity = semaforo_color(fecha)
        return {"fillColor": color, "color": "#34495E", "weight": 1, "fillOpacity": opacity}

    folium.GeoJson(
        data=gdf.to_json(),
        name="Riego",
        tooltip=folium.GeoJsonTooltip(fields=[c for c in gdf.columns if c != "geometry"][:8]),
        style_function=style_func,
        highlight_function=lambda feat: {"color": "#E9573F", "weight": 3},
    ).add_to(m_riego)
    folium.LayerControl().add_to(m_riego)
    st_folium(m_riego, width=None, height=600)
    st.dataframe(df[["poligono_id", "fecha", "antiguedad"]])

st.divider()
st.markdown("Hecho con **Streamlit**, **geopandas**, **folium**, **sqlite3**.")

