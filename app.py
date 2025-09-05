import os
import io
import json
import sqlite3
from pathlib import Path
from datetime import datetime, date

import pandas as pd
import geopandas as gpd
import streamlit as st
from streamlit_folium import st_folium
import folium

APP_DIR = Path(__file__).parent
DATA_GEOJSON = APP_DIR / "Lotes_geo.geojson"
DB_PATH = APP_DIR / "lotes.db"

st.set_page_config(
    page_title="Lotes - Python App",
    layout="wide",
    initial_sidebar_state="collapsed",
)

@st.cache_data
def load_geojson(path: Path, _mtime: float) -> gpd.GeoDataFrame:
    gdf = gpd.read_file(path)
    if "id" not in gdf.columns:
        gdf["id"] = range(1, len(gdf) + 1)
    geojson = json.loads(gdf.to_json())
    for feature in geojson.get("features", []):
        props = feature.setdefault("properties", {})
        if "id" not in props and "id" in feature:
            props["id"] = feature["id"]
    gdf = gpd.GeoDataFrame.from_features(geojson["features"], crs=gdf.crs)
    return gdf

def ensure_db_schema(db_path: Path):
    """Create base tables if they don't exist."""
    con = sqlite3.connect(db_path)
    cur = con.cursor()
    # Tabla base de lotes
    cur.execute(
        """
        CREATE TABLE IF NOT EXISTS lotes (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            poligono_id INTEGER UNIQUE,
            Sector TEXT,
            Lote TEXT,
            OCUPACION TEXT,
            Sup REAL
        )
        """
    )
    # Historial de riegos
    cur.execute(
        """
        CREATE TABLE IF NOT EXISTS riego_historial (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            poligono_id INTEGER NOT NULL,
            fecha_riego TEXT NOT NULL
        )
        """
    )
    con.commit()
    con.close()

def fetch_table() -> pd.DataFrame:
    """Devuelve lotes con la última fecha de riego."""
    con = sqlite3.connect(DB_PATH)
    query = """
    WITH ultimos AS (
        SELECT poligono_id, MAX(fecha_riego) AS ultima_fecha
        FROM riego_historial
        GROUP BY poligono_id
    )
    SELECT
        l.id,
        l.poligono_id,
        l.Sector,
        l.Lote,
        l.OCUPACION,
        l.Sup,
        u.ultima_fecha AS fecha
    FROM lotes l
    LEFT JOIN ultimos u ON l.poligono_id = u.poligono_id
    ORDER BY l.poligono_id
    """
    df = pd.read_sql_query(query, con)
    con.close()
    df["fecha"] = pd.to_datetime(df["fecha"], errors="coerce").dt.date
    return df

def upsert_lotes(df_meta: pd.DataFrame):
    """Inserta o actualiza metadatos de lotes."""
    con = sqlite3.connect(DB_PATH)
    cur = con.cursor()
    for _, row in df_meta.iterrows():
        pol_id = row.get("poligono_id")
        if pd.isna(pol_id):
            continue
        cur.execute("SELECT id FROM lotes WHERE poligono_id = ?", (int(pol_id),))
        exists = cur.fetchone()
        if exists:
            cur.execute(
                """
                UPDATE lotes
                SET Sector = ?, Lote = ?, OCUPACION = ?, Sup = ?
                WHERE poligono_id = ?
                """,
                (row.get("Sector"), row.get("Lote"), row.get("OCUPACION"),
                 row.get("Sup"), int(pol_id)),
            )
        else:
            cur.execute(
                """
                INSERT INTO lotes (poligono_id, Sector, Lote, OCUPACION, Sup)
                VALUES (?, ?, ?, ?, ?)
                """,
                (int(pol_id), row.get("Sector"), row.get("Lote"),
                 row.get("OCUPACION"), row.get("Sup")),
            )
    con.commit()
    con.close()

def insert_riego_rows(df_riego: pd.DataFrame):
    """Inserta registros de riego en el historial."""
    con = sqlite3.connect(DB_PATH)
    cur = con.cursor()
    for _, row in df_riego.iterrows():
        pol_id = row.get("poligono_id")
        f = row.get("fecha")
        if pd.isna(pol_id) or pd.isna(f):
            continue
        if isinstance(f, (datetime, pd.Timestamp)):
            f_str = f.date().isoformat()
        elif isinstance(f, date):
            f_str = f.isoformat()
        else:
            f_str = str(pd.to_datetime(f, errors="coerce").date())
        cur.execute(
            "INSERT INTO riego_historial (poligono_id, fecha_riego) VALUES (?, ?)",
            (int(pol_id), f_str),
        )
    con.commit()
    con.close()

def export_excel(df: pd.DataFrame) -> bytes:
    output = io.BytesIO()
    with pd.ExcelWriter(output, engine="xlsxwriter") as writer:
        df.to_excel(writer, index=False, sheet_name="lotes")
    return output.getvalue()

# --- Sidebar ---
st.sidebar.header("Opciones")
uploaded_geo = st.sidebar.file_uploader("Subir nuevo GeoJSON de Lotes", type=["geojson", "json"])
if uploaded_geo:
    DATA_GEOJSON.write_bytes(uploaded_geo.getbuffer())
    st.sidebar.success("GeoJSON actualizado. Recargá el mapa si no se ve.")

ensure_db_schema(DB_PATH)

if not DATA_GEOJSON.exists():
    st.warning("No encuentro 'Lotes_geo.geojson'. Subí uno desde la barra lateral para continuar.")
    gdf = gpd.GeoDataFrame({"id": [], "geometry": []}, geometry="geometry", crs="EPSG:4326")
else:
    gdf = load_geojson(DATA_GEOJSON, DATA_GEOJSON.stat().st_mtime)

df = fetch_table()
latest_by_poligono = df.set_index("poligono_id")["fecha"].to_dict()

def semaforo_color(fecha_val):
    if fecha_val is None or pd.isna(fecha_val):
        return "#FFFFFF"
    days = (datetime.today().date() - fecha_val).days
    if days <= 3:
        return "#4CAF50"
    if days <= 7:
        return "#FFEB3B"
    return "#F44336"

if "lotes_seleccionados" not in st.session_state:
    st.session_state["lotes_seleccionados"] = []

def lote_style(feat):
    """Estilo del polígono considerando la selección."""
    props = feat.get("properties", {})
    pid_val = props.get("id") or props.get("Lote")
    try:
        pid = int(pid_val)
    except (TypeError, ValueError):
        pid = None
    if pid is not None and pid in st.session_state.get("lotes_seleccionados", []):
        return {
            "fillColor": "#00FF00",
            "color": "#34495E",
            "weight": 2,
            "fillOpacity": 0.7,
        }
    return {
        "fillColor": semaforo_color(latest_by_poligono.get(pid)),
        "color": "#34495E",
        "weight": 1,
        "fillOpacity": 0.6,
    }

st.title("Gestión de Lotes (Python + Streamlit)")
st.caption("Replica simple de la versión Shiny: mapa, selección y registro de riego.")

st.subheader("Mapa de Lotes")
if not gdf.empty:
    bounds = gdf.to_crs(4326).total_bounds
    m = folium.Map(zoom_start=13, control_scale=True)
    m.fit_bounds([[bounds[1], bounds[0]], [bounds[3], bounds[2]]])

    # Capa satelital opcional + control de capas colapsado
    folium.TileLayer("Esri.WorldImagery", name="Satélite", overlay=False).add_to(m)

    folium.GeoJson(
        data=gdf.to_json(),
        name="Lotes",
        tooltip=folium.GeoJsonTooltip(fields=[c for c in gdf.columns if c != "geometry"][:8]),
        popup=folium.GeoJsonPopup(fields=[c for c in gdf.columns if c != "geometry"][:8]),
        style_function=lote_style,
        highlight_function=lambda feat: {"color": "#E9573F", "weight": 3},
    ).add_to(m)

    folium.LayerControl(collapsed=True).add_to(m)
    map_state = st_folium(m, use_container_width=True, height=800)

    # Captura robusta del clic según versión de streamlit-folium
    poligono_id = None
    if map_state:
        obj = None
        if map_state.get("last_object_clicked"):
            obj = map_state["last_object_clicked"]
        elif map_state.get("last_active_drawing"):
            obj = map_state["last_active_drawing"]
        if obj:
            props = obj.get("properties", {})
            pid_val = obj.get("id") or props.get("id") or props.get("Lote")
            if pid_val is not None:
                try:
                    poligono_id = int(pid_val)
                except (TypeError, ValueError):
                    poligono_id = None

    if poligono_id is not None and poligono_id not in st.session_state["lotes_seleccionados"]:
        st.session_state["lotes_seleccionados"].append(poligono_id)
        st.rerun()
else:
    st.info("Subí un GeoJSON para ver el mapa.")
    map_state = None

st.subheader("Lotes seleccionados")
df_sel = df[df["poligono_id"].isin([int(i) for i in st.session_state["lotes_seleccionados"]])]
if not df_sel.empty:
    st.table(
        df_sel[["Lote", "Sector", "OCUPACION"]]
        .rename(columns={"Sector": "Chacra", "OCUPACION": "Cobertura"})
    )
else:
    st.write("No hay lotes seleccionados.")

fecha_riego = st.date_input("Fecha de riego", datetime.today().date())
if st.button("Guardar riego de seleccionados") and st.session_state["lotes_seleccionados"]:
    to_insert = pd.DataFrame(
        [{"poligono_id": pid, "fecha": fecha_riego} for pid in st.session_state["lotes_seleccionados"]]
    )
    insert_riego_rows(to_insert)
    st.success("Riego guardado en historial.")
    st.session_state["lotes_seleccionados"] = []
    st.rerun()

st.divider()
st.markdown("Hecho con **Streamlit**, **geopandas**, **folium**, **sqlite3** 🌱")
