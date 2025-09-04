import os
import io
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

st.set_page_config(page_title="Lotes - Python App", layout="wide")

@st.cache_data
def load_geojson(path: Path, _mtime: float) -> gpd.GeoDataFrame:
    gdf = gpd.read_file(path)
    if "id" not in gdf.columns:
        gdf["id"] = range(1, len(gdf) + 1)
    return gdf

def ensure_db_schema(db_path: Path):
    """Create base tables if they don't exist."""
    con = sqlite3.connect(db_path)
    cur = con.cursor()
    # Tabla base de lotes (metadatos del polígono)
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
    # Historial de riegos (múltiples entradas por polígono)
    cur.execute(
        """
        CREATE TABLE IF NOT EXISTS riego_historial (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            poligono_id INTEGER NOT NULL,
            fecha_riego TEXT NOT NULL
        )
        """
    )
    # Indices útiles
    cur.execute("CREATE INDEX IF NOT EXISTS idx_riego_poligono ON riego_historial(poligono_id)")
    cur.execute("CREATE INDEX IF NOT EXISTS idx_riego_fecha ON riego_historial(fecha_riego)")
    con.commit()
    con.close()

def fetch_table() -> pd.DataFrame:
    """
    Devuelve los lotes con la última fecha de riego (si existe) para mostrar/editar.
    """
    con = sqlite3.connect(DB_PATH)
    query = """
    WITH ultimos AS (
        SELECT poligono_id, MAX(fecha_riego) AS ultima_fecha
        FROM riego_historial
        GROUP BY poligono
