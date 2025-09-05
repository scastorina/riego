import base64
import io
import json
import os
import sqlite3
from datetime import date, datetime
from pathlib import Path

import geopandas as gpd
import pandas as pd
from dash import Dash, Input, Output, State, dcc, html, exceptions
import dash_leaflet as dl
import dash_table

APP_DIR = Path(__file__).parent
DATA_GEOJSON = APP_DIR / "Lotes_geo.geojson"
DB_PATH = APP_DIR / "lotes.db"


def ensure_db_schema(db_path: Path) -> None:
    """Create base tables if they don't exist."""
    con = sqlite3.connect(db_path)
    cur = con.cursor()
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
        """,
    )
    cur.execute(
        """
        CREATE TABLE IF NOT EXISTS riego_historial (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            poligono_id INTEGER NOT NULL,
            fecha_riego TEXT NOT NULL
        )
        """,
    )
    con.commit()
    con.close()


def load_geojson(path: Path) -> gpd.GeoDataFrame:
    gdf = gpd.read_file(path)
    if "id" not in gdf.columns:
        gdf["id"] = range(1, len(gdf) + 1)
    return gdf


def fetch_table() -> pd.DataFrame:
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


def insert_riego_rows(df_riego: pd.DataFrame) -> None:
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


def semaforo_color(fecha_val):
    if fecha_val is None or pd.isna(fecha_val):
        return "#FFFFFF"
    days = (date.today() - fecha_val).days
    if days <= 3:
        return "#4CAF50"
    if days <= 7:
        return "#FFEB3B"
    return "#F44336"


def build_geojson(selected_ids):
    if not DATA_GEOJSON.exists():
        return {}
    gdf = load_geojson(DATA_GEOJSON)
    df = fetch_table()
    latest = df.set_index("poligono_id")["fecha"].to_dict()
    features = json.loads(gdf.to_json())["features"]
    for feat in features:
        props = feat.setdefault("properties", {})
        pid = props.get("id")
        color = semaforo_color(latest.get(pid))
        if pid in selected_ids:
            color = "#00FF00"
        props["style"] = {
            "fillColor": color,
            "color": "#34495E",
            "weight": 1,
            "fillOpacity": 0.6,
        }
    return {"type": "FeatureCollection", "features": features}


ensure_db_schema(DB_PATH)
initial_geojson = build_geojson([])
if DATA_GEOJSON.exists():
    bounds = gpd.read_file(DATA_GEOJSON).total_bounds
    center = [(bounds[1] + bounds[3]) / 2, (bounds[0] + bounds[2]) / 2]
else:
    center = [0, 0]

app = Dash(__name__)
app.title = "Gestión de Lotes"

app.layout = html.Div([
    dcc.Store(id="selected-lots", data=[]),
    html.H1("Gestión de Lotes (Dash)"),
    dcc.Upload(
        id="upload-geojson",
        children=html.Button("Subir GeoJSON"),
        multiple=False,
    ),
    dl.Map(
        id="map", center=center, zoom=13, style={"height": "500px"},
        children=[
            dl.TileLayer(),
            dl.GeoJSON(id="geojson", data=initial_geojson, zoomToBounds=True),
        ],
    ),
    html.H2("Lotes seleccionados"),
    dash_table.DataTable(
        id="selected-table",
        columns=[
            {"name": "Lote", "id": "Lote"},
            {"name": "Chacra", "id": "Sector"},
            {"name": "Cobertura", "id": "OCUPACION"},
        ],
        data=[],
    ),
    dcc.DatePickerSingle(id="fecha-riego", date=date.today()),
    html.Button("Guardar riego", id="guardar"),
    html.Button("Descargar Excel", id="download-btn"),
    dcc.Download(id="download"),
])


@app.callback(
    Output("selected-lots", "data"),
    Input("geojson", "click_feature"),
    State("selected-lots", "data"),
    prevent_initial_call=True,
)
def on_feature_click(feature, selected):
    if not feature:
        return selected
    pid = feature.get("properties", {}).get("id")
    if pid in selected:
        return selected
    return selected + [pid]


@app.callback(
    Output("geojson", "data"),
    Output("selected-table", "data"),
    Input("selected-lots", "data"),
)
def update_selection(selected):
    geojson = build_geojson(selected)
    df = fetch_table()
    df_sel = df[df["poligono_id"].isin(selected)]
    table = df_sel[["Lote", "Sector", "OCUPACION"]].rename(
        columns={"Sector": "Sector", "OCUPACION": "OCUPACION"}
    )
    return geojson, table.to_dict("records")


@app.callback(
    Output("geojson", "data"),
    Output("map", "center"),
    Input("upload-geojson", "contents"),
    State("selected-lots", "data"),
    prevent_initial_call=True,
)
def on_upload(contents, selected):
    if contents is None:
        raise exceptions.PreventUpdate
    content_type, content_string = contents.split(",")
    decoded = base64.b64decode(content_string)
    DATA_GEOJSON.write_bytes(decoded)
    geojson = build_geojson(selected)
    gdf = load_geojson(DATA_GEOJSON)
    bounds = gdf.total_bounds
    center = [(bounds[1] + bounds[3]) / 2, (bounds[0] + bounds[2]) / 2]
    return geojson, center


@app.callback(
    Output("selected-lots", "data"),
    Input("guardar", "n_clicks"),
    State("selected-lots", "data"),
    State("fecha-riego", "date"),
    prevent_initial_call=True,
)
def on_save(n_clicks, selected, fecha):
    if not n_clicks or not selected:
        return selected
    to_insert = pd.DataFrame(
        [{"poligono_id": pid, "fecha": fecha} for pid in selected]
    )
    insert_riego_rows(to_insert)
    return []


@app.callback(
    Output("download", "data"),
    Input("download-btn", "n_clicks"),
    prevent_initial_call=True,
)
def on_download(n_clicks):
    df = fetch_table()
    return dcc.send_bytes(export_excel(df), "lotes.xlsx")


if __name__ == "__main__":
    port = int(os.environ.get("PORT", 8050))
    app.run_server(debug=True, host="0.0.0.0", port=port)
