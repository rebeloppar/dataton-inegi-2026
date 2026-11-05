# =============================================================================
# Frente A — Extrae de los microdatos de la ENDUTIH 2022-2025 (INEGI) lo que usa
# 05_tendencia_categorias.R: quién compró en línea y qué categorías compró.
#   datos/procesados/endutih_2022_2025_categorias.csv
# Uso (desde la raíz del repositorio): python3 analisis/A_conexion/00_extrae_categorias.py
# Necesita las carpetas datos/endutih_AAAA/ tal como las descarga el INEGI.
# Las cuatro categorías tienen la misma redacción los cuatro años; en 2022 la
# pregunta es P7_22 y no tiene «alojamiento», así que la numeración va uno abajo.
# Uso e higiene personal = higiene, belleza y cosméticos + uso personal (ropa),
# igual que la gráfica 9 del INEGI.
# =============================================================================
import csv, glob, os

base = "datos"
compra_var = {2022: "P7_19", 2023: "P7_21", 2024: "P7_21", 2025: "P7_21"}
categorias = {  # categoría: variables por año
    "higiene": {2022: ["P7_22_10", "P7_22_11"], "resto": ["P7_24_11", "P7_24_12"]},
    "hogar":   {2022: ["P7_22_12"], "resto": ["P7_24_13"]},
    "alimentos": {2022: ["P7_22_6"], "resto": ["P7_24_6"]},
    "salud":   {2022: ["P7_22_16"], "resto": ["P7_24_17"]},
}

def abre(p):
    return csv.DictReader(open(p, encoding="utf-8-sig", errors="replace", newline=""))

os.makedirs(os.path.join(base, "procesados"), exist_ok=True)
out = csv.writer(open(os.path.join(base, "procesados", "endutih_2022_2025_categorias.csv"), "w", newline=""))
out.writerow(["anio", "edad", "usa", "compra"] + list(categorias) + ["fac", "upm", "est"])
for y in range(2022, 2026):
    f = [f for f in glob.glob(f"{base}/endutih_{y}/conj*/tr_endutih_usuario*_anual_{y}.csv")
         if "usuario2" not in f and "usuarios2" not in f][0]
    n = 0
    for r in abre(f):
        fila = [y, r["EDAD"].strip(), r["P7_1"].strip(), r[compra_var[y]].strip()]
        for c, v in categorias.items():
            vs = v.get(y, v["resto"])
            fila.append(int(any(r[x].strip() == "1" for x in vs)))
        fila += [r["FAC_PER"].strip(), r["UPM_DIS"].strip(), r["EST_DIS"].strip()]
        out.writerow(fila)
        n += 1
    print(y, os.path.basename(f), n, flush=True)
