# =============================================================================
# Frente C — Extrae de la ENIGH 2024 (INEGI) dónde compran los hogares (decisión 98)
# Una fila por hogar: gasto monetario trimestral del hogar (tipo_gasto G1) según
# el lugar de compra, agrupado en:
#   antad      = supermercados (06), tiendas departamentales (07),
#                tiendas con membresía (09)
#   conveniencia = tiendas de conveniencia (10): también son de la ANTAD, pero
#                se reportan aparte (comprar un refresco no es el kiosco con asesor)
#   tradicional = mercado (01), tianguis (02), ambulantes (03), abarrotes (04)
#   internet   = internet (18)
#   otro       = el resto de los lugares con lugar de compra (05, 08, 11-17)
# Solo cuenta gasto con lugar de compra (lugar_comp distinto de 00 «no aplica»:
# los servicios como luz o renta no tienen lugar de compra).
# Se une con concentradohogar: escolaridad, sexo y edad de la jefa o jefe,
# tamaño de localidad, estrato y diseño muestral.
#   datos/procesados/enigh_2024_lugar_compra.csv
# Uso (desde la raíz del repositorio): python3 analisis/C_territorio/00_extrae_enigh.py
# Necesita datos/enigh_2024/ tal como lo descarga el INEGI (datos abiertos, CSV).
# =============================================================================
import csv, glob, os
from collections import defaultdict

base = os.path.join("datos", "enigh_2024")
grupo = {"06": "antad", "07": "antad", "09": "antad", "10": "conveniencia",
         "01": "tradicional", "02": "tradicional", "03": "tradicional", "04": "tradicional",
         "18": "internet"}
lugares = ["antad", "conveniencia", "tradicional", "internet", "otro"]

def tabla(nombre):
    return glob.glob(os.path.join(base, f"conjunto_de_datos_{nombre}_enigh2024_ns",
                                  "conjunto_de_datos", "*.csv"))[0]

gasto = defaultdict(lambda: dict.fromkeys(lugares, 0.0))
n = 0
with open(tabla("gastoshogar"), encoding="utf-8-sig", newline="") as f:
    for r in csv.DictReader(f):
        lc = r["lugar_comp"].strip()
        if r["tipo_gasto"].strip() != "G1" or lc in ("", "00"):
            continue
        try:
            g = float(r["gasto_tri"])
        except ValueError:
            continue
        gasto[(r["folioviv"], r["foliohog"])][grupo.get(lc, "otro")] += g
        n += 1
print("gastos con lugar de compra:", n, "| hogares con alguno:", len(gasto), flush=True)

campos = ["folioviv", "foliohog", "educa_jefe", "sexo_jefe", "edad_jefe", "tam_loc", "est_socio",
          "ing_cor", "est_dis", "upm", "factor"]
os.makedirs(os.path.join("datos", "procesados"), exist_ok=True)
salida = os.path.join("datos", "procesados", "enigh_2024_lugar_compra.csv")
k = 0
with open(tabla("concentradohogar"), encoding="utf-8-sig", newline="") as f, \
     open(salida, "w", newline="") as o:
    w = csv.writer(o)
    w.writerow(campos + ["gasto_" + l for l in lugares])
    for r in csv.DictReader(f):
        g = gasto.get((r["folioviv"], r["foliohog"]), dict.fromkeys(lugares, 0.0))
        w.writerow([r[c].strip() for c in campos] + [round(g[l], 2) for l in lugares])
        k += 1
print("hogares:", k, "->", salida)
