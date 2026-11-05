# =============================================================================
# Frente C — Extrae de los microdatos de la ENDUTIH 2015-2024 (INEGI) solo las
# columnas que usa 03_serie_historica.R y las junta en un archivo:
#   datos/procesados/endutih_2015_2024_compra.csv
# Uso (desde la raíz del repositorio): python3 analisis/C_territorio/00_extrae_historicos.py
# Necesita las carpetas datos/endutih_AAAA/ tal como las descarga el INEGI
# (Datos abiertos, CSV). 2015: edad, sexo y nivel salen de la tabla de residentes.
# =============================================================================
import csv, glob, os

base = os.path.join("datos")
compra_var = {2015: "P7_10", 2016: "P7_10", 2017: "P7_19", 2018: "P7_19", 2019: "P7_19", 2020: "P7_19",
              2021: "P7_19", 2022: "P7_19", 2023: "P7_21", 2024: "P7_21"}

def abre(p):
    return csv.DictReader(open(p, encoding="utf-8-sig", errors="replace", newline=""))

def g(r, *ks):
    for k in ks:
        if k in r and r[k] is not None:
            return r[k].strip()
    return ""

os.makedirs(os.path.join(base, "procesados"), exist_ok=True)
out = csv.writer(open(os.path.join(base, "procesados", "endutih_2015_2024_compra.csv"), "w", newline=""))
out.writerow(["anio", "ent", "edad", "sexo", "nivel", "tloc", "estrato", "usa", "compra", "fac", "upm", "est"])
for y in range(2015, 2025):
    f = [f for f in glob.glob(f"{base}/endutih_{y}/conj*/tr_endutih_usuario*_anual_{y}.csv")
         if "usuario2" not in f and "usuarios2" not in f][0]
    res = {}
    if y == 2015:
        for r in abre(glob.glob(f"{base}/endutih_2015/conj*/tr_endutih_residente_anual_2015.csv")[0]):
            res[(r["UPM"], r["VIV_SEL"], r["HOGAR"], r["NREN"])] = r
    n = 0
    for r in abre(f):
        if y == 2015:
            rr = res.get((r["UPM"], r["VIV_SEL"], r["HOGAR"], r["NRENELEGI"]), {})
            edad, sexo, nivel = g(rr, "EDAD"), g(rr, "SEXO"), g(rr, "NIVEL")
        else:
            edad, sexo, nivel = g(r, "EDAD"), g(r, "SEXO"), g(r, "NIVEL")
        upm = g(r, "UPM_DIS", "UPM"); upm_geo = g(r, "UPM")
        ent = g(r, "ENT", "CVE_ENT") or (upm_geo[:2] if upm_geo else "")   # 2019: entidad = primeros 2 dígitos de la UPM
        out.writerow([y, ent, edad, sexo, nivel, g(r, "TLOC"), g(r, "ESTRATO"), g(r, "P7_1"),
                      g(r, compra_var[y]), g(r, "FAC_PER"), upm, g(r, "EST_DIS")])
        n += 1
    print(y, os.path.basename(f), n, flush=True)
