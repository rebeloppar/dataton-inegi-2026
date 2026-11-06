# Datos

Los datos crudos **no se suben al repositorio** (son públicos y pesados). Se descargan del INEGI (datos abiertos, CSV) y se colocan aquí con esta estructura:

```
datos/
└── endutih_2025/
    ├── conjunto_de_datos/      ← los 5 CSV (usuarios, usuarios2, residentes, hogares, viviendas)
    ├── diccionario_de_datos/
    ├── metadatos/
    └── modelo_entidad_relacion/
```

## ENDUTIH 2025

1. En la página de la ENDUTIH 2025 del INEGI, pestaña **Datos abiertos**, descarga el paquete en **CSV** (no el DBF de "Microdatos").
2. Descomprímelo y renombra la carpeta a `endutih_2025`.
3. Muévela dentro de `datos/`.

## ENDUTIH 2015 a 2024 (serie histórica)

1. En la página de cada año de la ENDUTIH (2015 a 2024), pestaña **Datos abiertos**, descarga el paquete en **CSV**.
2. Descomprime cada uno en `datos/endutih_AAAA/` (por ejemplo, `datos/endutih_2019/`), sin cambiar las carpetas de adentro.
3. Desde la raíz del repositorio corre `python3 analisis/C_territorio/00_extrae_historicos.py`: junta solo las columnas necesarias en `datos/procesados/endutih_2015_2024_compra.csv`.
4. Luego `analisis/C_territorio/03_serie_historica.R`.
5. Para la tendencia de categorías (frente A): `python3 analisis/A_conexion/00_extrae_categorias.py` (usa 2022 a 2025) y luego `analisis/A_conexion/04_tendencia_categorias.R`.

## ENIGH 2024 (dónde compran los hogares, decisión 98)

1. En la página de la ENIGH 2024 del INEGI, **Datos abiertos**, descarga el paquete en **CSV** y descomprímelo en `datos/enigh_2024/`, sin cambiar las carpetas de adentro (`conjunto_de_datos_gastoshogar_enigh2024_ns/`, `conjunto_de_datos_concentradohogar_enigh2024_ns/`, …).
2. Desde la raíz del repositorio corre `python3 analisis/C_territorio/00_extrae_enigh.py`: suma el gasto de cada hogar por lugar de compra en `datos/procesados/enigh_2024_lugar_compra.csv` (unos 20 segundos).
3. Luego `analisis/C_territorio/04_enigh_lugar_compra.R`.
