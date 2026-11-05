# Datos

Los datos crudos **no se suben al repositorio** (son públicos y pesados). Cada quien los descarga y los coloca aquí con esta estructura exacta:

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

## Otras fuentes

Se agregan aquí cuando el frente que las use las incorpore (ENIGH 2024, EIC 2025, DENUE…), con su propia sección en este archivo y su propio `R/prep_<fuente>.R`.

## ENDUTIH 2015 a 2024 (serie histórica, frente C)

1. En la página de cada año de la ENDUTIH (2015 a 2024), pestaña **Datos abiertos**, descarga el paquete en **CSV**.
2. Descomprime cada uno en `datos/endutih_AAAA/` (por ejemplo, `datos/endutih_2019/`), sin cambiar las carpetas de adentro.
3. Desde la raíz del repositorio corre `python3 analisis/C_territorio/00_extrae_historicos.py`: junta solo las columnas necesarias en `datos/procesados/endutih_2015_2024_compra.csv`.
4. Luego `analisis/C_territorio/03_serie_historica.R`.
