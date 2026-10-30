# Dashboard

App de Shiny con las 5 secciones del storyboard. Dueña: Rebeca.

## Correrlo

1. Abre `dataton-inegi-2026.Rproj` en RStudio.
2. Una sola vez: `install.packages(c("shiny", "bslib", "ggplot2"))`
3. `shiny::runApp("dashboard")`

Mientras quede algún archivo de prueba, arriba aparece un aviso amarillo.

## Cómo entregar tus cifras (cada frente)

Cada gráfica lee **un CSV** de `dashboard/datos/`. Hoy son datos de prueba hechos por `crear_datos_prueba.R`. Para poner las cifras finales:

1. En **tu** script de análisis, al final, escribe el CSV con **exactamente las mismas columnas** que el de prueba (mismo nombre de archivo, mismos nombres de columna, mismas categorías).
2. Pon `prueba = FALSE`.
3. Usa `write.csv(..., row.names = FALSE, fileEncoding = "UTF-8")`.
4. Actualiza tu fila en `como_se_calculo.csv` (fuente, universo, tipo, script, nota).
5. Corre la app para verificar y haz *commit* y *push*.

Si necesitas una columna o categoría nueva, avisa a Rebeca: cambia la gráfica, no el CSV a escondidas.

| Archivo | Frente | Columnas | Notas |
|---|---|---|---|
| `s1_embudo.csv` | A | `grupo_var, grupo, orden, eslabon, paso, estimacion, li90, ls90, prueba` | `grupo_var`: Nacional, Escolaridad, Edad, Sexo, Localidad. `orden`: orden de los grupos. `eslabon`: Hogar con internet, Usa internet, Compra en línea. En % |
| `s2_brechas.csv` | A, B | `factor, comparacion, tipo, estimacion, li90, ls90, prueba` | `tipo`: Bruta o Ajustada. En puntos porcentuales (pueden ser negativos) |
| `s2_perfiles.csv` | A | `perfil, estimacion, li90, ls90, prueba` | Probabilidad predicha en % |
| `s3_razones.csv` | B | `grupo_var, grupo, orden, razon, estimacion, li90, ls90, prueba` | % de quienes no compraron que da cada razón |
| `s4_entidades.csv` | C | `cve_ent, estimacion, li90, ls90, prueba` | `cve_ent` como número (1 a 32). En % |
| `s5_escenarios.csv` | C | `anio, escenario, estimacion, li90, ls90, prueba` | `escenario`: Observado, Inercial, Habilidades, Omnicanal. Observado sin intervalo (NA) |
| `s5_simulador.csv` | D | `grupo, millones, pct_compra, pct_no_sabe, pct_en_persona, prueba` | Insumos del simulador; `pct_no_sabe` y `pct_en_persona` son % de quienes no compran |

Archivos fijos (no son de prueba): `entidades_mosaico.csv` (posición de cada entidad en el mapa de mosaico y región provisional) y `como_se_calculo.csv` (tarjetas "Cómo se calculó").

## Por qué así

- **Un CSV por gráfica:** cada frente es dueño de su archivo y nadie toca el código de la app.
- **`dashboard/datos/` sí se sube** al repositorio (a diferencia de `resultados/`): la app publicada necesita los datos.
- **Mapa de mosaico** (un cuadro por entidad) en lugar de un mapa con fronteras: no depende de archivos geográficos y todas las entidades se ven del mismo tamaño, incluidas las pequeñas como CDMX y Tlaxcala.
