# Dashboard

App de Shiny: una portada y las 5 secciones del storyboard. Cada sección: afirmación → gráficas → «Lo que encontramos» → «Cómo se calculó». Dueña: Rebeca.

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
| `s1_embudo.csv` | A | `grupo_var, grupo, orden, eslabon, paso, estimacion, li90, ls90, prueba` | `grupo_var`: Nacional, Escolaridad, Edad, Sexo, Localidad, Estrato, Región. `orden`: orden de los grupos. `eslabon`: Hogar con internet, Usa internet, Compra en línea. En % |
| `s1_categorias.csv` | A | `grupo_var, grupo, orden, categoria, estimacion, li90, ls90, prueba` | P3: % de compradores que compró cada categoría |
| `s1_brechas_eslabon.csv` | A | `comparacion, eslabon, estimacion, li90, ls90, prueba` | `eslabon`: Usar internet o Comprar en línea. Brecha con controles, en pp |
| `s2_brechas.csv` | A, B | `factor, comparacion, tipo, estimacion, li90, ls90, prueba` | `tipo`: Bruta o Ajustada. En puntos porcentuales (pueden ser negativos) |
| `s2_perfiles.csv` | A | `perfil, estimacion, li90, ls90, prueba` | Probabilidad predicha en % |
| `s3_razones.csv` | B | `grupo_var, grupo, orden, razon, estimacion, li90, ls90, prueba` | % de quienes no compraron que da cada razón |
| `s3_problemas.csv` | B | `grupo_var, grupo, orden, problema, estimacion, li90, ls90, prueba` | P5. Incluir la fila «Algún problema». % de compradores |
| `s4_entidades.csv` | C | `cve_ent, estimacion, li90, ls90, prueba` | `cve_ent` como número (1 a 32). En % |
| `s4_regiones.csv` | C | `region, referencia, tipo, estimacion, li90, ls90, prueba` | `tipo`: Bruta o Ajustada. Diferencia frente a la región de referencia, en pp |
| `s5_escenarios.csv` | C | `anio, escenario, estimacion, li90, ls90, prueba` | `escenario`: Observado, Inercial, Habilidades, Omnicanal. Observado sin intervalo (NA) |
| `s5_simulador.csv` | D | `grupo, millones, pct_compra, pct_no_sabe, pct_en_persona, prueba` | Insumos del simulador; `pct_no_sabe` y `pct_en_persona` son % de quienes no compran |

Archivos de texto y fijos (no son de prueba):

- `hallazgos.csv`: los recuadros «Lo que encontramos» (`seccion, orden, texto`). Rebeca los mantiene; cada frente propone sus 2 o 3 hallazgos con cifra.
- `propuesta.csv`: los campos de la propuesta en la sección 5 (`campo, texto`).
- `como_se_calculo.csv`: las tarjetas «Cómo se calculó».
- `entidades_mosaico.csv`: posición de cada entidad en el mapa de mosaico y región provisional.

## Por qué así

- **Un CSV por gráfica:** cada frente es dueño de su archivo y nadie toca el código de la app.
- **`dashboard/datos/` sí se sube** al repositorio (a diferencia de `resultados/`): la app publicada necesita los datos.
- **Mapa de mosaico** (un cuadro por entidad) en lugar de un mapa con fronteras: no depende de archivos geográficos y todas las entidades se ven del mismo tamaño, incluidas las pequeñas como CDMX y Tlaxcala.
