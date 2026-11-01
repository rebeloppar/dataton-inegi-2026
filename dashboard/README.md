# Dashboard

App de Shiny: una portada y las 5 secciones del storyboard. Cada sección: afirmación → gráficas → «Lo que encontramos» → «Cómo se calculó». Dueña: Rebeca.

## Correrlo en tu computadora

**Necesitas:** R 4.1 o más reciente y RStudio. No hace falta descargar las bases del INEGI: el dashboard solo lee los CSV de `dashboard/datos/`, que ya vienen en el repositorio.

1. En GitHub Desktop, haz *pull* para tener la última versión.
2. Abre `dataton-inegi-2026.Rproj` en RStudio.
3. Una sola vez, en la consola: `install.packages(c("shiny", "bslib", "ggplot2"))`
4. Cada vez: `shiny::runApp("dashboard")`. Se abre en una ventana o en el navegador.

Para cerrarlo, presiona *Stop* en la consola (o Esc).

**Si algo falla:**

- *«there is no package called…»*: falta instalar el paquete del paso 3.
- *«cannot open file 'dashboard/datos/…'»*: abriste RStudio sin el `.Rproj`; ciérralo y vuelve a entrar por el paso 2.
- *Una gráfica dice «Error» en rojo*: alguien subió un CSV con columnas distintas a las de la tabla de abajo. Revisa el último *commit* de ese archivo.
- *Sin internet, la letra se ve distinta*: es normal; la tipografía se descarga de Google y, sin conexión, se usa la del sistema.

**Si trabajas en Python (Santiago):** para ver el dashboard necesitas R y RStudio instalados (solo los pasos 1 a 4; no hay que programar en R). Para *entregar* tus cifras no hace falta R: escribe tu CSV desde Python con las columnas de la tabla de abajo (`df.to_csv(..., index=False, encoding="utf-8")`) y pide a alguien que lo revise en el dashboard.

Probado con R 4.3, shiny 1.8, bslib 0.6 y ggplot2 3.4; funciona también con versiones más recientes. Mientras quede algún archivo de prueba, arriba aparece la insignia «Datos de prueba».

## Cifras reales que ya están

La regresión (sección 2) y el % que compra por entidad (mapa) ya son reales. Para regenerarlos: corre `analisis/diagnostico/02_cruces.R` y `04_regresion.R`, y luego `05_exportar_dashboard.R`.

## Cómo entregar tus cifras (cada frente)

Cada gráfica lee **un CSV** de `dashboard/datos/`. Hoy son datos de prueba hechos por `crear_datos_prueba.R`. Para poner las cifras finales:

1. En **tu** script de análisis, al final, escribe el CSV directamente en `dashboard/datos/` (decisión 55; `resultados/` no se sube y el dashboard no lo ve), con **exactamente las mismas columnas** que el de prueba (mismo nombre de archivo, mismos nombres de columna, mismas categorías).
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
| `s2_efectos.csv` **(real)** | A, B | `factor, orden_factor, etiqueta, estimacion, li90, ls90, prueba` | Efectos marginales del modelo completo, en pp. Lo escribe `analisis/diagnostico/05_exportar_dashboard.R` |
| `s2_bloques.csv` **(real)** | A, B | `brecha, paso, nombre_paso, estimacion, li90, ls90, prueba` | La regresión paso a paso (4 bloques). Mismo script |
| `s2_robustez.csv` **(real)** | A, B | `etiqueta, version, estimacion, prueba` | Modelo completo: todo el país vs. solo ciudades de 100 mil+. Mismo script |
| `s2_perfiles.csv` | A | `perfil, estimacion, li90, ls90, prueba` | Probabilidad predicha en % |
| `s3_razones.csv` | B | `grupo_var, grupo, orden, razon, estimacion, li90, ls90, prueba` | % de quienes no compraron que da cada razón |
| `s3_problemas.csv` | B | `grupo_var, grupo, orden, problema, estimacion, li90, ls90, prueba` | P5. Incluir la fila «Algún problema». % de compradores |
| `s4_entidades.csv` **(real)** | C | `cve_ent, estimacion, li90, ls90, prueba` | `cve_ent` como número (1 a 32). En % |
| `s4_regiones.csv` | C | `region, referencia, tipo, estimacion, li90, ls90, prueba` | `tipo`: Bruta o Ajustada. Diferencia frente a la región de referencia, en pp. Regiones del CEEY: Norte, Norte-occidente, Centro-norte, Centro, Sur (variable `REGION` de `R/prep_endutih.R`) |
| `s5_escenarios.csv` | C | `anio, escenario, estimacion, li90, ls90, prueba` | `escenario`: Observado, Inercia, Solo tiendas, Solo habilidades, Puerta abierta (los mismos de `s5_palancas.csv`, decisión 60). Observado sin intervalo (NA) |
| `s1_tendencia.csv` *(opcional)* | A | `categoria, anio, estimacion, prueba` | P3 en el tiempo, con tabulados publicados |
| `s2_hogar.csv` *(opcional)* | A | `composicion, orden, sexo, estimacion, li90, ls90, prueba` | P2: composición del hogar, por sexo |
| `s4_ritmo.csv` *(opcional)* | C | `cve_ent, inicio, fin, anio_inicio, anio_fin, prueba` | P6: % que compra en dos años |
| `s4_comercios.csv` *(opcional)* | C | `cve_ent, comercios_10mil, pct_compra, prueba` | P6: DENUE + EIC |
| `s5_mercado.csv` | C | `escenario, orden, minimo, maximo, prueba` | P7: rango de gasto digitalizable, miles de millones de pesos |
| `s5_simulador.csv` | D | `grupo, millones, pct_compra, pct_no_sabe, pct_en_persona, prueba` | Insumos del simulador; `pct_no_sabe` y `pct_en_persona` son % de quienes no compran |

**Opcionales:** si el archivo no existe, su pestaña muestra «Pendiente» en lugar de la gráfica. Cada gráfica tiene un enlace «Descargar datos (CSV)».

Archivos de texto y fijos (no son de prueba):

- `hallazgos.csv`: los recuadros «Lo que encontramos» (`seccion, orden, texto`). Rebeca los mantiene; cada frente propone sus 2 o 3 hallazgos con cifra.
- `propuesta.csv`: los campos de la propuesta en la sección 5 (`campo, texto`).
- `estrategias.csv`: estrategias públicas y privadas (`tipo, actor, accion, por_que`).
- `hallazgos.csv` lleva además una columna `cifra` (la cifra que se destaca a la izquierda; puede ir vacía).
- `como_se_calculo.csv`: las tarjetas «Cómo se calculó».
- `entidades.csv`: nombre, abreviatura, región del CEEY (decisión 62) y punto central de cada entidad.
- `mapa_estados.csv`: contornos simplificados de las 32 entidades (para dibujar el mapa sin paquetes geográficos).

## Por qué así

- **Un CSV por gráfica:** cada frente es dueño de su archivo y nadie toca el código de la app.
- **`dashboard/datos/` sí se sube** al repositorio (a diferencia de `resultados/`): la app publicada necesita los datos.
- **El mapa se dibuja con contornos guardados en un CSV** (`mapa_estados.csv`), así nadie necesita instalar paquetes geográficos. Pasar el cursor sobre una entidad muestra su cifra.
