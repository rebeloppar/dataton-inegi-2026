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

La regresión (sección 2), el % que compra por entidad (mapa), el embudo, las brechas por eslabón, el internet en casa y las categorías (sección 1, `analisis/A_conexion/`) ya son reales. Para regenerarlos: corre `analisis/diagnostico/02_cruces.R` y `04_regresion.R`, y luego `05_exportar_dashboard.R`.

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
| `s1_embudo.csv` **(real)** | A | `grupo_var, grupo, orden, eslabon, paso, estimacion, li90, ls90, prueba` | `grupo_var`: Nacional, Escolaridad, Edad, Sexo, Localidad, Estrato, Región. `orden`: orden de los grupos. `eslabon`: Hogar con internet, Usa internet, Compra en línea. En % |
| `s1_categorias.csv` **(real)** | A | `grupo_var, grupo, orden, categoria, estimacion, li90, ls90, prueba` | P3: % de compradores que compró cada categoría |
| `s1_brechas_eslabon.csv` **(real)** | A | `comparacion, eslabon, estimacion, li90, ls90, prueba` | `eslabon`: Usar internet o Comprar en línea. Brecha con controles, en pp |
| `s2_efectos.csv` **(real)** | A, B | `factor, orden_factor, etiqueta, estimacion, li90, ls90, prueba` | Efectos marginales del modelo completo, en pp. Lo escribe `analisis/diagnostico/05_exportar_dashboard.R` |
| `s2_bloques.csv` **(real)** | A, B | `brecha, paso, nombre_paso, estimacion, li90, ls90, prueba` | La regresión paso a paso (4 bloques). Mismo script |
| `s2_robustez.csv` **(real)** | A, B | `etiqueta, version, estimacion, prueba` | Modelo completo: todo el país vs. solo ciudades de 100 mil+. Mismo script |
| `s2_perfil_prob.csv` **(real)** | D | `sexo, edad, escolaridad, localidad, estrato, indigena, discapacidad, estimacion, li90, ls90, n, prueba` | «Arma un perfil»: probabilidad predicha por el modelo completo para cada una de las 3,840 combinaciones, más 960 filas con `indigena = discapacidad = "Todos"` (promedio ponderado; vista por defecto). `n` = casos con esa combinación de sexo, edad, escolaridad, localidad y estrato. Lo escribe `analisis/D_propuesta/08_perfiles_interactivos.R` |
| `s3_razones.csv` | B | `grupo_var, grupo, orden, razon, estimacion, li90, ls90, prueba` | % de quienes no compraron que da cada razón |
| `s3_problemas.csv` | B | `grupo_var, grupo, orden, problema, estimacion, li90, ls90, prueba` | P5. Incluir la fila «Algún problema». % de compradores |
| `s4_entidades.csv` **(real)** | C | `cve_ent, estimacion, li90, ls90, prueba` | `cve_ent` como número (1 a 32). En % |
| `s4_regiones.csv` **(real)** | C | `region, referencia, tipo, estimacion, li90, ls90, prueba` | `tipo`: Bruta o Ajustada. Diferencia frente al promedio nacional, en pp (decisión 89). Regiones del CEEY: Norte, Norte-occidente, Centro-norte, Centro, Sur. Lo escribe `analisis/C_territorio/01_brecha_region.R` |
| `s1_hogar.csv` **(real)** | A | `grupo_var, grupo, orden, medida, estimacion, li90, ls90, prueba` | `medida`: Vive en un hogar con internet, Usa internet. % de adultos (18+) |
| `s1_tendencia.csv` **(real)** | A | `categoria, anio, estimacion, li90, ls90, prueba` | P3 en el tiempo, microdatos 2022-2025, usuarios de 18+ que compraron |
| `s2_hogar.csv` **(real)** | A | `composicion, orden, sexo, estimacion, li90, ls90, prueba` | P2: composición del hogar, por sexo (usuarios de 35+). Lo escribe `analisis/A_conexion/03_composicion_hogar.R` |
| `s5_simulador.csv` **(real)** | D | `grupo, millones, pct_compra, pct_no_sabe, pct_en_persona, prueba` | «Lo que está en juego»: personas que llegan por cada puerta; `pct_no_sabe` y `pct_en_persona` son % de quienes no compran (quien da las dos razones va solo en `pct_no_sabe`). Lo escribe `analisis/D_propuesta/09_simulador.R`. Sin escenarios ni palancas (decisión 76) |
| `s6_cono.csv` **(real)** | C | `anio, serie, estimacion, li90, ls90, prueba` | `serie`: Observado (2016-2025, con IC), Observado (no comparable) (2015), Probable (curva S) (2016-2030, con IC), Techo alto y Si se estanca (2016-2030, sin IC). En % |
| `s6_impulsores.csv` **(listo)** | B, D | `impulsor, categoria, influencia, dependencia, prueba` | MICMAC simplificado. `categoria`: Político, Económico, Social, Tecnológico, Legal, Ambiental. Influencia y dependencia = suma de la fila y de la columna de la matriz |
| `s6_ejes.csv` **(listo)** | B, D | `eje, nombre, polo_alto, polo_bajo, prueba` | Los dos ejes de los escenarios (salen del MICMAC). `eje`: 1 (vertical) y 2 (horizontal) |
| `s6_escenarios.csv` **(listo)** | B, D | `eje1, eje2, nombre, narrativa, prueba` | `eje1`, `eje2`: alto o bajo. Narrativa corta, sin cifras. Un texto que empieza con «Por escribir» se ve en gris |
| `s6_retroproyeccion.csv` **(listo)** | D | `anio, hito, como_se_sabe, prueba` | Del preferible en 2030 hacia hoy. «Por definir» se ve en gris |
| `s6_senales.csv` **(listo)** | B | `senal, indicador, linea_base, apunta_a, prueba` | Señales tempranas con su línea base en la ENDUTIH |

**Opcionales:** si el archivo no existe, su pestaña no aparece (decisión 87). Ritmo de adopción por entidad (`s4_ritmo.csv`), comercios del DENUE (`s4_comercios.csv`) y tamaño del mercado (`s5_mercado.csv`) quedaron fuera. Cada gráfica tiene un enlace «Descargar datos (CSV)».

Archivos de texto y fijos (no son de prueba):

- `hallazgos.csv`: los recuadros «Lo que encontramos» (`seccion, orden, texto`). Rebeca los mantiene; cada frente propone sus 2 o 3 hallazgos con cifra.
- `propuesta.csv`: los campos de la propuesta en la sección 5 (`campo, resumen, texto, detalle`). Se ve el `resumen` (una línea); `texto` va plegado en «Ver más» y `detalle` en «Detalle (si lo preguntan)». Si `resumen` está vacío, se ve el `texto` completo (decisiones 79 y 81).
- `riesgos.csv`: los riesgos de la propuesta (`orden, riesgo, respuesta`); cada uno se ve como pregunta y su respuesta va plegada.
- `estrategias.csv`: estrategias públicas y privadas (`tipo, actor, accion, por_que`).
- `hallazgos.csv` lleva además una columna `cifra` (la cifra que se destaca a la izquierda; puede ir vacía).
- `como_se_calculo.csv`: las tarjetas «Cómo se calculó».
- `entidades.csv`: nombre, abreviatura, región del CEEY (decisión 62) y punto central de cada entidad.
- `mapa_estados.csv`: contornos simplificados de las 32 entidades (para dibujar el mapa sin paquetes geográficos).

## Por qué así

- **Un CSV por gráfica:** cada frente es dueño de su archivo y nadie toca el código de la app.
- **`dashboard/datos/` sí se sube** al repositorio (a diferencia de `resultados/`): la app publicada necesita los datos.
- **El mapa se dibuja con contornos guardados en un CSV** (`mapa_estados.csv`), así nadie necesita instalar paquetes geográficos. Pasar el cursor sobre una entidad muestra su cifra.
