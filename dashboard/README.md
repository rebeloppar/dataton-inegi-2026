# Dashboard

App de Shiny: Inicio y seis secciones. Cada sección tiene una afirmación, sus gráficas, «Lo que encontramos» y, en cada gráfica, «Cómo se calculó» y «Descargar datos (CSV)».

## Correrlo

**Necesitas:** R 4.1 o más reciente. No hace falta descargar las bases del INEGI: el dashboard solo lee los CSV de `dashboard/datos/`, que vienen en el repositorio.

1. Abre `dataton-inegi-2026.Rproj` en RStudio (o pon la raíz del repositorio como directorio de trabajo).
2. Una sola vez: `install.packages(c("shiny", "bslib", "ggplot2", "ggrepel", "jsonlite"))`
3. `shiny::runApp("dashboard")`

**Si algo falla:**

- *«there is no package called…»*: falta instalar un paquete del paso 2.
- *«cannot open file 'dashboard/datos/…'»*: el directorio de trabajo no es la raíz del repositorio; vuelve a entrar por el `.Rproj`.
- *Sin internet, la letra se ve distinta*: la tipografía se descarga de Google; sin conexión se usa la del sistema.

Probado con R 4.3, shiny 1.8, bslib 0.6, ggplot2 3.4 y ggrepel 0.9.

## De dónde sale cada gráfica

Cada gráfica lee un CSV de `dashboard/datos/`. Los scripts de análisis lo escriben al final (el orden para correrlos está en el [README principal](../README.md#cómo-reproducir)). Todos los archivos llevan además la columna `prueba` (hoy, `FALSE` en todos).

| Archivo | Lo escribe (`analisis/…`) | Columnas | Notas |
|---|---|---|---|
| `s1_embudo.csv` | `A_conexion/01_embudo_y_brechas.R` | `grupo_var, grupo, orden, eslabon, paso, estimacion, li90, ls90` | `grupo_var`: Nacional, Escolaridad, Edad, Sexo, Localidad, Estrato, Región. `eslabon`: Hogar con internet, Usa internet, Compra en línea. En % |
| `s1_brechas_eslabon.csv` | `A_conexion/01_embudo_y_brechas.R` | `comparacion, eslabon, estimacion, li90, ls90` | `eslabon`: Usar internet o Comprar en línea. Brecha con controles, en pp |
| `s1_hogar.csv` | `A_conexion/02_hogar_y_categorias.R` | `grupo_var, grupo, orden, medida, estimacion, li90, ls90` | `medida`: Vive en un hogar con internet, Usa internet. % de adultos (18+) |
| `s1_categorias.csv` | `A_conexion/02_hogar_y_categorias.R` | `grupo_var, grupo, orden, categoria, estimacion, li90, ls90` | P3: % de compradores que compró cada categoría |
| `s1_tendencia.csv` | `A_conexion/04_tendencia_categorias.R` | `categoria, anio, estimacion, li90, ls90` | P3 en el tiempo: microdatos 2022-2025, usuarios de 18+ que compraron |
| `s2_efectos.csv` | `diagnostico/05_exportar_dashboard.R` | `factor, orden_factor, etiqueta, estimacion, li90, ls90` | Efectos marginales del modelo completo, en pp |
| `s2_bloques.csv` | `diagnostico/05_exportar_dashboard.R` | `brecha, paso, nombre_paso, estimacion, li90, ls90` | La regresión paso a paso (4 bloques) |
| `s2_robustez.csv` | `diagnostico/05_exportar_dashboard.R` | `etiqueta, version, estimacion` | Modelo completo: todo el país vs. solo ciudades de 100 mil+ |
| `s2_perfil_prob.csv` | `D_propuesta/08_perfiles_interactivos.R` | `sexo, edad, escolaridad, localidad, estrato, indigena, discapacidad, estimacion, li90, ls90, n` | «Arma un perfil»: probabilidad predicha por el modelo completo para las 3,840 combinaciones, más 960 filas con `indigena = discapacidad = "Todos"` (promedio ponderado; vista por defecto) |
| `s2_hogar.csv` | `A_conexion/03_composicion_hogar.R` | `composicion, orden, sexo, estimacion, li90, ls90` | P2: composición del hogar, por sexo (usuarios de 35+) |
| `s2_hogar_controles.csv` | `A_conexion/03_composicion_hogar.R` | `grupo, orden, estimacion, li90, ls90` | Diferencia con controles, con joven vs. sin joven, para todos y por sexo (decisión 99) |
| `s3_razones.csv` | `B_barreras/01_razones_por_grupo.R` | `grupo_var, grupo, orden, razon, estimacion, li90, ls90` | % de quienes no compraron que da cada razón |
| `s3_problemas.csv` | `B_barreras/03_problemas_compradores.R` | `grupo_var, grupo, orden, problema, estimacion, li90, ls90` | P5, con la fila «Algún problema». % de compradores |
| `s3_genero.csv` | `B_barreras/04_genero_55mas.R` | `grupo, orden, estimacion, li90, ls90` | Diferencia mujeres − hombres en el % que dice «no sé cómo», con controles, en pp |
| `s4_entidades.csv` | `diagnostico/05_exportar_dashboard.R` | `cve_ent, estimacion, li90, ls90` | `cve_ent` de 1 a 32. En % |
| `s4_regiones.csv` | `C_territorio/01_brecha_region.R` | `region, referencia, tipo, estimacion, li90, ls90` | `tipo`: Bruta o Ajustada. Diferencia frente al promedio nacional, en pp (decisión 89). Regiones del CEEY |
| `s4_ritmo.csv` | `C_territorio/05_ritmo_entidades.R` | `cve_ent, anio_inicio, anio_fin, inicio, fin, avance, li90, ls90` | % que compra en 2016-17 y 2024-25 y avance en pp (decisión 104) |
| `s5_enigh.csv` | `C_territorio/04_enigh_lugar_compra.R` | `grupo_var, grupo, medida, indicador, estimacion, li90, ls90` | «¿Por qué la tienda?»: % de hogares que compró en cada lugar y % de su gasto (ENIGH 2024) |
| `s5_simulador.csv` | `D_propuesta/09_simulador.R` | `grupo, millones, pct_compra, pct_no_sabe, pct_en_persona` | «Lo que está en juego»: personas que llegan por cada puerta; quien da las dos razones cuenta solo en `pct_no_sabe` (decisión 76) |
| `s6_cono.csv` | `C_territorio/02_futuro_probable.R` | `anio, serie, estimacion, li90, ls90` | Observado (2016-2025), Probable (curva S, con IC), Techo alto y Si se estanca. En % |
| `s6_impulsores.csv` | `D_propuesta/10_micmac.R` | `impulsor, categoria, influencia, dependencia` | MICMAC: influencia y dependencia = suma de la fila y de la columna de la matriz |
| `s6_ejes.csv` | `D_propuesta/10_micmac.R` | `eje, nombre, polo_alto, polo_bajo` | Los dos ejes de los escenarios |
| `s6_escenarios.csv` | texto del equipo | `eje1, eje2, nombre, narrativa` | Los cuatro escenarios |
| `s6_retroproyeccion.csv` | texto del equipo | `anio, hito, como_se_sabe` | Del escenario preferible en 2030 hacia hoy |
| `s6_senales.csv` | texto del equipo | `senal, indicador, linea_base, apunta_a` | Señales tempranas, con su línea base en la ENDUTIH |

**Pestaña opcional:** si `s4_ritmo.csv` no existe, la pestaña «Ritmo» no aparece (decisión 87). Los comercios del DENUE y el tamaño del mercado con la ENIGH no se incluyeron.

**Archivos de texto y fijos:**

- `hallazgos.csv`: los recuadros «Lo que encontramos» (`seccion, orden, cifra, texto`); `cifra` es el número que se destaca y puede ir vacía.
- `propuesta.csv`: los campos de la propuesta (`campo, resumen, texto, detalle`). Se ve el `resumen`; `texto` va plegado en «Ver más» y `detalle` en «Detalle (si lo preguntan)» (decisiones 79 y 81).
- `riesgos.csv`: los riesgos de la propuesta (`orden, riesgo, respuesta`); la respuesta va plegada.
- `estrategias.csv`: actores públicos y privados (`tipo, actor, accion, por_que`).
- `como_se_calculo.csv`: las tarjetas «Cómo se calculó» (`grafica, fuente, universo, tipo, script, nota`).
- `entidades.csv`: nombre, abreviatura, región del CEEY (decisión 62) y punto central de cada entidad.
- `mapa_estados.csv`: contornos simplificados de las 32 entidades.

`crear_datos_prueba.R` generaba datos de prueba mientras se hacía el análisis; ya no sobrescribe ningún archivo con cifras reales (decisión 87).

## Por qué así

- **Un CSV por gráfica:** cada cifra se puede rastrear hasta el script que la produce, y el dashboard no hace cálculos.
- **`dashboard/datos/` sí se sube** al repositorio (a diferencia de `resultados/`), porque la app publicada necesita los datos.
- **El mapa se dibuja con contornos guardados en un CSV** (`mapa_estados.csv`), sin paquetes geográficos. Pasar el cursor sobre una entidad muestra su ficha.
