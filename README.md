# Datatón INEGI 2026

Diagnóstico de la compra en línea entre los adultos que ya usan internet en México y una propuesta para hacerla incluyente (reto ANTAD–INEGI). Análisis reproducible en R con datos oficiales del INEGI; tres extractores en Python preparan las bases de años anteriores y de la ENIGH.

**Dashboard:** *(enlace pendiente: se publica en Posit Connect Cloud)*

## Cómo reproducir

1. **Clona el repositorio.**
2. **Descarga los datos** del INEGI como indica [`datos/README.md`](datos/README.md).
3. **Abre `dataton-inegi-2026.Rproj`** en RStudio, para que el directorio de trabajo sea la raíz del repositorio.
4. **Instala los paquetes de R:** `install.packages(c("survey", "marginaleffects", "shiny", "bslib", "ggplot2", "ggrepel", "MASS", "jsonlite"))`
5. **Python:** los tres extractores (`00_extrae_*.py`) necesitan Python 3, sin paquetes extra.
6. **Corre los scripts en este orden** (desde la raíz del repositorio). Cada uno escribe sus cifras en `dashboard/datos/`.
7. **Abre el dashboard:** `shiny::runApp("dashboard")`. Solo lee los CSV de `dashboard/datos/`, que ya vienen en el repositorio, así que se puede abrir sin descargar las bases.

| Paso | Scripts | Qué hace |
| :- | :- | :- |
| 1 | `analisis/C_territorio/00_extrae_historicos.py`, `analisis/A_conexion/00_extrae_categorias.py`, `analisis/C_territorio/00_extrae_enigh.py` | Extraen de las bases del INEGI solo las columnas que se usan (`datos/procesados/`) |
| 2 | `analisis/diagnostico/02_cruces.R`, `04_regresion.R`, `05_exportar_dashboard.R` | Diagnóstico, modelo completo y % por entidad |
| 3 | `analisis/A_conexion/01` a `05` | Del acceso a la compra: embudo, hogar, categorías, adolescentes |
| 4 | `analisis/B_barreras/01`, `03`, `04` | Razones para no comprar, problemas de quienes compran, género |
| 5 | `analisis/C_territorio/01`, `03`, `02`, `04`, `05` | Regiones, serie histórica, futuro probable, ENIGH, ritmo por entidad (`02` usa lo que escribe `03`) |
| 6 | `analisis/D_propuesta/01` a `10` | Grupo objetivo, mecanismos, perfiles, «Lo que está en juego», MICMAC (`07` usa lo que escribe `A_conexion/02`) |

Los escenarios, la retroproyección y las señales de la sección 6 (`s6_escenarios.csv`, `s6_retroproyeccion.csv`, `s6_senales.csv`) son texto del equipo, no salen de un script.

## Estructura

```
R/
  00_config.R          configuración común: paquetes, rutas, constantes
  prep_endutih.R       prepara la ENDUTIH 2025 → `usu` y `dis`
  exportar_endutih.R   exporta la ENDUTIH preparada a datos/procesados/
analisis/
  diagnostico/         primer diagnóstico (cruces, traslape, regresión)
  A_conexion/          frente A: del acceso a la compra
  B_barreras/          frente B: barreras y mecanismos
  C_territorio/        frente C: territorio, serie histórica y ENIGH
  D_propuesta/         frente D: síntesis, género, propuesta y prospectiva
  */00_extrae_*.py     extractores de las bases de otros años y de la ENIGH
dashboard/             app de Shiny y sus datos (ver dashboard/README.md)
bitacoras/
  decisiones.md        qué decidimos, por qué y quién
  hallazgos.md         qué encontramos, con cifra y script
datos/                 datos crudos (no se suben; ver datos/README.md)
resultados/            tablas intermedias que generan los scripts (no se suben)
```

## Cómo se preparan los datos

**`R/prep_endutih.R` prepara la ENDUTIH 2025 igual para todos los análisis:** carga los datos, se queda con los usuarios de internet de 18 años o más, etiqueta las variables y define el diseño muestral. Cada script empieza con `source("R/prep_endutih.R")`, que deja listos `usu` (los datos) y `dis` (el diseño muestral).

- **Todas las estimaciones usan el diseño muestral** (`dis`, con UPM, estrato y factor de expansión), así que los porcentajes y los intervalos reflejan la encuesta y no solo la muestra.
- **Las demás bases** (ENDUTIH 2015-2024 y ENIGH 2024) pasan primero por un extractor en Python que guarda solo las columnas necesarias; el análisis y los errores estándar se hacen en R con el diseño de cada levantamiento.
- **No se unen personas entre encuestas distintas:** las fuentes se comparan por año, entidad, tamaño de localidad o grupo.

## Uso de inteligencia artificial

> **Pendiente:** falta que el resto del equipo esté de acuerdo con esta declaración.

Usamos Claude (Anthropic) como asistente durante todo el proyecto. Aquí decimos qué hizo y qué no.

**Lo que no hizo**
- No tomó decisiones. El enfoque, el universo de análisis, los controles, la propuesta y qué entra al tablero los decidió el equipo. Cada decisión está en `bitacoras/decisiones.md`, con quién la tomó.
- No es fuente de ningún dato. Todas las cifras salen de microdatos públicos del INEGI (ENDUTIH 2016-2025, ENIGH 2024) y se reproducen corriendo los scripts del repositorio. Las fuentes externas están citadas.
- No tuvo la última palabra en lo cualitativo. El equipo revisó y reescribió los textos del tablero y los puntajes del MICMAC.

**Lo que hizo**
- Escribió la mayor parte del código de análisis (R y Python) y del tablero (Shiny), a partir de lo que el equipo definió.
- Corrió las estimaciones con los microdatos del INEGI y las cotejó contra las cifras que publica el INEGI.
- Propuso redacciones para los textos del tablero y borradores del PESTEL y de los puntajes del MICMAC.
- Buscó fuentes de contexto (por ejemplo, los kioscos de Bodega Aurrera y Coppel) y llevó las bitácoras de decisiones y hallazgos.

**Cómo verificarlo**
- Los commits hechos con ayuda de la IA llevan la línea «Co-Authored-By: Claude».
- La IA se equivocó, y lo corregimos. Por ejemplo, etiquetó como «con controles» cifras que no lo eran, y citó un dato desactualizado. Por eso ninguna cifra entra al tablero sin cotejo.
- Solo usamos datos públicos y anónimos del INEGI; no compartimos datos personales con la IA.

## Fuentes

**Datos (INEGI, datos abiertos, CSV)**
- ENDUTIH 2025: universo de análisis, usuarios de internet de 18 años o más. Estimaciones con factores de expansión y diseño muestral; intervalos al 90 %.
- ENDUTIH 2015 a 2024: serie histórica de la compra en línea (comparable desde 2016), ritmo por entidad y tendencia de categorías.
- ENIGH 2024: dónde compran los hogares (gasto por lugar de compra).

**Otras fuentes**
- Regiones del Centro de Estudios Espinosa Yglesias (CEEY), con base en la ESRU-EMOVI 2017.
- Expansión (febrero de 2023): kioscos con asesor en Bodega Aurrera.
- Forbes México (enero de 2024): quioscos digitales de Coppel.
- Gallino, S. y Moreno, A. (2014). «Integration of Online and Offline Channels in Retail: The Impact of Sharing Reliable Inventory Availability Information». *Management Science*, 60(6).
