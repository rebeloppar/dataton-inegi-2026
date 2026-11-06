# Datatón INEGI 2026

Diagnóstico del consumo electrónico en México y propuestas para hacerlo incluyente (reto ANTAD–INEGI). Código reproducible en R con datos oficiales del INEGI.

## Empezar (una sola vez)

1. **Clona el repositorio** con GitHub Desktop (File → Clone repository). Guárdalo **fuera de OneDrive**, por ejemplo en `Documentos/GitHub/`: OneDrive y Git se llevan mal.
2. **Descarga los datos** como indica [`datos/README.md`](datos/README.md).
3. **Abre `dataton-inegi-2026.Rproj`** en RStudio. Siempre entra por aquí: así el directorio de trabajo es correcto en cualquier computadora.
4. **Instala los paquetes:** `install.packages(c("survey", "marginaleffects", "shiny", "bslib", "ggplot2"))`
5. **Prueba que todo funciona:** `source("analisis/diagnostico/02_cruces.R")`. Debe imprimir tablas sin errores.

## Estructura

```
R/
  00_config.R          configuración común: paquetes, rutas, constantes
  prep_endutih.R       prepara la ENDUTIH 2025 → `usu` y `dis`
  prep_<fuente>.R      una por cada fuente nueva (ENIGH, EIC…)
  exportar_endutih.R   exporta la ENDUTIH preparada a datos/procesados/ (para Python)
analisis/
  diagnostico/         primer diagnóstico (cruces, traslape, regresión)
  A_conexion/          frente A: del acceso a la compra
  B_barreras/          frente B: barreras y mecanismos
  C_territorio/        frente C: territorio y mercado
  D_propuesta/         frente D: síntesis, género y propuesta
dashboard/              app de Shiny y sus datos (ver dashboard/README.md)
bitacoras/
  decisiones.md        qué decidimos y por qué
  hallazgos.md         qué encontramos, con cifra y script
datos/                 datos crudos (no se suben; ver datos/README.md)
resultados/            tablas que generan los scripts (no se suben)
```

## Cómo funciona la preparación de datos

**`R/prep_endutih.R` prepara la ENDUTIH igual para todos:** carga los datos, se queda con usuarios de internet de 18 años o más, etiqueta las variables y define el diseño muestral. Cada script que use la ENDUTIH empieza así:

```r
source("R/prep_endutih.R")   # deja listos `usu` (datos) y `dis` (diseño muestral)
```

- **Nadie redefine variables en su propio script.** Si necesitas una variable nueva, se agrega en `prep_endutih.R` y se anota en `bitacoras/decisiones.md`. Si cada quien prepara los datos a su manera, obtendremos cuatro porcentajes distintos para lo mismo.
- **Siempre se usa `dis`** (el diseño muestral) para calcular, nunca `usu` directo. Sin el diseño, los porcentajes y los intervalos salen mal.
- **Cada fuente tiene su propio `prep_<fuente>.R`.** No se unen personas entre encuestas distintas: las fuentes se comparan por entidad, tamaño de localidad o grupo.

## Si trabajas en Python u otro lenguaje

- **No vuelvas a preparar la ENDUTIH:** corre una vez `source("R/exportar_endutih.R")` en R y lee `datos/procesados/endutih_usuarios_18mas.csv`.
- **Estimaciones puntuales ponderadas** (con `FAC_PER`) sí se pueden hacer en Python. Control: 43.67 % de usuarios de 18+ compra en línea.
- **Errores estándar, intervalos y modelos de encuestas, en R** (decisión 42): el diseño muestral no se maneja de forma confiable en Python.
- Dependencias de Python en `requirements.txt`.

## Reglas de trabajo

1. **Cada quien trabaja en sus propios scripts**, dentro de la carpeta de su frente. Nadie edita el script de otra persona.
2. **Antes de empezar:** *Pull* (traer cambios). **Al terminar:** *Commit* con un mensaje claro (qué hiciste) y *Push*.
3. **Cada archivo de `R/` tiene un solo dueño.** `00_config.R` y `prep_endutih.R` son de Rebeca; `exportar_endutih.R` también; cada `prep_<fuente>.R` nuevo es de quien incorpora esa fuente. Si necesitas un cambio en un archivo que no es tuyo, se lo pides a su dueño: lo hace, lo anota en la bitácora y avisa para que todos hagan *pull* y vuelvan a correr sus scripts.
4. **Toda decisión de análisis** va en `bitacoras/decisiones.md`; **todo hallazgo**, con su cifra y script, en `bitacoras/hallazgos.md`.
5. **Los datos crudos y los resultados no se suben.** Los resultados se regeneran corriendo los scripts.

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

- ENDUTIH 2025, INEGI (datos abiertos, CSV). Universo de análisis: usuarios de internet de 18 años o más. Estimaciones con factores de expansión y diseño muestral; intervalos al 90 %.
