# Bitácora de decisiones — Datatón INEGI 2026

Decisiones de análisis y su justificación. La más reciente, al final.

## 2026-10-25

| # | Decisión | Justificación |
|---|---|---|
| 1 | Datos de ENDUTIH 2025 en CSV ("Datos abiertos"), no DBF | Mismo contenido; formato abierto, legible en R y Stata; incluye diccionario y catálogos |
| 2 | Análisis en R | Gratuito y reproducible por todo el equipo; paquete `survey` maneja el diseño muestral |
| 3 | Siempre con factores de expansión y diseño (UPM_DIS, EST_DIS, FAC_PER) | Sin diseño, porcentajes sesgados y errores estándar incorrectos |
| 4 | Intervalos al 90 % | Es el nivel que publica el INEGI |
| 5 | SEXO: 1 = hombre, 2 = mujer | Diccionario de usuarios tiene error de copia; se toma la codificación de residentes (pendiente: confirmar con un tabulado) |
| 6 | Universo: usuarios de internet de 18 años o más | La sección 7 solo se pregunta a usuarios; los menores distorsionan razones de no compra ("no me permiten", "no tengo edad") |
| 7 | ESTRATO se lee como nivel socioeconómico del área, no ingreso del hogar | Lo asigna el marco muestral por zona (verificar en documento metodológico) |
| 8 | No usar matching (pscore) en el diagnóstico | Pregunta descriptiva; no da más causalidad que una regresión; posible después para la palanca de la propuesta |
| 9 | Regresión logística antes de priorizar brechas | Filtra por robustez y tamaño ajustado; alcance y accionabilidad se juzgan después sobre los sobrevivientes |
| 10 | Entidad fuera del modelo por ahora | No es prioritaria; después se puede probar una variable regional con una clasificación existente |
| 11 | Incluir estrato (nivel socioeconómico del área) en el modelo | Se traslapa con escolaridad y localidad; sin él, la escolaridad absorbe el efecto socioeconómico y se sobreestima |
| 12 | Indígena = autoadscripción (P6A_3), no lengua | Mayor alcance (28.5 % de usuarios adultos, 23.2 M); lengua (4.9 %) queda para análisis posterior |
| 13 | Uso de computadora fuera del modelo principal | Probable mediador (escolaridad → computadora → compra); controlarlo subestimaría la escolaridad. Se usa después en un modelo aparte de mecanismos |
| 14 | Discapacidad dentro del modelo | Alcance suficiente (8.1 %, 6.6 M) y brecha de 13 pp; puede ser en parte edad, el modelo lo separa |
| 15 | Modelo por bloques: (1) grupos: sexo, edad, indígena, discapacidad; (2) + escolaridad; (3) + tamaño de localidad; (4) + estrato | El modelo final no depende del orden, pero ver cuánto se reducen las brechas de los grupos al agregar cada explicación es el diagnóstico |
| 16 | Hipótesis previa (antes de correr el modelo): "Sospecho que el estrato absorberá buena parte de la brecha, pero no sé si eso refleja ingreso o solo su traslape con escolaridad y localidad" | Escrita antes de ver resultados para que sea una prueba y no una explicación a posteriori |
| 17 | Medir el traslape entre escolaridad, localidad y estrato antes del modelo | El estrato se construye con indicadores censales que incluyen escolaridad y localidad; saber cuánto se traslapan indica cuánto confiar en el reparto de efectos |
| 18 | Mantener los 4 estratos (no fusionar medio alto y alto) | Medio alto vs. alto difieren 14 pp en compra; el modelo sin interacciones no depende de las celdas rurales vacías |
| 19 | Interpretar el efecto del estrato como variación principalmente urbana | En localidades < 2,500 casi no hay estrato medio alto o alto (falta de soporte común) |
| 20 | Prueba de robustez: mismo modelo solo en localidades de 100 mil y más, después del modelo principal | Ahí el estrato no se confunde con ruralidad; si los efectos se parecen, el modelo principal es confiable. Diferencias en sexo indicarían heterogeneidad, no confusión |
| 21 | Sin interacción localidad × estrato | Celdas casi vacías darían estimaciones inestables |
| 22 | Resultado principal en efectos marginales promedio (puntos porcentuales) | Mismas unidades que las brechas brutas; comparables entre modelos por bloques (los coeficientes logit no lo son) |
| 23 | Probabilidades predichas por perfil para reporte y dashboard; coeficientes/razones de momios solo en anexo técnico | Los perfiles comunican mejor; los momios se malinterpretan con un resultado común (~44 %) |
| 24 | Perfiles a contrastar: **TBD** | Se definen después de ver qué variables pesan más |
| 25 | Discapacidad = "mucha dificultad" o "no puede" en al menos una actividad (P6A_2_1 a P6A_2_8) | Criterio estándar del Grupo de Washington |
| 26 | Categorías de referencia: hombre, 18-24, no indígena, sin discapacidad, primaria o menos, 100 mil y más, estrato bajo | Grupos de mayor tamaño o punto de partida natural; no cambia el modelo, solo contra qué se compara |
| 27 | Logit con pesos vía svyglm (quasibinomial) y efectos marginales con `marginaleffects`, IC 90 % | Respeta el diseño muestral; quasibinomial evita advertencias por pesos no enteros |
| 28 | El código vive en un repositorio Git (GitHub); documentos y borradores siguen fuera | Reproducibilidad es parte de la entrega; historial de cambios; OneDrive genera conflictos con archivos de R |
| 29 | Los datos crudos no se suben al repositorio; `datos/README.md` dice dónde descargarlos | Son públicos y pesados; para la entrega final se empaquetan aparte |
| 30 | Una preparación por fuente (`R/prep_<fuente>.R`) más una configuración común (`R/00_config.R`), no un solo archivo con todas las bases | Cada fuente tiene su universo y unidad (persona, hogar, municipio); no se unen personas entre encuestas; cargarlo todo en cada script sería lento; cada frente es dueño de su fuente |
| 31 | `01_primer_cruce.R` se retira | Usaba el universo de 6+ años; lo reemplaza `02_cruces.R` |
| 32 | Reglas de trabajo: cada quien en sus propios scripts dentro de su carpeta de frente; *pull* antes de empezar, *commit + push* al terminar; `R/` tiene dueño y los cambios se piden; todos abren el proyecto con el `.Rproj` | Evita conflictos y que el directorio de trabajo falle en cada computadora |
| 33 | Corrige la 32: cada archivo de `R/` tiene un solo dueño. `00_config.R` y `prep_endutih.R` son de ciencia de datos; cada `prep_<fuente>.R` nuevo es de quien incorpora esa fuente | La 32 decía que toda `R/` era de ciencia de datos, lo que contradecía la 30. Un dueño por archivo evita cambios silenciosos y conflictos sin concentrar todo en una persona |
| 34 | Escolaridad según el marco conceptual del Censo 2020 (INEGI), con la básica separada: sin escolaridad (00) · preescolar o primaria (01-02) · secundaria (03) · media superior (04 normal básica, 05 técnico con secundaria, 06 preparatoria) · superior (07 técnico superior, 08-11) | Cada nivel ambiguo se ubica por una regla oficial (requisito de ingreso), no por criterio propio; cifras comparables con publicaciones del INEGI. Reemplaza la agrupación anterior (04 y 05 estaban en secundaria; 00 con primaria) |
| 35 | Separar primaria y secundaria dentro de la básica | Son niveles oficiales distintos (códigos propios), no un corte buscado en los datos; los excluidos se concentran en la parte baja de la distribución, que es lo que pregunta el reto. Se vio el salto en los datos antes de decidir: por eso se prueba con la agrupación del INEGI tal cual (dec. 36) |
| 36 | Robustez de escolaridad: repetir los modelos con (a) la agrupación del INEGI tal cual (básica en un bloque) y (b) años de escolaridad (nivel + grado) | Comprobar que las conclusiones no dependen de cómo se agrupa la escolaridad |
| 37 | Referencia de escolaridad: "preescolar o primaria" (no "sin escolaridad") | "Sin escolaridad" tiene ~570 casos; como referencia daría contrastes ruidosos |
| 38 | Las brechas se priorizan con cuatro criterios: tamaño (pp entre grupos), alcance (millones de personas en el grupo rezagado), robustez (se sostiene con controles y pruebas) y accionabilidad (alguien puede actuar) | Hace explícita y defendible la selección; los tres primeros salen del análisis, el cuarto es juicio del equipo y se justifica por escrito |
| 39 | Hilo provisional revisado: "Entre quienes ya están conectados, la brecha en la compra digital es sobre todo de escolaridad, edad y territorio. Las barreras difieren por grupo: para personas mayores y de baja escolaridad son habilidades ('no sé cómo'); para quienes tienen más escolaridad, preferencia por lo presencial con algo de desconfianza. Los medios de pago son una barrera marginal." | La confianza resultó secundaria en general (hallazgos 12-14), pero no se descarta: es relevante en escolaridad alta, puede estar dentro de "prefiere en persona" y falta ver P5 |
