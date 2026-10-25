# =============================================================================
# Datatón INEGI 2026 — Preparación de la ENDUTIH 2025 (tabla de usuarios)
# Uso: source("R/prep_endutih.R") al inicio de cada script que use la ENDUTIH.
# Deja listos: `usu` (usuarios de internet de 18+) y `dis` (diseño muestral).
# Dueño: [frente de ciencia de datos]. Una variable nueva se agrega AQUÍ (no en
# tu script) y se anota en bitacoras/decisiones.md.
# =============================================================================

source(file.path("R", "00_config.R"))
ruta_datos <- revisar_datos(file.path(rutas$endutih,
                                      "tr_endutih_usuarios_anual_2025.csv"))

# --- Leer ----------------------------------------------------------------------
usu <- read.csv(ruta_datos, colClasses = "character", encoding = "UTF-8")
usu$FAC_PER <- as.numeric(usu$FAC_PER)
usu$EDAD    <- as.numeric(usu$EDAD)

# --- Universo: usuarios de internet de 18 años o más ---------------------------
# (la sección 7 solo se pregunta a usuarios de internet; dejamos fuera a menores
#  de edad y la edad no especificada, código 98)
usu <- subset(usu, P7_1 == "1" & EDAD >= 18 & EDAD <= 97)

# --- Resultado: compró por internet en los últimos 12 meses (P7_21) ------------
usu$compra <- ifelse(usu$P7_21 == "1", 1, ifelse(usu$P7_21 == "2", 0, NA))

# --- Cortes sociodemográficos --------------------------------------------------
# SEXO: el diccionario de usuarios dice "1 Sí, 2 No" (error de copia); en el de
# residentes, 1 = Hombre y 2 = Mujer.
usu$SEXO <- factor(usu$SEXO, levels = c("1", "2"), labels = c("Hombre", "Mujer"))

usu$TLOC <- factor(usu$TLOC, levels = c("1", "2", "3", "4"),
                   labels = c("100 mil y más", "15 mil a 99,999",
                              "2,500 a 14,999", "Menos de 2,500"))

usu$ESTRATO <- factor(usu$ESTRATO, levels = c("1", "2", "3", "4"),
                      labels = c("Bajo", "Medio bajo", "Medio alto", "Alto"))

usu$GRUPO_EDAD <- cut(usu$EDAD, breaks = c(17, 24, 34, 44, 54, 64, 97),
                      labels = c("18-24", "25-34", "35-44",
                                 "45-54", "55-64", "65+"))

# NIVEL: 00-02 ninguno a primaria · 03-05 secundaria, normal básica o técnico
# con secundaria · 06 preparatoria · 07-11 técnico superior a doctorado · 99 NS
nivel <- as.numeric(usu$NIVEL)
nivel[nivel == 99] <- NA
usu$ESCOLARIDAD <- cut(nivel, breaks = c(-1, 2, 5, 6, 11),
                       labels = c("Primaria o menos", "Secundaria",
                                  "Media superior", "Superior o más"))

# Autoadscripción indígena (P6A_3): 1 Sí, 2 No
usu$INDIGENA <- factor(usu$P6A_3, levels = c("2", "1"), labels = c("No", "Sí"))

# Discapacidad: "mucha dificultad" o "no puede" (3 o 4) en al menos una de las
# actividades P6A_2_1 a P6A_2_8 (criterio del Grupo de Washington)
dif <- sapply(paste0("P6A_2_", 1:8), function(v) usu[[v]] %in% c("3", "4"))
usu$DISCAPACIDAD <- factor(ifelse(rowSums(dif) > 0, "Sí", "No"),
                           levels = c("No", "Sí"))

usu$ENTIDAD <- factor(as.numeric(usu$CVE_ENT), levels = 1:32, labels = entidades)

# --- Razones para no comprar (P7_22_1 a P7_22_10), solo para quien no compró ---
razones <- c(
  P7_22_1  = "Prefiere en persona",
  P7_22_2  = "No le interesa o no lo necesita",
  P7_22_3  = "Falta de recursos económicos",
  P7_22_4  = "Desconfía de la calidad del producto",
  P7_22_5  = "Desconfía de la seguridad (datos, tarjeta)",
  P7_22_6  = "No tiene tarjeta o banca en línea",
  P7_22_7  = "No sabe cómo hacerlo",
  P7_22_8  = "No le permiten",
  P7_22_9  = "No tiene la edad suficiente",
  P7_22_10 = "Otra razón")
for (v in names(razones)) {
  usu[[v]] <- ifelse(usu[[v]] == "1", 1, ifelse(usu[[v]] == "2", 0, NA))
}

# --- Diseño muestral ---------------------------------------------------------
dis <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FAC_PER,
                 data = usu, nest = TRUE)

