# =============================================================================
# Frente B — Razones para no comprar en línea, por escolaridad, edad, estrato
# y región
# Pregunta del reto P4: ¿cómo varían las barreras según el grupo y la región?
# Universo: usuarios de internet de 18+ que NO compraron en los últimos 12 meses.
# Cada persona puede dar varias razones: los % de una columna no suman 100.
# =============================================================================

source(file.path("R", "prep_endutih.R"))

# REGION viene de R/prep_endutih.R: regionalización del CEEY (decisión 62).

dis_no <- subset(dis, compra == 0)
f_raz  <- as.formula(paste("~", paste(names(razones), collapse = " + ")))

# Razones que casi nadie da entre adultos (permiso, edad, otra): fuera de la tabla
principales <- setdiff(names(razones), c("P7_22_8", "P7_22_9", "P7_22_10"))

razones_por <- function(v) {
  r <- svyby(f_raz, as.formula(paste0("~", v)), dis_no, svymean,
             na.rm = TRUE, vartype = c("se", "cv"))
  grupos <- as.character(r[[v]])

  # % por razón (filas) y grupo (columnas)
  pct <- sapply(principales, function(x) round(100 * r[[x]], 1))
  pct <- t(pct); colnames(pct) <- grupos

  # Bandera de precisión: CV > 30 % se marca con "*"
  cv  <- t(sapply(principales, function(x) r[[paste0("cv.", x)]]))
  txt <- matrix(paste0(format(pct, nsmall = 1), ifelse(cv > 0.30, "*", "")),
                nrow = nrow(pct), dimnames = dimnames(pct))

  out <- data.frame(razon = unname(razones[principales]), txt,
                    check.names = FALSE, row.names = NULL)
  out$rango_pp <- round(apply(pct, 1, max) - apply(pct, 1, min), 1)

  n <- table(usu[[v]][usu$compra == 0])
  attr(out, "n") <- n[grupos]
  out[order(-rowMeans(pct)), ]
}

for (v in c("ESCOLARIDAD", "GRUPO_EDAD", "ESTRATO", "REGION")) {
  tab <- razones_por(v)
  cat("\n=====", v, ": % de quienes no compraron que da cada razón =====\n")
  cat("(* = CV > 30 %, estimación poco precisa; rango_pp = mayor − menor grupo)\n\n")
  print(tab, row.names = FALSE)
  cat("\nCasos en muestra por grupo:\n"); print(attr(tab, "n"))
  write.csv(tab, file.path("resultados", paste0("B1_razones_por_", v, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
}

# -----------------------------------------------------------------------------
# Dashboard, sección 3 «Por qué no compran» → dashboard/datos/s3_razones.csv
# Cortes: nacional, escolaridad, edad, sexo, localidad, estrato y región (CEEY).
# Mismas razones principales que las tablas de arriba, con nombre corto.
# -----------------------------------------------------------------------------
source(file.path("analisis", "B_barreras", "00_exportar_dashboard.R"))

razones_corto <- c(
  P7_22_1 = "Prefiere en persona",
  P7_22_2 = "No le interesa",
  P7_22_3 = "Falta de recursos",
  P7_22_4 = "Desconfía de la calidad",
  P7_22_5 = "Desconfía de la seguridad",
  P7_22_6 = "No tiene tarjeta",
  P7_22_7 = "No sabe cómo")
stopifnot(identical(names(razones_corto), principales))

s3_razones <- estimar_por_cortes(preparar_cortes(dis_no), principales, razones_corto)
escribir_dashboard(s3_razones, "s3_razones.csv", "razon")
