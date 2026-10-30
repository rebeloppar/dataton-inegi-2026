# =============================================================================
# Frente B — Razones para no comprar en línea, por escolaridad, edad, estrato
# y región
# Pregunta del reto P4: ¿cómo varían las barreras según el grupo y la región?
# Universo: usuarios de internet de 18+ que NO compraron en los últimos 12 meses.
# Cada persona puede dar varias razones: los % de una columna no suman 100.
# =============================================================================

source(file.path("R", "prep_endutih.R"))

# TEMPORAL: región mientras el frente C define la variable de región. Se usa la
# regionalización del Banco de México (Reporte sobre las Economías Regionales).
# Borrar cuando se integre en R/prep_endutih.R.
region_banxico <- c(
  "Baja California" = "Norte", "Chihuahua" = "Norte", "Coahuila" = "Norte",
  "Nuevo León" = "Norte", "Sonora" = "Norte", "Tamaulipas" = "Norte",
  "Aguascalientes" = "Centro norte", "Baja California Sur" = "Centro norte",
  "Colima" = "Centro norte", "Durango" = "Centro norte", "Jalisco" = "Centro norte",
  "Michoacán" = "Centro norte", "Nayarit" = "Centro norte",
  "San Luis Potosí" = "Centro norte", "Sinaloa" = "Centro norte",
  "Zacatecas" = "Centro norte",
  "Ciudad de México" = "Centro", "México" = "Centro", "Guanajuato" = "Centro",
  "Hidalgo" = "Centro", "Morelos" = "Centro", "Puebla" = "Centro",
  "Querétaro" = "Centro", "Tlaxcala" = "Centro",
  "Campeche" = "Sur", "Chiapas" = "Sur", "Guerrero" = "Sur", "Oaxaca" = "Sur",
  "Quintana Roo" = "Sur", "Tabasco" = "Sur", "Veracruz" = "Sur", "Yucatán" = "Sur")
usu$REGION <- factor(unname(region_banxico[as.character(usu$ENTIDAD)]),
                     levels = c("Norte", "Centro norte", "Centro", "Sur"))
stopifnot(!anyNA(usu$REGION))   # las 32 entidades quedan asignadas
dis <- update(dis, REGION = usu$REGION)

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
