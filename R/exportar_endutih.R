# =============================================================================
# Datatón INEGI 2026 — Exporta la ENDUTIH preparada para usarla fuera de R
# Para quien trabaja en Python u otro lenguaje: NO se vuelve a preparar la
# ENDUTIH; se lee este archivo, que sale de R/prep_endutih.R (fuente única).
# Uso: source("R/exportar_endutih.R")  → datos/procesados/endutih_usuarios_18mas.csv
# Volver a correrlo cada vez que cambie prep_endutih.R.
# =============================================================================

source(file.path("R", "prep_endutih.R"))

columnas <- c(
  # Llaves y diseño muestral (indispensables para estimar bien)
  "UPM", "VIV_SEL", "HOGAR", "NUM_REN", "UPM_DIS", "EST_DIS", "FAC_PER",
  # Resultado y razones
  "compra", names(razones),
  # Cortes ya construidos
  "SEXO", "EDAD", "GRUPO_EDAD", "ESCOLARIDAD", "ESCOLARIDAD_INEGI", "ANIOS_ESC",
  "TLOC", "ESTRATO", "CVE_ENT", "ENTIDAD", "INDIGENA", "DISCAPACIDAD")

dir.create(file.path("datos", "procesados"), showWarnings = FALSE, recursive = TRUE)
salida <- file.path("datos", "procesados", "endutih_usuarios_18mas.csv")
write.csv(usu[, columnas], salida, row.names = FALSE, fileEncoding = "UTF-8")

# Valor de control para comprobar la réplica en otro lenguaje
ctrl <- svymean(~compra, dis, na.rm = TRUE)
cat(sprintf("Exportado: %s (%d filas)\n", salida, nrow(usu)))
cat(sprintf("Control: %% que compra en línea (18+) = %.2f, EE = %.4f\n",
            100 * coef(ctrl), 100 * SE(ctrl)))
