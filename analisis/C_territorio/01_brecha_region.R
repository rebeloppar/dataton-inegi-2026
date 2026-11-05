# =============================================================================
# Frente C — Brecha por región (pregunta 6 del reto)
# ¿Cuánto menos compra cada región frente al Centro, antes y después de
# comparar personas iguales en sexo, edad, indígena, discapacidad,
# escolaridad, tamaño de localidad y estrato (los controles del modelo completo)?
# Región: regionalización del CEEY (decisión 62). Referencia: Centro, la región
# con más casos (decisión 85).
# Universo: usuarios de internet de 18+.
# Escribe: dashboard/datos/s4_regiones.csv, resultados/C1_brecha_region.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))
suppressMessages(library(marginaleffects))

usu$REGION <- relevel(usu$REGION, ref = "Centro")
usu$ESCOLARIDAD <- relevel(usu$ESCOLARIDAD, ref = "Preescolar o primaria")
dis <- update(dis, REGION = usu$REGION, ESCOLARIDAD = usu$ESCOLARIDAD)

controles <- "SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD + ESCOLARIDAD + TLOC + ESTRATO"
m_bruta    <- svyglm(compra ~ REGION, design = dis, family = quasibinomial())
m_ajustada <- svyglm(as.formula(paste("compra ~ REGION +", controles)), design = dis,
                     family = quasibinomial())

efecto <- function(m, tipo) {
  e <- as.data.frame(avg_comparisons(m, variables = "REGION", wts = "(weights)", conf_level = 0.90))
  data.frame(region = sub(" - Centro$", "", e$contrast), referencia = "Centro", tipo = tipo,
             estimacion = round(100 * e$estimate, 1),
             li90 = round(100 * e$conf.low, 1), ls90 = round(100 * e$conf.high, 1))
}
s4 <- rbind(efecto(m_bruta, "Bruta"), efecto(m_ajustada, "Ajustada"))
s4$region <- factor(s4$region, c("Norte", "Norte-occidente", "Centro-norte", "Sur"))
s4 <- s4[order(s4$tipo != "Bruta", s4$region), ]

cat("\n% que compra por región (usuarios de 18+):\n")
print(round(100 * svyby(~compra, ~REGION, dis, svymean, na.rm = TRUE)[, 2:3], 1))
cat("\n===== Brecha frente al Centro (pp, IC 90 %) =====\n")
print(s4, row.names = FALSE)

write.csv(s4, file.path("resultados", "C1_brecha_region.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(s4, prueba = FALSE), file.path("dashboard", "datos", "s4_regiones.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s4_regiones.csv\n")
