# =============================================================================
# Frente C — Brecha por región (pregunta 6 del reto)
# ¿Cuánto más o menos compra cada región que el promedio del país, antes y
# después de comparar personas iguales en sexo, edad, indígena, discapacidad,
# escolaridad, tamaño de localidad y estrato (los controles del modelo completo)?
# Región: regionalización del CEEY (decisión 62). Referencia: el promedio
# nacional (decisión 89, sustituye a la 85).
#   Observada: % que compra en la región menos el promedio ponderado de las
#   cinco regiones (= el % nacional).
#   Con controles: predicción promedio si TODO el país estuviera en esa región
#   (misma composición que el país) menos el promedio ponderado de esas cinco
#   predicciones. Pesos = población usuaria de cada región. IC 90 % por método
#   delta (marginaleffects, hypothesis con matriz de contrastes).
# Universo: usuarios de internet de 18+.
# Escribe: dashboard/datos/s4_regiones.csv, resultados/C1_brecha_region.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))
suppressMessages(library(marginaleffects))

usu$ESCOLARIDAD <- relevel(usu$ESCOLARIDAD, ref = "Preescolar o primaria")
dis <- update(dis, ESCOLARIDAD = usu$ESCOLARIDAD)
regiones <- levels(usu$REGION)
peso <- prop.table(tapply(usu$FAC_PER, usu$REGION, sum))[regiones]
# Contraste de cada región contra el promedio ponderado: columna r = e_r − peso
H <- diag(length(regiones)) - matrix(peso, length(regiones), length(regiones))
colnames(H) <- regiones

controles <- "SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD + ESCOLARIDAD + TLOC + ESTRATO"
m_bruta    <- svyglm(compra ~ REGION, design = dis, family = quasibinomial())
m_ajustada <- svyglm(as.formula(paste("compra ~ REGION +", controles)), design = dis,
                     family = quasibinomial())

efecto <- function(m, tipo) {
  # Orden en que marginaleffects devuelve las regiones; la matriz de contrastes sigue ese orden
  orden <- as.character(as.data.frame(avg_predictions(m, variables = "REGION", by = "REGION",
                                                       wts = "(weights)"))$REGION)
  w <- peso[orden]
  Hm <- diag(length(orden)) - matrix(w, length(orden), length(orden))
  e <- as.data.frame(avg_predictions(m, variables = "REGION", wts = "(weights)", by = "REGION",
                                     hypothesis = Hm, conf_level = 0.90))
  data.frame(region = orden, referencia = "Promedio nacional", tipo = tipo,
             estimacion = round(100 * e$estimate, 1),
             li90 = round(100 * e$conf.low, 1), ls90 = round(100 * e$conf.high, 1))
}
s4 <- rbind(efecto(m_bruta, "Bruta"), efecto(m_ajustada, "Ajustada"))
# Cotejo: la observada debe ser el % de la región menos el % nacional
directo <- svyby(~compra, ~REGION, dis, svymean, na.rm = TRUE)
nac <- coef(svymean(~compra, dis, na.rm = TRUE))[[1]]
chk <- round(100 * (directo$compra - nac), 1)[match(s4$region[s4$tipo == "Bruta"], directo$REGION)]
stopifnot(max(abs(chk - s4$estimacion[s4$tipo == "Bruta"])) <= 0.2)
s4$region <- factor(s4$region, regiones)
s4 <- s4[order(s4$tipo != "Bruta", s4$region), ]

cat("\n% que compra por región (usuarios de 18+):\n")
print(round(100 * svyby(~compra, ~REGION, dis, svymean, na.rm = TRUE)[, 2:3], 1))
cat("\n===== Diferencia frente al promedio nacional (pp, IC 90 %) =====\n")
print(s4, row.names = FALSE)

write.csv(s4, file.path("resultados", "C1_brecha_region.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(s4, prueba = FALSE), file.path("dashboard", "datos", "s4_regiones.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s4_regiones.csv\n")
