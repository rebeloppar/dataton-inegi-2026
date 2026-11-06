# =============================================================================
# Frente D — ¿Por qué las mujeres compran menos en línea? (hallazgo 36)
# Mujeres y hombres usan internet por igual, pero ellas compran menos (hallazgo 31).
# Se prueban tres candidatos con la ENDUTIH 2025, en usuarios de internet de 18+
# (todos y grupo objetivo: hasta secundaria):
#   1. Capacidad y confianza para transacciones: descargar apps, pagar en línea, banca
#   2. Medios de pago: "no tiene tarjeta o banca en línea" y "falta de recursos"
#      como razón para no comprar
#   6. Qué compran quienes sí compran (categorías del INEGI, s1_categorias.csv)
# Descriptivo, sin controles; y la misma diferencia con los controles del modelo
# completo (decisión 96), que es la que se cita como «con controles».
# Escribe: resultados/D7_genero_mecanismos.csv, resultados/D7_genero_controles.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))
z90 <- qnorm(0.95)

usu$hasta_sec <- usu$ESCOLARIDAD %in% c("Sin escolaridad", "Preescolar o primaria", "Secundaria")
uno <- function(v) as.numeric(usu[[v]] %in% "1")
usu$apps <- uno("P7_17_1"); usu$paga <- uno("P7_28"); usu$banca <- uno("P7_33")
usu$compra_n <- as.numeric(usu$compra %in% 1)
usu$no_tarjeta <- ifelse(usu$compra %in% 0, usu$P7_22_6, NA)
usu$sin_recursos <- ifelse(usu$compra %in% 0, usu$P7_22_3, NA)
usu$no_sabe <- ifelse(usu$compra %in% 0, usu$P7_22_7, NA)
usu$desconfia <- ifelse(usu$compra %in% 0, as.numeric(usu$P7_22_4 %in% 1 | usu$P7_22_5 %in% 1), NA)
vars <- c(compra_n = "Compra en línea", apps = "Descargó apps", paga = "Paga algo en línea",
          banca = "Usa banca en línea", no_sabe = "No compra: no sabe cómo",
          no_tarjeta = "No compra: no tiene tarjeta o banca", sin_recursos = "No compra: falta de recursos",
          desconfia = "No compra: desconfía (calidad o seguridad)")
dis <- update(dis, hasta_sec = usu$hasta_sec, apps = usu$apps, paga = usu$paga, banca = usu$banca,
              compra_n = usu$compra_n, no_tarjeta = usu$no_tarjeta, sin_recursos = usu$sin_recursos,
              no_sabe = usu$no_sabe, desconfia = usu$desconfia)

tabla <- function(d, universo) do.call(rbind, lapply(names(vars), function(v) {
  r <- svyby(as.formula(paste0("~", v)), ~SEXO, d, svymean, na.rm = TRUE)
  dif <- svyby(as.formula(paste0("~", v)), ~SEXO, d, svymean, na.rm = TRUE, covmat = TRUE)
  ct <- svycontrast(dif, c(-1, 1))   # mujer - hombre
  data.frame(universo = universo, indicador = vars[[v]],
             hombre = round(100 * r[[v]][1], 1), mujer = round(100 * r[[v]][2], 1),
             dif_pp = round(100 * unname(coef(ct)), 1),
             li90 = round(100 * unname(coef(ct) - z90 * SE(ct)), 1),
             ls90 = round(100 * unname(coef(ct) + z90 * SE(ct)), 1))
}))
res <- rbind(tabla(dis, "Usuarios de 18+"), tabla(subset(dis, hasta_sec), "Hasta secundaria"))
cat("\n===== Mujeres vs. hombres (dif = mujer − hombre, pp, IC 90 %) =====\n")
print(res, row.names = FALSE)
write.csv(res, file.path("resultados", "D7_genero_mecanismos.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Con controles (decisión 96): mismo modelo que la regresión principal; las
# razones para no comprar, entre quienes no compran
suppressMessages(library(marginaleffects))
dis <- update(dis, apps = usu$apps, paga = usu$paga, banca = usu$banca, no_sabe = usu$no_sabe,
              no_tarjeta = usu$no_tarjeta, desconfia = usu$desconfia)
ctrl <- do.call(rbind, lapply(c("apps", "paga", "banca", "no_sabe", "no_tarjeta", "desconfia"), function(v) {
  f <- as.formula(paste(v, "~ SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD + ESCOLARIDAD + TLOC + ESTRATO"))
  d <- if (v %in% c("no_sabe", "no_tarjeta", "desconfia")) subset(dis, compra == 0) else dis
  e <- avg_comparisons(svyglm(f, design = d, family = quasibinomial()), variables = "SEXO",
                       wts = "(weights)", conf_level = 0.90)
  data.frame(indicador = vars[[v]], dif_pp = round(100 * e$estimate, 1),
             li90 = round(100 * e$conf.low, 1), ls90 = round(100 * e$conf.high, 1))
}))
cat("\n===== Mujeres vs. hombres con controles (usuarios de 18+) =====\n")
print(ctrl, row.names = FALSE)
write.csv(ctrl, file.path("resultados", "D7_genero_controles.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 6. Categorías por sexo (compradores de 18+), de s1_categorias.csv
cat("\n===== Qué compran (compradores de 18+), % por sexo =====\n")
c6 <- read.csv(file.path("dashboard", "datos", "s1_categorias.csv"), fileEncoding = "UTF-8")
c6 <- subset(c6, grupo_var == "Sexo")
w <- reshape(c6[, c("categoria", "grupo", "estimacion", "li90", "ls90")], idvar = "categoria",
             timevar = "grupo", direction = "wide")
w$dif <- w$estimacion.Mujer - w$estimacion.Hombre
print(w[order(-w$dif), c("categoria", "estimacion.Hombre", "estimacion.Mujer", "dif",
                         "li90.Mujer", "ls90.Mujer", "li90.Hombre", "ls90.Hombre")], row.names = FALSE)
