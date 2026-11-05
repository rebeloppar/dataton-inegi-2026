# =============================================================================
# Frente C — Cono de futuros: el futuro probable a 2030 (sección 6, decisión 86)
#
# 1. Tendencia. Serie publicada del INEGI, % de personas usuarias de internet
#    (6 años y más) que compró por internet: 2023 33.4 %, 2024 35.8 %, 2025
#    37.3 % (presentaciones y reporte de resultados de la ENDUTIH 2024 y 2025).
#    Subió 2.4 pp en 2024 y 1.5 pp en 2025. Esos dos ritmos se aplican a
#    nuestro universo (usuarios de 18+, 43.7 % en 2025): central = promedio
#    (1.95 pp por año), rango = el ritmo más lento y el más rápido observados.
#    Supuesto: el ritmo de 18+ es como el de 6+; no hay serie publicada para 18+.
# 2. Efecto demográfico. ¿Cuánto cambiaría el % que compra solo porque la
#    población envejece 5 años y entran nuevas generaciones, si nadie cambia
#    de conducta? Se envejece 5 años a los usuarios de internet de 13+ de la
#    ENDUTIH 2025 y se les aplica la probabilidad que hoy tiene alguien de su
#    sexo, edad, escolaridad, localidad y estrato (modelo logit con diseño).
#    Quienes hoy tienen 13-17 entran en 2030 con la escolaridad que hoy tienen
#    los usuarios de 18-22 de su mismo sexo. No se descuenta la mortalidad ni
#    se suman personas que hoy no usan internet.
#
# Escribe: dashboard/datos/s6_cono.csv, resultados/C2_futuro_probable.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))

# --- 1. Tendencia --------------------------------------------------------------
serie_inegi <- c(`2023` = 33.4, `2024` = 35.8, `2025` = 37.3)   # usuarios de 6+
ritmos <- diff(serie_inegi)                                     # 2.4 y 1.5 pp
base <- 100 * coef(svymean(~compra, dis, na.rm = TRUE))[[1]]    # usuarios de 18+, 2025
anios <- 2025:2030
probable <- data.frame(anio = anios, serie = "Probable (tendencia)",
  estimacion = round(pmin(100, base + mean(ritmos) * (anios - 2025)), 1),
  li90 = round(pmin(100, base + min(ritmos) * (anios - 2025)), 1),
  ls90 = round(pmin(100, base + max(ritmos) * (anios - 2025)), 1))
cat(sprintf("Base 2025 (18+): %.1f %%. Ritmos publicados: %s pp por año\n", base,
            paste(ritmos, collapse = " y ")))

# --- 2. Efecto demográfico -----------------------------------------------------
m <- svyglm(compra ~ SEXO + GRUPO_EDAD + ESCOLARIDAD + TLOC + ESTRATO, design = dis,
            family = quasibinomial())

crudo <- read.csv(ruta_datos, colClasses = "character", encoding = "UTF-8")
crudo <- subset(crudo, P7_1 == "1")
crudo$EDAD <- as.numeric(crudo$EDAD); crudo$FAC_PER <- as.numeric(crudo$FAC_PER)
crudo <- subset(crudo, EDAD >= 13 & EDAD <= 92)                 # en 2030: 18 a 97
crudo$SEXO <- factor(crudo$SEXO, c("1", "2"), c("Hombre", "Mujer"))
crudo$TLOC <- factor(crudo$TLOC, c("1", "2", "3", "4"), levels(usu$TLOC))
crudo$ESTRATO <- factor(crudo$ESTRATO, c("1", "2", "3", "4"), levels(usu$ESTRATO))
nivel <- as.numeric(crudo$NIVEL); nivel[nivel == 99] <- NA
crudo$ESCOLARIDAD <- cut(nivel, c(-1, 0, 2, 3, 6, 11), levels(usu$ESCOLARIDAD))
crudo$EDAD_2030 <- crudo$EDAD + 5
crudo$GRUPO_EDAD <- cut(crudo$EDAD_2030, c(17, 24, 34, 44, 54, 64, 97), levels(usu$GRUPO_EDAD))

# Quienes hoy tienen 13-17: escolaridad de los usuarios de 18-22 de su sexo
jovenes_hoy <- subset(usu, EDAD >= 18 & EDAD <= 22 & !is.na(ESCOLARIDAD))
reparto <- prop.table(xtabs(FAC_PER ~ SEXO + ESCOLARIDAD, jovenes_hoy), 1)
entran <- subset(crudo, EDAD < 18)
entran <- do.call(rbind, lapply(levels(usu$ESCOLARIDAD), function(e) {
  x <- entran; x$ESCOLARIDAD <- factor(e, levels(usu$ESCOLARIDAD))
  x$FAC_PER <- x$FAC_PER * as.numeric(reparto[as.character(x$SEXO), e]); x }))
pob_2030 <- rbind(subset(crudo, EDAD >= 18 & !is.na(ESCOLARIDAD)), entran)

p_2030 <- predict(m, newdata = pob_2030, type = "response")
demografico_2030 <- 100 * weighted.mean(as.numeric(p_2030), pob_2030$FAC_PER)
# Misma cuenta con la población de hoy, para comparar con el modelo y no con el dato observado
hoy <- subset(usu, !is.na(ESCOLARIDAD))
p_hoy <- predict(m, newdata = hoy, type = "response")
modelo_hoy <- 100 * weighted.mean(as.numeric(p_hoy), hoy$FAC_PER)
efecto <- demografico_2030 - modelo_hoy
cat(sprintf("Efecto demográfico a 2030: %+.1f pp (modelo hoy %.1f %% → 2030 %.1f %%)\n",
            efecto, modelo_hoy, demografico_2030))
cat(sprintf("Entran en 2030 (hoy 13-17): %.1f millones; usuarios de 18+ hoy: %.1f millones\n",
            sum(entran$FAC_PER) / 1e6, sum(usu$FAC_PER) / 1e6))

demografico <- data.frame(anio = anios, serie = "Solo efecto demográfico",
  estimacion = round(base + efecto * (anios - 2025) / 5, 1), li90 = NA, ls90 = NA)
observado <- data.frame(anio = 2025, serie = "Observado", estimacion = round(base, 1), li90 = NA, ls90 = NA)

cono <- rbind(observado, probable, demografico)
print(cono, row.names = FALSE)
write.csv(cono, file.path("resultados", "C2_futuro_probable.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(cono, prueba = FALSE), file.path("dashboard", "datos", "s6_cono.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s6_cono.csv\n")
