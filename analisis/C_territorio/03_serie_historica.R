# =============================================================================
# Frente C — Serie histórica de la compra en línea, ENDUTIH 2015 a 2025
# (decisión 91). Mismo indicador en todos los años: «En los últimos 12 meses,
# ¿ha realizado compras por Internet?», entre usuarios de internet de 18+.
#   Pregunta de compra: P7_10 (2015-2016), P7_19 (2017-2022), P7_21 (2023-2025)
#   Usuario de internet: P7_1. Ojo: en 2015 es «en los últimos doce meses»;
#   desde 2016, «en los últimos tres meses». 2015 se marca como no comparable.
#   2015: edad, sexo y nivel vienen de la tabla de residentes (renglón elegido).
#   2017 no trae el estrato de diseño (EST_DIS): se usa la entidad como estrato.
# Cotejo: con usuarios de 6+, 2023 y 2024 deben dar lo que publicó el INEGI
# (33.4 % y 35.8 %).
# Necesita: datos/procesados/endutih_2015_2024_compra.csv (lo arma el
#   analisis/C_territorio/00_extrae_historicos.py a partir de los microdatos del INEGI)
# Escribe: resultados/C3_serie_historica.csv, resultados/C3_serie_entidades.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))

h <- read.csv(file.path("datos", "procesados", "endutih_2015_2024_compra.csv"), colClasses = "character")
h$edad <- as.numeric(h$edad); h$fac <- as.numeric(h$fac); h$anio <- as.integer(h$anio)
h <- subset(h, usa == "1" & compra %in% c("1", "2") & edad <= 97)
h$compra <- as.numeric(h$compra == "1")
h$est[h$est == ""] <- h$ent[h$est == ""]          # 2017 sin EST_DIS
h$cve_ent <- as.integer(h$ent)

estima <- function(x) {
  dd <- svydesign(ids = ~upm, strata = ~est, weights = ~fac, data = x, nest = TRUE)
  m <- svymean(~compra, dd)
  c(est = 100 * coef(m)[[1]], se = 100 * SE(m)[[1]])
}
serie <- do.call(rbind, lapply(sort(unique(h$anio)), function(y) {
  x <- subset(h, anio == y)
  a <- estima(x); b <- estima(subset(x, edad >= 18))
  data.frame(anio = y, usuarios_6mas = round(a[["est"]], 1), estimacion = round(b[["est"]], 1),
             li90 = round(b[["est"]] - z90 * b[["se"]], 1), ls90 = round(b[["est"]] + z90 * b[["se"]], 1),
             n = sum(x$edad >= 18))
}))
# 2025: de la base actual (usuarios de 18+ y de 6+)
crudo25 <- read.csv(ruta_datos, colClasses = "character", encoding = "UTF-8")
crudo25 <- subset(crudo25, P7_1 == "1" & P7_21 %in% c("1", "2"))
crudo25$FAC_PER <- as.numeric(crudo25$FAC_PER)
p6 <- 100 * weighted.mean(crudo25$P7_21 == "1", crudo25$FAC_PER)
m18 <- svymean(~compra, dis, na.rm = TRUE)
serie <- rbind(serie, data.frame(anio = 2025, usuarios_6mas = round(p6, 1), estimacion = round(100 * coef(m18)[[1]], 1),
                                 li90 = round(100 * (coef(m18)[[1]] - z90 * SE(m18)[[1]]), 1),
                                 ls90 = round(100 * (coef(m18)[[1]] + z90 * SE(m18)[[1]]), 1), n = nrow(usu)))
serie$comparable <- serie$anio >= 2016
cat("\n===== % que compró en línea, usuarios de internet =====\n"); print(serie, row.names = FALSE)
cat("\nCotejo con el INEGI (6+): 2023 publicado 33.4, 2024 publicado 35.8, 2025 publicado 37.3\n")

# Por entidad, usuarios de 18+ (para el ritmo de adopción)
ent <- do.call(rbind, lapply(c(2016, 2020, 2024), function(y) {
  x <- subset(h, anio == y & edad >= 18)
  dd <- svydesign(ids = ~upm, strata = ~est, weights = ~fac, data = x, nest = TRUE)
  r <- svyby(~compra, ~cve_ent, dd, svymean)
  data.frame(anio = y, cve_ent = r$cve_ent, estimacion = round(100 * r$compra, 1), se = round(100 * r$se, 2))
}))
write.csv(serie, file.path("resultados", "C3_serie_historica.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(ent, file.path("resultados", "C3_serie_entidades.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: resultados/C3_serie_historica.csv y C3_serie_entidades.csv\n")
