# =============================================================================
# Frente D — Insumos de «Lo que está en juego», sección 5 (decisiones 74 y 76)
# Ya no hay escenarios ni palancas: la pestaña dice cuántas personas suma cada
# punto de conversión y qué tendría que mostrar el piloto para una meta.
# Para cada grupo: usuarios de internet de 18+ (millones), % que compra en
# línea y, de quienes NO compran, % que llega por cada puerta:
#   puerta 1 = dice «no sé cómo» (P7_22_7)
#   puerta 2 = prefiere en persona (P7_22_1) y NO dice «no sé cómo»
# Quien da las dos razones cuenta solo en la puerta 1 (primero hay que
# aprender), para no contarlo dos veces.
# Grupos: los dos anillos de la decisión 67 (hasta secundaria; núcleo:
# primaria o menos) y el grupo por sexo (decisión 47).
# Escribe: dashboard/datos/s5_simulador.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))

usu$objetivo <- usu$ESCOLARIDAD %in% c("Sin escolaridad", "Preescolar o primaria", "Secundaria")
usu$nucleo   <- usu$ESCOLARIDAD %in% c("Sin escolaridad", "Preescolar o primaria")
usu$puerta1  <- as.numeric(usu$P7_22_7 %in% 1)
usu$puerta2  <- as.numeric(usu$P7_22_1 %in% 1 & !(usu$P7_22_7 %in% 1))
usu$ambas    <- as.numeric(usu$P7_22_1 %in% 1 & usu$P7_22_7 %in% 1)
usu$uno      <- 1
dis <- update(dis, objetivo = usu$objetivo, nucleo = usu$nucleo, puerta1 = usu$puerta1,
              puerta2 = usu$puerta2, ambas = usu$ambas, uno = usu$uno)

grupos <- list(
  "Grupo: hasta secundaria"     = quote(objetivo),
  "Núcleo: primaria o menos"    = quote(nucleo),
  "Mujeres del grupo"           = quote(objetivo & SEXO == "Mujer"),
  "Hombres del grupo"           = quote(objetivo & SEXO == "Hombre"))

fila <- function(nombre) {
  d  <- subset(dis, eval(grupos[[nombre]], usu))
  nc <- subset(d, compra == 0)
  r  <- coef(svymean(~puerta1 + puerta2 + ambas, nc))
  data.frame(grupo = nombre,
             millones = round(coef(svytotal(~uno, d))[[1]] / 1e6, 2),
             pct_compra = round(100 * coef(svymean(~compra, d, na.rm = TRUE))[[1]], 1),
             pct_no_sabe = round(100 * r[["puerta1"]], 1),
             pct_en_persona = round(100 * r[["puerta2"]], 1),
             pct_ambas = round(100 * r[["ambas"]], 1),
             prueba = FALSE)
}
sim <- do.call(rbind, lapply(names(grupos), fila))
print(sim, row.names = FALSE)
write.csv(sim[, c("grupo", "millones", "pct_compra", "pct_no_sabe", "pct_en_persona", "prueba")],
          file.path("dashboard", "datos", "s5_simulador.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cat("\nListo: s5_simulador.csv\n")
