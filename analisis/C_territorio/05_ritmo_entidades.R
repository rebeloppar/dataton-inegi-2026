# =============================================================================
# Frente C — Ritmo de adopción por entidad (pregunta 6 del reto; decisión 104)
# % de usuarios de internet de 18+ que compró en línea, al inicio (promedio
# 2016-2017) y al final (promedio 2024-2025) del periodo comparable, por
# entidad. Se promedian dos años en cada punta porque un solo año por entidad
# es ruidoso (Chiapas: 29.4 % en 2024 y 25.1 % en 2025).
#   Promedio de dos años: (a + b) / 2, error estándar raíz(se_a² + se_b²) / 2
#   (muestras independientes). Cambio: fin − inicio, error estándar raíz de la
#   suma de los cuadrados. Intervalos al 90 %.
# Observado, sin controles: el ritmo de una entidad puede deberse a la
# escolaridad, la localidad o el estrato de su población (como pasa con el Sur).
# ¿Convergen? Correlación entre el nivel inicial y el avance, y dispersión
# (desviación estándar entre entidades) al inicio y al final.
# Necesita: datos/procesados/endutih_2015_2024_compra.csv (00_extrae_historicos.py)
#   y la ENDUTIH 2025 (R/prep_endutih.R)
# Escribe: dashboard/datos/s4_ritmo.csv, resultados/C5_ritmo_entidades.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))

h <- read.csv(file.path("datos", "procesados", "endutih_2015_2024_compra.csv"), colClasses = "character")
h$edad <- as.numeric(h$edad); h$fac <- as.numeric(h$fac); h$anio <- as.integer(h$anio)
h <- subset(h, usa == "1" & compra %in% c("1", "2") & edad >= 18 & edad <= 97)
h$compra <- as.numeric(h$compra == "1")
h$est[h$est == ""] <- h$ent[h$est == ""]          # 2017 sin EST_DIS
h$cve_ent <- as.integer(h$ent)

por_entidad <- function(dd, y) {
  r <- svyby(~compra, ~cve_ent, dd, svymean, na.rm = TRUE)
  data.frame(anio = y, cve_ent = as.integer(as.character(r$cve_ent)), est = 100 * r$compra, se = 100 * r$se)
}
e <- do.call(rbind, lapply(c(2016, 2017, 2024), function(y) {
  x <- subset(h, anio == y)
  por_entidad(svydesign(ids = ~upm, strata = ~est, weights = ~fac, data = x, nest = TRUE), y)
}))
dis25 <- update(dis, cve_ent = as.integer(as.character(CVE_ENT)))
e <- rbind(e, por_entidad(dis25, 2025))
cat("Por año y entidad (18+):\n"); print(reshape(e[, c("anio", "cve_ent", "est")], idvar = "cve_ent",
                                                 timevar = "anio", direction = "wide"), digits = 3, row.names = FALSE)

promedio <- function(y1, y2) {
  a <- subset(e, anio == y1); b <- subset(e, anio == y2)
  b <- b[match(a$cve_ent, b$cve_ent), ]
  data.frame(cve_ent = a$cve_ent, est = (a$est + b$est) / 2, se = sqrt(a$se^2 + b$se^2) / 2)
}
ini <- promedio(2016, 2017); fin <- promedio(2024, 2025)
fin <- fin[match(ini$cve_ent, fin$cve_ent), ]
d <- fin$est - ini$est; s <-sqrt(ini$se^2 + fin$se^2)
r <- data.frame(cve_ent = ini$cve_ent, anio_inicio = "2016-17", anio_fin = "2024-25",
                inicio = round(ini$est, 1), fin = round(fin$est, 1),
                avance = round(d, 1), li90 = round(d - z90 * s, 1), ls90 = round(d + z90 * s, 1))
r <- r[order(r$avance), ]
cat("\nAvance por entidad (pp, IC 90 %):\n"); print(r, row.names = FALSE)

cat("\n¿Convergen?\n")
cat(sprintf("  Correlación nivel inicial – avance: %.2f\n", cor(r$inicio, r$avance)))
cat(sprintf("  Desviación estándar entre entidades: %.1f (2016-17) -> %.1f (2024-25)\n", sd(r$inicio), sd(r$fin)))
cat(sprintf("  Avance: mínimo %.1f, mediana %.1f, máximo %.1f\n", min(r$avance), median(r$avance), max(r$avance)))
n <- nrow(r); cat(sprintf("  Cuántas entidades avanzan claramente menos que la mediana: %d\n",
                          sum(r$ls90 < median(r$avance))))

write.csv(r, file.path("resultados", "C5_ritmo_entidades.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(r, prueba = FALSE), file.path("dashboard", "datos", "s4_ritmo.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s4_ritmo.csv\n")
