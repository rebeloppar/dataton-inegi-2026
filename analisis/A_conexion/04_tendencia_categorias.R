# =============================================================================
# Frente A — ¿Cómo ha cambiado lo que se compra? (pregunta 3 del reto)
# Con los microdatos de la ENDUTIH 2022-2025 (decisión 94; antes eran cifras
# publicadas para usuarios de 6+). Universo: usuarios de internet de 18+ que
# compraron en línea; % que compró cada categoría, con IC al 90 % y diseño
# muestral de cada año. Solo las cuatro categorías que tienen la misma
# redacción los cuatro años (ver 00_extrae_categorias.py).
# Cambio 2022-2025 y 2024-2025: diferencia de proporciones de muestras
# independientes, error estándar = raíz de la suma de los cuadrados.
# Cotejo: con usuarios de 6+ debe dar lo que publicó el INEGI (2022: hogar
# 40.7; 2025: hogar 50.6, higiene 78.7).
# Necesita: datos/procesados/endutih_2022_2025_categorias.csv
#   (lo arma analisis/A_conexion/00_extrae_categorias.py)
# Escribe: dashboard/datos/s1_tendencia.csv, resultados/A4_tendencia_categorias.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))

h <- read.csv(file.path("datos", "procesados", "endutih_2022_2025_categorias.csv"), colClasses = "character")
for (v in c("edad", "fac", "higiene", "hogar", "alimentos", "salud")) h[[v]] <- as.numeric(h[[v]])
h$anio <- as.integer(h$anio)
h <- subset(h, usa == "1" & compra == "1" & edad <= 97)

nombres <- c(higiene = "Uso e higiene personal (incl. ropa)", hogar = "Artículos para el hogar",
             alimentos = "Alimentos y bebidas", salud = "Artículos de salud")
estima <- function(x) {
  dd <- svydesign(ids = ~upm, strata = ~est, weights = ~fac, data = x, nest = TRUE)
  m <- svymean(~higiene + hogar + alimentos + salud, dd)
  data.frame(var = names(nombres), est = 100 * coef(m), se = 100 * SE(m))
}
t <- do.call(rbind, lapply(2022:2025, function(y) {
  x <- subset(h, anio == y)
  a <- estima(subset(x, edad >= 18)); b <- estima(x)
  data.frame(categoria = nombres[a$var], anio = y, estimacion = round(a$est, 1),
             li90 = round(a$est - z90 * a$se, 1), ls90 = round(a$est + z90 * a$se, 1),
             se = a$se, usuarios_6mas = round(b$est, 1), n = sum(x$edad >= 18))
}))
cat("Cotejo con lo publicado (usuarios de 6+):\n")
print(reshape(t[, c("categoria", "anio", "usuarios_6mas")], idvar = "categoria", timevar = "anio", direction = "wide"),
      row.names = FALSE)
cat("\nUsuarios de 18+ que compraron (IC 90 %):\n")
print(t[, c("categoria", "anio", "estimacion", "li90", "ls90", "n")], row.names = FALSE)

cambio <- function(y0, y1) {
  a <- subset(t, anio == y0); b <- subset(t, anio == y1)
  d <- b$estimacion - a$estimacion; s <- sqrt(a$se^2 + b$se^2)
  data.frame(categoria = a$categoria, desde = y0, hasta = y1, pp = round(d, 1),
             li90 = round(d - z90 * s, 1), ls90 = round(d + z90 * s, 1), claro = abs(d) > z90 * s)
}
cat("\nCambios (pp, IC 90 %):\n")
print(rbind(cambio(2022, 2025), cambio(2024, 2025)), row.names = FALSE)

write.csv(t, file.path("resultados", "A4_tendencia_categorias.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(t[, c("categoria", "anio", "estimacion", "li90", "ls90")], prueba = FALSE),
          file.path("dashboard", "datos", "s1_tendencia.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s1_tendencia.csv\n")
