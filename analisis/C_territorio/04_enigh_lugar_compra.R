# =============================================================================
# Frente C — ¿El grupo ya compra en las tiendas de la ANTAD? (decisión 98)
# Pone a prueba el supuesto de la propuesta: «el grupo ya va a la tienda».
# ENIGH 2024, hogares. Gasto monetario trimestral con lugar de compra
# (00_extrae_enigh.py). Formatos ANTAD = supermercados, departamentales y
# tiendas con membresía; las de conveniencia se reportan aparte.
# Ojo: la escolaridad es la de la jefa o jefe del hogar, no la de cada persona
# (la ENIGH registra el gasto por hogar); es la mejor aproximación al grupo.
# Por escolaridad de la jefatura y por tamaño de localidad:
#   1. % de hogares que gastó algo en cada lugar (en el periodo de referencia)
#   2. % de su gasto con lugar de compra que va a cada lugar (razón de totales)
# Diseño muestral de la ENIGH; IC al 90 %.
# Necesita: datos/procesados/enigh_2024_lugar_compra.csv
# Escribe: dashboard/datos/s5_enigh.csv, resultados/C4_enigh_lugar_compra.csv
# =============================================================================

source(file.path("R", "00_config.R"))

h <- read.csv(file.path("datos", "procesados", "enigh_2024_lugar_compra.csv"), colClasses = "character")
lug <- c("antad", "conveniencia", "tradicional", "internet", "otro")
for (v in c("factor", paste0("gasto_", lug))) h[[v]] <- as.numeric(h[[v]])
h$total <- rowSums(h[paste0("gasto_", lug)])
h$antad_conv <- h$gasto_antad + h$gasto_conveniencia
e <- as.integer(h$educa_jefe)
h$escolaridad <- factor(cut(e, c(0, 4, 6, 8, 11), c("Primaria o menos", "Secundaria", "Media superior", "Superior")),
                        c("Primaria o menos", "Secundaria", "Media superior", "Superior"))
h$hasta_sec <- ifelse(e <= 6, "Hasta secundaria", "Media superior o más")
h$localidad <- ifelse(h$tam_loc == "4", "Rural (menos de 2,500)", "Urbana (2,500 y más)")
for (l in c(lug, "antad_conv")) h[[paste0("algo_", l)]] <- as.numeric(h[[if (l == "antad_conv") l else paste0("gasto_", l)]] > 0)
h$nacional <- "Todos los hogares"
d <- svydesign(ids = ~upm, strata = ~est_dis, weights = ~factor, data = h, nest = TRUE)

medidas <- c(antad = "Supermercados, departamentales o membresía", antad_conv = "Formatos ANTAD con conveniencia",
             conveniencia = "Tiendas de conveniencia", tradicional = "Mercado, tianguis o abarrotes",
             internet = "Internet")
estima <- function(por) {
  do.call(rbind, lapply(names(medidas), function(l) {
    a <- svyby(as.formula(paste0("~algo_", l)), as.formula(paste0("~", por)), d, svymean)
    num <- if (l == "antad_conv") "antad_conv" else paste0("gasto_", l)
    r <- svyby(as.formula(paste0("~", num)), as.formula(paste0("~", por)), d, svyratio,
               denominator = ~total)
    sr <- SE(r)
    rbind(data.frame(grupo_var = por, grupo = a[[por]], medida = medidas[[l]], indicador = "% de hogares que compró ahí",
                     estimacion = 100 * a[[2]], se = 100 * a[[3]]),
          data.frame(grupo_var = por, grupo = r[[por]], medida = medidas[[l]], indicador = "% de su gasto en compras",
                     estimacion = 100 * r[[2]], se = 100 * as.numeric(sr)))
  }))
}
res <- do.call(rbind, lapply(c("nacional", "hasta_sec", "escolaridad", "localidad"), estima))

# El grupo por localidad: hasta secundaria, rural vs. urbano
d_gr <- subset(d, hasta_sec == "Hasta secundaria")
x <- svyby(~algo_antad + algo_antad_conv + algo_internet, ~localidad, d_gr, svymean)
cat("\nHogares con jefatura hasta secundaria, por localidad (% que compró ahí):\n")
print(data.frame(localidad = x$localidad, antad = round(100 * x$algo_antad, 1),
                 antad_conv = round(100 * x$algo_antad_conv, 1), internet = round(100 * x$algo_internet, 1)),
      row.names = FALSE)
gr_loc <- do.call(rbind, lapply(c("antad", "antad_conv", "tradicional", "internet"), function(l) {
  a <- svyby(as.formula(paste0("~algo_", l)), ~localidad, d_gr, svymean)
  data.frame(grupo_var = "hasta_sec_localidad", grupo = paste("Hasta secundaria,", tolower(a$localidad)),
             medida = medidas[[l]], indicador = "% de hogares que compró ahí",
             estimacion = 100 * a[[2]], se = 100 * a[[3]])
}))
res <- rbind(res, gr_loc)

res$li90 <- round(res$estimacion - z90 * res$se, 1)
res$ls90 <- round(res$estimacion + z90 * res$se, 1)
res$estimacion <- round(res$estimacion, 1)
res$cv <- round(res$se / res$estimacion, 3)
cat("\nResultados (IC 90 %):\n")
print(res[, c("grupo", "medida", "indicador", "estimacion", "li90", "ls90")], row.names = FALSE)
cat("\nHogares en la muestra:", nrow(h), "| sin gasto con lugar de compra:", sum(h$total == 0), "\n")
cat("Hogares (millones) con jefatura hasta secundaria:",
    round(sum(h$factor[h$hasta_sec == "Hasta secundaria"]) / 1e6, 1), "de", round(sum(h$factor) / 1e6, 1), "\n")

write.csv(res, file.path("resultados", "C4_enigh_lugar_compra.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(res[, c("grupo_var", "grupo", "medida", "indicador", "estimacion", "li90", "ls90")], prueba = FALSE),
          file.path("dashboard", "datos", "s5_enigh.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s5_enigh.csv\n")
