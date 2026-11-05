# =============================================================================
# Frente C — Cono de futuros: el futuro probable a 2030 (sección 6, decisiones 86, 91 y 92)
#
# 1. Tendencia (decisión 91). Serie de la ENDUTIH 2016-2025 con microdatos,
#    usuarios de 18+ (03_serie_historica.R). Se ajusta una curva S (logística),
#    la forma clásica de la adopción de tecnologías (difusión de innovaciones,
#    Rogers; modelo de Bass): % = K / (1 + exp(-r (año - t0))).
#    - Mínimos cuadrados ponderados por la precisión de cada año (1 / error
#      estándar al cuadrado): ningún año se fuerza, y los de muestra chica
#      (2019) pesan menos.
#    - El techo K no se puede estimar bien con diez años de datos, así que se
#      fija con grupos que ya existen en México:
#        probable: K = % que compra hoy entre usuarios con educación superior
#        techo alto: K = % que compra hoy entre usuarios de 18-34 con superior
#        si se estanca: K estimado solo con los datos
#    - Banda del probable: IC 90 % simulando los parámetros con su matriz de
#      covarianzas (2,000 simulaciones).
#    - Las curvas se dibujan desde 2016, así se ve dónde ajustan y dónde no
#      (en 2025 la curva probable va arriba del dato observado).
# El efecto demográfico (envejecer 5 años a la población con la probabilidad de
# hoy) se quitó en la decisión 92: no distingue edad de generación y no era
# defendible.
#
# Escribe: dashboard/datos/s6_cono.csv, resultados/C2_futuro_probable.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))

# --- 1. Tendencia: curva S con tres techos ----------------------------------
serie <- read.csv(file.path("resultados", "C3_serie_historica.csv"))
if (is.null(serie$comparable)) stop("Corre antes analisis/C_territorio/03_serie_historica.R")
ajuste <- subset(serie, comparable)
ajuste$se <- (ajuste$ls90 - ajuste$li90) / (2 * z90)
ajuste$w <- 1 / ajuste$se^2
dis_t <- update(dis, sup = usu$ESCOLARIDAD %in% "Superior", sup_joven = usu$ESCOLARIDAD %in% "Superior" & usu$EDAD <= 34)
techo_sup   <- 100 * coef(svymean(~compra, subset(dis_t, sup), na.rm = TRUE))[[1]]
techo_joven <- 100 * coef(svymean(~compra, subset(dis_t, sup_joven), na.rm = TRUE))[[1]]
curva <- function(K = NA) {
  if (is.na(K)) nls(estimacion ~ K / (1 + exp(-r * (anio - t0))), ajuste, weights = w,
                    start = list(K = 60, r = 0.3, t0 = 2020))
  else nls(estimacion ~ K / (1 + exp(-r * (anio - t0))), ajuste, weights = w, start = list(r = 0.3, t0 = 2020))
}
anios <- 2016:2030
banda <- function(m, K = NA) {
  set.seed(2026)
  B <- MASS::mvrnorm(2000, coef(m), vcov(m))
  sim <- apply(B, 1, function(b) { KK <- if (is.na(K)) b[["K"]] else K
                                  KK / (1 + exp(-b[["r"]] * (anios - b[["t0"]]))) })
  list(li = apply(sim, 1, quantile, 0.05), ls = apply(sim, 1, quantile, 0.95))
}
linea <- function(m, nombre, K = NA, con_banda = FALSE) {
  p <- as.numeric(predict(m, data.frame(anio = anios)))
  b <- if (con_banda) banda(m, K) else list(li = NA, ls = NA)
  data.frame(anio = anios, serie = nombre, estimacion = round(p, 1), li90 = round(b$li, 1), ls90 = round(b$ls, 1))
}
m_prob <- curva(techo_sup); m_alto <- curva(techo_joven); m_bajo <- curva()
curvas <- rbind(linea(m_prob, "Probable (curva S)", techo_sup, con_banda = TRUE),
                linea(m_alto, "Techo alto"), linea(m_bajo, "Si se estanca"))
cat(sprintf("Techos: superior %.1f %%, superior 18-34 %.1f %%, solo datos %.1f %%\n",
            techo_sup, techo_joven, coef(m_bajo)[["K"]]))
print(subset(curvas, anio %in% c(2022:2025, 2030)), row.names = FALSE)

observado <- data.frame(anio = serie$anio,
                        serie = ifelse(serie$comparable, "Observado", "Observado (no comparable)"),
                        estimacion = serie$estimacion, li90 = serie$li90, ls90 = serie$ls90)

cono <- rbind(observado, curvas)
print(cono, row.names = FALSE)
write.csv(cono, file.path("resultados", "C2_futuro_probable.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(cono, prueba = FALSE), file.path("dashboard", "datos", "s6_cono.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s6_cono.csv\n")
