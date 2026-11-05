# =============================================================================
# Frente C — Cono de futuros: el futuro probable a 2030 (sección 6, decisión 86)
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

# --- 1. Tendencia: curva S con tres techos ----------------------------------
serie <- read.csv(file.path("resultados", "C3_serie_historica.csv"))
if (is.null(serie$comparable)) stop("Corre antes analisis/C_territorio/03_serie_historica.R")
ajuste <- subset(serie, comparable)
ajuste$se <- (ajuste$ls90 - ajuste$li90) / (2 * z90)
ajuste$w <- 1 / ajuste$se^2
base <- 100 * coef(svymean(~compra, dis, na.rm = TRUE))[[1]]    # usuarios de 18+, 2025
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

demografico <- data.frame(anio = 2025:2030, serie = "Solo efecto demográfico",
  estimacion = round(base + efecto * (2025:2030 - 2025) / 5, 1), li90 = NA, ls90 = NA)
observado <- data.frame(anio = serie$anio,
                        serie = ifelse(serie$comparable, "Observado", "Observado (no comparable)"),
                        estimacion = serie$estimacion, li90 = serie$li90, ls90 = serie$ls90)

cono <- rbind(observado, curvas, demografico)
print(cono, row.names = FALSE)
write.csv(cono, file.path("resultados", "C2_futuro_probable.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(cono, prueba = FALSE), file.path("dashboard", "datos", "s6_cono.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s6_cono.csv\n")
