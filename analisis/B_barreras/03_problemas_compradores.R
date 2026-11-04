# =============================================================================
# Frente B — Problemas de quienes sí compran en línea (P7_26 y P7_27)
# Pregunta del reto P5: ¿qué problemas reportan quienes sí compran y en quiénes
# se concentran? Pregunta guía: ¿los grupos rezagados que sí compran tienen
# más problemas? (eso alimentaría la barrera de confianza)
# Universo: usuarios de internet de 18+ que compraron en los últimos 12 meses.
# Tipos de problema (P7_27): solo quienes tuvieron alguno; respuesta múltiple,
# los % no suman 100.
# Secciones: 1. % con problema por grupo · 2. tipo de problema (nacional y por
# grupo) · 3. comparaciones con grupos más grandes y prueba de diferencia.
# =============================================================================

source(file.path("R", "prep_endutih.R"))

# TEMPORAL: mientras P7_26 y P7_27_* se integran en R/prep_endutih.R
# (solicitado a la dueña de R/). Borrar este bloque cuando estén en el prep.
usu$problema <- ifelse(usu$P7_26 == "1", 1, ifelse(usu$P7_26 == "2", 0, NA))
problemas <- c(
  P7_27_1 = "Entrega",
  P7_27_2 = "Seguridad del pago",
  P7_27_3 = "Seguridad de los datos personales",
  P7_27_4 = "Calidad del producto",
  P7_27_5 = "Estado del producto",
  P7_27_6 = "Otro problema")
for (v in names(problemas)) {
  usu[[v]] <- ifelse(usu[[v]] == "1", 1, ifelse(usu[[v]] == "2", 0, NA))
}
dis <- update(dis, problema = usu$problema,
              P7_27_1 = usu$P7_27_1, P7_27_2 = usu$P7_27_2, P7_27_3 = usu$P7_27_3,
              P7_27_4 = usu$P7_27_4, P7_27_5 = usu$P7_27_5, P7_27_6 = usu$P7_27_6)
# FIN TEMPORAL

# Grupos más grandes para las comparaciones de la sección 3 (precisión): con
# pocos casos por grupo, los % por tipo de problema son inestables. Solo para
# este análisis, a partir de variables del prep (como los g_* de
# analisis/D_propuesta/01_alcance.R); no sustituyen a las del prep.
#   - escolaridad: "primaria o menos" = sin escolaridad + preescolar o primaria,
#     la misma agrupación del frente D y del grupo objetivo (decisión 46)
#   - edad: 18-34 · 35-54 · 55+ (55+ es el corte de las preguntas de género del
#     frente D; 18-34 y 35-54 separan jóvenes de mediana edad con casos suficientes)
#   - localidad: menos de 2,500 hab. vs. el resto (hallazgo 6)
usu$esc_p5 <- factor(ifelse(usu$ESCOLARIDAD %in% c("Sin escolaridad", "Preescolar o primaria"),
                            "Primaria o menos", as.character(usu$ESCOLARIDAD)),
                     levels = c("Primaria o menos", "Secundaria", "Media superior", "Superior"))
usu$edad_p5 <- cut(usu$EDAD, breaks = c(17, 34, 54, 97),
                   labels = c("18-34", "35-54", "55+"))
usu$loc_p5  <- factor(ifelse(usu$TLOC == "Menos de 2,500", "Menos de 2,500", "2,500 y más"),
                      levels = c("2,500 y más", "Menos de 2,500"))
dis <- update(dis, esc_p5 = usu$esc_p5, edad_p5 = usu$edad_p5, loc_p5 = usu$loc_p5)

cortes <- c("ESCOLARIDAD", "GRUPO_EDAD", "SEXO", "TLOC")
dis_c  <- subset(dis, compra == 1)     # compraron en línea
dis_p  <- subset(dis, problema == 1)   # compraron y tuvieron algún problema

# -----------------------------------------------------------------------------
# 1. ¿Qué % de quienes compran tuvo algún problema? Nacional y por grupo
# -----------------------------------------------------------------------------
nac <- svymean(~problema, dis_c, na.rm = TRUE)
cat(sprintf("\n%.1f %% de quienes compraron en línea tuvo algún problema (IC 90 %%: %.1f–%.1f).\n",
            100 * coef(nac), 100 * (coef(nac) - z90 * SE(nac)),
            100 * (coef(nac) + z90 * SE(nac))))

tabla_1 <- do.call(rbind, lapply(cortes, function(v) {
  r <- svyby(~problema, as.formula(paste0("~", v)), dis_c, svymean,
             na.rm = TRUE, vartype = c("se", "cv"))
  n <- table(usu[[v]][usu$compra == 1])
  data.frame(corte        = v,
             grupo        = r[[v]],
             pct_problema = round(100 * r$problema, 1),
             ic90_inf     = round(100 * (r$problema - z90 * r$se), 1),
             ic90_sup     = round(100 * (r$problema + z90 * r$se), 1),
             cv_pct       = round(100 * r$cv, 1),
             n            = as.vector(n[as.character(r[[v]])]))
}))
rownames(tabla_1) <- NULL

# Distancia entre el grupo con más y con menos problemas, por corte
rango <- aggregate(pct_problema ~ corte, tabla_1, function(x) max(x) - min(x))
names(rango)[2] <- "rango_pp"

cat("\n===== 1. % de compradores que tuvo algún problema, por grupo =====\n")
cat("(cv_pct > 30 = estimación poco precisa; n = compradores en muestra)\n\n")
print(tabla_1, row.names = FALSE)
cat("\nRango entre grupos (pp):\n"); print(rango, row.names = FALSE)

write.csv(tabla_1, "resultados/B3_problema_por_grupo.csv", row.names = FALSE,
          fileEncoding = "UTF-8")

# -----------------------------------------------------------------------------
# 2. ¿Qué tipo de problema? Entre quienes tuvieron alguno
# -----------------------------------------------------------------------------
f_prob <- as.formula(paste("~", paste(names(problemas), collapse = " + ")))

tipos <- svymean(f_prob, dis_p, na.rm = TRUE)
tabla_2 <- data.frame(problema = unname(problemas),
                      pct      = round(100 * coef(tipos), 1),
                      cv_pct   = round(100 * SE(tipos) / coef(tipos), 1),
                      row.names = NULL)
tabla_2 <- tabla_2[order(-tabla_2$pct), ]

cat("\n===== 2. Tipo de problema (% de quienes tuvieron alguno) =====\n")
cat(sprintf("(casos en muestra: %d)\n\n", sum(usu$problema == 1, na.rm = TRUE)))
print(tabla_2, row.names = FALSE)

write.csv(tabla_2, "resultados/B3_tipos_problema.csv", row.names = FALSE,
          fileEncoding = "UTF-8")

# Por grupo, con la misma bandera que 01_razones_por_grupo.R
tipos_por <- function(v) {
  r <- svyby(f_prob, as.formula(paste0("~", v)), dis_p, svymean,
             na.rm = TRUE, vartype = c("se", "cv"))
  grupos <- as.character(r[[v]])

  pct <- t(sapply(names(problemas), function(x) round(100 * r[[x]], 1)))
  colnames(pct) <- grupos

  # Bandera de precisión: CV > 30 % (o no calculable, si nadie dio ese
  # problema en el grupo) se marca con "*"
  cv  <- t(sapply(names(problemas), function(x) r[[paste0("cv.", x)]]))
  txt <- matrix(paste0(format(pct, nsmall = 1),
                       ifelse(is.na(cv) | cv > 0.30, "*", "")),
                nrow = nrow(pct), dimnames = dimnames(pct))

  out <- data.frame(problema = unname(problemas), txt,
                    check.names = FALSE, row.names = NULL)
  out$rango_pp <- round(apply(pct, 1, max) - apply(pct, 1, min), 1)

  n <- table(usu[[v]][usu$problema %in% 1])
  attr(out, "n") <- n[grupos]
  out[order(-rowMeans(pct)), ]
}

for (v in cortes) {
  tab <- tipos_por(v)
  cat("\n=====", v, ": % de quienes tuvieron problema, por tipo =====\n")
  cat("(* = CV > 30 %, estimación poco precisa; rango_pp = mayor − menor grupo)\n\n")
  print(tab, row.names = FALSE)
  cat("\nCasos en muestra por grupo:\n"); print(attr(tab, "n"))
  write.csv(tab, file.path("resultados", paste0("B3_tipos_problema_por_", v, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
}

# -----------------------------------------------------------------------------
# 3. Comparaciones con grupos más grandes: ¿la diferencia es clara?
#    Cada grupo se compara con su referencia (el primero de cada corte).
#    clara = el IC 90 % de la diferencia no incluye 0 (mismo criterio que
#    analisis/diagnostico/02_cruces.R). La sección 1 y los B3_tipos_problema_por_*
#    conservan el detalle completo, con su bandera de precisión.
# -----------------------------------------------------------------------------
compara <- function(v, ys, d, etiquetas) {
  f <- as.formula(paste("~", paste(ys, collapse = " + ")))
  r <- svyby(f, as.formula(paste0("~", v)), d, svymean, na.rm = TRUE, covmat = TRUE)
  b <- coef(r); V <- vcov(r)
  grupos <- as.character(r[[v]]); ref <- grupos[1]
  clave  <- function(g, y) if (length(ys) == 1) g else paste0(g, ":", y)
  n <- table(as.character(d$variables[[v]])[weights(d) > 0])

  do.call(rbind, lapply(ys, function(y) do.call(rbind, lapply(grupos, function(g) {
    k <- clave(g, y); k0 <- clave(ref, y)
    es_ref <- g == ref
    dif <- if (es_ref) NA else b[k] - b[k0]
    se  <- if (es_ref) NA else sqrt(V[k, k] + V[k0, k0] - 2 * V[k, k0])
    data.frame(corte    = v,
               que      = etiquetas[[y]],
               grupo    = g,
               pct      = round(100 * b[k], 1),
               cv_pct   = round(100 * sqrt(V[k, k]) / b[k], 1),
               n        = as.vector(n[g]),
               dif_pp   = round(100 * dif, 1),
               ic90_inf = round(100 * (dif - z90 * se), 1),
               ic90_sup = round(100 * (dif + z90 * se), 1),
               clara    = if (es_ref) "ref." else ifelse(abs(dif) > z90 * se, "sí", "no"),
               row.names = NULL)
  }))))
}

cortes_p5 <- c("esc_p5", "edad_p5", "loc_p5", "SEXO")

# 3a. % de compradores que tuvo algún problema
tabla_3a <- do.call(rbind, lapply(cortes_p5, compara, ys = "problema", d = dis_c,
                                  etiquetas = list(problema = "Tuvo algún problema")))
cat("\n===== 3a. % de compradores con algún problema, grupos comparables =====\n")
cat("(dif_pp = grupo − referencia; clara = IC 90 % de la diferencia sin 0)\n\n")
print(tabla_3a[, -2], row.names = FALSE)
write.csv(tabla_3a, "resultados/B3_comparaciones_problema.csv", row.names = FALSE,
          fileEncoding = "UTF-8")

# 3b. Tipo de problema, entre quienes tuvieron alguno
tabla_3b <- do.call(rbind, lapply(cortes_p5, compara, ys = names(problemas), d = dis_p,
                                  etiquetas = as.list(problemas)))
cat("\n===== 3b. Tipo de problema por grupo comparable (% de quienes tuvieron alguno) =====\n")
cat("(cv_pct > 30 = poco preciso; dif_pp = grupo − referencia; clara = IC 90 % sin 0)\n\n")
print(tabla_3b, row.names = FALSE)
write.csv(tabla_3b, "resultados/B3_comparaciones_tipos.csv", row.names = FALSE,
          fileEncoding = "UTF-8")

# -----------------------------------------------------------------------------
# 4. Dashboard, sección 3 «Problemas de quienes sí compran»
#    → dashboard/datos/s3_problemas.csv
#    Universo: TODOS los que compraron (como pide dashboard/README.md), no solo
#    quienes tuvieron problema como en la sección 2: quien no tuvo problemas
#    cuenta como 0 en cada tipo. Así «Algún problema» y los tipos están en la
#    misma base y se leen como «% de compradores que tuvo...» (decisión 84).
# -----------------------------------------------------------------------------
source(file.path("analisis", "B_barreras", "00_exportar_dashboard.R"))

for (v in names(problemas)) {
  usu[[paste0("c_", v)]] <- ifelse(usu$problema %in% 0, 0, usu[[v]])
}
dis <- update(dis,
              c_P7_27_1 = usu$c_P7_27_1, c_P7_27_2 = usu$c_P7_27_2,
              c_P7_27_3 = usu$c_P7_27_3, c_P7_27_4 = usu$c_P7_27_4,
              c_P7_27_5 = usu$c_P7_27_5, c_P7_27_6 = usu$c_P7_27_6)

vars_dash <- c("problema", paste0("c_", names(problemas)))
etiquetas_dash <- c(problema = "Algún problema",
                    setNames(unname(problemas), paste0("c_", names(problemas))))

# Control: % con algún problema igual al de la sección 1
dis_c <- subset(dis, compra == 1)
s3_problemas <- estimar_por_cortes(preparar_cortes(dis_c), vars_dash, etiquetas_dash)
algun_nac <- s3_problemas$estimacion[s3_problemas$grupo_var == "Nacional" &
                                     s3_problemas$categoria == "Algún problema"]
cat(sprintf("\nControl: algún problema = %.1f %% (sección 1: %.1f %%). Deben coincidir;\n",
            algun_nac, 100 * coef(nac)))
cat("si difieren, hay compradores con problema sin tipo (P7_27 vacío).\n")
escribir_dashboard(s3_problemas, "s3_problemas.csv", "problema")
