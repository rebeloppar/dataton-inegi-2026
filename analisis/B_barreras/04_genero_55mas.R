# =============================================================================
# Frente B — Razones para no comprar de mujeres vs. hombres de 55+
# Pregunta del frente D (género): ¿las mujeres de 55+ dan razones distintas a
# las de los hombres de 55+? ¿La brecha es mayor que en 18-54?
#   1. Razones por sexo en 55+ y en 18-54 (sin controles, como la tabla C de
#      analisis/diagnostico/02_cruces.R)
#   2. ¿La brecha mujer − hombre cambia con la edad? (diferencia de brechas)
#   3. "No sabe cómo hacerlo" con controles: brecha mujer − hombre con las
#      variables del modelo completo de analisis/diagnostico/04_regresion.R
# Universo: usuarios de internet de 18+ que NO compraron en los últimos 12 meses.
# Cada persona puede dar varias razones: los % no suman 100.
# Problemas reportados por sexo (quienes sí compran): ver
# analisis/B_barreras/03_problemas_compradores.R, sección 3b.
# =============================================================================

source(file.path("R", "prep_endutih.R"))
suppressMessages(library(marginaleffects))

# Grupo de edad para esta comparación (55+ = corte de las preguntas de género
# del frente D). Solo para este análisis, a partir de EDAD del prep.
usu$edad_55 <- factor(ifelse(usu$EDAD >= 55, "55+", "18-54"), levels = c("18-54", "55+"))
dis <- update(dis, edad_55 = usu$edad_55)

dis_no <- subset(dis, compra == 0)
f_raz  <- as.formula(paste("~", paste(names(razones), collapse = " + ")))

# Razones por edad (18-54, 55+) y sexo, con covarianzas para comparar
r <- svyby(f_raz, ~edad_55 + SEXO, dis_no, svymean, na.rm = TRUE, covmat = TRUE)
b <- coef(r); V <- vcov(r)
k <- function(e, s, v) paste0(e, ".", s, ":", v)

# -----------------------------------------------------------------------------
# 1. Razones por sexo dentro de cada grupo de edad (sin controles)
# -----------------------------------------------------------------------------
tabla_1 <- do.call(rbind, lapply(c("55+", "18-54"), function(e) {
  do.call(rbind, lapply(names(razones), function(v) {
    h <- k(e, "Hombre", v); m <- k(e, "Mujer", v)
    dif <- b[m] - b[h]
    se  <- sqrt(V[m, m] + V[h, h] - 2 * V[m, h])
    data.frame(edad        = e,
               razon       = razones[[v]],
               hombres_pct = round(100 * b[h], 1),
               mujeres_pct = round(100 * b[m], 1),
               brecha_pp   = round(100 * dif, 1),
               ic90_inf    = round(100 * (dif - z90 * se), 1),
               ic90_sup    = round(100 * (dif + z90 * se), 1),
               clara       = ifelse(abs(dif) > z90 * se, "sí", "no"),
               row.names   = NULL)
  }))
}))

cat("\n===== 1. Razones para no comprar, mujeres vs. hombres (% de quienes no compraron) =====\n")
cat("(brecha_pp = mujeres − hombres; clara = el IC 90 % no incluye 0)\n")
for (e in c("55+", "18-54")) {
  cat("\n---", e, "---\n")
  t <- tabla_1[tabla_1$edad == e, -1]
  print(t[order(-(t$hombres_pct + t$mujeres_pct)), ], row.names = FALSE)
}
cat("\nCasos en muestra (no compraron):\n")
print(table(usu$edad_55[usu$compra == 0], usu$SEXO[usu$compra == 0]))

write.csv(tabla_1, "resultados/B4_razones_genero_55mas.csv", row.names = FALSE,
          fileEncoding = "UTF-8")

# -----------------------------------------------------------------------------
# 2. ¿La brecha mujer − hombre es distinta en 55+ que en 18-54?
#    Diferencia de brechas = (M − H en 55+) − (M − H en 18-54)
# -----------------------------------------------------------------------------
tabla_2 <- do.call(rbind, lapply(names(razones), function(v) {
  cc <- c(k("55+", "Mujer", v), k("55+", "Hombre", v),
          k("18-54", "Mujer", v), k("18-54", "Hombre", v))
  w  <- c(1, -1, -1, 1)
  dd <- sum(w * b[cc])
  se <- sqrt(as.numeric(t(w) %*% V[cc, cc] %*% w))
  data.frame(razon        = razones[[v]],
             brecha_55mas = round(100 * (b[cc[1]] - b[cc[2]]), 1),
             brecha_18_54 = round(100 * (b[cc[3]] - b[cc[4]]), 1),
             diferencia_pp = round(100 * dd, 1),
             ic90_inf     = round(100 * (dd - z90 * se), 1),
             ic90_sup     = round(100 * (dd + z90 * se), 1),
             clara        = ifelse(abs(dd) > z90 * se, "sí", "no"),
             row.names    = NULL)
}))

cat("\n===== 2. ¿La brecha de género cambia con la edad? (pp) =====\n")
cat("(diferencia_pp = brecha en 55+ − brecha en 18-54; clara = IC 90 % sin 0)\n\n")
print(tabla_2, row.names = FALSE)

write.csv(tabla_2, "resultados/B4_diferencia_brechas_edad.csv", row.names = FALSE,
          fileEncoding = "UTF-8")

# -----------------------------------------------------------------------------
# 3. "No sabe cómo hacerlo": brecha mujer − hombre con controles
#    Mismos controles, referencias y método que el modelo completo (bloque 4)
#    de analisis/diagnostico/04_regresion.R. Efecto marginal promedio en pp.
# -----------------------------------------------------------------------------
usu$ESCOLARIDAD <- relevel(usu$ESCOLARIDAD, ref = "Preescolar o primaria")
dis <- update(dis, ESCOLARIDAD = usu$ESCOLARIDAD)

f_completo <- P7_22_7 ~ SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD +
                        ESCOLARIDAD + TLOC + ESTRATO

brecha_ajustada <- function(d, nombre) {
  # Quita grupos de edad sin casos en el subconjunto (p. ej., 18-24 en 55+)
  d <- update(d, GRUPO_EDAD = droplevels(GRUPO_EDAD))
  m <- svyglm(f_completo, design = d, family = quasibinomial())
  e <- avg_comparisons(m, variables = "SEXO", wts = "(weights)", conf_level = 0.90)
  data.frame(grupo    = nombre,
             ame_pp   = round(100 * e$estimate, 1),
             ic90_inf = round(100 * e$conf.low, 1),
             ic90_sup = round(100 * e$conf.high, 1))
}

dis_no <- subset(dis, compra == 0)
tabla_3 <- rbind(brecha_ajustada(subset(dis_no, edad_55 == "55+"),   "55+"),
                 brecha_ajustada(subset(dis_no, edad_55 == "18-54"), "18-54"),
                 brecha_ajustada(dis_no,                             "Todos 18+"))
r_todos <- svyby(~P7_22_7, ~SEXO, dis_no, svymean, na.rm = TRUE)
tabla_3$sin_controles_pp <- c(
  round(100 * (b[k("55+", "Mujer", "P7_22_7")] - b[k("55+", "Hombre", "P7_22_7")]), 1),
  round(100 * (b[k("18-54", "Mujer", "P7_22_7")] - b[k("18-54", "Hombre", "P7_22_7")]), 1),
  round(100 * (coef(r_todos)["Mujer"] - coef(r_todos)["Hombre"]), 1))

cat("\n===== 3. 'No sabe cómo hacerlo': brecha mujer − hombre con controles (pp) =====\n")
cat("(controles: edad, indígena, discapacidad, escolaridad, localidad y estrato;\n")
cat(" si la brecha se mantiene, no se explica por esas características)\n\n")
print(tabla_3, row.names = FALSE)

write.csv(tabla_3, "resultados/B4_no_sabe_como_sexo_ajustada.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
