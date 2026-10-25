# =============================================================================
# Datatón INEGI 2026 — Cruces de compra en línea (ENDUTIH 2025)
#   A. Todos los cortes simples en una sola tabla
#   B. Brecha mujer – hombre dentro de cada grupo de los demás cortes
#   C. Razones para no comprar, por sexo
# Universo: usuarios de internet de 18 años o más. Resultados en resultados/.
# =============================================================================

source(file.path("R", "prep_endutih.R"))

cortes <- c("SEXO", "TLOC", "ESTRATO", "GRUPO_EDAD", "ESCOLARIDAD", "ENTIDAD")

# -----------------------------------------------------------------------------
# A. Cortes simples: % que compró en línea en cada grupo
# -----------------------------------------------------------------------------
tabla_a <- do.call(rbind, lapply(cortes, function(v) {
  r <- svyby(~compra, as.formula(paste0("~", v)), dis, svymean,
             na.rm = TRUE, vartype = c("se", "cv"))
  data.frame(corte      = v,
             grupo      = r[[v]],
             pct_compra = round(100 * r$compra, 1),
             ic90_inf   = round(100 * (r$compra - z90 * r$se), 1),
             ic90_sup   = round(100 * (r$compra + z90 * r$se), 1),
             cv_pct     = round(100 * r$cv, 1))
}))

# Distancia entre el grupo que más y el que menos compra, por corte
rango <- aggregate(pct_compra ~ corte, tabla_a, function(x) max(x) - min(x))
names(rango)[2] <- "brecha_max_pp"
rango <- rango[order(-rango$brecha_max_pp), ]

cat("\n===== A. ¿Qué corte separa más a quienes compran de quienes no? =====\n")
print(rango, row.names = FALSE)
cat("\n(puntos porcentuales entre el grupo con mayor y menor % de compra)\n")
cat("\n===== A. Detalle por grupo =====\n")
print(tabla_a[tabla_a$corte != "ENTIDAD", -1], row.names = FALSE)

write.csv(tabla_a, "resultados/A_compra_cortes_simples.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# -----------------------------------------------------------------------------
# B. Sexo × otro corte: brecha mujer – hombre dentro de cada grupo
#    (una brecha nacional pequeña puede esconder brechas grandes en subgrupos)
# -----------------------------------------------------------------------------
brecha_sexo <- function(v) {
  r <- svyby(~compra, as.formula(paste0("~", v, " + SEXO")), dis, svymean,
             na.rm = TRUE, covmat = TRUE)
  V <- vcov(r)
  grupos <- levels(usu[[v]])
  do.call(rbind, lapply(grupos, function(g) {
    h <- paste(g, "Hombre", sep = "."); m <- paste(g, "Mujer", sep = ".")
    if (!all(c(h, m) %in% rownames(V))) return(NULL)
    dif <- coef(r)[m] - coef(r)[h]
    se  <- sqrt(V[m, m] + V[h, h] - 2 * V[m, h])
    data.frame(corte = v, grupo = g,
               hombres_pct = round(100 * coef(r)[h], 1),
               mujeres_pct = round(100 * coef(r)[m], 1),
               brecha_pp   = round(100 * dif, 1),
               ic90_inf    = round(100 * (dif - z90 * se), 1),
               ic90_sup    = round(100 * (dif + z90 * se), 1),
               clara       = ifelse(abs(dif) > z90 * se, "sí", "no"))
  }))
}

tabla_b <- do.call(rbind, lapply(setdiff(cortes, "SEXO"), brecha_sexo))
rownames(tabla_b) <- NULL

cat("\n===== B. Brecha mujer – hombre dentro de cada grupo =====\n")
cat("(negativa = las mujeres compran menos; clara = el IC 90 % no incluye 0)\n\n")
print(tabla_b[tabla_b$corte != "ENTIDAD", -1], row.names = FALSE)
cat("\nEntidades con brecha clara:\n")
print(subset(tabla_b, corte == "ENTIDAD" & clara == "sí")[, -1], row.names = FALSE)

write.csv(tabla_b, "resultados/B_brecha_sexo_por_grupo.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# -----------------------------------------------------------------------------
# C. Razones para no comprar, por sexo (solo usuarios que NO compraron)
#    Cada persona puede dar varias razones: los % no suman 100.
# -----------------------------------------------------------------------------
dis_no <- subset(dis, compra == 0)
f_raz  <- as.formula(paste("~", paste(names(razones), collapse = " + ")))
r <- svyby(f_raz, ~SEXO, dis_no, svymean, na.rm = TRUE, covmat = TRUE)
V <- vcov(r)

tabla_c <- do.call(rbind, lapply(names(razones), function(v) {
  h <- paste0("Hombre:", v); m <- paste0("Mujer:", v)
  dif <- coef(r)[m] - coef(r)[h]
  se  <- sqrt(V[m, m] + V[h, h] - 2 * V[m, h])
  data.frame(razon       = razones[[v]],
             hombres_pct = round(100 * coef(r)[h], 1),
             mujeres_pct = round(100 * coef(r)[m], 1),
             brecha_pp   = round(100 * dif, 1),
             clara       = ifelse(abs(dif) > z90 * se, "sí", "no"))
}))
tabla_c <- tabla_c[order(-(tabla_c$hombres_pct + tabla_c$mujeres_pct)), ]

cat("\n===== C. Razones para no comprar, por sexo (% de quienes no compraron) =====\n")
print(tabla_c, row.names = FALSE)

write.csv(tabla_c, "resultados/C_razones_no_compra_por_sexo.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
