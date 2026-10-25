# =============================================================================
# Datatón INEGI 2026 — ¿Cuánto se traslapan escolaridad, localidad y estrato?
# Antes del modelo: si dos variables son casi intercambiables, el modelo se
# reparte su efecto de forma arbitraria y hay que interpretarlo con cautela.
# Universo: usuarios de internet de 18 años o más (ver R/prep_endutih.R).
# =============================================================================

source(file.path("R", "prep_endutih.R"))

# --- 1. V de Cramér: una medida de asociación entre dos variables categóricas --
# Va de 0 (sin relación) a 1 (una determina por completo a la otra).
# Guía aproximada: < 0.1 débil · 0.1–0.3 moderada · > 0.3 fuerte · > 0.5 muy fuerte
cramer_v <- function(a, b) {
  tab  <- svytable(as.formula(paste0("~", a, " + ", b)), dis)
  n    <- sum(!is.na(usu[[a]]) & !is.na(usu[[b]]))     # tamaño de muestra real
  tab  <- tab / sum(tab) * n                            # escala a la muestra
  esp  <- outer(rowSums(tab), colSums(tab)) / n         # frecuencias esperadas
  chi2 <- sum((tab - esp)^2 / esp)
  sqrt(chi2 / (n * (min(dim(tab)) - 1)))
}

pares <- list(c("ESCOLARIDAD", "ESTRATO"),
              c("TLOC", "ESTRATO"),
              c("ESCOLARIDAD", "TLOC"))

asoc <- data.frame(
  par       = sapply(pares, paste, collapse = " × "),
  v_cramer  = sapply(pares, function(p) round(cramer_v(p[1], p[2]), 2)))
cat("\n===== Asociación entre las tres explicaciones (V de Cramér) =====\n")
print(asoc, row.names = FALSE)

# --- 2. Tablas cruzadas: cómo se distribuye cada grupo --------------------------
# Porcentajes por fila: de cada grupo de la primera variable, qué % cae en
# cada categoría de la segunda. Si las filas se parecen mucho entre sí, hay
# poco traslape; si cada fila se concentra en una columna distinta, hay mucho.
pct_fila <- function(a, b) {
  tab <- svytable(as.formula(paste0("~", a, " + ", b)), dis)
  round(100 * prop.table(tab, 1), 1)
}

for (p in pares) {
  cat("\n=====", p[1], "(filas) ×", p[2], "(columnas), % por fila =====\n")
  print(pct_fila(p[1], p[2]))
}

write.csv(asoc, "resultados/D_traslape_cramer.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
