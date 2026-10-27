# =============================================================================
# Frente D — VISTA PREVIA: razones de no compra por estrato socioeconómico
# Provisional para orientar la propuesta. La versión oficial es del frente B
# (analisis/B_barreras/01_razones_por_grupo.R con "ESTRATO"); cuando exista,
# esta vista previa se descarta.
# Universo: usuarios de internet de 18+ que no compraron.
# =============================================================================

source(file.path("R", "prep_endutih.R"))

dis_no <- subset(dis, compra == 0)
principales <- setdiff(names(razones), c("P7_22_8", "P7_22_9", "P7_22_10"))
f <- as.formula(paste("~", paste(principales, collapse = " + ")))
r <- svyby(f, ~ESTRATO, dis_no, svymean, na.rm = TRUE, vartype = "cv")

pct <- t(sapply(principales, function(x) round(100 * r[[x]], 1)))
colnames(pct) <- as.character(r$ESTRATO)
cv  <- t(sapply(principales, function(x) r[[paste0("cv.", x)]]))
txt <- matrix(paste0(format(pct, nsmall = 1), ifelse(cv > 0.30, "*", "")),
              nrow = nrow(pct), dimnames = dimnames(pct))
tabla <- data.frame(razon = unname(razones[principales]), txt, check.names = FALSE)
tabla <- tabla[order(-rowMeans(pct)), ]

cat("\n% de quienes no compran que da cada razón, por estrato (* = CV > 30 %)\n\n")
print(tabla, row.names = FALSE)
cat("\nCasos por estrato:\n"); print(table(usu$ESTRATO[usu$compra == 0]))
write.csv(tabla, "resultados/D2_preview_razones_estrato.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
