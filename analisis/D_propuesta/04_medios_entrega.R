# =============================================================================
# Frente D — ¿Cómo reciben sus compras quienes ya compran en línea? (P7_25)
# Pista de omnicanalidad: cuántos ya recogen en punto de venta, por escolaridad.
# Universo: usuarios de internet de 18+ que compraron en línea.
# P7_25 es de respuesta múltiple: los % no suman 100.
# =============================================================================

source(file.path("R", "prep_endutih.R"))

medios <- c(P7_25_1 = "Paquetería", P7_25_2 = "Entrega directa del vendedor",
            P7_25_3 = "Recoge en punto de venta o distribución",
            P7_25_4 = "Descarga en línea", P7_25_5 = "Otro medio")
for (v in names(medios)) {
  usu[[v]] <- ifelse(usu[[v]] == "1", 1, ifelse(usu[[v]] == "2", 0, NA))
}
dis <- update(dis, P7_25_1 = usu$P7_25_1, P7_25_2 = usu$P7_25_2,
              P7_25_3 = usu$P7_25_3, P7_25_4 = usu$P7_25_4, P7_25_5 = usu$P7_25_5)
d <- subset(dis, compra == 1)

f <- as.formula(paste("~", paste(names(medios), collapse = " + ")))
nac <- svymean(f, d, na.rm = TRUE)
cat("\n% de compradores (18+) que recibió compras por cada medio:\n")
print(data.frame(medio = unname(medios), pct = round(100 * coef(nac), 1)), row.names = FALSE)

r <- svyby(~P7_25_2 + P7_25_3, ~ESCOLARIDAD, d, svymean, na.rm = TRUE, vartype = "cv")
tabla <- data.frame(escolaridad = r$ESCOLARIDAD,
                    entrega_directa = round(100 * r$P7_25_2, 1),
                    recoge_punto_venta = round(100 * r$P7_25_3, 1),
                    cv_recoge = round(100 * r$cv.P7_25_3, 1))
cat("\nPor escolaridad (cv > 30 % = poco preciso):\n")
print(tabla, row.names = FALSE)
write.csv(tabla, "resultados/D4_medios_entrega_escolaridad.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
