# =============================================================================
# Frente D — ¿La baja escolaridad es solo un tema de personas mayores?
# Edad de los usuarios de 18+ con baja escolaridad que no compran en línea,
# y % que dice "no sé cómo" en cada grupo de edad.
# =============================================================================

source(file.path("R", "prep_endutih.R"))

baja <- c("Sin escolaridad", "Preescolar o primaria")
d1 <- subset(dis, compra == 0 & ESCOLARIDAD %in% baja)
d2 <- subset(dis, compra == 0 & ESCOLARIDAD %in% c(baja, "Secundaria"))

dist1 <- prop.table(svytable(~GRUPO_EDAD, d1))
dist2 <- prop.table(svytable(~GRUPO_EDAD, d2))
nosabe <- svyby(~P7_22_7, ~GRUPO_EDAD, d1, svymean, na.rm = TRUE, vartype = "cv")

tabla <- data.frame(edad = names(dist1),
                    pct_primaria_o_menos = round(100 * as.vector(dist1), 1),
                    pct_hasta_secundaria = round(100 * as.vector(dist2), 1),
                    pct_no_sabe_como = round(100 * nosabe$P7_22_7, 1),
                    cv_no_sabe_pct = round(100 * nosabe$cv, 1))
cat(sprintf("\nNo compran con primaria o menos: %.1f M · hasta secundaria: %.1f M\n\n",
            sum(svytable(~GRUPO_EDAD, d1)) / 1e6, sum(svytable(~GRUPO_EDAD, d2)) / 1e6))
print(tabla, row.names = FALSE)
write.csv(tabla, "resultados/D3_baja_escolaridad_por_edad.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
