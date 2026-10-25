# =============================================================================
# Frente B — ¿Qué esconde "prefiere realizarlo en persona"?
# Entre quienes no compraron y eligieron esa razón: ¿cuántos la dan sola y
# con qué otras razones aparece? Pregunta de respuesta múltiple (P7_22).
# Universo: usuarios de internet de 18+ que no compraron.
# =============================================================================

source(file.path("R", "prep_endutih.R"))

otras <- setdiff(names(razones), "P7_22_1")
usu$n_otras <- rowSums(usu[, otras] == 1, na.rm = TRUE)
usu$en_persona_sola <- ifelse(usu$P7_22_1 == 1, as.numeric(usu$n_otras == 0), NA)
dis <- update(dis, n_otras = usu$n_otras, en_persona_sola = usu$en_persona_sola)

dis_ep <- subset(dis, compra == 0 & P7_22_1 == 1)

# --- 1. ¿Cuántos dan "prefiere en persona" como única razón? -----------------
sola <- svymean(~en_persona_sola, dis_ep, na.rm = TRUE)
cat(sprintf("\nDe quienes eligieron 'prefiere en persona', %.1f %% no dio ninguna otra razón.\n",
            100 * coef(sola)))

# --- 2. ¿Con qué otras razones aparece? ---------------------------------------
f_otras <- as.formula(paste("~", paste(otras, collapse = " + ")))
co <- svymean(f_otras, dis_ep, na.rm = TRUE)
tab_co <- data.frame(razon_acompanante = unname(razones[otras]),
                     pct = round(100 * coef(co), 1))
tab_co <- tab_co[order(-tab_co$pct), ]
cat("\n% de quienes eligieron 'prefiere en persona' que además eligió...\n")
print(tab_co, row.names = FALSE)

# --- 3. ¿Cambia por escolaridad y edad? ---------------------------------------
por_grupo <- function(v) {
  r <- svyby(~en_persona_sola, as.formula(paste0("~", v)), dis_ep, svymean,
             na.rm = TRUE, vartype = "cv")
  data.frame(corte = v, grupo = r[[v]],
             pct_sola = round(100 * r$en_persona_sola, 1),
             cv_pct = round(100 * r$cv, 1))
}
tab_g <- rbind(por_grupo("ESCOLARIDAD"), por_grupo("GRUPO_EDAD"))
cat("\n% que da 'prefiere en persona' como ÚNICA razón, por grupo:\n")
print(tab_g, row.names = FALSE)

write.csv(tab_co, "resultados/B2_en_persona_acompanantes.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv(tab_g, "resultados/B2_en_persona_sola_por_grupo.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
