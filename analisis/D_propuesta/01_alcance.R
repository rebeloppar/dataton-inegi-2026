# =============================================================================
# Frente D — Alcance de las brechas candidatas (criterio 2 de priorización)
# ¿Cuántos millones de personas hay en cada grupo rezagado, y cuántas de
# ellas no compran en línea?
# Universo: usuarios de internet de 18+ (ENDUTIH 2025), con diseño muestral.
# =============================================================================

source(file.path("R", "prep_endutih.R"))

# Grupos rezagados de cada brecha candidata (tabla de priorización del frente D)
usu$g_escolaridad <- usu$ESCOLARIDAD %in% c("Sin escolaridad", "Preescolar o primaria")
usu$g_edad        <- usu$GRUPO_EDAD == "65+"
usu$g_rural       <- usu$TLOC == "Menos de 2,500"
usu$g_mujer       <- usu$SEXO == "Mujer"
usu$g_indigena    <- usu$INDIGENA == "Sí"
usu$g_discap      <- usu$DISCAPACIDAD == "Sí"
grupos <- c(g_escolaridad = "Primaria o menos (incl. sin escolaridad)",
            g_edad = "65 años o más", g_rural = "Localidades de menos de 2,500 hab.",
            g_mujer = "Mujeres", g_indigena = "Se considera indígena",
            g_discap = "Con discapacidad")

for (g in names(grupos)) usu[[paste0(g, "_no")]] <- usu[[g]] & usu$compra %in% 0
dis <- update(dis, g_escolaridad = usu$g_escolaridad, g_edad = usu$g_edad,
              g_rural = usu$g_rural, g_mujer = usu$g_mujer,
              g_indigena = usu$g_indigena, g_discap = usu$g_discap,
              g_escolaridad_no = usu$g_escolaridad_no, g_edad_no = usu$g_edad_no,
              g_rural_no = usu$g_rural_no, g_mujer_no = usu$g_mujer_no,
              g_indigena_no = usu$g_indigena_no, g_discap_no = usu$g_discap_no)

millones <- function(v) {
  t <- svytotal(as.formula(paste0("~as.numeric(", v, ")")), dis, na.rm = TRUE)
  c(est = unname(coef(t)) / 1e6, se = unname(SE(t)) / 1e6)
}

tabla <- do.call(rbind, lapply(names(grupos), function(g) {
  tot <- millones(g); no <- millones(paste0(g, "_no"))
  data.frame(grupo = grupos[[g]],
             millones_en_grupo = round(tot[["est"]], 1),
             millones_que_no_compran = round(no[["est"]], 1),
             ic90_no_compran = sprintf("%.1f–%.1f", no[["est"]] - z90 * no[["se"]],
                                       no[["est"]] + z90 * no[["se"]]))
}))

total <- sum(weights(dis)) / 1e6
cat(sprintf("\nUsuarios de internet de 18+: %.1f millones\n\n", total))
print(tabla, row.names = FALSE)
write.csv(tabla, "resultados/D1_alcance_grupos.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
