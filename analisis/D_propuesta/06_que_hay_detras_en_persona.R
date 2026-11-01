# =============================================================================
# Frente D — ¿Qué hay detrás de "prefiere en persona"? (hallazgo 34)
# La ENDUTIH no pregunta por qué alguien prefiere comprar en persona. Se prueba
# una implicación: si es una elección informada, quien la da debería parecerse
# a quien compra en sus otras actividades en línea (buscar productos, pagar,
# banca, trámites). Si no conoce lo que ofrece lo digital, debería parecerse a
# quien "no sabe cómo".
# Universo: usuarios de internet de 18+ con escolaridad hasta secundaria
# (grupo objetivo, decisión 67); referencia: "en persona" con educación superior.
# Escribe: resultados/D6_en_persona_actividades.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))
z90 <- qnorm(0.95)

uno <- function(v) as.numeric(usu[[v]] %in% "1")
actividades <- c(
  a_diario    = "Usa internet a diario",                 # P7_3
  a_5anios    = "Usa internet hace más de 5 años",       # P7_7
  a_busca     = "Busca en línea información de bienes o servicios",  # P7_10_7
  a_tutorial  = "Ve tutoriales",                          # P7_11_3
  a_vende     = "Ha vendido por internet",               # P7_19
  a_paga      = "Ha pagado algo por internet",           # P7_28
  a_banca     = "Usa banca en línea",                    # P7_33
  a_tramite   = "Hace trámites de gobierno en línea")    # P7_35_4
usu$a_diario <- uno("P7_3");   usu$a_5anios <- as.numeric(usu$P7_7 %in% "4")
usu$a_busca  <- uno("P7_10_7"); usu$a_tutorial <- uno("P7_11_3")
usu$a_vende  <- uno("P7_19");   usu$a_paga <- uno("P7_28")
usu$a_banca  <- uno("P7_33");   usu$a_tramite <- uno("P7_35_4")

hasta_sec <- usu$ESCOLARIDAD %in% c("Sin escolaridad", "Preescolar o primaria", "Secundaria")
no_compra <- usu$compra %in% 0
usu$perfil <- NA
usu$perfil[hasta_sec & usu$compra %in% 1]                <- "Compra"
usu$perfil[hasta_sec & no_compra & usu$P7_22_2 %in% 1]   <- "No compra: no le interesa"
usu$perfil[hasta_sec & no_compra & usu$P7_22_7 %in% 1]   <- "No compra: no sabe cómo"
usu$perfil[hasta_sec & no_compra & usu$P7_22_1 %in% 1]   <- "No compra: prefiere en persona"
usu$perfil[usu$ESCOLARIDAD %in% "Superior" & no_compra & usu$P7_22_1 %in% 1] <-
  "Referencia: prefiere en persona, educación superior"
# Quien da varias razones queda en la última asignada ("en persona" manda);
# 87 % de quienes no compran da una sola razón (hallazgo 15).
usu$perfil <- factor(usu$perfil, levels = c("Compra", "No compra: prefiere en persona",
  "No compra: no sabe cómo", "No compra: no le interesa",
  "Referencia: prefiere en persona, educación superior"))

dis <- update(dis, perfil = usu$perfil, a_diario = usu$a_diario, a_5anios = usu$a_5anios,
              a_busca = usu$a_busca, a_tutorial = usu$a_tutorial, a_vende = usu$a_vende,
              a_paga = usu$a_paga, a_banca = usu$a_banca, a_tramite = usu$a_tramite)
r <- svyby(as.formula(paste("~", paste(names(actividades), collapse = " + "))), ~perfil,
           subset(dis, !is.na(perfil)), svymean)

tab <- do.call(rbind, lapply(names(actividades), function(a) data.frame(
  actividad = actividades[[a]], perfil = r$perfil,
  pct = round(100 * r[[a]], 1),
  li90 = round(100 * (r[[a]] - z90 * r[[paste0("se.", a)]]), 1),
  ls90 = round(100 * (r[[a]] + z90 * r[[paste0("se.", a)]]), 1))))
cat("\n===== % que hace cada actividad, por perfil =====\n")
print(reshape(tab[, c("actividad", "perfil", "pct")], idvar = "actividad", timevar = "perfil",
              direction = "wide"), row.names = FALSE)
cat("\nCasos por perfil:\n"); print(table(usu$perfil))

# ¿La preferencia por lo presencial es solo para comprar? Razón para no pagar
# por internet (P7_29) entre quienes prefieren comprar en persona y no pagan en línea
d <- subset(dis, perfil == "No compra: prefiere en persona" & P7_28 == "2")
p <- svymean(~I(P7_29 == "1"), d, na.rm = TRUE)
cat(sprintf("\nDe quienes prefieren comprar en persona y no pagan en línea, %.1f %% (IC 90 %%: %.1f–%.1f) también prefiere pagar en persona\n",
            100 * coef(p)[2], 100 * (coef(p)[2] - z90 * SE(p)[2]), 100 * (coef(p)[2] + z90 * SE(p)[2])))
write.csv(tab, file.path("resultados", "D6_en_persona_actividades.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")
