# =============================================================================
# Frente A — Del acceso a la compra
# P1: ¿en qué eslabón se pierde la gente? Embudo por grupo y brechas con
# controles en cada eslabón.
#
#   Eslabón "Usa internet":    % de la población de 18+ que usa internet
#   Eslabón "Compra en línea": % de la población de 18+ que compró en línea
# Los dos eslabones están en el MISMO universo (población de 18+), así que el
# embudo sí se puede leer de arriba abajo. La conversión (de usuarios, cuántos
# compran) también se guarda en resultados/.
# El eslabón "Hogar con internet" necesita la tabla de hogares (pendiente).
#
# Escribe: dashboard/datos/s1_embudo.csv, dashboard/datos/s1_brechas_eslabon.csv
#          resultados/A1_embudo.csv, resultados/A1_brechas_eslabon.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))
suppressMessages(library(marginaleffects))

# En la población de 18+: compró = usa internet y compró (no usuarios = 0)
pob$compra_pob <- ifelse(pob$usa_internet == 0, 0, pob$compra)
dis_pob <- update(dis_pob, compra_pob = pob$compra_pob)

z90 <- qnorm(0.95)

# --- 1. Embudo por grupo -------------------------------------------------------
cortes <- c(Nacional = "NACIONAL", Escolaridad = "ESCOLARIDAD", Edad = "GRUPO_EDAD",
            Sexo = "SEXO", Localidad = "TLOC", Estrato = "ESTRATO", Región = "REGION")
pob$NACIONAL <- factor("Nacional")
dis_pob <- update(dis_pob, NACIONAL = pob$NACIONAL)

# Orden de lectura en el dashboard: de menor a mayor (localidad de rural a urbana)
pob$TLOC <- factor(pob$TLOC, levels = rev(levels(pob$TLOC)))
dis_pob <- update(dis_pob, TLOC = pob$TLOC)

embudo <- do.call(rbind, lapply(names(cortes), function(g) {
  v <- cortes[[g]]
  r <- svyby(~usa_internet + compra_pob, as.formula(paste0("~", v)), dis_pob,
             svymean, na.rm = TRUE)
  conv <- svyby(~compra, as.formula(paste0("~", v)),
                subset(dis_pob, usa_internet == 1), svymean, na.rm = TRUE)
  grupos <- as.character(r[[v]])
  n <- table(pob[[v]])[grupos]
  rbind(
    data.frame(grupo_var = g, grupo = grupos, orden = seq_along(grupos),
               eslabon = "Usa internet", paso = 2,
               est = r$usa_internet, se = r$se.usa_internet, n = as.vector(n)),
    data.frame(grupo_var = g, grupo = grupos, orden = seq_along(grupos),
               eslabon = "Compra en línea", paso = 3,
               est = r$compra_pob, se = r$se.compra_pob, n = as.vector(n)),
    data.frame(grupo_var = g, grupo = grupos, orden = seq_along(grupos),
               eslabon = "Conversión (de usuarios, % que compra)", paso = NA,
               est = conv$compra, se = conv$se, n = as.vector(n)))
}))
embudo$estimacion <- round(100 * embudo$est, 1)
embudo$li90 <- round(100 * (embudo$est - z90 * embudo$se), 1)
embudo$ls90 <- round(100 * (embudo$est + z90 * embudo$se), 1)
embudo$cv   <- round(embudo$se / embudo$est, 3)

cat("\n===== Embudo (población de 18+) =====\n")
print(embudo[, c("grupo_var", "grupo", "eslabon", "estimacion", "li90", "ls90", "cv", "n")],
      row.names = FALSE)
write.csv(embudo, file.path("resultados", "A1_embudo.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")

s1e <- subset(embudo, !is.na(paso))
s1e <- data.frame(s1e[, c("grupo_var", "grupo", "orden", "eslabon", "paso",
                          "estimacion", "li90", "ls90")], prueba = FALSE)
write.csv(s1e, file.path("dashboard", "datos", "s1_embudo.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")

# --- 2. Brechas con controles en cada eslabón -------------------------------
# Mismos controles que el modelo completo de la regresión (decisión del
# diagnóstico): sexo, edad, indígena, discapacidad, escolaridad, localidad, estrato.
# Usar internet: población de 18+. Comprar: usuarios de internet de 18+.
pob$ESCOLARIDAD <- relevel(pob$ESCOLARIDAD, ref = "Preescolar o primaria")
pob$TLOC        <- relevel(pob$TLOC, ref = "100 mil y más")
usu$ESCOLARIDAD <- relevel(usu$ESCOLARIDAD, ref = "Preescolar o primaria")
dis_pob <- update(dis_pob, ESCOLARIDAD = pob$ESCOLARIDAD, TLOC = pob$TLOC)
dis     <- update(dis, ESCOLARIDAD = usu$ESCOLARIDAD)

controles <- "SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD + ESCOLARIDAD + TLOC + ESTRATO"
m_usa <- svyglm(as.formula(paste("usa_internet ~", controles)), design = dis_pob,
                family = quasibinomial())
m_com <- svyglm(as.formula(paste("compra ~", controles)), design = dis,
                family = quasibinomial())

contrastes <- c("Superior - Preescolar o primaria" = "Educación superior vs. primaria",
                "65+ - 18-24"                      = "65+ vs. 18-24 años",
                "Menos de 2,500 - 100 mil y más"   = "Rural (< 2,500) vs. 100 mil+",
                "Alto - Bajo"                      = "Estrato alto vs. bajo",
                "Mujer - Hombre"                   = "Mujer vs. hombre",
                "Sí - No"                          = "")
efectos <- function(m, nombre) {
  e <- as.data.frame(avg_comparisons(m, wts = "(weights)", conf_level = 0.90))
  e <- e[e$contrast %in% names(contrastes), ]
  e$comparacion <- contrastes[e$contrast]
  e$comparacion[e$term == "INDIGENA"]     <- "Indígena vs. no indígena"
  e$comparacion[e$term == "DISCAPACIDAD"] <- "Con vs. sin discapacidad"
  data.frame(comparacion = e$comparacion, eslabon = nombre,
             estimacion = round(100 * e$estimate, 1),
             li90 = round(100 * e$conf.low, 1), ls90 = round(100 * e$conf.high, 1))
}
br <- rbind(efectos(m_usa, "Usar internet"), efectos(m_com, "Comprar en línea"))

cat("\n===== Brechas con controles por eslabón (pp) =====\n")
print(br, row.names = FALSE)
write.csv(br, file.path("resultados", "A1_brechas_eslabon.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv(data.frame(br, prueba = FALSE), file.path("dashboard", "datos", "s1_brechas_eslabon.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s1_embudo.csv y s1_brechas_eslabon.csv\n")
