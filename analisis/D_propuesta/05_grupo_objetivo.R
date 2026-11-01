# =============================================================================
# Frente D — El grupo objetivo en cifras (decisión 67)
# Grupo: adultos (18+) con escolaridad hasta secundaria que usan internet pero
# no compran en línea. Núcleo: dentro de él, primaria o menos.
# Los adultos que no usan internet van como contexto (la conectividad es otro
# problema; el foco es la compra).
# Cifras en millones de personas, con intervalo al 90 %, y por sexo
# (género como dimensión transversal, decisión 47).
# Escribe: resultados/D5_grupo_objetivo.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))
z90 <- qnorm(0.95)

pob$anillo <- factor(ifelse(pob$ESCOLARIDAD %in% c("Sin escolaridad", "Preescolar o primaria"),
                            "Primaria o menos",
                            ifelse(pob$ESCOLARIDAD == "Secundaria", "Secundaria", "Más que secundaria")),
                     levels = c("Primaria o menos", "Secundaria", "Más que secundaria"))
pob$objetivo   <- pob$anillo %in% c("Primaria o menos", "Secundaria")
pob$no_usa     <- as.numeric(pob$usa_internet == 0)
pob$usa        <- as.numeric(pob$usa_internet == 1)
pob$compra_si  <- as.numeric(pob$usa_internet == 1 & pob$compra %in% 1)
pob$no_compra  <- as.numeric(pob$usa_internet == 1 & pob$compra %in% 0)
pob$no_sabe    <- as.numeric(pob$no_compra == 1 & pob$P7_22_7 %in% 1)
pob$en_persona <- as.numeric(pob$no_compra == 1 & pob$P7_22_1 %in% 1)
pob$adulto     <- 1
dis_pob <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FAC_PER, data = pob, nest = TRUE)
# Personas sin escolaridad reportada (NIVEL 99) quedan fuera del grupo
cat("Adultos sin escolaridad reportada:", sum(is.na(pob$anillo)), "casos\n")

medidas <- c(adulto = "Adultos", no_usa = "No usan internet (contexto)", usa = "Usan internet",
             compra_si = "Compran en línea", no_compra = "Usan internet y no compran (GRUPO)",
             no_sabe = "…y dicen «no sé cómo» (puerta 1)",
             en_persona = "…y prefieren en persona (puerta 2)")
f <- as.formula(paste("~", paste(names(medidas), collapse = " + ")))

totales <- function(diseno, etiqueta) {
  t <- svytotal(f, diseno, na.rm = TRUE)
  data.frame(segmento = etiqueta, medida = unname(medidas),
             millones = round(coef(t) / 1e6, 2),
             li90 = round((coef(t) - z90 * SE(t)) / 1e6, 2),
             ls90 = round((coef(t) + z90 * SE(t)) / 1e6, 2), row.names = NULL)
}
segmentos <- list(
  "Grupo: hasta secundaria"            = quote(objetivo),
  "Núcleo: primaria o menos"           = quote(anillo == "Primaria o menos"),
  "Secundaria"                         = quote(anillo == "Secundaria"),
  "Grupo: mujeres"                     = quote(objetivo & SEXO == "Mujer"),
  "Grupo: hombres"                     = quote(objetivo & SEXO == "Hombre"),
  "Núcleo: mujeres"                    = quote(anillo == "Primaria o menos" & SEXO == "Mujer"),
  "Núcleo: hombres"                    = quote(anillo == "Primaria o menos" & SEXO == "Hombre"),
  "Todos los adultos (referencia)"     = quote(rep(TRUE, nrow(pob))))
tab <- do.call(rbind, lapply(names(segmentos), function(s)
  totales(subset(dis_pob, eval(segmentos[[s]], pob)), s)))

# Porcentajes que la propuesta cita: entre quienes usan internet y no compran
pct <- function(s) {
  d <- subset(dis_pob, eval(segmentos[[s]], pob) & no_compra == 1)
  r <- svymean(~no_sabe + en_persona, d)
  x <- 100 * coef(r); e <- 100 * SE(r)
  sprintf("%s: no sé cómo %.1f %% (%.1f–%.1f); en persona %.1f %% (%.1f–%.1f)", s,
          x[1], x[1] - z90 * e[1], x[1] + z90 * e[1], x[2], x[2] - z90 * e[2], x[2] + z90 * e[2])
}
cat("\n===== Millones de personas (IC 90 %) =====\n")
for (s in names(segmentos)) { cat("\n--", s, "\n"); print(subset(tab, segmento == s)[, -1], row.names = FALSE) }
cat("\n===== % de quienes usan internet y no compran =====\n")
for (s in names(segmentos)) cat(pct(s), "\n")

# Perfil del grupo (usan internet y no compran, hasta secundaria)
g <- subset(dis_pob, objetivo & no_compra == 1)
cat("\n===== Perfil del grupo =====\n")
print(round(100 * coef(svymean(~I(EDAD < 55) + I(SEXO == "Mujer") + I(TLOC == "Menos de 2,500") +
                                 I(ESTRATO %in% c("Bajo", "Medio bajo")), g)), 1))
write.csv(tab, file.path("resultados", "D5_grupo_objetivo.csv"), row.names = FALSE, fileEncoding = "UTF-8")
