# =============================================================================
# Frente A — Conexión en el hogar y qué se compra
#
# 1. % de adultos (18+) que vive en un hogar con internet (P4_4, tabla de
#    hogares), por grupo. Va como dato aparte, no como eslabón del embudo:
#    más adultos usan internet (85 %) que los que tienen internet en casa,
#    porque muchos se conectan con datos móviles. Decisión 64.
# 2. P3: qué se compra en línea (P7_24), agrupado como la gráfica 9 del
#    Reporte de resultados de la ENDUTIH 2025 del INEGI, para poder cotejar.
#    Universo del dashboard: usuarios de 18+ que compraron en línea.
#
# Necesita: tr_endutih_hogares_anual_2025.csv en datos/endutih_2025/
# Escribe: dashboard/datos/s1_categorias.csv, dashboard/datos/s1_hogar.csv,
#          resultados/A2_hogar_internet.csv, resultados/A2_categorias.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))
z90 <- qnorm(0.95)

cortes <- c(Nacional = "NACIONAL", Escolaridad = "ESCOLARIDAD_S1", Edad = "GRUPO_EDAD",
            Sexo = "SEXO", Localidad = "TLOC", Estrato = "ESTRATO", Región = "REGION")

# Tabla de proporciones por corte, con IC al 90 % y CV
por_cortes <- function(formula, diseno, datos) {
  datos$NACIONAL <- factor("Nacional")
  diseno <- update(diseno, NACIONAL = datos$NACIONAL)
  vars <- all.vars(formula)
  do.call(rbind, lapply(names(cortes), function(g) {
    v <- cortes[[g]]
    r <- svyby(formula, as.formula(paste0("~", v)), diseno, svymean, na.rm = TRUE)
    grupos <- as.character(r[[v]])
    do.call(rbind, lapply(vars, function(x) {
      est <- r[[x]]; se <- r[[paste0("se.", x)]]
      data.frame(grupo_var = g, grupo = grupos, orden = seq_along(grupos), variable = x,
                 estimacion = round(100 * est, 1), li90 = round(100 * (est - z90 * se), 1),
                 ls90 = round(100 * (est + z90 * se), 1), cv = round(se / est, 3),
                 n = as.vector(table(datos[[v]])[grupos]))
    }))
  }))
}

# --- 1. Hogar con internet ----------------------------------------------------
hog <- read.csv(revisar_datos(file.path(rutas$endutih, "tr_endutih_hogares_anual_2025.csv")),
                colClasses = "character", encoding = "UTF-8")[, c("UPM", "VIV_SEL", "HOGAR", "P4_4")]
pob <- merge(pob, hog, by = c("UPM", "VIV_SEL", "HOGAR"), all.x = TRUE, sort = FALSE)
stopifnot(!anyNA(pob$P4_4))   # cada adulto encuentra su hogar
pob$hogar_internet <- ifelse(pob$P4_4 == "1", 1, 0)
pob$TLOC <- factor(pob$TLOC, levels = rev(levels(pob$TLOC)))   # de rural a urbana
dis_hog <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FAC_PER, data = pob, nest = TRUE)

h <- por_cortes(~hogar_internet + usa_internet, dis_hog, pob)
cat("\n===== Adultos (18+) en hogar con internet vs. que usan internet =====\n")
print(reshape(h[, c("grupo_var", "grupo", "variable", "estimacion")],
              idvar = c("grupo_var", "grupo"), timevar = "variable", direction = "wide"),
      row.names = FALSE)
# ¿Cuántos usan internet SIN tenerlo en casa?
x <- svymean(~interaction(hogar_internet, usa_internet), dis_hog)
cat("\nHogar con internet (0/1) × usa internet (0/1), % de adultos:\n"); print(round(100 * coef(x), 1))
write.csv(h, file.path("resultados", "A2_hogar_internet.csv"), row.names = FALSE, fileEncoding = "UTF-8")
s1h <- data.frame(h[, c("grupo_var", "grupo", "orden")],
                  medida = c(hogar_internet = "Vive en un hogar con internet",
                             usa_internet = "Usa internet")[h$variable],
                  h[, c("estimacion", "li90", "ls90")], prueba = FALSE)
write.csv(s1h, file.path("dashboard", "datos", "s1_hogar.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# --- 2. Qué se compra (P3) ----------------------------------------------------
# Grupos de la gráfica 9 del INEGI (P7_24_*). Los demás artículos no salen en
# esa gráfica y van juntos en "Otros".
grupos_inegi <- list(
  "Uso e higiene personal (incl. ropa)"   = c("P7_24_11", "P7_24_12"),
  "Artículos para el hogar"               = "P7_24_13",
  "Transporte por plataforma"             = "P7_24_19",
  "Alimentos y bebidas"                   = "P7_24_6",
  "Boletos de viaje o entretenimiento"    = c("P7_24_7", "P7_24_8"),
  "Celulares o accesorios"                = "P7_24_4",
  "Artículos de salud"                    = "P7_24_17",
  "Aparatos electrónicos"                 = "P7_24_10",
  "Vehículos o refacciones"               = "P7_24_14",
  "Alojamiento y hospedaje"               = "P7_24_9",
  "Otros"                                 = paste0("P7_24_", c(1, 2, 3, 5, 15, 16, 18, 20)))
nombre_var <- function(i) paste0("CAT_", i)
agrupar <- function(d) {
  for (i in seq_along(grupos_inegi)) {
    d[[nombre_var(i)]] <- as.numeric(rowSums(sapply(grupos_inegi[[i]], function(v) d[[v]] == "1")) > 0)
  }
  d
}

# Cotejo con el INEGI: usuarios de 6+ que compraron (universo de la gráfica 9)
crudo <- read.csv(ruta_datos, colClasses = "character", encoding = "UTF-8")
crudo <- agrupar(subset(crudo, P7_1 == "1" & P7_21 == "1"))
crudo$FAC_PER <- as.numeric(crudo$FAC_PER)
inegi <- c(78.7, 50.6, 38.2, 37.4, 31.2, 30.1, 23.7, 21.2, 20.0, 18.4, NA)
cotejo <- data.frame(grupo = names(grupos_inegi), inegi = inegi,
                     nuestro = round(100 * sapply(seq_along(grupos_inegi), function(i)
                       weighted.mean(crudo[[nombre_var(i)]], crudo$FAC_PER)), 1))
cat("\n===== Cotejo con la gráfica 9 del INEGI (usuarios de 6+ que compraron) =====\n")
print(cotejo, row.names = FALSE)

# Para el dashboard: usuarios de 18+ que compraron
com <- agrupar(subset(usu, compra == 1))
dis_com <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FAC_PER, data = com, nest = TRUE)
f_cat <- as.formula(paste("~", paste(nombre_var(seq_along(grupos_inegi)), collapse = " + ")))
cat_ <- por_cortes(f_cat, dis_com, com)
cat_$categoria <- names(grupos_inegi)[match(cat_$variable, nombre_var(seq_along(grupos_inegi)))]
write.csv(cat_, file.path("resultados", "A2_categorias.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\nCategorías con CV > 30 % (poco precisas):\n")
print(subset(cat_, cv > 0.3)[, c("grupo", "categoria", "estimacion", "cv", "n")], row.names = FALSE)

write.csv(data.frame(cat_[, c("grupo_var", "grupo", "orden", "categoria", "estimacion", "li90", "ls90")],
                     prueba = FALSE),
          file.path("dashboard", "datos", "s1_categorias.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s1_categorias.csv\n")
