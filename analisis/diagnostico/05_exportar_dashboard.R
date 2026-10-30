# =============================================================================
# Diagnóstico — Exportar resultados al dashboard
# Toma las tablas que generan 02_cruces.R y 04_regresion.R (en resultados/) y
# escribe los CSV que lee el dashboard (dashboard/datos/), con prueba = FALSE.
#
# Correr DESPUÉS de 02_cruces.R y 04_regresion.R:
#   source("analisis/diagnostico/05_exportar_dashboard.R")
# =============================================================================

leer_res <- function(n) read.csv(file.path("resultados", n), fileEncoding = "UTF-8")
guardar  <- function(df, n) {
  write.csv(df, file.path("dashboard", "datos", n), row.names = FALSE, fileEncoding = "UTF-8")
  cat("  dashboard/datos/", n, "\n", sep = "")
}

reg <- leer_res("E_regresion_bloques.csv")

# Nombres en lenguaje claro para cada contraste del modelo
etiquetas <- c(
  "Superior - Preescolar o primaria"        = "Superior",
  "Media superior - Preescolar o primaria"  = "Media superior",
  "Secundaria - Preescolar o primaria"      = "Secundaria",
  "Sin escolaridad - Preescolar o primaria" = "Sin escolaridad",
  "25-34 - 18-24" = "25-34 años", "35-44 - 18-24" = "35-44 años", "45-54 - 18-24" = "45-54 años",
  "55-64 - 18-24" = "55-64 años", "65+ - 18-24" = "65 años o más",
  "Menos de 2,500 - 100 mil y más"  = "Menos de 2,500 hab.",
  "2,500 a 14,999 - 100 mil y más"  = "2,500 a 14,999 hab.",
  "15 mil a 99,999 - 100 mil y más" = "15 mil a 99,999 hab.",
  "Alto - Bajo" = "Alto", "Medio alto - Bajo" = "Medio alto", "Medio bajo - Bajo" = "Medio bajo",
  "Mujer - Hombre" = "Mujer")
factores <- c(ESCOLARIDAD = "Escolaridad (frente a preescolar o primaria)",
              GRUPO_EDAD = "Edad (frente a 18-24)",
              TLOC = "Localidad (frente a 100 mil hab. o más)",
              ESTRATO = "Estrato (frente a bajo)",
              SEXO = "Sexo (frente a hombre)",
              INDIGENA = "Indígena (frente a no indígena)",
              DISCAPACIDAD = "Discapacidad (frente a sin discapacidad)")
etiquetar <- function(d) {
  d$etiqueta <- ifelse(d$contraste %in% names(etiquetas), etiquetas[d$contraste],
                       ifelse(d$variable == "INDIGENA", "Se considera indígena",
                              ifelse(d$variable == "DISCAPACIDAD", "Con discapacidad", d$contraste)))
  d$factor <- factores[d$variable]
  d
}

cat("Exportando resultados reales al dashboard:\n")

# 1. Efectos del modelo completo (bloque 4): cuánto pesa cada factor
s2e <- etiquetar(subset(reg, modelo == "4 +Estrato"))
s2e <- data.frame(factor = s2e$factor, orden_factor = match(s2e$variable, names(factores)),
                  etiqueta = s2e$etiqueta, estimacion = s2e$ame_pp,
                  li90 = s2e$ic90_inf, ls90 = s2e$ic90_sup, prueba = FALSE)
guardar(s2e, "s2_efectos.csv")

# 2. La regresión paso a paso: brechas de los grupos al agregar cada bloque
pasos <- c("1 Grupos" = "Solo grupos", "2 +Escolaridad" = "+ Escolaridad",
           "3 +Localidad" = "+ Tamaño de localidad", "4 +Estrato" = "+ Estrato")
clave <- (reg$variable == "SEXO") | (reg$variable == "INDIGENA") |
         (reg$variable == "DISCAPACIDAD") | (reg$contraste == "65+ - 18-24")
s2b <- etiquetar(reg[clave, ])
s2b <- data.frame(brecha = c(SEXO = "Mujer vs. hombre", INDIGENA = "Indígena vs. no indígena",
                             DISCAPACIDAD = "Con vs. sin discapacidad",
                             GRUPO_EDAD = "65+ vs. 18-24 años")[s2b$variable],
                  paso = match(s2b$modelo, names(pasos)), nombre_paso = pasos[s2b$modelo],
                  estimacion = s2b$ame_pp, li90 = s2b$ic90_inf, ls90 = s2b$ic90_sup, prueba = FALSE)
guardar(s2b, "s2_bloques.csv")

# 3. ¿Se sostiene? Modelo completo en todo el país vs. solo ciudades de 100 mil+
rob <- etiquetar(leer_res("F_robustez_ciudades.csv"))
rob <- rob[rob$contraste %in% c("Superior - Preescolar o primaria", "65+ - 18-24", "Alto - Bajo",
                                "Mujer - Hombre", "Sí - No"), ]
rob$etiqueta <- c("Superior - Preescolar o primaria" = "Educación superior vs. primaria",
                  "65+ - 18-24" = "65+ vs. 18-24 años", "Alto - Bajo" = "Estrato alto vs. bajo",
                  "Mujer - Hombre" = "Mujer vs. hombre")[rob$contraste]
rob$etiqueta[rob$variable == "INDIGENA"] <- "Indígena vs. no indígena"
rob$etiqueta[rob$variable == "DISCAPACIDAD"] <- "Con vs. sin discapacidad"
s2r <- rbind(data.frame(etiqueta = rob$etiqueta, version = "Todo el país", estimacion = rob$ame_pp_nacional),
             data.frame(etiqueta = rob$etiqueta, version = "Solo ciudades de 100 mil+", estimacion = rob$ame_pp_ciudades))
s2r$prueba <- FALSE
guardar(s2r, "s2_robustez.csv")

# 4. Compra en línea por entidad (cortes simples de 02_cruces.R)
cortes <- leer_res("A_compra_cortes_simples.csv")
ent <- read.csv(file.path("dashboard", "datos", "entidades.csv"), fileEncoding = "UTF-8")
nombres <- c("Coahuila" = "Coahuila", "Estado de México" = "México", "Michoacán" = "Michoacán",
             "Veracruz" = "Veracruz")
ent$nombre_res <- ifelse(ent$entidad %in% names(nombres), nombres[ent$entidad], ent$entidad)
e <- merge(ent[, c("cve_ent", "nombre_res")], subset(cortes, corte == "ENTIDAD"),
           by.x = "nombre_res", by.y = "grupo", all.x = TRUE)
if (anyNA(e$pct_compra)) stop("Entidades sin cifra: ", paste(e$nombre_res[is.na(e$pct_compra)], collapse = ", "))
guardar(data.frame(cve_ent = e$cve_ent, estimacion = e$pct_compra, li90 = e$ic90_inf,
                   ls90 = e$ic90_sup, prueba = FALSE), "s4_entidades.csv")

cat("Listo.\n")
