# =============================================================================
# Frente A — ¿Por qué el universo es 18+? Cifras de contexto (decisión 95)
# El INEGI publica la compra en línea para usuarios de internet de 6+. Aquí:
#   1. % que compra en línea: usuarios de 6+ (cotejo con lo publicado), 12-17 y 18+
#   2. De los usuarios de 12-17 que no compran, % que da cada razón (P7_22),
#      sobre todo «no se considera con la edad suficiente» (P7_22_9)
# No escribe archivos del tablero: las cifras van en «Cómo se calculó» de s1.
# Escribe: resultados/A5_adolescentes.csv
# =============================================================================

source(file.path("R", "00_config.R"))
x <- read.csv(revisar_datos(file.path(rutas$endutih, "tr_endutih_usuarios_anual_2025.csv")),
              colClasses = "character", encoding = "UTF-8")
x$EDAD <- as.numeric(x$EDAD); x$FAC_PER <- as.numeric(x$FAC_PER)
x <- subset(x, P7_1 == "1" & P7_21 %in% c("1", "2") & EDAD <= 97)
x$compra <- as.numeric(x$P7_21 == "1")
x$grupo <- cut(x$EDAD, c(5, 11, 17, 97), c("6-11", "12-17", "18+"))
d <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FAC_PER, data = x, nest = TRUE)

ic <- function(m) { b <- as.numeric(coef(m)); s <- as.numeric(SE(m))
  data.frame(estimacion = round(100 * b, 1), li90 = round(100 * (b - z90 * s), 1), ls90 = round(100 * (b + z90 * s), 1)) }
total <- svymean(~compra, d)
por_grupo <- svyby(~compra, ~grupo, d, svymean)
compra <- rbind(data.frame(grupo = "6+ (lo que publica el INEGI)", ic(total)),
                data.frame(grupo = as.character(por_grupo$grupo),
                           estimacion = round(100 * por_grupo$compra, 1),
                           li90 = round(100 * (por_grupo$compra - z90 * por_grupo$se), 1),
                           ls90 = round(100 * (por_grupo$compra + z90 * por_grupo$se), 1)))
cat("% de usuarios de internet que compró en línea, 2025:\n"); print(compra, row.names = FALSE)

razones <- c(P7_22_1 = "Prefiere comprar en persona", P7_22_2 = "No le interesa o no lo necesita",
             P7_22_3 = "Falta de recursos económicos", P7_22_4 = "Desconfía de la calidad",
             P7_22_5 = "Desconfía de la seguridad", P7_22_6 = "No tiene tarjeta o banca en línea",
             P7_22_7 = "No sabe cómo hacerlo", P7_22_8 = "No le permiten realizarlo",
             P7_22_9 = "No se considera con la edad suficiente", P7_22_10 = "Otra razón")
for (v in names(razones)) x[[v]] <- as.numeric(x[[v]] %in% "1")
d <- svydesign(ids = ~UPM_DIS, strata = ~EST_DIS, weights = ~FAC_PER, data = x, nest = TRUE)
nc <- subset(d, grupo == "12-17" & compra == 0)
m <- svymean(as.formula(paste("~", paste(names(razones), collapse = "+"))), nc)
r <- data.frame(razon = razones, ic(m))
r <- r[order(-r$estimacion), ]
cat("\nUsuarios de 12-17 que no compran: % que da cada razón (respuesta múltiple)\n"); print(r, row.names = FALSE)
x$permiso_edad <- as.numeric(x$P7_22_8 == 1 | x$P7_22_9 == 1)
d <- update(d, permiso_edad = x$permiso_edad)
pe <- ic(svymean(~permiso_edad, subset(d, grupo == "12-17" & compra == 0)))
cat("Dan «no le permiten» o «no tiene la edad»:", pe$estimacion, "% (IC 90 %:", pe$li90, "a", pe$ls90, ")\n")
r <- rbind(r, data.frame(razon = "No le permiten o no tiene la edad (cualquiera de las dos)", pe))
cat("Casos 12-17 que no compran:", sum(x$grupo == "12-17" & x$compra == 0), "\n")

write.csv(rbind(data.frame(tabla = "compra", concepto = compra$grupo, compra[, -1]),
                data.frame(tabla = "razones 12-17", concepto = r$razon, r[, -1])),
          file.path("resultados", "A5_adolescentes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
