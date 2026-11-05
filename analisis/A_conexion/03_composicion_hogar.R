# =============================================================================
# Frente A — Composición del hogar (pregunta 2 del reto)
# ¿Compran más en línea las personas que viven con jóvenes que usan internet?
# Universo: usuarios de internet de 35+ (para quien tiene 18-34 la pregunta no
# tiene sentido: el joven es la persona misma).
# Composición, según los demás residentes del hogar (tabla de residentes):
#   1. Vive solo/a
#   2. Vive con otras personas, pero ninguna de 18-34 que use internet
#   3. Vive con al menos una persona de 18-34 que usa internet (P3_9_2)
# Descriptivo, por sexo, con IC al 90 %. Además, la diferencia 3 vs. 2 con los
# controles del modelo completo, para ver si se sostiene.
# Necesita: tr_endutih_residentes_anual_2025.csv en datos/endutih_2025/
# Escribe: dashboard/datos/s2_hogar.csv, resultados/A3_composicion_hogar.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))
suppressMessages(library(marginaleffects))

res <- read.csv(revisar_datos(file.path(rutas$endutih, "tr_endutih_residentes_anual_2025.csv")),
                colClasses = "character", encoding = "UTF-8")[, c("UPM", "VIV_SEL", "HOGAR", "NUM_REN", "EDAD", "P3_9_2")]
res$EDAD <- as.numeric(res$EDAD)
res$hogar <- paste(res$UPM, res$VIV_SEL, res$HOGAR)
res$joven_internet <- res$EDAD >= 18 & res$EDAD <= 34 & res$P3_9_2 %in% "1"

usu$hogar <- paste(usu$UPM, usu$VIV_SEL, usu$HOGAR)
# Por cada usuario: cuántas OTRAS personas viven en su hogar y si alguna es joven que usa internet
otros <- merge(usu[, c("hogar", "NUM_REN")], res[, c("hogar", "NUM_REN", "joven_internet")],
               by = "hogar", suffixes = c("", "_otro"))
otros <- otros[otros$NUM_REN != otros$NUM_REN_otro, ]
n_otros <- tapply(rep(1, nrow(otros)), paste(otros$hogar, otros$NUM_REN), sum)
jov     <- tapply(otros$joven_internet, paste(otros$hogar, otros$NUM_REN), any)
clave   <- paste(usu$hogar, usu$NUM_REN)
usu$n_otros <- ifelse(is.na(n_otros[clave]), 0, n_otros[clave])
usu$con_joven <- ifelse(is.na(jov[clave]), FALSE, jov[clave])
niveles <- c("Vive solo/a", "Con otras personas, sin jóvenes de 18-34 que usen internet",
             "Con al menos un joven de 18-34 que usa internet")
usu$COMPOSICION <- factor(ifelse(usu$n_otros == 0, niveles[1], ifelse(usu$con_joven, niveles[3], niveles[2])), niveles)
dis <- update(dis, COMPOSICION = usu$COMPOSICION)
cat("Usuarios de 18+ por composición (casos):\n"); print(table(usu$COMPOSICION))

d35 <- subset(dis, EDAD >= 35)
r <- svyby(~compra, ~COMPOSICION + SEXO, d35, svymean, na.rm = TRUE)
s2h <- data.frame(composicion = r$COMPOSICION, orden = as.integer(r$COMPOSICION), sexo = r$SEXO,
                  estimacion = round(100 * r$compra, 1),
                  li90 = round(100 * (r$compra - z90 * r$se), 1), ls90 = round(100 * (r$compra + z90 * r$se), 1))
s2h <- s2h[order(s2h$orden, s2h$sexo), ]
cat("\n===== % que compra, usuarios de 35+ =====\n"); print(s2h, row.names = FALSE)
print(round(100 * coef(svymean(~COMPOSICION, d35)), 1))

# ¿Se sostiene con controles? Joven en casa vs. vive con otros sin joven
usu$ESCOLARIDAD <- relevel(usu$ESCOLARIDAD, ref = "Preescolar o primaria")
d35 <- subset(update(dis, ESCOLARIDAD = usu$ESCOLARIDAD), EDAD >= 35 & COMPOSICION != niveles[1])
m <- svyglm(compra ~ COMPOSICION + SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD + ESCOLARIDAD + TLOC + ESTRATO,
            design = d35, family = quasibinomial())
e <- as.data.frame(avg_comparisons(m, variables = "COMPOSICION", wts = "(weights)", conf_level = 0.90))
e <- e[grepl("joven de 18-34 que usa", e$contrast) & !grepl("^Vive solo", e$contrast), ]
cat(sprintf("\nCon controles, vivir con un joven que usa internet vs. sin él (35+): %+.1f pp (IC 90 %%: %.1f a %.1f)\n",
            100 * e$estimate, 100 * e$conf.low, 100 * e$conf.high))

write.csv(s2h, file.path("resultados", "A3_composicion_hogar.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(data.frame(s2h, prueba = FALSE), file.path("dashboard", "datos", "s2_hogar.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s2_hogar.csv\n")
