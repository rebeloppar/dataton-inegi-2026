# =============================================================================
# Frente D — Probabilidad de comprar en línea para cada perfil (sección 2,
# pestaña interactiva «Arma un perfil»). Sustituye a s2_perfiles.csv.
# Modelo completo de la regresión (04_regresion.R, bloque 4): logit con diseño
# muestral, usuarios de internet de 18+. Se predice para cada combinación de
# sexo × edad × escolaridad × tamaño de localidad × estrato, con indígena = No y
# discapacidad = No (decisión 72). Intervalo al 90 % en escala logit,
# transformado. n = casos en la encuesta con esa combinación exacta.
# Escribe: dashboard/datos/s2_perfil_prob.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))
z90 <- qnorm(0.95)

m <- svyglm(compra ~ SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD + ESCOLARIDAD + TLOC + ESTRATO,
            design = dis, family = quasibinomial())

malla <- expand.grid(SEXO = levels(usu$SEXO), GRUPO_EDAD = levels(usu$GRUPO_EDAD),
                     ESCOLARIDAD = levels(usu$ESCOLARIDAD), TLOC = levels(usu$TLOC),
                     ESTRATO = levels(usu$ESTRATO), INDIGENA = "No", DISCAPACIDAD = "No")
for (v in names(malla)) malla[[v]] <- factor(malla[[v]], levels = levels(usu[[v]]))
p <- predict(m, newdata = malla, type = "link", se.fit = TRUE)
eta <- as.numeric(p); se <- sqrt(attr(p, "var"))
inv <- function(x) 100 / (1 + exp(-x))
malla$estimacion <- round(inv(eta), 1)
malla$li90 <- round(inv(eta - z90 * se), 1)
malla$ls90 <- round(inv(eta + z90 * se), 1)

n <- aggregate(list(n = rep(1, nrow(usu))),
               usu[, c("SEXO", "GRUPO_EDAD", "ESCOLARIDAD", "TLOC", "ESTRATO")], sum)
malla <- merge(malla, n, all.x = TRUE); malla$n[is.na(malla$n)] <- 0

out <- data.frame(sexo = malla$SEXO, edad = malla$GRUPO_EDAD, escolaridad = malla$ESCOLARIDAD,
                  localidad = malla$TLOC, estrato = malla$ESTRATO,
                  estimacion = malla$estimacion, li90 = malla$li90, ls90 = malla$ls90,
                  n = malla$n, prueba = FALSE)
write.csv(out, file.path("dashboard", "datos", "s2_perfil_prob.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")
cat("Combinaciones:", nrow(out), "| con menos de 20 casos:", sum(out$n < 20), "\n")
print(summary(out$estimacion))
print(subset(out, sexo == "Mujer" & edad == "35-44" & estrato == "Bajo")[1:6, ])
