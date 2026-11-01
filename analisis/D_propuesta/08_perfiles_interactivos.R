# =============================================================================
# Frente D — Probabilidad de comprar en línea para cada perfil (sección 2,
# pestaña interactiva «Arma un perfil»). Sustituye a s2_perfiles.csv.
# Modelo completo de la regresión (04_regresion.R, bloque 4): logit con diseño
# muestral, usuarios de internet de 18+. Se predice para cada combinación de
# sexo × edad × escolaridad × tamaño de localidad × estrato × indígena ×
# discapacidad (decisión 72). Intervalo al 90 % en escala logit,
# transformado. n = casos en la encuesta con esa combinación de sexo, edad,
# escolaridad, localidad y estrato (sin separar por indígena ni discapacidad,
# porque con siete características casi todas las celdas quedan vacías).
# Además, para cada combinación de las cinco primeras, una fila con
# indigena = discapacidad = "Todos": el promedio ponderado de sus cuatro
# versiones (vista por defecto; indígena y discapacidad se activan aparte).
# Escribe: dashboard/datos/s2_perfil_prob.csv
# =============================================================================

source(file.path("R", "prep_endutih.R"))
z90 <- qnorm(0.95)

m <- svyglm(compra ~ SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD + ESCOLARIDAD + TLOC + ESTRATO,
            design = dis, family = quasibinomial())

malla <- expand.grid(SEXO = levels(usu$SEXO), GRUPO_EDAD = levels(usu$GRUPO_EDAD),
                     ESCOLARIDAD = levels(usu$ESCOLARIDAD), TLOC = levels(usu$TLOC),
                     ESTRATO = levels(usu$ESTRATO), INDIGENA = levels(usu$INDIGENA),
                     DISCAPACIDAD = levels(usu$DISCAPACIDAD))
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

# --- Perfil sin indígena ni discapacidad (vista por defecto del dashboard) ----
# Promedio de las cuatro versiones (indígena × discapacidad) de cada perfil,
# ponderado por qué tan comunes son entre las personas reales de ese perfil
# (factores de expansión). Si el perfil tiene menos de 20 casos, las
# proporciones salen del grupo más amplio de misma edad, localidad y estrato;
# si tampoco hay casos ahí, de todo el país. Intervalo al 90 % por método
# delta con la matriz de covarianzas del modelo, en escala logit.
# Se guarda con indigena = discapacidad = "Todos".
cinco <- c("SEXO", "GRUPO_EDAD", "ESCOLARIDAD", "TLOC", "ESTRATO")
amplio <- c("GRUPO_EDAD", "TLOC", "ESTRATO")
usu$combo <- paste(usu$INDIGENA, usu$DISCAPACIDAD)
combos <- c("No No", "Sí No", "No Sí", "Sí Sí")
proporciones <- function(filas) {
  w <- tapply(usu$FAC_PER[filas], factor(usu$combo[filas], levels = combos), sum)
  w <- as.numeric(w); w[is.na(w)] <- 0; w / sum(w)
}
w_pais <- proporciones(rep(TRUE, nrow(usu)))
X <- model.matrix(delete.response(terms(m)), malla)
V <- vcov(m); b <- coef(m)
malla$clave5 <- do.call(paste, c(malla[cinco], sep = "|"))
malla$combo  <- paste(malla$INDIGENA, malla$DISCAPACIDAD)
clave5_usu <- do.call(paste, c(usu[cinco], sep = "|"))
clave3_usu <- do.call(paste, c(usu[amplio], sep = "|"))
prom <- do.call(rbind, lapply(split(seq_len(nrow(malla)), malla$clave5), function(i) {
  f <- malla[i[1], ]
  en_celda <- clave5_usu == f$clave5
  if (sum(en_celda) >= 20) { w <- proporciones(en_celda); fuente <- "perfil"
  } else {
    en_grupo <- clave3_usu == paste(f$GRUPO_EDAD, f$TLOC, f$ESTRATO, sep = "|")
    if (sum(en_grupo) > 0) { w <- proporciones(en_grupo); fuente <- "edad, localidad y estrato"
    } else { w <- w_pais; fuente <- "nacional" }
  }
  i <- i[match(combos, malla$combo[i])]
  pr <- as.numeric(plogis(X[i, , drop = FALSE] %*% b))
  pm <- sum(w * pr)
  g  <- colSums(w * pr * (1 - pr) * X[i, , drop = FALSE])   # gradiente de pm
  se_logit <- sqrt(as.numeric(t(g) %*% V %*% g)) / (pm * (1 - pm))
  data.frame(f[cinco], INDIGENA = "Todos", DISCAPACIDAD = "Todos",
             estimacion = round(100 * pm, 1),
             li90 = round(100 * plogis(qlogis(pm) - z90 * se_logit), 1),
             ls90 = round(100 * plogis(qlogis(pm) + z90 * se_logit), 1),
             n = f$n, fuente_pesos = fuente)
}))
cat("Origen de las proporciones indígena × discapacidad:\n"); print(table(prom$fuente_pesos))
malla$fuente_pesos <- NA
malla <- rbind(malla[, names(prom)], prom)

out <- data.frame(sexo = malla$SEXO, edad = malla$GRUPO_EDAD, escolaridad = malla$ESCOLARIDAD,
                  localidad = malla$TLOC, estrato = malla$ESTRATO,
                  indigena = malla$INDIGENA, discapacidad = malla$DISCAPACIDAD,
                  estimacion = malla$estimacion, li90 = malla$li90, ls90 = malla$ls90,
                  n = malla$n, prueba = FALSE)
out <- out[order(out$indigena != "Todos"), ]   # primero la vista por defecto
write.csv(out, file.path("dashboard", "datos", "s2_perfil_prob.csv"), row.names = FALSE,
          fileEncoding = "UTF-8")
cat("Combinaciones:", nrow(out), "| con menos de 20 casos:", sum(out$n < 20), "\n")
print(summary(out$estimacion))
print(subset(out, sexo == "Mujer" & edad == "55-64" & estrato == "Bajo" & escolaridad == "Preescolar o primaria" & localidad == "100 mil y más"))
