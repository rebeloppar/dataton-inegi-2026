# =============================================================================
# Datatón INEGI 2026 — Regresión logística por bloques (ENDUTIH 2025)
# ¿Cuánto se reducen las brechas de los grupos al agregar cada explicación?
#   Bloque 1: grupos (sexo, edad, indígena, discapacidad)
#   Bloque 2: + escolaridad
#   Bloque 3: + tamaño de localidad
#   Bloque 4: + estrato socioeconómico (modelo completo)
#   Robustez: modelo completo solo en localidades de 100 mil y más
# Resultados en efectos marginales promedio (puntos porcentuales).
# Universo: usuarios de internet de 18 años o más. Decisiones: bitacoras/decisiones.md
# =============================================================================

source(file.path("R", "prep_endutih.R"))
suppressMessages(library(marginaleffects))

# --- 1. Categorías de referencia ----------------------------------------------
# Cada efecto se lee "frente a" la primera categoría de su variable:
# Hombre · 18-24 · No indígena · Sin discapacidad · Primaria o menos ·
# 100 mil y más · Estrato bajo. Cambiar la referencia no cambia el modelo,
# solo contra qué grupo se compara cada número (se cambia con relevel()).

# --- 2. Modelos por bloques ----------------------------------------------------
# quasibinomial = logit con pesos de encuesta (evita advertencias por pesos no enteros)
bloques <- list(
  "1 Grupos"       = compra ~ SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD,
  "2 +Escolaridad" = compra ~ SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD +
                              ESCOLARIDAD,
  "3 +Localidad"   = compra ~ SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD +
                              ESCOLARIDAD + TLOC,
  "4 +Estrato"     = compra ~ SEXO + GRUPO_EDAD + INDIGENA + DISCAPACIDAD +
                              ESCOLARIDAD + TLOC + ESTRATO)

ajustar <- function(f, diseno) svyglm(f, design = diseno, family = quasibinomial())

# Efectos marginales promedio: para cada categoría, cuántos puntos cambia la
# probabilidad de comprar frente a la referencia, dejando todo lo demás igual
# y promediando sobre la población (con sus pesos).
efectos <- function(mod, nombre) {
  e <- avg_comparisons(mod, wts = "(weights)", conf_level = 0.90)
  data.frame(modelo   = nombre,
             variable = e$term,
             contraste = e$contrast,
             ame_pp   = round(100 * e$estimate, 1),
             ic90_inf = round(100 * e$conf.low, 1),
             ic90_sup = round(100 * e$conf.high, 1))
}

modelos <- lapply(bloques, ajustar, diseno = dis)
tabla <- do.call(rbind, Map(efectos, modelos, names(modelos)))
rownames(tabla) <- NULL

# --- 3. Tabla principal: ¿cómo cambian las brechas de los grupos? --------------
grupos <- c("SEXO", "GRUPO_EDAD", "INDIGENA", "DISCAPACIDAD")
ancha <- reshape(tabla[tabla$variable %in% grupos, c("modelo", "variable",
                                                     "contraste", "ame_pp")],
                 idvar = c("variable", "contraste"), timevar = "modelo",
                 direction = "wide")
names(ancha) <- sub("ame_pp\\.", "", names(ancha))
cat("\n===== Brechas de los grupos al agregar explicaciones (pp) =====\n")
cat("Cada columna agrega un bloque. Si la cifra se acerca a 0, la nueva\n")
cat("explicación da cuenta de esa parte de la brecha.\n\n")
print(ancha, row.names = FALSE)

# --- 4. Modelo completo: todos los efectos con su intervalo --------------------
cat("\n===== Modelo completo (bloque 4), efectos marginales con IC 90 % =====\n")
print(subset(tabla, modelo == "4 +Estrato")[, -1], row.names = FALSE)

# --- 5. Robustez: modelo completo solo en ciudades de 100 mil y más ------------
# Ahí el estrato no se confunde con ruralidad (la localidad no varía).
dis_ciudad <- subset(dis, TLOC == "100 mil y más")
f_ciudad <- update(bloques[["4 +Estrato"]], . ~ . - TLOC)
rob <- efectos(ajustar(f_ciudad, dis_ciudad), "Solo 100 mil y más")
comp <- merge(subset(tabla, modelo == "4 +Estrato")[, c("variable", "contraste", "ame_pp")],
              rob[, c("variable", "contraste", "ame_pp")],
              by = c("variable", "contraste"), suffixes = c("_nacional", "_ciudades"))
cat("\n===== Robustez: modelo completo nacional vs. solo ciudades (pp) =====\n")
print(comp, row.names = FALSE)

# --- 6. Guardar ----------------------------------------------------------------
write.csv(tabla, "resultados/E_regresion_bloques.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
write.csv(comp, "resultados/F_robustez_ciudades.csv", row.names = FALSE,
          fileEncoding = "UTF-8")
