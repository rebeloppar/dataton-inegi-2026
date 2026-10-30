# =============================================================================
# Dashboard — Datos de PRUEBA
# Crea un CSV por gráfica en dashboard/datos/ con cifras INVENTADAS, para que
# el cascarón funcione antes de tener las cifras finales.
#
# Cada frente reemplaza "su" archivo por uno generado desde su script de
# análisis, con EXACTAMENTE las mismas columnas (ver dashboard/README.md).
# Las cifras de prueba llevan prueba = TRUE; el dashboard muestra un aviso
# mientras quede algún archivo de prueba.
#
# Correr desde la raíz del proyecto: source("dashboard/crear_datos_prueba.R")
# =============================================================================

set.seed(2026)
dir_datos <- file.path("dashboard", "datos")
dir.create(dir_datos, showWarnings = FALSE, recursive = TRUE)

# Intervalo de prueba alrededor de una estimación (en %)
# acotar = TRUE para porcentajes (0-100); FALSE para brechas, que pueden ser negativas
con_ic <- function(df, ancho = 2, acotar = TRUE) {
  df$li90 <- round(df$estimacion - runif(nrow(df), 0.5, ancho), 1)
  df$ls90 <- round(df$estimacion + runif(nrow(df), 0.5, ancho), 1)
  if (acotar) { df$li90 <- pmax(0, df$li90); df$ls90 <- pmin(100, df$ls90) }
  df$prueba <- TRUE
  df
}
guardar <- function(df, nombre) {
  write.csv(df, file.path(dir_datos, nombre), row.names = FALSE, fileEncoding = "UTF-8")
  cat("  ", nombre, "\n")
}

grupos <- list(
  Nacional     = "Nacional",
  Escolaridad  = c("Sin escolaridad", "Primaria", "Secundaria", "Media superior", "Superior"),
  Edad         = c("18-24", "25-34", "35-44", "45-54", "55-64", "65+"),
  Sexo         = c("Hombre", "Mujer"),
  Localidad    = c("Menos de 2,500", "2,500-14,999", "15,000-99,999", "100,000 y más"),
  Estrato      = c("Bajo", "Medio bajo", "Medio alto", "Alto"),
  Región       = c("Norte", "Centro Norte", "Centro", "Sur")   # provisional (frente C)
)
grupos_df <- do.call(rbind, lapply(names(grupos), function(g)
  data.frame(grupo_var = g, grupo = grupos[[g]], orden = seq_along(grupos[[g]]))))

cat("Creando datos de prueba en", dir_datos, "\n")

# --- Sección 1: embudo (frente A) --------------------------------------------
eslabones <- c("Hogar con internet", "Usa internet", "Compra en línea")
s1 <- merge(grupos_df, data.frame(eslabon = eslabones, paso = 1:3))
# Embudo de prueba coherente: cada eslabón es una fracción del anterior
s1 <- s1[order(s1$grupo_var, s1$orden, s1$paso), ]
s1$estimacion <- ave(runif(nrow(s1)), s1$grupo_var, s1$grupo, FUN = function(u)
  round(cumprod(c(60 + 30 * u[1], 0.75 + 0.2 * u[2], 0.35 + 0.4 * u[3])), 1))
s1 <- con_ic(s1[order(s1$grupo_var, s1$orden, s1$paso), ])
guardar(s1, "s1_embudo.csv")

# P3: qué se compra (% de compradores que compró cada categoría)
categorias <- c("Ropa y calzado", "Comida a domicilio", "Electrónicos", "Belleza y cuidado personal",
                "Artículos para el hogar", "Boletos y entretenimiento", "Supermercado", "Libros y música")
s1c <- merge(grupos_df, data.frame(categoria = categorias, k = seq_along(categorias)))
s1c$estimacion <- round(c(55, 45, 30, 28, 25, 20, 15, 10)[s1c$k] * runif(nrow(s1c), 0.6, 1.4), 1)
s1c <- con_ic(s1c[order(s1c$grupo_var, s1c$orden, s1c$k), c("grupo_var", "grupo", "orden", "categoria", "estimacion")])
guardar(s1c, "s1_categorias.csv")

# Brechas en cada eslabón: ¿se pierde más gente al conectarse o al comprar?
s1b <- data.frame(
  comparacion = rep(c("Superior vs. primaria", "65+ vs. 18-24", "Rural vs. 100 mil+", "Mujer vs. hombre"), 2),
  eslabon = rep(c("Usar internet", "Comprar en línea"), each = 4),
  estimacion = c(35, -50, -20, -2, 42, -42, -11, -3))
guardar(con_ic(s1b, ancho = 3, acotar = FALSE), "s1_brechas_eslabon.csv")

# --- Sección 2: brechas brutas vs. ajustadas (frentes A y B) -----------------
s2 <- data.frame(
  factor      = c("Escolaridad", "Edad", "Localidad", "Estrato", "Sexo", "Indígena", "Discapacidad"),
  comparacion = c("Superior vs. primaria", "65+ vs. 18-24", "Rural vs. 100 mil+", "Alto vs. bajo",
                  "Mujer vs. hombre", "Indígena vs. no indígena", "Con vs. sin discapacidad"))
s2 <- rbind(transform(s2, tipo = "Bruta",    estimacion = c(50, -45, -25, 35, -4, -13, -4)),
            transform(s2, tipo = "Ajustada", estimacion = c(42, -42, -11, 18, -3, -2, -1)))
s2 <- con_ic(s2, ancho = 3, acotar = FALSE)
guardar(s2, "s2_brechas.csv")

s2p <- data.frame(perfil = c("Perfil A (por definir)", "Perfil B (por definir)",
                             "Perfil C (por definir)", "Perfil D (por definir)"),
                  estimacion = c(12, 35, 58, 81))
guardar(con_ic(s2p, ancho = 5), "s2_perfiles.csv")

# --- Sección 3: razones de no compra (frente B) ------------------------------
razones <- c("Prefiere en persona", "No le interesa", "No sabe cómo",
             "Desconfía", "No tiene tarjeta", "No tiene dinero")
s3 <- merge(grupos_df, data.frame(razon = razones, r = 1:6))
s3$estimacion <- round(c(40, 30, 20, 8, 4, 4)[s3$r] * runif(nrow(s3), 0.5, 1.6), 1)
s3 <- con_ic(s3[order(s3$grupo_var, s3$orden, s3$r), c("grupo_var", "grupo", "orden", "razon", "estimacion")])
guardar(s3, "s3_razones.csv")

# P5: problemas al comprar (% de compradores que tuvo cada problema)
problemas <- c("Algún problema", "Producto dañado o distinto", "No llegó o llegó tarde",
               "Cobro indebido o fraude", "Difícil devolver")
s3p <- merge(grupos_df, data.frame(problema = problemas, k = seq_along(problemas)))
s3p$estimacion <- round(c(20, 9, 8, 4, 3)[s3p$k] * runif(nrow(s3p), 0.6, 1.5), 1)
s3p <- con_ic(s3p[order(s3p$grupo_var, s3p$orden, s3p$k), c("grupo_var", "grupo", "orden", "problema", "estimacion")])
guardar(s3p, "s3_problemas.csv")

# --- Sección 4: territorio (frente C) ----------------------------------------
s4 <- read.csv(file.path(dir_datos, "entidades.csv"), fileEncoding = "UTF-8")
s4 <- data.frame(cve_ent = s4$cve_ent, estimacion = round(runif(32, 25, 60), 1))
guardar(con_ic(s4, ancho = 6), "s4_entidades.csv")

# Brecha por región frente a la región con mayor compra, bruta y con controles
s4r <- data.frame(region = rep(c("Norte", "Centro Norte", "Sur"), 2),
                  referencia = "Centro",
                  tipo = rep(c("Bruta", "Ajustada"), each = 3),
                  estimacion = c(-3, -8, -18, -1, -4, -7))
guardar(con_ic(s4r, ancho = 3, acotar = FALSE), "s4_regiones.csv")

# --- Sección 5: escenarios 2030 y simulador (frentes C y D) ------------------
hist <- data.frame(anio = 2019:2025, escenario = "Observado",
                   estimacion = round(seq(25, 44, length.out = 7), 1))
proy <- do.call(rbind, lapply(c("Inercial", "Habilidades", "Omnicanal"), function(e) {
  extra <- c(Inercial = 0, Habilidades = 4, Omnicanal = 8)[[e]]
  data.frame(anio = 2026:2030, escenario = e,
             estimacion = round(44 + (1:5) * 2.2 + (1:5) / 5 * extra, 1))
}))
s5 <- rbind(hist, proy)
s5 <- con_ic(s5, ancho = 1)
s5$li90[s5$escenario == "Observado"] <- NA   # la serie publicada no trae intervalo
s5$ls90[s5$escenario == "Observado"] <- NA
guardar(s5, "s5_escenarios.csv")

s5sim <- data.frame(
  grupo          = c("Primaria o menos", "Secundaria", "65 años o más", "Mujeres", "Rural"),
  millones       = c(12.7, 14.2, 6.4, 43.5, 14.5),
  pct_compra     = c(11, 30, 13, 42, 25),
  pct_no_sabe    = c(30, 22, 43, 19, 25),   # % de quienes NO compran que da la razón
  pct_en_persona = c(30, 38, 35, 42, 33),
  prueba = TRUE)
guardar(s5sim, "s5_simulador.csv")

cat("Listo. Todas las cifras son INVENTADAS (prueba = TRUE).\n")
