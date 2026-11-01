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
  Región       = c("Norte", "Norte-occidente", "Centro-norte", "Centro", "Sur")   # CEEY, decisión 62
)
grupos_df <- do.call(rbind, lapply(names(grupos), function(g)
  data.frame(grupo_var = g, grupo = grupos[[g]], orden = seq_along(grupos[[g]]))))

cat("Creando datos de prueba en", dir_datos, "\n")

# --- Sección 1 (frente A) ---------------------------------------------------
# s1_embudo.csv, s1_brechas_eslabon.csv, s1_categorias.csv y s1_hogar.csv ya son
# reales (analisis/A_conexion/01_embudo_y_brechas.R y 02_hogar_y_categorias.R).

# --- Sección 2 (frentes A y B) ------------------------------------------------
# Las brechas del modelo (s2_efectos, s2_bloques, s2_robustez) ya son reales:
# las escribe analisis/diagnostico/05_exportar_dashboard.R.
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
# s4_entidades.csv ya es real (05_exportar_dashboard.R).

# Brecha por región frente a la región con mayor compra, bruta y con controles
s4r <- data.frame(region = rep(c("Norte", "Norte-occidente", "Centro-norte", "Sur"), 2),
                  referencia = "Centro",
                  tipo = rep(c("Bruta", "Ajustada"), each = 4),
                  estimacion = c(-3, -9, -6, -18, -1, -4, -3, -7))
guardar(con_ic(s4r, ancho = 3, acotar = FALSE), "s4_regiones.csv")

# --- Sección 5: escenarios 2030 y simulador (frentes C y D) ------------------
hist <- data.frame(anio = 2019:2025, escenario = "Observado",
                   estimacion = round(seq(25, 44, length.out = 7), 1))
# Los cuatro escenarios son las cuatro posiciones de las palancas (s5_palancas.csv)
proy <- do.call(rbind, lapply(c("Inercia", "Solo tiendas", "Solo habilidades", "Puerta abierta"), function(e) {
  extra <- c(Inercia = 0, `Solo tiendas` = 3, `Solo habilidades` = 4, `Puerta abierta` = 9)[[e]]
  data.frame(anio = 2026:2030, escenario = e,
             estimacion = round(44 + (1:5) * 2.2 + (1:5) / 5 * extra, 1))
}))
s5 <- rbind(hist, proy)
s5 <- con_ic(s5, ancho = 1)
s5$li90[s5$escenario == "Observado"] <- NA   # la serie publicada no trae intervalo
s5$ls90[s5$escenario == "Observado"] <- NA
guardar(s5, "s5_escenarios.csv")

s5sim <- data.frame(
  # Grupos de la decisión 59. Cifras INVENTADAS: Rebeca las reemplaza el sábado
  grupo          = c("Primaria o menos", "Hasta secundaria", "Mujeres con primaria o menos",
                     "Rural (menos de 2,500 hab.)"),
  millones       = c(12.7, 27.0, 6.8, 14.5),
  pct_compra     = c(11, 20, 10, 25),
  pct_no_sabe    = c(30, 25, 33, 25),   # % de quienes NO compran que da la razón
  pct_en_persona = c(30, 34, 28, 33),
  prueba = TRUE)
guardar(s5sim, "s5_simulador.csv")

# Posición de las palancas en cada escenario (los ejes de la matriz 2 x 2).
# Valores PROVISIONALES: "bajo" y "alto" se calibran con la literatura del memo.
s5p <- data.frame(
  escenario   = c("Inercia", "Solo tiendas", "Solo habilidades", "Puerta abierta"),
  orden       = 1:4,
  habilidades = c(5, 5, 40, 40),
  tienda      = c(5, 40, 5, 40),
  descripcion = c("Nadie aprende ni cambia la tienda: sigue la tendencia.",
                  "Las tiendas se integran al canal digital, pero quien no sabe cómo sigue igual.",
                  "Más personas aprenden, pero no tienen una tienda que las acompañe.",
                  "Funcionan las dos puertas: el escenario al que apunta la propuesta."),
  prueba = TRUE)
guardar(s5p, "s5_palancas.csv")

# --- Opcionales (deseables): si un frente no los entrega, su pestaña queda ----
# --- como "pendiente". Borra el archivo para ver cómo se ve sin él.         ----
ent <- read.csv(file.path(dir_datos, "entidades.csv"), fileEncoding = "UTF-8")

# P3 en el tiempo (frente A, tabulados publicados)
cats <- c("Ropa y calzado", "Comida a domicilio", "Electrónicos", "Belleza y cuidado personal", "Supermercado")
s1t <- do.call(rbind, lapply(seq_along(cats), function(k)
  data.frame(categoria = cats[k], anio = 2020:2025,
             estimacion = round(c(45, 30, 28, 22, 8)[k] + (0:5) * c(1.5, 3, 0.5, 1, 1.8)[k] + rnorm(6, 0, 0.8), 1))))
s1t$prueba <- TRUE
guardar(s1t, "s1_tendencia.csv")

# P2 composición del hogar, con lente de género (frente A)
comp <- c("Vive solo/a", "Hogar sin jóvenes de 18-34", "Hogar con jóvenes de 18-34")
s2h <- merge(data.frame(composicion = comp, orden = 1:3), data.frame(sexo = c("Hombre", "Mujer")))
s2h$estimacion <- round(c(30, 33, 48)[s2h$orden] - ifelse(s2h$sexo == "Mujer", runif(6, 2, 6), 0), 1)
guardar(con_ic(s2h, ancho = 2.5), "s2_hogar.csv")

# P6 ritmo de adopción por entidad (frente C)
s4t <- data.frame(cve_ent = ent$cve_ent, inicio = round(runif(32, 12, 35), 1))
s4t$fin <- round(s4t$inicio + runif(32, 6, 22), 1)
s4t$anio_inicio <- 2020; s4t$anio_fin <- 2025; s4t$prueba <- TRUE
guardar(s4t, "s4_ritmo.csv")

# P6 el porqué: comercios al por menor (frente C, DENUE + EIC)
s4c <- data.frame(cve_ent = ent$cve_ent, comercios_10mil = round(runif(32, 60, 160)))
s4c$pct_compra <- round(15 + 0.22 * s4c$comercios_10mil + rnorm(32, 0, 5), 1)
s4c$prueba <- TRUE
guardar(s4c, "s4_comercios.csv")

# P7 tamaño del mercado (frente C, ENIGH): rango de gasto digitalizable
s5m <- data.frame(escenario = c("Hoy (2024)", "Inercia 2030", "Solo tiendas 2030", "Solo habilidades 2030", "Puerta abierta 2030"),
                  orden = 1:5, minimo = c(180, 260, 275, 285, 320), maximo = c(240, 340, 360, 375, 420), prueba = TRUE)
guardar(s5m, "s5_mercado.csv")

cat("Listo. Estos archivos de prueba llevan cifras INVENTADAS (prueba = TRUE).\n")
