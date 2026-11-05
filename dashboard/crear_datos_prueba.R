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
# Nunca sobrescribe un archivo que ya tiene cifras reales (prueba = FALSE)
guardar <- function(df, nombre) {
  ruta <- file.path(dir_datos, nombre)
  if (file.exists(ruta)) {
    ya <- read.csv(ruta, fileEncoding = "UTF-8")
    if ("prueba" %in% names(ya) && !any(ya$prueba)) { cat("   (real, no se toca)", nombre, "\n"); return(invisible(NULL)) }
  }
  write.csv(df, ruta, row.names = FALSE, fileEncoding = "UTF-8")
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
# s2_perfil_prob.csv (Arma un perfil) es real: analisis/D_propuesta/08_perfiles_interactivos.R

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

# --- Sección 5 (frente D) ---------------------------------------------------
# s5_simulador.csv es real («Lo que está en juego»): analisis/D_propuesta/09_simulador.R

# --- Sección 6: prospectiva 2030 (frentes C, B y D; decisión 76) ------------
# Solo el futuro probable lleva números. Los escenarios se escriben con
# palabras: no hay evidencia de cuántas personas cambiaría cada uno.

# Cono de futuros (frente C): serie observada, futuro probable (tendencia con
# rango). El archivo real ya existe, así que esto no se usa
obs <- data.frame(anio = 2019:2025, serie = "Observado",
                  estimacion = round(seq(25, 44, length.out = 7), 1), li90 = NA, ls90 = NA)
prob <- data.frame(anio = 2025:2030, serie = "Probable (tendencia)",
                   estimacion = round(44 + (0:5) * 2.2, 1))
prob$li90 <- round(prob$estimacion - (0:5) * 1.4, 1); prob$ls90 <- round(prob$estimacion + (0:5) * 1.4, 1)
s6c <- rbind(obs, prob); s6c$prueba <- TRUE
guardar(s6c, "s6_cono.csv")

# Impulsores y matriz de influencia y dependencia (MICMAC, frentes B y D)
s6i <- data.frame(
  impulsor = c("Habilidades digitales de quienes no compran", "Tiendas integradas al canal digital",
               "Confianza en pagar en línea", "Cobertura de internet móvil", "Cuentas y tarjetas (inclusión financiera)",
               "Costo y alcance de los envíos fuera de las ciudades", "Reglas de comercio electrónico y protección al consumidor",
               "Reemplazo generacional", "Programas públicos de inclusión digital", "Precio de los datos móviles"),
  categoria = c("Social", "Económico", "Social", "Tecnológico", "Económico", "Económico", "Legal",
                "Social", "Político", "Económico"),
  influencia = c(26, 24, 18, 15, 12, 14, 9, 20, 11, 10),
  dependencia = c(14, 12, 22, 9, 16, 11, 6, 4, 18, 8),
  prueba = TRUE)
guardar(s6i, "s6_impulsores.csv")

# Ejes de los escenarios (los dos impulsores más influyentes e inciertos)
s6e <- data.frame(eje = 1:2,
  nombre = c("Habilidades y confianza de quienes hoy no compran",
             "Integración de la tienda física con el canal digital"),
  polo_alto = c("Aprenden y confían", "La tienda acompaña la compra en línea"),
  polo_bajo = c("Siguen sin saber cómo", "La tienda sigue aparte"),
  prueba = TRUE)
guardar(s6e, "s6_ejes.csv")

# Cuatro escenarios: narrativa corta, sin cifras (decisión 76)
s6s <- data.frame(
  eje1 = c("alto", "alto", "bajo", "bajo"), eje2 = c("bajo", "alto", "bajo", "alto"),
  nombre = c("Solo habilidades", "Puerta abierta", "Inercia", "Solo tiendas"),
  narrativa = c("Por escribir: más personas saben comprar, pero no tienen cerca una tienda que las acompañe.",
                "Por escribir: el escenario preferible, al que apunta la propuesta.",
                "Por escribir: sigue la tendencia; el grupo se queda atrás.",
                "Por escribir: las cadenas integran la tienda, pero quien no sabe cómo no se acerca."),
  prueba = TRUE)
guardar(s6s, "s6_escenarios.csv")

# Retroproyección: qué tiene que pasar, año por año, para llegar al preferible (D)
s6r <- data.frame(anio = c(2026, 2027, 2028, 2030),
  hito = c("Por definir: piloto en tiendas con kiosco frente a tiendas comparables",
           "Por definir: resultados del piloto; decidir si se amplía",
           "Por definir: ampliación a más tiendas y cadenas",
           "Puerta abierta: el escenario preferible"),
  como_se_sabe = c("Por definir", "Por definir", "Por definir", "Por definir"),
  prueba = TRUE)
guardar(s6r, "s6_retroproyeccion.csv")

# Señales tempranas (frente B): indicadores que dirían hacia qué escenario vamos.
# Las líneas base marcadas prueba = FALSE ya son reales (ENDUTIH 2025).
s6n <- data.frame(
  senal = c("Más gente del grupo compra en línea", "Menos gente dice «no sé cómo»",
            "Más compradores recogen en tienda", "Por definir"),
  indicador = c("% de usuarios del grupo (hasta secundaria) que compró en línea",
                "% de quienes no compran en el grupo que dice «no sé cómo»",
                "% de compradores que recoge en el punto de venta", "Por definir"),
  linea_base = c("21.9 % (ENDUTIH 2025)", "22.7 % (ENDUTIH 2025)", "18.5 % (ENDUTIH 2025)", "—"),
  apunta_a = c("Cualquier escenario con una puerta abierta", "Solo habilidades o Puerta abierta",
               "Solo tiendas o Puerta abierta", "—"),
  prueba = c(FALSE, FALSE, FALSE, TRUE))
guardar(s6n, "s6_senales.csv")

# --- Opcionales ---------------------------------------------------------------
# s1_tendencia y s2_hogar ya son reales (frente A). Ritmo de adopción por
# entidad, comercios (DENUE) y tamaño del mercado (ENIGH) no se pudieron hacer
# con datos oficiales a tiempo y salen del dashboard (decisión 87): su pestaña
# solo aparece si el archivo existe.

cat("Listo. Estos archivos de prueba llevan cifras INVENTADAS (prueba = TRUE).\n")
