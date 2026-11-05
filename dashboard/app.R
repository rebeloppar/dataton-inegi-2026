# =============================================================================
# Dashboard — Datatón INEGI 2026
# Portada y 5 secciones del storyboard. Cada sección: afirmación → filtros →
# gráficas → "Lo que encontramos". Debajo de cada gráfica: "Cómo se calculó" y
# "Descargar datos".
#
# Cada gráfica lee un CSV de dashboard/datos/. Hoy muchos son datos de PRUEBA;
# cada frente los reemplaza por los finales con las mismas columnas
# (ver dashboard/README.md). Los archivos opcionales pueden faltar: su pestaña
# muestra "pendiente". Los textos viven en hallazgos.csv, propuesta.csv y
# estrategias.csv, así se editan sin tocar código.
#
# Correr: abrir el proyecto en RStudio y ejecutar shiny::runApp("dashboard")
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

# --- Datos -------------------------------------------------------------------
dir_datos <- if (dir.exists("datos")) "datos" else file.path("dashboard", "datos")
ruta <- function(nombre) file.path(dir_datos, nombre)
leer <- function(nombre) read.csv(ruta(nombre), fileEncoding = "UTF-8")
leer_opcional <- function(nombre) if (file.exists(ruta(nombre))) leer(nombre) else NULL

embudo     <- leer("s1_embudo.csv")
categorias <- leer("s1_categorias.csv")
eslabon    <- leer("s1_brechas_eslabon.csv")
efectos    <- leer("s2_efectos.csv")
bloques    <- leer("s2_bloques.csv")
robustez   <- leer("s2_robustez.csv")
perfil_prob <- leer("s2_perfil_prob.csv")
razones    <- leer("s3_razones.csv")
problemas  <- leer("s3_problemas.csv")
genero     <- leer("s3_genero.csv")
ent_base   <- leer("entidades.csv")
entidades  <- merge(ent_base, leer("s4_entidades.csv"), by = "cve_ent")
mapa       <- leer("mapa_estados.csv")
regiones   <- leer("s4_regiones.csv")
simul      <- leer("s5_simulador.csv")
# Sección 6: prospectiva 2030 (decisión 76)
cono       <- leer("s6_cono.csv")
impulsores <- leer("s6_impulsores.csv")
ejes6      <- leer("s6_ejes.csv")
escen6     <- leer("s6_escenarios.csv")
retro      <- leer("s6_retroproyeccion.csv")
senales    <- leer("s6_senales.csv")

# Opcionales (deseables)
tendencia  <- leer_opcional("s1_tendencia.csv")
conexion   <- leer_opcional("s1_hogar.csv")
hogar      <- leer_opcional("s2_hogar.csv")
ritmo      <- leer_opcional("s4_ritmo.csv")
comercios  <- leer_opcional("s4_comercios.csv")
mercado    <- leer_opcional("s5_mercado.csv")

calculo     <- leer("como_se_calculo.csv")
hallazgos   <- leer("hallazgos.csv")
propuesta   <- leer("propuesta.csv")
riesgos     <- leer("riesgos.csv")
estrategias <- leer("estrategias.csv")

hay_prueba <- any(sapply(list(embudo, categorias, eslabon, efectos, bloques, robustez, perfil_prob, razones, genero,
                              problemas, entidades, regiones, simul,
                              cono, impulsores, ejes6, escen6, retro, senales,
                              tendencia, conexion, hogar, ritmo, comercios, mercado),
                         function(d) !is.null(d) && isTRUE(any(d$prueba))))

# --- Estilo de las gráficas --------------------------------------------------
# Categórica validada (daltonismo y contraste): azul, verde, morado, ocre.
# Secuencial: rampa cálida de la figura de referencia. Texto siempre en tinta.
col <- list(
  tinta = "#1F2937", gris = "#6B7280", gris_claro = "#9CA3AF", borde = "#D5D9D2",
  reja = "#ECEEEA", azul = "#0F72A8", verde = "#3E9A66", morado = "#6A4C93",
  ocre = "#B97A1C", calido = "#F8E2CF", frio = "#EEF3EE"
)
# Las 5 regiones del CEEY: validada en todos los pares (es un mapa), decisión 62
col_region <- c(Norte = "#0F72A8", `Norte-occidente` = "#3E9A66", `Centro-norte` = "#B97A1C",
                Centro = "#BB1B86", Sur = "#8F2432")
rampa <- c("#EEF3EE", "#F3F2E2", "#FBEFD0", "#F7DCC0", "#F0BEB2", "#E2928C")

theme_set(
  theme_minimal(base_size = 13, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 15, colour = col$tinta, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 11.5, colour = col$gris, margin = margin(b = 12)),
      plot.caption = element_text(size = 9.5, colour = col$gris, hjust = 0,
                                  margin = margin(t = 12), lineheight = 1.2),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(colour = col$borde, fill = NA, linewidth = 0.5),
      panel.grid.major = element_line(colour = col$reja, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = col$gris, size = 10.5),
      axis.title = element_text(face = "bold", colour = col$tinta, size = 11),
      strip.text = element_text(face = "bold", colour = col$tinta, size = 11.5),
      legend.position = "bottom", legend.justification = "left",
      legend.title = element_text(face = "bold", colour = col$tinta, size = 10.5),
      legend.text = element_text(colour = col$tinta, size = 10.5),
      plot.margin = margin(10, 14, 10, 10)
    )
)
pct <- function(x) paste0(x, " %")
pp  <- function(x) ifelse(x > 0, paste0("+", x), as.character(x))
punto <- function(...) geom_point(shape = 21, colour = "white", stroke = 1.3, ...)
`%|%` <- function(x, y) ifelse(is.na(x), y, x)
sin_ejes <- theme(axis.text = element_blank(), panel.grid.major = element_blank(),
                  panel.border = element_blank())

# --- Estilo de la página -----------------------------------------------------
tema <- bs_theme(
  version = 5, primary = col$azul, bg = "#FFFFFF", fg = col$tinta,
  base_font = font_collection(font_google("Atkinson Hyperlegible", local = FALSE),
                              "system-ui", "-apple-system", "Segoe UI", "Helvetica Neue", "Arial", "sans-serif"),
  "border-radius" = "10px", "font-size-base" = "1rem"
)
tema <- bs_add_rules(tema, "
  :root { --tinta:#1F2937; --gris:#6B7280; --borde:#E3E6E0; --menta:#F2F6F1; --azul:#0F72A8;
          --morado:#6A4C93; --durazno:#F8E2CF; --verde:#3E9A66; --ocre:#B97A1C; }
  body { background:#FFFFFF; }
  .navbar { background:#FFFFFF !important; border-bottom:1px solid var(--borde); padding-top:.6rem; padding-bottom:.6rem; }
  .navbar-brand { font-weight:700; letter-spacing:-.01em; }
  .navbar .nav-link { color:var(--gris) !important; font-weight:600; }
  .navbar .nav-link.active { color:var(--tinta) !important; box-shadow: inset 0 -3px 0 var(--azul); }
  .insignia-prueba { background:#FDF3D7; color:#7A5A12; border:1px solid #F1DFA6; border-radius:999px;
                     padding:.2rem .75rem; font-size:.8rem; font-weight:700; white-space:nowrap; }
  .contenedor { max-width:1180px; margin:0 auto; padding: 0 1rem 3rem; }

  /* Portada */
  .portada { background:var(--menta); border-bottom:1px solid var(--borde); margin: 0 -1rem 2rem; padding: 3rem 1rem 2.5rem; }
  .portada-interior { max-width:1180px; margin:0 auto; }
  .portada .contexto { color:var(--gris); font-size:1.05rem; max-width:62ch; margin-bottom:1.75rem; }
  .portada h1 { font-size:clamp(2.2rem, 4.5vw, 3.6rem); font-weight:700; letter-spacing:-.025em; line-height:1.05; margin-bottom:.75rem; }
  .barreras { display:grid; grid-template-columns: 1fr 1fr; gap:0; border:1px solid var(--borde); border-radius:12px; overflow:hidden; background:#fff; margin-top:2rem; }
  .barrera { padding:1.5rem 1.75rem; }
  .barrera + .barrera { border-left:1px solid var(--borde); }
  .barrera h3 { font-size:1.35rem; font-weight:700; margin:.1rem 0 .5rem; }
  .barrera .quienes { color:var(--gris); margin-bottom:1rem; }
  .barrera .cifra { font-size:2.6rem; font-weight:700; letter-spacing:-.02em; line-height:1; }
  .barrera .cifra-nota { color:var(--gris); font-size:.95rem; margin-top:.35rem; max-width:40ch; }
  .barrera.no-pueden { box-shadow: inset 6px 0 0 var(--azul); }
  .barrera.no-quieren { box-shadow: inset 6px 0 0 #E2928C; }
  .datos-clave { display:flex; flex-wrap:wrap; gap:2.5rem; margin-top:1.75rem; }
  .dato .valor { font-size:1.6rem; font-weight:700; letter-spacing:-.01em; }
  .dato .etiqueta { color:var(--gris); font-size:.92rem; max-width:24ch; }
  .recorrido { border-top:1px solid var(--borde); }
  .recorrido a { display:grid; grid-template-columns: 3rem 1fr auto; align-items:baseline; gap:1rem;
                 padding:1rem .25rem; border-bottom:1px solid var(--borde); color:var(--tinta); text-decoration:none; }
  .recorrido a:hover .titulo-paso, .recorrido a:focus .titulo-paso { color:var(--azul); text-decoration:underline; }
  .recorrido .num { font-size:1.4rem; font-weight:700; color:var(--azul); }
  .recorrido .titulo-paso { font-weight:700; font-size:1.1rem; }
  .recorrido .afirm { color:var(--gris); }
  .recorrido .ir { color:var(--gris); font-size:.9rem; }

  /* Secciones */
  .cabecera { display:grid; grid-template-columns: auto 1fr; gap:1.25rem; align-items:start; margin: 2.25rem 0 1.25rem; }
  .cabecera .num { font-size:3.2rem; font-weight:700; line-height:.9; color:var(--azul); }
  .cabecera h2 { font-size:clamp(1.6rem, 2.8vw, 2.25rem); font-weight:700; letter-spacing:-.02em; line-height:1.1; margin:0 0 .35rem; }
  .cabecera p { color:var(--gris); margin:0; max-width:75ch; }
  .filtros { display:flex; flex-wrap:wrap; align-items:center; gap:.75rem; margin-bottom:1rem; }
  .filtros .rotulo { color:var(--gris); font-weight:600; }
  .filtros .form-group { margin:0; }
  .pastillas .shiny-options-group { display:flex; flex-wrap:wrap; gap:.4rem; }
  .pastillas .radio-inline { margin:0 !important; padding:.35rem .95rem !important; border:1px solid var(--borde); border-radius:999px;
                             background:#fff; cursor:pointer; font-weight:600; color:var(--gris); }
  .pastillas .radio-inline input { position:absolute; opacity:0; width:1px; height:1px; }
  .pastillas .radio-inline:has(input:checked) { background:var(--tinta); border-color:var(--tinta); color:#fff; }
  .pastillas .radio-inline:has(input:focus-visible) { outline:3px solid var(--azul); outline-offset:2px; }
  .lienzo { border:1px solid var(--borde); border-radius:12px; background:#fff; padding:.75rem 1rem 1rem; height:100%; }
  .lienzo .nav-underline, .lienzo .nav-tabs { margin-bottom:.5rem; }
  .card { border-color:var(--borde); box-shadow:none; }
  .nav-underline .nav-link { color:var(--gris); font-weight:600; }
  .nav-underline .nav-link.active { color:var(--tinta); border-bottom-color:var(--azul); }
  .pie-grafica { display:flex; flex-wrap:wrap; justify-content:space-between; gap:.5rem 1.5rem; align-items:flex-start;
                 border-top:1px solid var(--borde); padding-top:.6rem; margin-top:.25rem; font-size:.9rem; }
  .pie-grafica details { flex:1 1 220px; }
  .pie-grafica summary { color:var(--azul); font-weight:600; cursor:pointer; }
  .pie-grafica dl { display:grid; grid-template-columns: 7rem 1fr; gap:.25rem .75rem; margin:.6rem 0 0; color:var(--tinta); }
  .pie-grafica dt { color:var(--gris); font-weight:600; }
  .pie-grafica dd { margin:0; }
  .pie-grafica .descarga { color:var(--azul); font-weight:600; white-space:nowrap; }
  .pendiente { border:1.5px dashed var(--borde); border-radius:12px; padding:2.5rem 1.5rem; text-align:center; color:var(--gris); margin:1rem 0; }
  .pendiente strong { color:var(--tinta); display:block; margin-bottom:.25rem; }
  .detalle-mapa { min-height:1.5rem; color:var(--tinta); }
  .como-leer { border-left:4px solid var(--azul); background:var(--menta); padding:.9rem 1.1rem; border-radius:0 10px 10px 0; margin-bottom:1rem; max-width:110ch; }

  /* Lo que encontramos */
  .encontramos { margin-top:2rem; background:var(--menta); border-radius:12px; padding:1.25rem 1.5rem .5rem; }
  .encontramos h3 { font-size:1.15rem; font-weight:700; margin:0 0 .5rem; }
  .hallazgo { display:grid; grid-template-columns: 8.5rem 1fr; gap:1.25rem; padding:.9rem 0; border-top:1px solid var(--borde); align-items:baseline; }
  .hallazgo .cifra { font-size:1.45rem; font-weight:700; color:var(--azul); letter-spacing:-.01em; }
  .hallazgo .texto { max-width:78ch; }
  .hallazgo.pendiente-texto .cifra { display:none; }
  .hallazgo.pendiente-texto .texto { grid-column:1 / -1; color:var(--gris); font-style:italic; }

  /* Propuesta */
  .ficha { display:grid; grid-template-columns: 13rem 1fr; gap:0; border-top:1px solid var(--borde); }
  .ficha dt, .ficha dd { padding:.8rem 0; border-bottom:1px solid var(--borde); margin:0; }
  .ficha dt { font-weight:700; padding-right:1rem; }
  .ficha dd { max-width:70ch; }
  .estrategias { display:grid; grid-template-columns:1fr 1fr; gap:1.5rem; margin-top:.5rem; }
  .estrategias h4 { font-size:1.1rem; font-weight:700; margin:0 0 .25rem; }
  .estrategias .nota { color:var(--gris); font-size:.9rem; margin-bottom:.75rem; }
  .estrategia { padding:.8rem 1rem; border-left:4px solid var(--azul); background:#fff; border-top:1px solid var(--borde);
                border-right:1px solid var(--borde); border-bottom:1px solid var(--borde); border-radius:0 10px 10px 0; margin-bottom:.6rem; }
  .publica .estrategia { border-left-color:var(--morado); }
  .estrategia .actor { font-weight:700; }
  .estrategia .porque { color:var(--gris); font-size:.92rem; margin-top:.2rem; }
  .kpis { display:grid; grid-template-columns: repeat(3, 1fr); gap:1rem; margin-bottom:1rem; }
  .kpi { border:1px solid var(--borde); border-radius:12px; padding:1rem 1.25rem; background:#fff; }
  .kpi .etiqueta { color:var(--gris); font-weight:600; }
  .kpi .valor { font-size:2.2rem; font-weight:700; letter-spacing:-.02em; }
  .kpi.destacado { background:var(--tinta); color:#fff; border-color:var(--tinta); }
  .kpi.destacado .etiqueta { color:#D1D5DB; }

  .ficha details.detalle { margin-top:.4rem; font-size:.9rem; color:var(--gris); }
  .ficha details.detalle summary { cursor:pointer; color:var(--azul); font-weight:600; }
  .ficha details.detalle p { margin:.4rem 0 0; }
  /* Sección 6: prospectiva */
  .esc-matriz { display:grid; grid-template-columns:2.2rem 1fr 1fr; grid-template-rows:auto auto auto; gap:.75rem; margin-top:.75rem; }
  .esc-eje-y { grid-row:1 / 3; writing-mode:vertical-rl; transform:rotate(180deg); text-align:center; font-weight:700; color:var(--gris); font-size:.9rem; }
  .esc-eje-x { grid-column:2 / 4; text-align:center; font-weight:700; color:var(--gris); font-size:.9rem; }
  .esc { border:1px solid var(--borde); border-radius:12px; padding:1rem 1.1rem; background:#fff; }
  .esc .polos { color:var(--gris); font-size:.82rem; margin-bottom:.35rem; }
  .esc h4 { font-size:1.1rem; font-weight:700; margin:0 0 .35rem; }
  .esc.preferible { border:2px solid var(--azul); background:#F3F8FC; }
  .esc .etq { display:inline-block; font-size:.75rem; font-weight:700; color:#fff; background:var(--azul); border-radius:999px; padding:.1rem .55rem; margin-bottom:.35rem; }
  .retro { list-style:none; padding:0; margin:.75rem 0 0; border-left:3px solid var(--borde); }
  .retro li { position:relative; padding:0 0 1.1rem 1.4rem; }
  .retro li::before { content:''; position:absolute; left:-.55rem; top:.2rem; width:.9rem; height:.9rem; border-radius:50%; background:var(--azul); border:2px solid #fff; }
  .retro .anio { font-weight:700; font-size:1.05rem; }
  .retro .como { color:var(--gris); font-size:.9rem; }
  .tabla-senales { width:100%; border-collapse:collapse; margin-top:.75rem; font-size:.95rem; }
  .tabla-senales th { text-align:left; color:var(--gris); font-weight:700; border-bottom:2px solid var(--borde); padding:.5rem .6rem; }
  .tabla-senales td { border-bottom:1px solid var(--borde); padding:.55rem .6rem; vertical-align:top; }
  .por-definir { color:var(--gris); font-style:italic; }

  @media (max-width: 768px) {
    .esc-matriz { grid-template-columns:1fr; }
    .esc-eje-y { writing-mode:horizontal-tb; transform:none; grid-row:auto; }
    .esc-eje-x { grid-column:auto; }
    .barreras, .estrategias, .kpis { grid-template-columns:1fr; }
    .barrera + .barrera { border-left:0; border-top:1px solid var(--borde); }
    .hallazgo, .ficha { grid-template-columns:1fr; gap:.25rem; }
    .cabecera .num { font-size:2.4rem; }
    .datos-clave { display:grid; grid-template-columns:1fr 1fr; gap:1.25rem; }
    .recorrido a { grid-template-columns: 2rem 1fr; }
    .recorrido .ir { display:none; }
  }
  @media (prefers-reduced-motion: reduce) { * { transition:none !important; } }
")

# --- Piezas de la interfaz ---------------------------------------------------
# Pie de cada gráfica: "Cómo se calculó" (desplegable) y descarga del CSV
pie_grafica <- function(id_calculo, archivo) {
  m <- calculo[calculo$grafica == id_calculo, ]
  detalle <- if (nrow(m) == 1) tags$details(
    tags$summary("Cómo se calculó"),
    tags$dl(tags$dt("Fuente"), tags$dd(m$fuente), tags$dt("Universo"), tags$dd(m$universo),
            tags$dt("Tipo"), tags$dd(m$tipo), tags$dt("Script"), tags$dd(tags$code(m$script)),
            tags$dt("Nota"), tags$dd(m$nota)))
  div(class = "pie-grafica", detalle,
      downloadLink(paste0("dl_", sub("\\.csv$", "", archivo)), "Descargar datos (CSV)", class = "descarga"))
}

grafica <- function(salida, alto, id_calculo, archivo, ...) {
  tagList(plotOutput(salida, height = alto, ...), pie_grafica(id_calculo, archivo))
}

# Pestaña opcional: si el frente aún no entrega su archivo, se ve "pendiente"
opcional <- function(datos, salida, alto, id_calculo, archivo, quien) {
  if (is.null(datos)) div(class = "pendiente", strong("Pendiente"), quien)
  else grafica(salida, alto, id_calculo, archivo)
}

# «Arma un perfil» (decisión 72): la persona elige características y ve la
# probabilidad predicha por el modelo completo. Todo corre en el navegador
# (www/arma_un_perfil.js) con las 3,840 combinaciones de s2_perfil_prob.csv.
arma_un_perfil <- function() {
  claves <- paste(perfil_prob$sexo, perfil_prob$edad, perfil_prob$escolaridad, perfil_prob$localidad,
                  perfil_prob$estrato, perfil_prob$indigena, perfil_prob$discapacidad, sep = "|")
  valores <- lapply(seq_len(nrow(perfil_prob)), function(i)
    c(perfil_prob$estimacion[i], perfil_prob$li90[i], perfil_prob$ls90[i], perfil_prob$n[i]))
  datos <- jsonlite::toJSON(setNames(valores, claves), auto_unbox = FALSE)
  tagList(
    tags$link(rel = "stylesheet", href = "arma_un_perfil.css"),
    p(class = "text-muted mt-3", "Elige las características de una persona usuaria de internet de 18+ y mira qué tan probable es que compre en línea, según el modelo completo de la regresión."),
    div(class = "ap-card",
        div(id = "ap-controles"),
        div(class = "ap-centro", tags$svg(class = "ap-figura", id = "ap-fig", viewBox = "0 0 240 300"),
            div(class = "ap-insignias", id = "ap-ins")),
        div(class = "ap-derecha", div(id = "ap-res"), div(class = "ap-sugeridos",
          div(class = "ap-lab", "Prueba estos perfiles"),
          tags$ul(
            tags$li("Una mujer de 65+ con primaria en una localidad rural. Luego cambia solo su escolaridad."),
            tags$li("Un hombre de 35-44 con secundaria en una ciudad media. ¿Cuánto sube con universidad?"),
            tags$li("Cualquier perfil: cambia solo el sexo y mira cuánto se mueve."))))),
    p(class = "ap-nota", "Probabilidad predicha por el modelo logit con diseño muestral (ENDUTIH 2025), con sexo, edad, escolaridad, tamaño de localidad, estrato, indígena (autoadscripción) y discapacidad (mucha dificultad o no poder hacer alguna actividad básica). Si no se activan indígena y discapacidad, la cifra es el promedio de las cuatro combinaciones, ponderado por qué tan comunes son entre las personas de ese perfil. Línea vertical negra: promedio nacional (43.7 %)."),
    tags$script(HTML(paste0("window.PERFIL_DATOS=", datos, ";"))),
    tags$script(src = "arma_un_perfil.js"),
    pie_grafica("s2_perfil_prob", "s2_perfil_prob.csv"))
}

# Campos de «La propuesta» (decisiones 79 y 81): se ve un resumen de una
# línea; el texto completo va plegado en «Ver más» y el detalle operativo en
# «Detalle (si lo preguntan)». Los riesgos salen de riesgos.csv, cada uno con
# su respuesta plegada.
lleno <- function(x) !is.null(x) && length(x) == 1 && !is.na(x) && nzchar(x)
campo_propuesta <- function(f) {
  detalle <- if (lleno(f$detalle)) tags$details(class = "detalle", tags$summary("Detalle (si lo preguntan)"), p(f$detalle))
  if (f$campo == "Riesgos") {
    r <- riesgos[order(riesgos$orden), ]
    return(tagList(if (lleno(f$resumen)) p(class = "resumen", f$resumen),
      div(class = "riesgos", lapply(seq_len(nrow(r)), function(i)
        tags$details(class = "riesgo", tags$summary(r$riesgo[i]), p(r$respuesta[i]))))))
  }
  if (!lleno(f$resumen)) return(tagList(f$texto, detalle))
  tagList(p(class = "resumen", f$resumen),
          if (lleno(f$texto)) tags$details(class = "mas", tags$summary("Ver más"), p(f$texto), detalle) else detalle)
}

# Núcleo de la propuesta (decisión 80): dos puertas → la tienda (3 pasos) →
# compradora independiente. Las cifras salen de s5_simulador.csv, igual que
# «Lo que está en juego» (quien da las dos razones cuenta solo en la puerta 1).
nucleo_propuesta <- function() {
  g <- simul[simul$grupo == "Grupo: hasta secundaria", ]
  no_compran <- g$millones * (1 - g$pct_compra / 100)
  m <- function(x) sprintf("%.1f M", x)
  paso <- function(n, titulo, texto) div(class = "np-paso", div(class = "num", n), div(class = "ti", titulo), div(class = "tx", texto))
  div(class = "np",
    tags$link(rel = "stylesheet", href = "nucleo_propuesta.css"),
    h3("La tienda como puerta de entrada"),
    p(class = "sub", "Dos barreras, una misma puerta: la infraestructura que las cadenas ya tienen, usada para que la persona aprenda a comprar por su cuenta."),
    div(class = "np-flujo",
      div(class = "np-puertas",
        div(class = "np-puerta p1", div(class = "et", "Puerta 1"), div(class = "q", "«No sé cómo»"),
            div(class = "n", m(no_compran * g$pct_no_sabe / 100)), div(class = "nn", "del grupo que no compra")),
        div(class = "np-puerta p2", div(class = "et", "Puerta 2"), div(class = "q", "«Prefiero en persona»"),
            div(class = "n", m(no_compran * g$pct_en_persona / 100)), div(class = "nn", "del grupo que no compra"))),
      div(class = "np-flecha", "→"),
      div(class = "np-tienda",
        div(class = "tt", "En la tienda"),
        div(class = "ya", "Lo que ya existe: kioscos con asesor, recoger y pagar en tienda"),
        div(class = "np-pasos",
          paso(1, "Te invitan", "En cada ventana de oportunidad: cuando no está lo que buscas, cuando hay ofertas de artículos grandes."),
          paso(2, "Lo haces tú, con alguien al lado", "Haces el pedido y el pago tú misma; el asesor acompaña, no compra por ti."),
          paso(3, "Compras por tu cuenta", "El apoyo ya no hace falta; un recordatorio te avisa de la siguiente oportunidad.")),
        div(class = "np-apoyo", span("Apoyo del asesor"), div(class = "barra"), span("ninguno")),
        div(class = "np-genero", strong("Lente de género: "), "énfasis en el paso del pago, primeras compras en lo que las mujeres ya compran, todo medido por sexo.")),
      div(class = "np-flecha", "→"),
      div(class = "np-salida", div(class = "ti", "Compradora en línea independiente"),
          div(class = "tx", "Para quien prefiere en persona, pedir en línea y recoger o pagar en tienda ya cuenta."))),
    div(class = "np-medir", strong("Se mide antes de extenderlo: "),
        sprintf("piloto en tiendas que lo aplican frente a tiendas parecidas que no; en el país, con la ENDUTIH (hoy compra %.1f %% del grupo).", g$pct_compra)))
}

# Sección 6 (decisión 76): escenarios como matriz 2 x 2 de textos, sin cifras
texto_o_pendiente <- function(x) if (grepl("^Por (escribir|definir)", x)) span(class = "por-definir", x) else x
escenarios_ui <- function() {
  e1 <- ejes6[ejes6$eje == 1, ]; e2 <- ejes6[ejes6$eje == 2, ]
  tarjeta <- function(a1, a2) {
    e <- escen6[escen6$eje1 == a1 & escen6$eje2 == a2, ]
    pref <- a1 == "alto" && a2 == "alto"
    div(class = paste("esc", if (pref) "preferible"),
        if (pref) span(class = "etq", "Preferible"),
        h4(e$nombre),
        div(class = "polos", if (a1 == "alto") e1$polo_alto else e1$polo_bajo, " · ",
            if (a2 == "alto") e2$polo_alto else e2$polo_bajo),
        div(texto_o_pendiente(e$narrativa)))
  }
  tagList(
    p(class = "text-muted mt-3", style = "max-width:85ch",
      "Los dos ejes son los impulsores más influyentes e inciertos (salen del MICMAC). Cada combinación es un escenario plausible para 2030. ",
      "Se describen con palabras, no con porcentajes: no hay evidencia de cuántas personas cambiaría cada uno."),
    div(class = "esc-matriz",
        div(class = "esc-eje-y", "↑ ", e1$nombre),
        tarjeta("alto", "bajo"), tarjeta("alto", "alto"),
        tarjeta("bajo", "bajo"), tarjeta("bajo", "alto"),
        div(), div(class = "esc-eje-x", e2$nombre, " →")))
}
retro_ui <- function() {
  r <- retro[order(retro$anio), ]
  tagList(
    p(class = "text-muted mt-3", style = "max-width:85ch",
      "Retroproyección: se parte del escenario preferible en 2030 y se pregunta qué tiene que haber pasado antes, año por año. Cada hito se conecta con la teoría del cambio y con el piloto."),
    tags$ol(class = "retro", lapply(seq_len(nrow(r)), function(i)
      tags$li(div(class = "anio", r$anio[i]), div(texto_o_pendiente(r$hito[i])),
              div(class = "como", "Cómo lo sabríamos: ", texto_o_pendiente(r$como_se_sabe[i]))))))
}
senales_ui <- function() {
  tagList(
    p(class = "text-muted mt-3", style = "max-width:85ch",
      "Indicadores que, año con año, dirían hacia qué escenario vamos. Se miden con la ENDUTIH, así que cualquiera puede seguirlos."),
    tags$table(class = "tabla-senales",
      tags$thead(tags$tr(tags$th("Señal"), tags$th("Indicador"), tags$th("Línea base"), tags$th("Apunta a"))),
      tags$tbody(lapply(seq_len(nrow(senales)), function(i)
        tags$tr(tags$td(texto_o_pendiente(senales$senal[i])), tags$td(texto_o_pendiente(senales$indicador[i])),
                tags$td(senales$linea_base[i]), tags$td(senales$apunta_a[i]))))))
}

cabecera <- function(num, afirmacion, apoyo) {
  div(class = "cabecera", div(class = "num", num),
      div(h2(afirmacion), p(apoyo)))
}

filtro <- function(id, opciones, rotulo = "Ver por") {
  div(class = "filtros", span(class = "rotulo", rotulo),
      div(class = "pastillas",
          radioButtons(id, NULL, opciones, selected = opciones[1], inline = TRUE)))
}
# Los filtros muestran solo los cortes que vienen en los datos (decisión 56):
# si un frente entrega menos cortes, no aparece un botón vacío.
orden_cortes <- c("Nacional", "Escolaridad", "Edad", "Sexo", "Localidad", "Estrato", "Región")
cortes_en <- function(...) {
  presentes <- unique(unlist(lapply(list(...), function(d) d$grupo_var)))
  c(intersect(orden_cortes, presentes), setdiff(presentes, orden_cortes))
}

lo_que_encontramos <- function(seccion) {
  h <- hallazgos[hallazgos$seccion == seccion, ]
  h <- h[order(h$orden), ]
  h$cifra[is.na(h$cifra)] <- ""
  div(class = "encontramos", h3("Lo que encontramos"),
      lapply(seq_len(nrow(h)), function(i) {
        pendiente <- grepl("^Pendiente", h$texto[i])
        div(class = paste("hallazgo", if (pendiente) "pendiente-texto"),
            div(class = "cifra", h$cifra[i]), div(class = "texto", h$texto[i]))
      }))
}

seccion <- function(...) div(class = "contenedor", ...)

# --- Interfaz ----------------------------------------------------------------
ui <- page_navbar(
  id = "nav",
  title = "Comprar en línea en México",
  theme = tema,
  fillable = FALSE,
  window_title = "Comprar en línea en México (Datatón INEGI 2026)",

  nav_panel("Inicio", value = "inicio",
    div(class = "portada", div(class = "portada-interior",
      p(class = "contexto",
        "Diagnóstico con datos oficiales del INEGI (ENDUTIH 2025) sobre los adultos que ya usan internet, y una propuesta para que más de ellos compren en línea."),
      h1("Una brecha, dos barreras"),
      p(class = "contexto mb-0",
        "Entre quienes ya están conectados, comprar en línea depende sobre todo de la escolaridad, la edad y el territorio. Y quienes no compran no lo hacen por la misma razón."),
      div(class = "barreras",
        div(class = "barrera no-pueden",
          h3("No saben cómo"),
          div(class = "quienes", "Sube con la edad y baja con la escolaridad."),
          div(class = "cifra", "43 %"),
          div(class = "cifra-nota", "de quienes tienen 65+ y primaria o menos, y no compran, dicen «no sé cómo». Entre los de 18-24, 21 %.")),
        div(class = "barrera no-quieren",
          h3("Prefieren en persona"),
          div(class = "quienes", "Más común en el estrato alto; ligada a la confianza."),
          div(class = "cifra", "52 %"),
          div(class = "cifra-nota", "de quienes no compran en el estrato alto prefieren hacerlo en persona. En el estrato bajo, 36 %."))),
      div(class = "datos-clave",
        div(class = "dato", div(class = "valor", "43.7 %"), div(class = "etiqueta", "de los usuarios de internet de 18+ compró en línea")),
        div(class = "dato", div(class = "valor", "+42.5 pp"), div(class = "etiqueta", "entre educación superior y primaria, con controles")),
        div(class = "dato", div(class = "valor", "11.3 millones"), div(class = "etiqueta", "de usuarios con primaria o menos no compran")),
        div(class = "dato", div(class = "valor", "1–4 %"), div(class = "etiqueta", "dice que no compra por no tener tarjeta"))))),
    seccion(
      h2("Recorrido", class = "h4 fw-bold mb-2"),
      div(class = "recorrido",
        lapply(list(
          c("1", "s1", "Panorama", "Del acceso a la compra, dónde se pierde la gente"),
          c("2", "s2", "¿Quién queda fuera?", "La brecha es de escolaridad, edad y territorio"),
          c("3", "s3", "¿Por qué?", "Unos no pueden y otros no quieren"),
          c("4", "s4", "¿Dónde?", "El territorio pesa aun con controles"),
          c("5", "s5", "¿Qué hacer?", "La tienda como puerta de entrada"),
          c("6", "s6", "¿Hacia dónde vamos?", "Prospectiva 2030: qué pasa si no hacemos nada")), function(x)
          tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('ir', '%s', {priority: 'event'}); return false;", x[2]),
                 span(class = "num", x[1]),
                 span(span(class = "titulo-paso", x[3]), br(), span(class = "afirm", x[4])),
                 span(class = "ir", "Ver sección")))),
      p(class = "text-muted small mt-3",
        "Cifras del diagnóstico preliminar; se actualizarán con las finales. Brechas con controles; intervalos al 90 %."))),

  nav_panel("1. Panorama", value = "s1", seccion(
    cabecera("1", "Del acceso a la compra, la gente se pierde en eslabones distintos",
             "Afirmación provisional. Frente A. Preguntas P1 y P3."),
    filtro("s1_grupo", cortes_en(embudo, categorias)),
    div(class = "lienzo", navset_underline(
      nav_panel("Embudo", grafica("g_embudo", "560px", "s1_embudo", "s1_embudo.csv")),
      nav_panel("¿Conectarse o comprar?", grafica("g_eslabon", "440px", "s1_brechas_eslabon", "s1_brechas_eslabon.csv")),
      nav_panel("¿Internet en casa?", opcional(conexion, "g_conexion", "520px", "s1_hogar", "s1_hogar.csv",
                                             "Frente A: adultos en hogar con internet vs. que usan internet.")),
      nav_panel("¿Qué se compra?", grafica("g_categorias", "520px", "s1_categorias", "s1_categorias.csv")),
      nav_panel("¿Cómo ha cambiado?",
        p(class = "text-muted small mt-2 mb-0",
          "Nota: el cambio en las categorías a lo largo de los años solo está disponible a nivel nacional; esta gráfica no cambia con el filtro de arriba."),
        opcional(tendencia, "g_tendencia", "380px", "s1_tendencia", "s1_tendencia.csv",
                                             "Frente A: tendencia de las categorías con los tabulados publicados.")))),
    lo_que_encontramos("s1"))),

  nav_panel("2. ¿Quién queda fuera?", value = "s2", seccion(
    cabecera("2", "La brecha es de escolaridad, edad y territorio",
             "Otras brechas se explican en buena parte por escolaridad. Frentes A y B. Pregunta P2."),
    div(class = "como-leer",
      strong("Cómo leer estas cifras. "),
      "Usamos una regresión: compara a personas que son iguales en todo (edad, sexo, escolaridad, localidad, estrato) excepto en una característica. ",
      "Así, «Superior: +42.5 pp» quiere decir que, entre dos personas idénticas en todo lo demás, la que tiene educación superior tiene 42.5 puntos porcentuales más de probabilidad de comprar en línea que la que tiene primaria."),
    div(class = "lienzo", navset_underline(
      nav_panel("Arma un perfil", arma_un_perfil()),
      nav_panel("¿Cuánto pesa cada factor?",
        grafica("g_efectos", "720px", "s2_efectos", "s2_efectos.csv")),
      nav_panel("La regresión paso a paso",
        grafica("g_bloques", "680px", "s2_bloques", "s2_bloques.csv")),
      nav_panel("¿Se sostiene en las ciudades?",
        grafica("g_robustez", "520px", "s2_robustez", "s2_robustez.csv")),
      nav_panel("Composición del hogar",
        opcional(hogar, "g_hogar", "420px", "s2_hogar", "s2_hogar.csv",
                 "Frente A: compra según la composición del hogar, por sexo.")))),
    lo_que_encontramos("s2"))),

  nav_panel("3. ¿Por qué?", value = "s3", seccion(
    cabecera("3", "Unos no pueden (habilidades) y otros no quieren (prefieren lo presencial)",
             "Los medios de pago pesan poco. Frente B. Preguntas P4 y P5."),
    filtro("s3_grupo", cortes_en(razones, problemas)),
    div(class = "lienzo", navset_underline(
      nav_panel("Por qué no compran", grafica("g_razones", "500px", "s3_razones", "s3_razones.csv")),
      nav_panel("Problemas de quienes sí compran", grafica("g_problemas", "460px", "s3_problemas", "s3_problemas.csv")),
      nav_panel("Mujeres y hombres", grafica("g_genero", "380px", "s3_genero", "s3_genero.csv")))),
    lo_que_encontramos("s3"))),

  nav_panel("4. ¿Dónde?", value = "s4", seccion(
    cabecera("4", "El territorio pesa aun con controles",
             "Afirmación provisional. Frente C. Pregunta P6."),
    div(class = "lienzo", navset_underline(
      nav_panel("Mapa y regiones",
        # Mapa por entidad a la izquierda; a la derecha, el mapa de las regiones
        # con sus colores y debajo la brecha por región con los mismos colores
        layout_columns(col_widths = c(6, 6),
          tagList(plotOutput("g_mapa", height = "500px",
                             hover = hoverOpts("mapa_hover", delay = 80, delayType = "throttle")),
                  div(class = "detalle-mapa small", uiOutput("mapa_detalle")),
                  pie_grafica("s4_entidades", "s4_entidades.csv")),
          tagList(plotOutput("g_mapa_regiones", height = "340px"),
                  grafica("g_regiones", "460px", "s4_regiones", "s4_regiones.csv")))),
      # Pestañas opcionales: solo aparecen si el archivo existe (decisión 87)
      if (!is.null(ritmo)) nav_panel("Ritmo de adopción",
        opcional(ritmo, "g_ritmo", "780px", "s4_ritmo", "s4_ritmo.csv",
                 "Frente C: cambio en el % que compra por entidad entre dos levantamientos.")),
      if (!is.null(comercios)) nav_panel("¿Por qué?",
        opcional(comercios, "g_comercios", "480px", "s4_comercios", "s4_comercios.csv",
                 "Frente C: comercios al por menor por habitante (DENUE) frente a la compra en línea.")))),
    lo_que_encontramos("s4"))),

  nav_panel("5. ¿Qué hacer?", value = "s5", seccion(
    cabecera("5", "La tienda como puerta de entrada a la compra en línea",
             "Propuesta en borrador. Frentes D y C. Preguntas P7 y P8."),
    div(class = "lienzo", navset_underline(
      nav_panel("La propuesta",
        nucleo_propuesta(),
        div(class = "row g-4 mt-1",
          div(class = "col-lg-7",
            tags$link(rel = "stylesheet", href = "propuesta.css"),
            tags$dl(class = "ficha", lapply(seq_len(nrow(propuesta)), function(i)
              tagList(tags$dt(propuesta$campo[i]), tags$dd(campo_propuesta(propuesta[i, ])))))),
          div(class = "col-lg-5", lo_que_encontramos("s5")))),
      nav_panel("Estrategias públicas y privadas",
        div(class = "estrategias mt-2",
          lapply(c("Pública", "Privada"), function(t) {
            e <- estrategias[estrategias$tipo == t, ]
            div(class = if (t == "Pública") "publica" else "privada",
                h4(if (t == "Pública") "Desde lo público" else "Desde lo privado"),
                div(class = "nota", "Qué haría cada actor y por qué le conviene. Hipótesis por validar."),
                lapply(seq_len(nrow(e)), function(i)
                  div(class = "estrategia", div(class = "actor", e$actor[i]),
                      div(e$accion[i]), div(class = "porque", e$por_que[i]))))
          }))),
      nav_panel("Lo que está en juego",
        p(class = "text-muted mt-2 mb-3", style = "max-width:85ch",
          strong("No predecimos cuántas personas empezarán a comprar: eso lo mide el piloto. "),
          "Esta calculadora dice qué está en juego en cada puerta y qué tendría que mostrar el piloto para alcanzar una meta. ",
          "Las cifras de personas son de la ENDUTIH 2025; la meta la eliges tú (decisión 76)."),
        layout_columns(col_widths = c(4, 8),
          div(
            selectInput("ej_grupo", "Grupo", simul$grupo),
            sliderInput("ej_meta", "Meta: personas nuevas que compran en línea (millones)",
                        min = 0.25, max = 5, value = 1, step = 0.25, post = " M"),
            div(class = "filtros mb-2", span(class = "rotulo", "Por qué puerta")),
            div(class = "pastillas mb-3",
                radioButtons("ej_puerta", NULL, c("Las dos", "Solo puerta 1", "Solo puerta 2"), inline = TRUE)),
            uiOutput("ej_base"),
            p(class = "small text-muted",
              "Puerta 1: quienes no compran y dicen «no sé cómo». Puerta 2: quienes prefieren comprar en persona y no dicen «no sé cómo» (quien da las dos razones cuenta solo en la puerta 1). ",
              "«Las dos» supone la misma conversión en ambas puertas.")),
          div(uiOutput("ej_kpis"),
            plotOutput("g_en_juego", height = "380px"),
            pie_grafica("s5_simulador", "s5_simulador.csv")))),
      if (!is.null(mercado)) nav_panel("Tamaño del mercado",
        opcional(mercado, "g_mercado", "340px", "s5_mercado", "s5_mercado.csv",
                 "Frente C: rango de gasto de los hogares que podría hacerse en línea (ENIGH).")))))),

  nav_panel("6. 2030", value = "s6", seccion(
    cabecera("6", "2030: qué pasa si no hacemos nada, y qué tendría que pasar",
             "Análisis prospectivo. Frentes C, B y D. Pregunta P8."),
    div(class = "como-leer",
      strong("Cómo leer esta sección. "),
      "No es un pronóstico. Usamos el cono de futuros: el futuro probable (si nada cambia) lleva cifras; los futuros plausibles se construyen con los impulsores más influyentes e inciertos y se describen con palabras; ",
      "el preferible es al que apunta la propuesta, y la retroproyección dice qué tiene que pasar para llegar. Las señales tempranas dicen, cada año, hacia dónde vamos."),
    div(class = "lienzo", navset_underline(
      nav_panel("Cono de futuros", grafica("g_cono", "460px", "s6_cono", "s6_cono.csv")),
      nav_panel("Impulsores (MICMAC)", grafica("g_micmac", "540px", "s6_impulsores", "s6_impulsores.csv")),
      nav_panel("Escenarios", escenarios_ui(), pie_grafica("s6_escenarios", "s6_escenarios.csv")),
      nav_panel("Del 2030 a hoy", retro_ui(), pie_grafica("s6_retroproyeccion", "s6_retroproyeccion.csv")),
      nav_panel("Señales tempranas", senales_ui(), pie_grafica("s6_senales", "s6_senales.csv")))),
    lo_que_encontramos("s6"))),

  nav_spacer(),
  nav_item(if (hay_prueba) span(class = "insignia-prueba", title = "Las gráficas usan cifras inventadas; las de «Lo que encontramos» son del diagnóstico preliminar.", "Datos de prueba")),
  nav_item(tags$a(icon("github"), "Código", href = "https://github.com/rebeloppar/dataton-inegi-2026",
                  target = "_blank", class = "nav-link"))
)

# --- Servidor ----------------------------------------------------------------
server <- function(input, output, session) {

  observeEvent(input$ir, nav_select("nav", input$ir))

  # Descargas: un enlace por archivo de datos
  for (archivo in list.files(dir_datos, pattern = "^s[0-9].*\\.csv$")) local({
    a <- archivo
    output[[paste0("dl_", sub("\\.csv$", "", a))]] <- downloadHandler(
      filename = a, content = function(f) file.copy(ruta(a), f))
  })

  # renderPlot con resolución alta. Se pasa la expresión sin evaluar (quoted) para
  # que la gráfica se vuelva a dibujar cuando cambian los filtros
  graficar <- function(expr) {
    renderPlot(substitute(expr), quoted = TRUE, env = parent.frame(), res = 110)
  }

  # Si un archivo no trae el corte elegido (p. ej., P5 con grupos más amplios),
  # la gráfica lo dice en lugar de romperse
  por_grupo <- function(d, g) {
    validate(need(g %in% d$grupo_var,
                  paste0("Esta gráfica no tiene el corte «", g, "». Elige otro.")))
    d <- d[d$grupo_var == g, ]
    d$grupo <- factor(d$grupo, unique(d$grupo[order(d$orden)]))
    d
  }

  # Brechas en barras: a la izquierda del 0 es desventaja, a la derecha ventaja.
  # 'grupo' distingue dos versiones de la misma brecha (p. ej., observada y con controles).
  barras_brecha <- function(d, y, grupo, valores, titulo, subtitulo, nota) {
    d$valor_txt <- pp(d$estimacion)
    # El primer nivel va arriba en cada par (ggplot apila al revés en barras horizontales)
    d[[grupo]] <- factor(d[[grupo]], rev(levels(factor(d[[grupo]]))))
    lim <- max(abs(c(d$estimacion, d$li90, d$ls90)), na.rm = TRUE) * 1.25
    ggplot(d, aes(estimacion, .data[[y]], fill = .data[[grupo]])) +
      annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = col$calido, alpha = 0.6) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, fill = col$frio) +
      geom_col(position = position_dodge(width = 0.86), width = 0.82) +
      geom_vline(xintercept = 0, colour = col$tinta, linewidth = 0.5) +
      geom_text(aes(label = valor_txt, hjust = ifelse(estimacion < 0, 1.15, -0.15)),
                position = position_dodge(width = 0.86), size = 3.6, colour = col$tinta) +
      scale_fill_manual(values = valores, name = NULL, guide = guide_legend(reverse = TRUE)) +
      scale_x_continuous(limits = c(-lim, lim), labels = pp) +
      labs(title = titulo, subtitle = subtitulo, caption = nota,
           x = "Puntos porcentuales (pp)", y = NULL) +
      theme(panel.grid.major.y = element_blank(), legend.position = "top")
  }

  calor <- function(d, fila, titulo, subtitulo, nota) {
    prom <- tapply(d$estimacion, d[[fila]], mean)
    d[[fila]] <- factor(d[[fila]], names(sort(prom)))
    ggplot(d, aes(grupo, .data[[fila]], fill = estimacion)) +
      geom_tile(colour = "white", linewidth = 2) +
      geom_text(aes(label = sprintf("%.0f", estimacion)), colour = col$tinta, size = 4.2, fontface = "bold") +
      scale_fill_gradientn(colours = rampa, name = "%", limits = c(0, NA)) +
      scale_x_discrete(position = "top") +
      labs(title = titulo, subtitle = subtitulo, caption = nota, x = NULL, y = NULL) +
      theme(panel.grid.major = element_blank(), panel.border = element_blank(), legend.position = "right",
            axis.text.x = element_text(colour = col$tinta), axis.text.y = element_text(colour = col$tinta))
  }

  # Dispersión entre entidades, con abreviaturas
  dispersion <- function(d, x, xlab, titulo, subtitulo, nota) {
    d <- merge(d, ent_base, by = "cve_ent")
    ggplot(d, aes(.data[[x]], pct_compra)) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = col$gris_claro,
                  linewidth = 0.7, linetype = "dashed") +
      punto(fill = col$azul, size = 4) +
      geom_text(aes(label = abrev), size = 2.8, colour = col$gris, vjust = -1.3) +
      scale_y_continuous(labels = pct, expand = expansion(mult = c(0.05, 0.12))) +
      labs(title = titulo, subtitle = subtitulo, caption = nota, x = xlab, y = "% que compra en línea")
  }

  # 1. Panorama ---------------------------------------------------------------
  output$g_embudo <- graficar({
    d <- por_grupo(embudo, input$s1_grupo)
    pasos <- c("Hogar con internet", "Usa internet", "Compra en línea")
    d$paso <- match(d$eslabon, pasos)
    pasos <- pasos[pasos %in% d$eslabon]           # funciona con 2 o 3 eslabones
    d$paso <- match(d$eslabon, pasos)
    poli <- do.call(rbind, lapply(split(d, d$grupo), function(g) {
      if (nrow(g) == 0) return(NULL)
      g <- g[order(g$paso), ]
      do.call(rbind, lapply(seq_len(nrow(g)), function(k) {
        arriba <- g$estimacion[k] / 2
        abajo  <- if (k < nrow(g)) g$estimacion[k + 1] / 2 else g$estimacion[k] / 2 * 0.82
        y1 <- -(k - 1); y2 <- -k + 0.08
        data.frame(grupo = g$grupo[k], eslabon = g$eslabon[k], id = paste(g$grupo[k], k),
                   x = c(-arriba, arriba, abajo, -abajo), y = c(y1, y1, y2, y2))
      }))
    }))
    etiquetas <- transform(d, y = -(paso - 0.5))
    poli$eslabon <- factor(poli$eslabon, pasos)
    ggplot() +
      geom_polygon(data = poli, aes(x, y, group = id, fill = eslabon), colour = "white", linewidth = 1) +
      geom_text(data = etiquetas, aes(0, y + 0.1, label = sprintf("%.0f %%", estimacion)),
                colour = col$tinta, fontface = "bold", size = 4.6) +
      geom_text(data = etiquetas, aes(0, y - 0.2, label = sprintf("%.0f–%.0f", li90, ls90)),
                colour = col$gris, size = 2.9) +
      facet_wrap(~grupo) +
      scale_fill_manual(values = setNames(c(rampa[3], rampa[4], rampa[5]),
                                          c("Hogar con internet", "Usa internet", "Compra en línea")),
                        name = NULL, drop = TRUE) +
      coord_cartesian(xlim = c(-52, 52)) +
      labs(title = "De conectarse a comprar en línea",
           subtitle = paste0("% en cada eslabón, ", if (input$s1_grupo == "Nacional") "a nivel nacional" else paste("por", tolower(input$s1_grupo)),
                             ". Debajo de cada cifra, su intervalo al 90 %."),
           caption = "Todos los eslabones en % de la población de 18+ del grupo: el ancho del embudo es comparable de arriba abajo.",
           x = NULL, y = NULL) + sin_ejes
  })

  output$g_eslabon <- graficar({
    d <- eslabon
    d$eslabon <- factor(d$eslabon, c("Usar internet", "Comprar en línea"))
    barras_brecha(d, "comparacion", "eslabon", c(`Usar internet` = col$morado, `Comprar en línea` = col$azul),
          "¿Se pierde más gente al conectarse o al comprar?",
          "Diferencia con controles en la probabilidad de usar internet y de comprar en línea. No depende del filtro.",
          "Usar internet: población de 18+. Comprar: usuarios de internet de 18+. Líneas: intervalo al 90 %.")
  })

  # Tener internet en casa vs. usarlo: para 65+ la conexión está, el uso no
  output$g_conexion <- graficar({
    d <- por_grupo(conexion, input$s1_grupo)
    d$grupo <- factor(d$grupo, rev(levels(d$grupo)))
    d$medida <- factor(d$medida, c("Usa internet", "Vive en un hogar con internet"))
    ggplot(d, aes(estimacion, grupo, fill = medida)) +
      geom_col(position = position_dodge(width = 0.75), width = 0.7) +
      geom_errorbar(aes(xmin = li90, xmax = ls90), position = position_dodge(width = 0.75),
                    width = 0.25, orientation = "y", colour = col$tinta, linewidth = 0.4) +
      geom_text(aes(x = ls90, label = sprintf("%.0f %%", estimacion)), position = position_dodge(width = 0.75),
                hjust = -0.25, size = 3.6, colour = col$tinta) +
      scale_fill_manual(values = c(`Vive en un hogar con internet` = col$verde, `Usa internet` = col$azul),
                        breaks = c("Vive en un hogar con internet", "Usa internet"), name = NULL) +
      scale_x_continuous(limits = c(0, 108), breaks = seq(0, 100, 25), labels = pct, expand = c(0, 0)) +
      labs(title = "¿Tener internet en casa basta para usarlo?",
           subtitle = paste0("% de adultos (18+)", if (input$s1_grupo == "Nacional") "" else paste0(", por ", tolower(input$s1_grupo)),
                             ". Si la barra verde es más larga, la conexión está pero no se usa."),
           caption = "Una persona puede usar internet sin tenerlo en casa (datos móviles, trabajo, escuela): por eso la barra azul puede ser más larga.\nLíneas: intervalo al 90 %.",
           x = NULL, y = NULL) +
      theme(panel.grid.major.y = element_blank(), legend.position = "top",
            axis.text.y = element_text(colour = col$tinta, size = 11.5))
  })

  output$g_categorias <- graficar(calor(
    por_grupo(categorias, input$s1_grupo), "categoria", "¿Qué se compra en línea?",
    paste0("% de quienes compraron que compró cada categoría", if (input$s1_grupo == "Nacional") "" else paste0(", por ", tolower(input$s1_grupo))),
    "Respuesta múltiple: los porcentajes no suman 100."))

  output$g_tendencia <- graficar({
    d <- tendencia
    final <- d[d$anio == max(d$anio), ]
    ggplot(d, aes(anio, estimacion)) +
      geom_line(colour = col$azul, linewidth = 1) +
      punto(fill = col$azul, size = 2.8) +
      geom_text(data = final, aes(label = sprintf("%.0f %%", estimacion)), colour = col$tinta,
                fontface = "bold", size = 3.5, vjust = -1.2) +
      facet_wrap(~categoria, nrow = 1, labeller = label_wrap_gen(16)) +
      scale_y_continuous(labels = pct, expand = expansion(mult = c(0.05, 0.2))) +
      scale_x_continuous(breaks = range(d$anio), expand = expansion(mult = c(0.08, 0.18))) +
      coord_cartesian(clip = "off") +
      labs(title = "¿Cómo ha cambiado lo que se compra?",
           subtitle = "% de compradores en línea que compró cada categoría, por año. Solo a nivel nacional.",
           caption = "Cifras publicadas por el INEGI (ENDUTIH 2022 a 2025), usuarios de 6+ que compraron. Solo las categorías que aparecen los cuatro años.", x = NULL, y = NULL) +
      theme(panel.spacing = unit(1.2, "lines"))
  })

  # 2. ¿Quién queda fuera? ----------------------------------------------------
  output$g_efectos <- graficar({
    d <- efectos
    d$factor <- factor(d$factor, unique(d$factor[order(d$orden_factor)]))
    d <- d[order(d$orden_factor, d$estimacion), ]
    d$etiqueta <- factor(d$etiqueta, unique(d$etiqueta))
    d$signo <- ifelse(d$estimacion >= 0, "Más probabilidad", "Menos probabilidad")
    lim <- max(abs(c(d$li90, d$ls90))) * 1.18
    ggplot(d, aes(estimacion, etiqueta)) +
      geom_vline(xintercept = 0, colour = col$tinta, linewidth = 0.5) +
      geom_col(aes(fill = signo), width = 0.68) +
      geom_errorbar(aes(xmin = li90, xmax = ls90), width = 0.25, orientation = "y", colour = col$gris, linewidth = 0.45) +
      geom_text(aes(x = ifelse(estimacion < 0, li90, ls90), label = pp(estimacion),
                    hjust = ifelse(estimacion < 0, 1.2, -0.2)), size = 3.6, colour = col$tinta, fontface = "bold") +
      facet_grid(factor ~ ., scales = "free_y", space = "free_y", switch = "y") +
      scale_fill_manual(values = c(`Más probabilidad` = col$azul, `Menos probabilidad` = "#E2928C"), name = NULL) +
      scale_x_continuous(limits = c(-lim, lim), labels = pp) +
      labs(title = "¿Cuánto pesa cada factor en la probabilidad de comprar en línea?",
           subtitle = "Diferencia frente al grupo de referencia de cada factor, comparando personas iguales en todo lo demás",
           caption = "Modelo completo (logit con diseño muestral), efectos marginales promedio en puntos porcentuales.\nLíneas: intervalo al 90 %. Si la línea cruza el 0, la diferencia no es clara.",
           x = "Puntos porcentuales (pp)", y = NULL) +
      theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold", size = 10),
            panel.grid.major.y = element_blank(), panel.spacing.y = unit(0.35, "lines"), legend.position = "top")
  })

  output$g_bloques <- graficar({
    d <- bloques
    orden <- d[d$paso == 1, ]; d$brecha <- factor(d$brecha, orden$brecha[order(orden$estimacion)])
    pasos <- unique(d[order(d$paso), c("paso", "nombre_paso")])
    ggplot(d, aes(paso, estimacion)) +
      geom_hline(yintercept = 0, colour = col$tinta, linewidth = 0.5) +
      geom_ribbon(aes(ymin = li90, ymax = ls90), fill = rampa[5], alpha = 0.35) +
      geom_line(colour = col$azul, linewidth = 1.1) +
      punto(fill = col$azul, size = 4) +
      geom_text(aes(label = pp(estimacion)), vjust = -1.1, size = 3.5, colour = col$tinta, fontface = "bold") +
      facet_wrap(~brecha, scales = "free_y", nrow = 2) +
      scale_x_continuous(breaks = pasos$paso, labels = sub("Tamaño de ", "Tamaño de\n", pasos$nombre_paso),
                         expand = expansion(add = 0.35)) +
      scale_y_continuous(labels = pp, expand = expansion(mult = c(0.15, 0.25))) +
      expand_limits(y = 0) +
      labs(title = "La regresión paso a paso: ¿qué explica cada brecha?",
           subtitle = "Agregamos una explicación a la vez. Si la línea sube hacia el 0, ese paso explica parte de la brecha.",
           caption = "Lectura: la brecha indígena pasa de −12.9 a −5.5 pp al agregar escolaridad, y a −1.9 con todo; se explica casi toda.\nLa de 65+ apenas cambia: la edad pesa por sí misma. Banda: intervalo al 90 %.",
           x = NULL, y = "Diferencia en puntos porcentuales") +
      theme(axis.text.x = element_text(size = 9.5, lineheight = 0.9), panel.spacing = unit(1.2, "lines"))
  })

  output$g_robustez <- graficar({
    d <- robustez
    d$version <- factor(d$version, c("Todo el país", "Solo ciudades de 100 mil+"))
    o <- d[d$version == "Todo el país", ]; d$etiqueta <- factor(d$etiqueta, o$etiqueta[order(o$estimacion)])
    d$li90 <- d$estimacion; d$ls90 <- d$estimacion
    barras_brecha(d, "etiqueta", "version",
                  c(`Todo el país` = col$gris_claro, `Solo ciudades de 100 mil+` = col$azul),
                  "¿Los resultados se sostienen si solo vemos ciudades?",
                  "Mismo modelo completo, en todo el país y solo en ciudades de 100 mil habitantes o más",
                  "En ciudades el estrato no se confunde con lo rural. El estrato baja (17.7 a 14.5 pp): parte de su efecto era ruralidad.\nEscolaridad, edad y género casi no cambian.")
  })

  output$g_hogar <- graficar({
    d <- hogar
    d$composicion <- factor(d$composicion, unique(d$composicion[order(d$orden)]))
    levels(d$composicion) <- gsub("^(Con otras personas,) ", "\\1\n", levels(d$composicion))
    levels(d$composicion) <- gsub("^(Con al menos un joven) ", "\\1\n", levels(d$composicion))
    ggplot(d, aes(estimacion, composicion)) +
      geom_line(aes(group = composicion), colour = col$borde, linewidth = 1.6) +
      geom_errorbar(aes(xmin = li90, xmax = ls90, colour = sexo), width = 0.15, orientation = "y", show.legend = FALSE) +
      punto(aes(fill = sexo), size = 5.5) +
      scale_fill_manual(values = c(Hombre = col$azul, Mujer = col$morado), name = NULL) +
      scale_colour_manual(values = c(Hombre = col$azul, Mujer = col$morado), guide = "none") +
      scale_x_continuous(labels = pct) +
      labs(title = "¿Vivir con jóvenes ayuda a comprar en línea?",
           subtitle = "% que compra en línea según la composición del hogar, por sexo",
           caption = "Usuarios de internet de 35+ (para quien tiene 18-34, el joven es la persona misma). Líneas: intervalo al 90 %.\nCon controles, vivir con un joven que usa internet no se asocia con comprar más (−0.3 pp; IC 90 %: −1.6 a 1.1).", x = NULL, y = NULL) +
      theme(panel.grid.major.y = element_blank())
  })

  # 3. ¿Por qué? --------------------------------------------------------------
  output$g_razones <- graficar(calor(
    por_grupo(razones, input$s3_grupo), "razon", "¿Por qué no compran en línea?",
    paste0("% de quienes no compraron que da cada razón, por ", tolower(input$s3_grupo)),
    "Respuesta múltiple: los porcentajes no suman 100. 87 % da una sola razón."))

  output$g_genero <- graficar({
    d <- genero
    d$grupo <- factor(d$grupo, rev(d$grupo[order(d$orden)]))
    ggplot(d, aes(estimacion, grupo)) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, fill = col$frio) +
      geom_vline(xintercept = 0, colour = col$tinta, linewidth = 0.5) +
      geom_errorbar(aes(xmin = li90, xmax = ls90), width = 0.18, orientation = "y", colour = col$morado, linewidth = 0.8) +
      punto(fill = col$morado, size = 6) +
      geom_text(aes(label = paste0(pp(estimacion), " pp")), vjust = -1.5, size = 3.8,
                colour = col$tinta, fontface = "bold") +
      scale_x_continuous(limits = c(-1, 9), labels = pp, breaks = seq(0, 8, 2)) +
      labs(title = "¿Las mujeres dicen más «no sé cómo»? Sí, a cualquier edad",
           subtitle = "Diferencia mujeres − hombres en el % de quienes no compran que dice «no sé cómo»,\ncomparando personas iguales en todo lo demás",
           caption = "La brecha en 55+ no es claramente mayor que en 18-54 (diferencia +2.3 pp; IC 90 %: −0.3 a 4.9).\nLíneas: intervalo al 90 %.",
           x = "Puntos porcentuales (pp)", y = NULL) +
      theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = col$tinta, size = 11.5))
  })

  output$g_problemas <- graficar(calor(
    por_grupo(problemas, input$s3_grupo), "problema", "Problemas de quienes sí compran",
    paste0("% de quienes compraron en línea que tuvo cada problema, por ", tolower(input$s3_grupo)),
    "Los tipos de problema son de respuesta múltiple."))

  # 4. ¿Dónde? ----------------------------------------------------------------
  output$g_mapa <- graficar({
    d <- merge(mapa, entidades, by = "cve_ent")
    p <- ggplot(d, aes(long, lat, group = pieza))
    p <- p + geom_polygon(aes(fill = estimacion), colour = "#A8AEB8", linewidth = 0.25) +
      # Invertida: el rezago (poca compra) es lo que se resalta en salmón
      scale_fill_gradientn(colours = rev(rampa), name = "% que compra",
                           guide = guide_colourbar(barwidth = 12, barheight = 0.6))
    p + coord_quickmap(xlim = c(-117.5, -86.5), ylim = c(14.4, 32.8), expand = FALSE) +
      labs(title = "Compra en línea por entidad",
           subtitle = "% de usuarios de internet de 18+ que compró en línea",
           caption = "Más salmón = menos compra.", x = NULL, y = NULL) +
      sin_ejes
  })

  # Las cinco regiones del CEEY (decisión 62), con los colores de la brecha por región
  output$g_mapa_regiones <- graficar({
    d <- merge(mapa, entidades, by = "cve_ent")
    cr <- aggregate(cbind(long_c, lat_c) ~ region, entidades, mean)
    # Centro y Centro-norte quedan muy juntas: se separan sus rótulos
    cr$long_c <- cr$long_c + c(`Centro-norte` = -1.8, Centro = 1.2)[cr$region] %|% 0
    cr$lat_c  <- cr$lat_c  + c(`Centro-norte` = 0.3, Centro = -0.6)[cr$region] %|% 0
    ggplot(d, aes(long, lat, group = pieza)) +
      geom_polygon(aes(fill = region), colour = "white", linewidth = 0.35) +
      scale_fill_manual(values = col_region, guide = "none") +
      geom_label(data = cr, aes(long_c, lat_c, label = region), inherit.aes = FALSE,
                 size = 3, fontface = "bold", colour = col$tinta, fill = "white",
                 label.size = 0, alpha = 0.85, label.padding = unit(0.12, "lines")) +
      coord_quickmap(xlim = c(-117.5, -86.5), ylim = c(14.4, 32.8), expand = FALSE) +
      labs(title = "Las cinco regiones", subtitle = "Regionalización del CEEY (ESRU-EMOVI 2017)", x = NULL, y = NULL) +
      sin_ejes
  })

  output$mapa_detalle <- renderUI({
    h <- input$mapa_hover
    vacio <- span(class = "text-muted", "Pasa el cursor sobre una entidad para ver su cifra.")
    if (is.null(h)) return(vacio)
    dist <- (entidades$long_c - h$x)^2 + (entidades$lat_c - h$y)^2
    if (min(dist) > 4) return(vacio)
    e <- entidades[which.min(dist), ]
    span(strong(e$entidad), sprintf(": %.1f %% compra en línea (intervalo al 90 %%: %.1f a %.1f). Región %s.",
                                   e$estimacion, e$li90, e$ls90, e$region))
  })

  output$g_regiones <- graficar({
    d <- regiones
    d$etiqueta <- factor(d$region, rev(unique(d$region)))
    d$tipo <- factor(ifelse(d$tipo == "Bruta", "Observada", "Con controles"), c("Observada", "Con controles"))
    # Cada región con su color del mapa: observada en tono claro, con controles en tono fuerte
    d$tipo <- factor(d$tipo, c("Con controles", "Observada"))   # con controles abajo en cada par
    d$valor_txt <- pp(d$estimacion)
    lim <- max(abs(c(d$estimacion, d$li90, d$ls90)), na.rm = TRUE) * 1.25
    ggplot(d, aes(estimacion, etiqueta, group = tipo)) +
      annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf, fill = col$calido, alpha = 0.6) +
      annotate("rect", xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, fill = col$frio) +
      geom_col(aes(fill = region, alpha = tipo), position = position_dodge(width = 0.86), width = 0.82) +
      geom_vline(xintercept = 0, colour = col$tinta, linewidth = 0.5) +
      geom_text(aes(label = valor_txt, hjust = ifelse(estimacion < 0, 1.15, -0.15)),
                position = position_dodge(width = 0.86), size = 3.6, colour = col$tinta) +
      scale_fill_manual(values = col_region, guide = "none") +
      scale_alpha_manual(values = c(Observada = 0.35, `Con controles` = 1), name = NULL, breaks = c("Observada", "Con controles"),
                         guide = guide_legend(override.aes = list(fill = col$gris))) +
      scale_x_continuous(limits = c(-lim, lim), labels = pp) +
      labs(title = "Brecha por región", subtitle = "% que compra: diferencia con el promedio nacional (43.7 %)",
           caption = "Tono claro: observada. Tono fuerte: con controles, comparando\npersonas iguales en escolaridad, edad, sexo, localidad, estrato,\nindígena y discapacidad.",
           x = "Puntos porcentuales (pp)", y = NULL) +
      theme(panel.grid.major.y = element_blank(), legend.position = "top")
  })

  output$g_ritmo <- graficar({
    d <- merge(ritmo, ent_base, by = "cve_ent")
    d$entidad <- factor(d$entidad, d$entidad[order(d$fin - d$inicio)])
    largo <- rbind(data.frame(entidad = d$entidad, anio = as.character(d$anio_inicio), v = d$inicio),
                   data.frame(entidad = d$entidad, anio = as.character(d$anio_fin), v = d$fin))
    ggplot(largo, aes(v, entidad)) +
      geom_line(aes(group = entidad), colour = col$borde, linewidth = 1.3) +
      punto(aes(fill = anio), size = 3.4) +
      geom_text(data = d, aes(x = fin, label = sprintf("+%.0f", fin - inicio)), hjust = -0.6,
                size = 3, colour = col$gris) +
      scale_fill_manual(values = setNames(c(col$gris_claro, col$azul), sort(unique(largo$anio))), name = NULL) +
      scale_x_continuous(labels = pct, expand = expansion(mult = c(0.02, 0.1))) +
      labs(title = "¿Qué tan rápido se adopta la compra en línea?",
           subtitle = "% que compra en línea al inicio y al final del periodo; ordenadas por avance (pp)",
           caption = "Usuarios de internet de 18+.", x = NULL, y = NULL) +
      theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = col$tinta, size = 9.5))
  })

  output$g_comercios <- graficar(dispersion(
    comercios, "comercios_10mil", "Comercios al por menor por cada 10 mil habitantes",
    "¿Se compra más en línea donde hay más comercio?", "Cada punto es una entidad",
    "Asociación entre entidades, no causalidad."))

  # 5. ¿Qué hacer? ------------------------------------------------------------
  # «Lo que está en juego» (decisión 76): personas por cada punto de conversión
  # y conversión que haría falta para una meta. Sin escenarios ni supuestos.
  juego <- reactive({
    g <- simul[simul$grupo == input$ej_grupo, ]
    no_compran <- g$millones * (1 - g$pct_compra / 100)
    p1 <- no_compran * g$pct_no_sabe / 100      # millones que llegan por la puerta 1
    p2 <- no_compran * g$pct_en_persona / 100   # millones que llegan por la puerta 2
    base <- switch(input$ej_puerta, "Las dos" = p1 + p2, "Solo puerta 1" = p1, "Solo puerta 2" = p2)
    list(g = g, no_compran = no_compran, p1 = p1, p2 = p2, base = base,
         requerida = 100 * input$ej_meta / base)
  })

  output$ej_base <- renderUI({
    j <- juego()
    p(class = "small", sprintf("En este grupo, %.1f M usan internet y no compran: %.1f M llegan por la puerta 1 y %.1f M por la puerta 2.",
                               j$no_compran, j$p1, j$p2))
  })

  output$ej_kpis <- renderUI({
    j <- juego()
    req <- if (j$requerida > 100) "No alcanza" else sprintf("%.1f %%", j$requerida)
    div(class = "kpis",
        div(class = "kpi", div(class = "etiqueta", "Por cada 1 % en la puerta 1"),
            div(class = "valor", sprintf("%s mil", format(round(j$p1 * 10), big.mark = ",")))),
        div(class = "kpi", div(class = "etiqueta", "Por cada 1 % en la puerta 2"),
            div(class = "valor", sprintf("%s mil", format(round(j$p2 * 10), big.mark = ",")))),
        div(class = "kpi destacado", div(class = "etiqueta", sprintf("Conversión que tendría que mostrar el piloto para %s M", format(input$ej_meta))),
            div(class = "valor", req)))
  })

  output$g_en_juego <- graficar({
    j <- juego()
    tope <- min(100, max(30, ceiling(j$requerida * 1.25 / 10) * 10))
    x <- seq(0, tope, length.out = 101)
    d <- rbind(data.frame(x = x, y = x / 100 * j$p1, linea = "Solo puerta 1"),
               data.frame(x = x, y = x / 100 * j$p2, linea = "Solo puerta 2"),
               data.frame(x = x, y = x / 100 * (j$p1 + j$p2), linea = "Las dos"))
    colores <- c(`Solo puerta 1` = col$verde, `Solo puerta 2` = col$ocre, `Las dos` = col$azul)
    fin <- d[d$x == tope, ]
    g <- ggplot(d, aes(x, y, colour = linea)) +
      geom_hline(yintercept = input$ej_meta, linetype = "22", colour = col$tinta) +
      annotate("text", x = 0, y = input$ej_meta, label = paste0("Meta: ", input$ej_meta, " M"),
               hjust = 0, vjust = -0.6, size = 3.6, colour = col$tinta, fontface = "bold") +
      geom_line(aes(linewidth = linea == input$ej_puerta), show.legend = FALSE) +
      scale_linewidth_manual(values = c(`TRUE` = 1.6, `FALSE` = 0.7), guide = "none") +
      geom_text(data = fin, aes(label = linea), hjust = 0, nudge_x = tope * 0.015, size = 3.6, show.legend = FALSE) +
      scale_colour_manual(values = colores, guide = "none") +
      scale_x_continuous(labels = pct, expand = expansion(mult = c(0, 0.2))) +
      scale_y_continuous(labels = function(v) paste0(v, " M")) +
      labs(title = "¿Cuántas personas nuevas, según cuántas convierta el piloto?",
           subtitle = "Millones de personas nuevas, según el % de cada puerta que empieza a comprar",
           caption = "Aritmética, no pronóstico: el % que convierte lo mide el piloto.",
           x = "% que empieza a comprar", y = NULL)
    if (j$requerida <= tope)
      g <- g + punto(data = data.frame(x = j$requerida, y = input$ej_meta, linea = input$ej_puerta),
                     aes(fill = linea), size = 5, show.legend = FALSE) +
        scale_fill_manual(values = colores, guide = "none")
    g
  })

  # 6. ¿Hacia dónde vamos? -----------------------------------------------------
  output$g_cono <- graficar({
    d <- cono
    colores <- c(Observado = col$tinta, `Observado (no comparable)` = col$gris_claro,
                 `Probable (curva S)` = col$azul, `Solo efecto demográfico` = col$gris)
    d$serie <- factor(d$serie, names(colores))
    obs <- d[grepl("^Observado", d$serie), ]
    prob <- d[d$serie == "Probable (curva S)", ]
    dem <- d[d$serie == "Solo efecto demográfico", ]
    fin <- prob[prob$anio == max(prob$anio), ]
    ultimo_obs <- max(obs$anio)
    ggplot() +
      annotate("rect", xmin = ultimo_obs, xmax = Inf, ymin = -Inf, ymax = Inf, fill = rampa[1]) +
      # El cono: entre «si el crecimiento se estanca» y el techo alto
      geom_ribbon(data = prob, aes(anio, ymin = li90, ymax = ls90), fill = col$azul, alpha = 0.15) +
      geom_line(data = prob, aes(anio, li90), colour = col$azul, linewidth = 0.4, linetype = "22") +
      geom_line(data = prob, aes(anio, ls90), colour = col$azul, linewidth = 0.4, linetype = "22") +
      geom_line(data = prob, aes(anio, estimacion), colour = col$azul, linewidth = 1.2) +
      geom_line(data = dem, aes(anio, estimacion), colour = col$gris, linewidth = 0.9, linetype = "22") +
      geom_errorbar(data = obs, aes(anio, ymin = li90, ymax = ls90, colour = serie), width = 0.2, show.legend = FALSE) +
      geom_line(data = obs[obs$serie == "Observado", ], aes(anio, estimacion), colour = col$tinta, linewidth = 1) +
      punto(data = obs, aes(anio, estimacion, fill = serie), size = 3, show.legend = FALSE) +
      geom_text(data = obs[obs$anio == ultimo_obs, ], aes(anio, estimacion, label = paste0(estimacion, " %")),
                vjust = -1.1, hjust = 1, size = 3.5, colour = col$tinta) +
      annotate("text", x = max(prob$anio) + 0.2, y = fin$estimacion, hjust = 0, size = 3.5, colour = col$azul,
               fontface = "bold", label = paste0("Probable: ", round(fin$estimacion), " %")) +
      annotate("text", x = max(prob$anio) + 0.2, y = fin$ls90 + 1.2, hjust = 0, size = 3.2, colour = col$azul,
               label = paste0("Techo alto: ", round(fin$ls90), " %")) +
      annotate("text", x = max(prob$anio) + 0.2, y = fin$li90 + 0.8, hjust = 0, size = 3.2, colour = col$azul,
               label = paste0("Si se estanca: ", round(fin$li90), " %")) +
      annotate("text", x = max(dem$anio) + 0.2, y = tail(dem$estimacion, 1) - 1.2, hjust = 0, size = 3.2, colour = col$gris,
               label = paste0("Solo demografía: ", round(tail(dem$estimacion, 1)), " %")) +
      scale_fill_manual(values = colores) + scale_colour_manual(values = colores) +
      scale_x_continuous(breaks = seq(2015, 2030, 1), expand = expansion(add = c(0.4, 4.2))) +
      scale_y_continuous(labels = pct, limits = c(0, NA)) +
      labs(title = "El futuro probable: si nada cambia",
           subtitle = "% de usuarios de internet de 18+ que compró en línea. Observado 2015-2025; cono a 2030 con curvas S de adopción",
           caption = paste("Curva S (difusión de innovaciones). Techo del probable: lo que hoy compran los usuarios con educación superior;",
                           "techo alto: los de 18-34 con superior; «si se estanca»: techo estimado solo con los datos.",
                           "2015 en gris: ese año, «usuario» era quien usó internet en 12 meses (desde 2016, en 3). Líneas: IC 90 %.", sep = "\n"),
           x = NULL, y = NULL) +
      theme(panel.grid.minor = element_blank())
  })

  output$g_micmac <- graficar({
    d <- impulsores
    cx <- mean(range(d$dependencia)); cy <- mean(range(d$influencia))
    rx <- range(d$dependencia); ry <- range(d$influencia)
    cuad <- data.frame(x = c(-Inf, Inf, -Inf, Inf), y = c(Inf, Inf, -Inf, -Inf),
                       h = c(-0.05, 1.05, -0.05, 1.05), v = c(1.6, 1.6, -0.8, -0.8),
                       etq = c("Motrices: mueven el sistema", "De enlace: inestables, clave para los escenarios",
                               "Autónomos: poco conectados", "Dependientes: resultados"))
    ggplot(d, aes(dependencia, influencia)) +
      geom_vline(xintercept = cx, colour = col$borde) + geom_hline(yintercept = cy, colour = col$borde) +
      geom_text(data = cuad, aes(x, y, label = etq, hjust = h, vjust = v), colour = col$gris, size = 3.3, fontface = "italic") +
      punto(aes(fill = categoria), size = 4) +
      ggrepel::geom_text_repel(aes(label = impulsor), size = 3.4, colour = col$tinta, seed = 1, box.padding = 0.45,
                               min.segment.length = 0.2, max.overlaps = Inf) +
      scale_fill_manual(values = c(Social = col$azul, Económico = col$ocre, Tecnológico = col$verde,
                                   Político = col$morado, Legal = col$gris, Ambiental = col$gris_claro), name = "Tipo") +
      scale_x_continuous(expand = expansion(mult = 0.12)) + scale_y_continuous(expand = expansion(mult = 0.2)) +
      labs(title = "¿Qué impulsores mueven el sistema?",
           subtitle = "Matriz de influencia y dependencia (MICMAC simplificado)",
           caption = "Los ejes de los escenarios salen de los impulsores más influyentes: motrices y de enlace.",
           x = "Dependencia (cuánto lo mueven los demás)", y = "Influencia (cuánto mueve a los demás)")
  })

  output$g_mercado <- graficar({
    d <- mercado
    d$escenario <- factor(d$escenario, rev(d$escenario[order(d$orden)]))
    ggplot(d, aes(y = escenario)) +
      geom_segment(aes(x = minimo, xend = maximo, yend = escenario), colour = rampa[5], linewidth = 9, lineend = "round") +
      geom_text(aes(x = minimo, label = minimo), hjust = 1.6, size = 3.5, colour = col$tinta) +
      geom_text(aes(x = maximo, label = maximo), hjust = -0.6, size = 3.5, colour = col$tinta) +
      scale_x_continuous(expand = expansion(mult = 0.15)) +
      labs(title = "¿Cuánto gasto podría pasar al canal digital?",
           subtitle = "Rango de gasto anual de los hogares en categorías que se compran en línea (miles de millones de pesos)",
           caption = "Gasto potencialmente digitalizable, no gasto electrónico observado.", x = NULL, y = NULL) +
      theme(panel.grid.major.y = element_blank())
  })
}

shinyApp(ui, server)
