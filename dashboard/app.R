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
perfiles   <- leer("s2_perfiles.csv")
razones    <- leer("s3_razones.csv")
problemas  <- leer("s3_problemas.csv")
genero     <- leer("s3_genero.csv")
ent_base   <- leer("entidades.csv")
entidades  <- merge(ent_base, leer("s4_entidades.csv"), by = "cve_ent")
mapa       <- leer("mapa_estados.csv")
regiones   <- leer("s4_regiones.csv")
escen      <- leer("s5_escenarios.csv")
simul      <- leer("s5_simulador.csv")
palancas   <- leer("s5_palancas.csv")
palancas   <- palancas[order(palancas$orden), ]

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
estrategias <- leer("estrategias.csv")

hay_prueba <- any(sapply(list(embudo, categorias, eslabon, efectos, bloques, robustez, perfiles, razones, genero,
                              problemas, entidades, regiones, escen, simul, palancas,
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
          --morado:#6A4C93; --durazno:#F8E2CF; }
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

  @media (max-width: 768px) {
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
      h1("No es una brecha: son dos barreras."),
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
          c("5", "s5", "¿Qué hacer?", "La tienda como puerta de entrada")), function(x)
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
      nav_panel("¿Cómo ha cambiado?", opcional(tendencia, "g_tendencia", "380px", "s1_tendencia", "s1_tendencia.csv",
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
      nav_panel("¿Cuánto pesa cada factor?",
        grafica("g_efectos", "720px", "s2_efectos", "s2_efectos.csv")),
      nav_panel("La regresión paso a paso",
        grafica("g_bloques", "680px", "s2_bloques", "s2_bloques.csv")),
      nav_panel("¿Se sostiene en las ciudades?",
        grafica("g_robustez", "520px", "s2_robustez", "s2_robustez.csv")),
      nav_panel("Perfiles",
        grafica("g_perfiles", "440px", "s2_perfiles", "s2_perfiles.csv")),
      nav_panel("Composición del hogar",
        opcional(hogar, "g_hogar", "360px", "s2_hogar", "s2_hogar.csv",
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
        filtro("s4_color", c("% que compra" = "estimacion", "Región" = "region"), "Colorear por"),
        layout_columns(col_widths = c(6, 6),
          tagList(plotOutput("g_mapa", height = "500px",
                             hover = hoverOpts("mapa_hover", delay = 80, delayType = "throttle")),
                  div(class = "detalle-mapa small", uiOutput("mapa_detalle")),
                  pie_grafica("s4_entidades", "s4_entidades.csv")),
          grafica("g_regiones", "500px", "s4_regiones", "s4_regiones.csv"))),
      nav_panel("Ritmo de adopción",
        opcional(ritmo, "g_ritmo", "780px", "s4_ritmo", "s4_ritmo.csv",
                 "Frente C: cambio en el % que compra por entidad entre dos levantamientos.")),
      nav_panel("¿Por qué?",
        opcional(comercios, "g_comercios", "480px", "s4_comercios", "s4_comercios.csv",
                 "Frente C: comercios al por menor por habitante (DENUE) frente a la compra en línea.")))),
    lo_que_encontramos("s4"))),

  nav_panel("5. ¿Qué hacer?", value = "s5", seccion(
    cabecera("5", "La tienda como puerta de entrada a la compra en línea",
             "Propuesta en borrador. Frentes D y C. Preguntas P7 y P8."),
    div(class = "lienzo", navset_underline(
      nav_panel("La propuesta",
        div(class = "row g-4 mt-1",
          div(class = "col-lg-7",
            tags$dl(class = "ficha", lapply(seq_len(nrow(propuesta)), function(i)
              tagList(tags$dt(propuesta$campo[i]), tags$dd(propuesta$texto[i]))))),
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
      nav_panel("Simulador",
        p(class = "text-muted mt-2 mb-3", style = "max-width:85ch",
          "Las dos palancas son las dos puertas de la propuesta. Cada escenario de 2030 es una posición de las palancas: elige uno para ver dónde cae, o mueve las palancas a mano."),
        layout_columns(col_widths = c(4, 8),
          div(
            selectInput("sim_grupo", "Grupo objetivo", simul$grupo),
            div(class = "filtros mb-2", span(class = "rotulo", "Escenario")),
            div(class = "pastillas mb-3",
                radioButtons("sim_escenario", NULL, c(palancas$escenario, "Manual"),
                             selected = palancas$escenario[nrow(palancas)], inline = TRUE)),
            sliderInput("sim_sabe", "Puerta 1: de quienes dicen «no sé cómo», % que aprende y compra",
                        0, 60, palancas$habilidades[nrow(palancas)], step = 5, post = " %"),
            sliderInput("sim_persona", "Puerta 2: de quienes prefieren en persona, % que compra con ver, recoger y devolver en tienda",
                        0, 60, palancas$tienda[nrow(palancas)], step = 5, post = " %"),
            uiOutput("sim_descripcion")),
          div(uiOutput("sim_kpis"),
            layout_columns(col_widths = c(6, 6),
              plotOutput("g_matriz", height = "400px"),
              plotOutput("g_cascada", height = "400px")),
            pie_grafica("s5_simulador", "s5_simulador.csv")))),
      nav_panel("Escenarios 2030", grafica("g_escenarios", "460px", "s5_escenarios", "s5_escenarios.csv")),
      nav_panel("Tamaño del mercado",
        opcional(mercado, "g_mercado", "340px", "s5_mercado", "s5_mercado.csv",
                 "Frente C: rango de gasto de los hogares que podría hacerse en línea (ENIGH).")))))),

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
      facet_wrap(~categoria, nrow = 1) +
      scale_y_continuous(labels = pct, expand = expansion(mult = c(0.05, 0.2))) +
      scale_x_continuous(breaks = range(d$anio)) +
      labs(title = "¿Cómo ha cambiado lo que se compra?",
           subtitle = "% de compradores en línea que compró cada categoría, por año",
           caption = "Solo categorías comparables entre años.", x = NULL, y = NULL) +
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

  output$g_perfiles <- graficar({
    d <- perfiles
    d$perfil <- factor(d$perfil, d$perfil[order(d$estimacion)])
    ggplot(d, aes(estimacion, perfil)) +
      geom_segment(aes(x = 0, xend = estimacion, yend = perfil), colour = col$borde, linewidth = 1.4) +
      geom_errorbar(aes(xmin = li90, xmax = ls90), width = 0.2, orientation = "y", colour = col$gris_claro) +
      punto(fill = col$azul, size = 11) +
      geom_text(aes(label = sprintf("%.0f", estimacion)), colour = "white", fontface = "bold", size = 3.6) +
      scale_x_continuous(limits = c(0, 100), labels = pct, expand = c(0, 2)) +
      labs(title = "Probabilidad de comprar, por perfil", subtitle = "Predicha por el modelo completo",
           caption = "Perfiles por definir (decisión 24).", x = NULL, y = NULL) +
      theme(panel.grid.major.y = element_blank())
  })

  output$g_hogar <- graficar({
    d <- hogar
    d$composicion <- factor(d$composicion, unique(d$composicion[order(d$orden)]))
    ggplot(d, aes(estimacion, composicion)) +
      geom_line(aes(group = composicion), colour = col$borde, linewidth = 1.6) +
      geom_errorbar(aes(xmin = li90, xmax = ls90, colour = sexo), width = 0.15, orientation = "y", show.legend = FALSE) +
      punto(aes(fill = sexo), size = 5.5) +
      scale_fill_manual(values = c(Hombre = col$azul, Mujer = col$morado), name = NULL) +
      scale_colour_manual(values = c(Hombre = col$azul, Mujer = col$morado), guide = "none") +
      scale_x_continuous(labels = pct) +
      labs(title = "¿Vivir con jóvenes ayuda a comprar en línea?",
           subtitle = "% que compra en línea según la composición del hogar, por sexo",
           caption = "Usuarios de internet de 18+. Líneas: intervalo al 90 %.", x = NULL, y = NULL) +
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
    if (input$s4_color == "estimacion") {
      p <- p + geom_polygon(aes(fill = estimacion), colour = "#A8AEB8", linewidth = 0.25) +
        # Invertida: el rezago (poca compra) es lo que se resalta en salmón
        scale_fill_gradientn(colours = rev(rampa), name = "% que compra",
                             guide = guide_colourbar(barwidth = 12, barheight = 0.6))
    } else {
      # Regiones del CEEY (decisión 62). Paleta de 5 validada en todos los pares
      # (daltonismo y contraste); además se rotula cada región sobre el mapa.
      cr <- aggregate(cbind(long_c, lat_c) ~ region, entidades, mean)
      # Centro y Centro-norte quedan muy juntas: se separan sus rótulos
      cr$long_c <- cr$long_c + c(`Centro-norte` = -1.8, Centro = 1.2)[cr$region] %|% 0
      cr$lat_c  <- cr$lat_c  + c(`Centro-norte` = 0.3, Centro = -0.6)[cr$region] %|% 0
      p <- p + geom_polygon(aes(fill = region), colour = "white", linewidth = 0.35) +
        scale_fill_manual(values = col_region, name = NULL, guide = "none") +
        geom_label(data = cr, aes(long_c, lat_c, label = region), inherit.aes = FALSE,
                   size = 3.6, fontface = "bold", colour = col$tinta, fill = "white",
                   label.size = 0, alpha = 0.85)
    }
    p + coord_quickmap(xlim = c(-117.5, -86.5), ylim = c(14.4, 32.8), expand = FALSE) +
      labs(title = "Compra en línea por entidad",
           subtitle = "% de usuarios de internet de 18+ que compró en línea",
           caption = if (input$s4_color == "estimacion") "Más salmón = menos compra. Regiones del CEEY (ESRU-EMOVI)." else "Regiones del CEEY (ESRU-EMOVI 2017).", x = NULL, y = NULL) +
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
    d$etiqueta <- paste(d$region, "vs.", d$referencia)
    d$tipo <- factor(ifelse(d$tipo == "Bruta", "Observada", "Con controles"), c("Observada", "Con controles"))
    barras_brecha(d, "etiqueta", "tipo", c(Observada = col$gris_claro, `Con controles` = col$azul),
          "Brecha por región", paste("Diferencia en el % que compra frente a la región", unique(d$referencia)[1]),
          "Con controles: comparando personas iguales\nen escolaridad, edad, sexo y estrato.")
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
  # Elegir un escenario mueve las palancas; moverlas a mano pasa a "Manual"
  observeEvent(input$sim_escenario, {
    e <- palancas[palancas$escenario == input$sim_escenario, ]
    if (nrow(e) == 1) {
      updateSliderInput(session, "sim_sabe", value = e$habilidades)
      updateSliderInput(session, "sim_persona", value = e$tienda)
    }
  })
  observeEvent(list(input$sim_sabe, input$sim_persona), {
    e <- palancas[palancas$habilidades == input$sim_sabe & palancas$tienda == input$sim_persona, ]
    nuevo <- if (nrow(e) == 1) e$escenario else "Manual"
    if (!identical(nuevo, input$sim_escenario)) updateRadioButtons(session, "sim_escenario", selected = nuevo)
  }, ignoreInit = TRUE)

  # Cálculo del simulador: personas que no compran × razón × % que convierte cada puerta
  sim <- reactive({
    g <- simul[simul$grupo == input$sim_grupo, ]
    no_compran <- g$millones * (1 - g$pct_compra / 100)
    p1 <- no_compran * g$pct_no_sabe / 100 * input$sim_sabe / 100
    p2 <- no_compran * g$pct_en_persona / 100 * input$sim_persona / 100
    list(g = g, p1 = p1, p2 = p2, nuevos = p1 + p2,
         pp1 = 100 * p1 / g$millones, pp2 = 100 * p2 / g$millones)
  })

  output$sim_descripcion <- renderUI({
    e <- palancas[palancas$escenario == input$sim_escenario, ]
    if (nrow(e) == 1) p(class = "small text-muted", strong(e$escenario), ": ", e$descripcion)
    else p(class = "small text-muted", "Posición manual de las palancas.")
  })

  # Dónde cae la posición actual en la matriz de escenarios
  output$g_matriz <- graficar({
    corte_h <- mean(range(palancas$habilidades)); corte_t <- mean(range(palancas$tienda))
    cuadros <- data.frame(
      xmin = c(0, corte_t, 0, corte_t), xmax = c(corte_t, 60, corte_t, 60),
      ymin = c(0, 0, corte_h, corte_h), ymax = c(corte_h, corte_h, 60, 60),
      nombre = c("Inercia", "Solo\ntiendas", "Solo\nhabilidades", "Puerta\nabierta"),
      relleno = c(rampa[2], rampa[4], rampa[3], col$frio))
    cuadros$x <- (cuadros$xmin + cuadros$xmax) / 2; cuadros$y <- cuadros$ymax - 5.5
    ggplot() +
      geom_rect(data = cuadros, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = I(relleno)),
                colour = "white", linewidth = 1.5) +
      geom_text(data = cuadros, aes(x, y, label = nombre), colour = col$tinta, fontface = "bold", size = 3.2) +
      geom_point(data = palancas, aes(tienda, habilidades), shape = 21, fill = "white",
                 colour = col$gris, size = 3, stroke = 1) +
      punto(data = data.frame(x = input$sim_persona, y = input$sim_sabe), aes(x, y), fill = col$azul, size = 7) +
      scale_x_continuous(limits = c(0, 60), expand = c(0, 0), breaks = seq(0, 60, 20)) +
      scale_y_continuous(limits = c(0, 60), expand = c(0, 0), breaks = seq(0, 60, 20)) +
      coord_fixed() +
      labs(title = "¿En qué escenario estás?",
           subtitle = "Azul: tu posición",
           x = "Puerta 2: tienda (%)", y = "Puerta 1: habilidades (%)") +
      theme(panel.grid.major = element_blank())
  })

  # Cuánto aporta cada puerta al % que compra
  output$g_cascada <- graficar({
    s <- sim(); hoy <- s$g$pct_compra
    pasos <- c("Hoy", "+ Puerta\n1", "+ Puerta\n2", "Con la\npropuesta")
    d <- data.frame(paso = factor(pasos, pasos),
                    inicio = c(0, hoy, hoy + s$pp1, 0),
                    fin = c(hoy, hoy + s$pp1, hoy + s$pp1 + s$pp2, hoy + s$pp1 + s$pp2),
                    tipo = c("base", "p1", "p2", "total"))
    d$etiqueta <- ifelse(d$tipo %in% c("p1", "p2"), sprintf("+%.1f", d$fin - d$inicio), sprintf("%.0f %%", d$fin))
    ggplot(d, aes(xmin = as.numeric(paso) - 0.35, xmax = as.numeric(paso) + 0.35, ymin = inicio, ymax = fin, fill = tipo)) +
      geom_rect() +
      geom_text(aes(x = as.numeric(paso), y = fin, label = etiqueta), vjust = -0.6, size = 3.8,
                colour = col$tinta, fontface = "bold") +
      scale_fill_manual(values = c(base = col$gris_claro, p1 = col$verde, p2 = col$ocre, total = col$azul), guide = "none") +
      scale_x_continuous(breaks = 1:4, labels = levels(d$paso)) +
      scale_y_continuous(labels = pct, expand = expansion(mult = c(0, 0.15))) +
      expand_limits(y = 0) +
      labs(title = "¿Cuánto aporta cada puerta?",
           subtitle = "% del grupo que compra en línea",
           x = NULL, y = NULL) +
      theme(panel.grid.major.x = element_blank())
  })

  output$sim_kpis <- renderUI({
    s <- sim(); g <- s$g; nuevos <- s$nuevos
    nuevo <- g$pct_compra + s$pp1 + s$pp2
    div(class = "kpis",
        div(class = "kpi", div(class = "etiqueta", "Compran hoy"), div(class = "valor", sprintf("%.0f %%", g$pct_compra))),
        div(class = "kpi destacado", div(class = "etiqueta", "Comprarían"), div(class = "valor", sprintf("%.0f %%", nuevo))),
        div(class = "kpi", div(class = "etiqueta", "Personas nuevas"), div(class = "valor", sprintf("%.1f M", nuevos))))
  })

  output$g_escenarios <- graficar({
    d <- escen
    ultimo <- d[d$escenario == "Observado" & d$anio == max(d$anio[d$escenario == "Observado"]), ]
    puentes <- do.call(rbind, lapply(palancas$escenario, function(e)
      transform(ultimo, escenario = e, li90 = estimacion, ls90 = estimacion)))
    d <- rbind(d, puentes)
    d$escenario <- factor(d$escenario, c("Observado", palancas$escenario))
    colores <- c(Observado = col$tinta, Inercia = col$gris_claro, `Solo tiendas` = col$ocre,
                 `Solo habilidades` = col$verde, `Puerta abierta` = col$azul)
    final <- d[d$anio == max(d$anio), ]
    # Separar etiquetas finales que quedarían encimadas (mínimo 3.2 pp entre sí)
    final <- final[order(final$estimacion), ]
    final$y_etq <- final$estimacion
    for (i in seq_len(nrow(final))[-1]) final$y_etq[i] <- max(final$y_etq[i], final$y_etq[i - 1] + 3.2)
    ggplot(d, aes(anio, estimacion, colour = escenario)) +
      annotate("rect", xmin = max(ultimo$anio), xmax = Inf, ymin = -Inf, ymax = Inf, fill = rampa[3], alpha = 0.6) +
      geom_ribbon(data = d[!is.na(d$li90), ], aes(ymin = li90, ymax = ls90, fill = escenario),
                  alpha = 0.18, colour = NA, show.legend = FALSE) +
      geom_line(linewidth = 1) +
      punto(data = d[!(d$anio == max(ultimo$anio) & d$escenario != "Observado"), ],
            aes(fill = escenario), size = 3.2) +
      geom_text(data = final, aes(y = y_etq, label = paste0(escenario, " ", round(estimacion), " %")),
                hjust = 0, nudge_x = 0.25, colour = col$tinta, size = 3.6, show.legend = FALSE) +
      scale_colour_manual(values = colores, name = "Escenario") +
      scale_fill_manual(values = colores, guide = "none") +
      scale_x_continuous(breaks = 2019:2030, expand = expansion(add = c(0.3, 2.6))) +
      scale_y_continuous(labels = pct) +
      labs(title = "Escenarios 2030", subtitle = "% de usuarios de internet que compra en línea. Cada línea es una posición de las palancas del simulador.",
           caption = "Los escenarios son supuestos explícitos, no pronósticos. Bandas: rango del escenario.",
           x = NULL, y = NULL)
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
