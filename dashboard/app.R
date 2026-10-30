# =============================================================================
# Dashboard — Datatón INEGI 2026
# Una portada y 5 secciones del storyboard. Cada sección: afirmación → gráficas
# → "Lo que encontramos" → "Cómo se calculó".
#
# Cada gráfica lee un CSV de dashboard/datos/. Hoy muchos son datos de PRUEBA;
# cada frente los reemplaza por los finales con las mismas columnas
# (ver dashboard/README.md). Los textos viven en hallazgos.csv y propuesta.csv,
# así se editan sin tocar código.
#
# Correr: abrir el proyecto en RStudio y ejecutar shiny::runApp("dashboard")
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

# --- Datos -------------------------------------------------------------------
dir_datos <- if (dir.exists("datos")) "datos" else file.path("dashboard", "datos")
leer <- function(nombre) read.csv(file.path(dir_datos, nombre), fileEncoding = "UTF-8")

embudo     <- leer("s1_embudo.csv")
categorias <- leer("s1_categorias.csv")
eslabon    <- leer("s1_brechas_eslabon.csv")
brechas    <- leer("s2_brechas.csv")
perfiles   <- leer("s2_perfiles.csv")
razones    <- leer("s3_razones.csv")
problemas  <- leer("s3_problemas.csv")
entidades  <- merge(leer("entidades.csv"), leer("s4_entidades.csv"), by = "cve_ent")
mapa       <- leer("mapa_estados.csv")
regiones   <- leer("s4_regiones.csv")
escen      <- leer("s5_escenarios.csv")
simul      <- leer("s5_simulador.csv")
calculo    <- leer("como_se_calculo.csv")
hallazgos  <- leer("hallazgos.csv")
propuesta  <- leer("propuesta.csv")

hay_prueba <- any(sapply(list(embudo, categorias, eslabon, brechas, perfiles, razones,
                              problemas, entidades, regiones, escen, simul),
                         function(d) isTRUE(any(d$prueba))))

# --- Estilo ------------------------------------------------------------------
# Paleta categórica validada (daltonismo y contraste): azul, verde, morado, ocre.
# Secuencial: un solo tono, de claro a oscuro. Texto siempre en tinta, no en color.
col <- list(
  tinta = "#1F2937", gris = "#6B7280", gris_claro = "#9CA3AF", borde = "#C9CED6",
  fondo = "#FCFBF8", reja = "#ECEEF1",
  azul = "#0F72A8", verde = "#3E9A66", morado = "#6A4C93", ocre = "#B97A1C",
  calido = "#F8E2CF", frio = "#EEF3EE"
)
# Rampa cálida de la figura de referencia: muy bajo → extremo
rampa <- c("#EEF3EE", "#F3F2E2", "#FBEFD0", "#F7DCC0", "#F0BEB2", "#E2928C")

tema <- bs_theme(version = 5, primary = col$azul, bg = "#FFFFFF", fg = col$tinta,
                 base_font = font_google("Inter", local = FALSE),
                 "border-radius" = "0.6rem")
tema <- bs_add_rules(tema, "
  body { background: #FCFBF8; }
  .card { border-color: #E5E7EB; box-shadow: none; }
  .navbar { border-bottom: 1px solid #E5E7EB; }
  .seccion-titulo { font-weight: 700; letter-spacing: -0.01em; }
  .hallazgos li { margin-bottom: .35rem; }
  .bslib-value-box .value-box-title { color: #6B7280; }
")

theme_set(
  theme_minimal(base_size = 13, base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 16, colour = col$tinta,
                                margin = margin(b = 4)),
      plot.subtitle = element_text(size = 12, colour = col$gris, margin = margin(b = 12)),
      plot.caption = element_text(size = 10, colour = col$gris, hjust = 0,
                                  margin = margin(t = 12), lineheight = 1.2),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(colour = col$borde, fill = NA, linewidth = 0.6),
      panel.grid.major = element_line(colour = col$reja, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = col$gris, size = 11),
      axis.title = element_text(face = "bold", colour = col$tinta, size = 12),
      strip.text = element_text(face = "bold", colour = col$tinta, size = 12),
      legend.position = "bottom", legend.justification = "left",
      legend.title = element_text(face = "bold", colour = col$tinta, size = 11),
      legend.text = element_text(colour = col$tinta, size = 11),
      plot.margin = margin(12, 16, 12, 12)
    )
)
pct <- function(x) paste0(x, " %")
pp  <- function(x) ifelse(x > 0, paste0("+", x), as.character(x))

# Marcador como en la figura de referencia: círculo relleno con borde blanco
punto <- function(...) geom_point(shape = 21, colour = "white", stroke = 1.3, ...)

# --- Piezas de la interfaz ---------------------------------------------------
como_se_calculo <- function(id) {
  m <- calculo[calculo$grafica == id, ]
  if (nrow(m) == 0) return(NULL)
  accordion(open = FALSE, class = "mt-2", accordion_panel(
    "Cómo se calculó", icon = icon("circle-info"),
    tags$dl(class = "row small mb-0",
            tags$dt(class = "col-sm-3", "Fuente"),   tags$dd(class = "col-sm-9", m$fuente),
            tags$dt(class = "col-sm-3", "Universo"), tags$dd(class = "col-sm-9", m$universo),
            tags$dt(class = "col-sm-3", "Tipo"),     tags$dd(class = "col-sm-9", m$tipo),
            tags$dt(class = "col-sm-3", "Script"),   tags$dd(class = "col-sm-9", tags$code(m$script)),
            tags$dt(class = "col-sm-3", "Nota"),     tags$dd(class = "col-sm-9", m$nota))))
}

lo_que_encontramos <- function(seccion) {
  h <- hallazgos[hallazgos$seccion == seccion, ]
  h <- h[order(h$orden), ]
  card(style = "border: 0; border-left: 4px solid #0F72A8;",
       card_body(h5(icon("lightbulb"), " Lo que encontramos", class = "fw-bold mb-3"),
                 tags$ol(class = "hallazgos mb-0", lapply(h$texto, tags$li))))
}

afirmacion <- function(texto, apoyo) {
  div(class = "mt-3 mb-3", h2(texto, class = "seccion-titulo"),
      p(apoyo, class = "text-muted mb-0"))
}

selector_grupo <- function(id, opciones = c("Escolaridad", "Edad", "Sexo", "Localidad",
                                            "Estrato", "Región"), nacional = FALSE) {
  if (nacional) opciones <- c("Nacional", opciones)
  selectInput(id, "Ver por", opciones, selected = "Escolaridad")
}

grafica <- function(salida, alto, id_calculo, ...) {
  card(card_body(plotOutput(salida, height = alto, ...), como_se_calculo(id_calculo)))
}

aviso_prueba <- if (hay_prueba) {
  div(class = "alert alert-warning py-2 mb-0 rounded-0 text-center small",
      strong("Datos de prueba: "),
      "las gráficas usan cifras inventadas. Las cifras de los recuadros «Lo que encontramos» son del diagnóstico preliminar.")
}

# --- Interfaz ----------------------------------------------------------------
ui <- page_navbar(
  title = "¿Quién compra en línea en México?",
  theme = tema,
  header = aviso_prueba,
  fillable = FALSE,

  nav_panel("Inicio",
    div(class = "py-4",
      h1("Comprar en línea en México: quién se queda fuera, por qué y qué hacer",
         class = "seccion-titulo"),
      p(class = "lead text-muted",
        "Diagnóstico con datos oficiales del INEGI (ENDUTIH 2025) y una propuesta para que más personas compren en línea.")),
    layout_columns(fill = FALSE,
      value_box("Usuarios de internet de 18+ que compran en línea", "43.7 %",
                showcase = icon("cart-shopping"), theme = "primary"),
      value_box("Brecha por escolaridad (superior vs. primaria)", "+42.5 pp",
                showcase = icon("graduation-cap")),
      value_box("Brecha por edad (65+ vs. 18-24)", "−41.8 pp",
                showcase = icon("person-cane")),
      value_box("Usuarios con primaria o menos que no compran", "11.3 millones",
                showcase = icon("users"))),
    lo_que_encontramos("inicio"),
    p(class = "text-muted small mt-3",
      "Cifras del diagnóstico preliminar; se actualizarán con las finales. Brechas con controles, intervalos al 90 %.")),

  nav_panel("1. Panorama",
    afirmacion("Del acceso a la compra, la gente se pierde en eslabones distintos",
               "Afirmación provisional. Frente A. Preguntas P1 y P3."),
    layout_sidebar(
      sidebar = sidebar(selector_grupo("s1_grupo", nacional = TRUE), open = "always", width = 220),
      navset_card_underline(
        nav_panel("Embudo", plotOutput("g_embudo", height = "560px"), como_se_calculo("s1_embudo")),
        nav_panel("¿Conectarse o comprar?",
          plotOutput("g_eslabon", height = "440px"), como_se_calculo("s1_brechas_eslabon")),
        nav_panel("¿Qué se compra?",
          plotOutput("g_categorias", height = "520px"), como_se_calculo("s1_categorias")))),
    lo_que_encontramos("s1")),

  nav_panel("2. ¿Quién queda fuera?",
    afirmacion("La brecha es de escolaridad, edad y territorio",
               "Otras brechas se explican en buena parte por escolaridad. Frentes A y B. Pregunta P2."),
    layout_columns(col_widths = c(7, 5),
      grafica("g_brechas", "520px", "s2_brechas"),
      grafica("g_perfiles", "520px", "s2_perfiles")),
    lo_que_encontramos("s2")),

  nav_panel("3. ¿Por qué?",
    afirmacion("Unos no pueden (habilidades) y otros no quieren (prefieren lo presencial)",
               "Los medios de pago pesan poco. Frente B. Preguntas P4 y P5."),
    layout_sidebar(
      sidebar = sidebar(selector_grupo("s3_grupo"), open = "always", width = 220),
      navset_card_underline(
        nav_panel("Por qué no compran",
          plotOutput("g_razones", height = "500px"), como_se_calculo("s3_razones")),
        nav_panel("Problemas de quienes sí compran",
          plotOutput("g_problemas", height = "460px"), como_se_calculo("s3_problemas")))),
    lo_que_encontramos("s3")),

  nav_panel("4. ¿Dónde?",
    afirmacion("El territorio pesa aun con controles",
               "Afirmación provisional. Frente C. Pasa el cursor sobre una entidad para ver su cifra."),
    layout_sidebar(
      sidebar = sidebar(open = "always", width = 220,
        radioButtons("s4_color", "Colorear el mapa por",
                     c("% que compra" = "estimacion", "Región" = "region"))),
      layout_columns(col_widths = c(6, 6),
        card(card_body(
          plotOutput("g_mapa", height = "520px",
                     hover = hoverOpts("mapa_hover", delay = 80, delayType = "throttle")),
          uiOutput("mapa_detalle"),
          como_se_calculo("s4_entidades"))),
        grafica("g_regiones", "520px", "s4_regiones"))),
    lo_que_encontramos("s4")),

  nav_panel("5. ¿Qué hacer?",
    afirmacion("La tienda como puerta de entrada a la compra en línea",
               "Propuesta en borrador. Frentes D y C."),
    navset_card_underline(
      nav_panel("La propuesta",
        layout_columns(col_widths = c(7, 5),
          div(class = "mt-3", tags$dl(lapply(seq_len(nrow(propuesta)), function(i)
            tagList(tags$dt(propuesta$campo[i]), tags$dd(class = "mb-3", propuesta$texto[i]))))),
          div(class = "mt-3", lo_que_encontramos("s5")))),
      nav_panel("Simulador",
        layout_sidebar(
          sidebar = sidebar(open = "always", width = 280,
            selectInput("sim_grupo", "Grupo objetivo", simul$grupo),
            sliderInput("sim_sabe", "De quienes dicen «no sé cómo», % que aprende y compra",
                        0, 100, 30, step = 5, post = " %"),
            sliderInput("sim_persona", "De quienes prefieren en persona, % que compra con recoger en tienda",
                        0, 100, 20, step = 5, post = " %")),
          layout_columns(fill = FALSE,
            value_box("Compran hoy", textOutput("vb_hoy"), showcase = icon("cart-shopping")),
            value_box("Comprarían", textOutput("vb_nuevo"), showcase = icon("arrow-trend-up"), theme = "primary"),
            value_box("Personas nuevas que compran", textOutput("vb_personas"), showcase = icon("users"))),
          como_se_calculo("s5_simulador"))),
      nav_panel("Escenarios 2030",
        plotOutput("g_escenarios", height = "480px"), como_se_calculo("s5_escenarios")))),

  nav_spacer(),
  nav_item(tags$a(icon("github"), "Código",
                  href = "https://github.com/rebeloppar/dataton-inegi-2026", target = "_blank"))
)

# --- Servidor ----------------------------------------------------------------
server <- function(input, output, session) {

  graficar <- function(expr) renderPlot(expr, res = 110)

  por_grupo <- function(d, g) {
    d <- d[d$grupo_var == g, ]
    d$grupo <- factor(d$grupo, unique(d$grupo[order(d$orden)]))
    d
  }

  # Brechas tipo "pesas": fondo cálido = desventaja, frío = ventaja
  pesas <- function(d, y, color, valores, titulo, subtitulo, nota, xlab = "Diferencia en puntos porcentuales") {
    rango <- range(c(d$li90, d$ls90, 0), na.rm = TRUE); rango <- rango + c(-3, 3)
    ggplot(d, aes(estimacion, .data[[y]])) +
      annotate("rect", xmin = rango[1], xmax = 0, ymin = -Inf, ymax = Inf, fill = col$calido) +
      annotate("rect", xmin = 0, xmax = rango[2], ymin = -Inf, ymax = Inf, fill = col$frio) +
      geom_vline(xintercept = 0, colour = col$gris_claro, linewidth = 0.5) +
      geom_line(aes(group = .data[[y]]), colour = col$borde, linewidth = 1.6) +
      geom_errorbarh(aes(xmin = li90, xmax = ls90, colour = .data[[color]]),
                     height = 0.18, linewidth = 0.6, show.legend = FALSE) +
      punto(aes(fill = .data[[color]]), size = 5.5) +
      scale_fill_manual(values = valores, name = NULL) +
      scale_colour_manual(values = valores, guide = "none") +
      scale_x_continuous(limits = rango, expand = c(0, 0), labels = pp, n.breaks = 5) +
      labs(title = titulo, subtitle = subtitulo, caption = nota,
           x = xlab, y = NULL) +
      theme(panel.grid.major.y = element_blank())
  }

  calor <- function(d, fila, titulo, subtitulo, nota) {
    prom <- tapply(d$estimacion, d[[fila]], mean)
    d[[fila]] <- factor(d[[fila]], names(sort(prom)))
    corte <- max(d$estimacion) * 0.55
    ggplot(d, aes(grupo, .data[[fila]], fill = estimacion)) +
      geom_tile(colour = "white", linewidth = 2) +
      geom_text(aes(label = sprintf("%.0f", estimacion),
                    colour = estimacion > corte), size = 4.2, fontface = "bold") +
      scale_fill_gradientn(colours = rampa, name = "%", limits = c(0, NA)) +
      scale_colour_manual(values = c(`TRUE` = col$tinta, `FALSE` = col$tinta), guide = "none") +
      scale_x_discrete(position = "top") +
      labs(title = titulo, subtitle = subtitulo, caption = nota, x = NULL, y = NULL) +
      theme(panel.grid.major = element_blank(), panel.border = element_blank(),
            legend.position = "right", axis.text.x = element_text(colour = col$tinta),
            axis.text.y = element_text(colour = col$tinta))
  }

  # 1. Panorama ---------------------------------------------------------------
  output$g_embudo <- graficar({
    d <- por_grupo(embudo, input$s1_grupo)
    pasos <- c("Hogar con internet", "Usa internet", "Compra en línea")
    d$paso <- match(d$eslabon, pasos)
    # Cada eslabón es un trapecio: arriba su ancho, abajo el del siguiente eslabón
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
      scale_fill_manual(values = c(rampa[3], rampa[4], rampa[5]), name = NULL, drop = FALSE) +
      coord_cartesian(xlim = c(-52, 52)) +
      labs(title = "Del hogar conectado a la compra en línea",
           subtitle = paste0("% en cada eslabón, por ", tolower(input$s1_grupo),
                             ". Debajo de cada cifra, su intervalo al 90 %."),
           caption = "Cada eslabón tiene su propio universo: hogares, residentes de 18+ y usuarios de internet de 18+.",
           x = NULL, y = NULL) +
      theme(axis.text = element_blank(), panel.grid.major = element_blank(), panel.border = element_blank())
  })

  output$g_eslabon <- graficar({
    d <- eslabon
    d$eslabon <- factor(d$eslabon, c("Usar internet", "Comprar en línea"))
    pesas(d, "comparacion", "eslabon",
          c(`Usar internet` = col$morado, `Comprar en línea` = col$azul),
          "¿Se pierde más gente al conectarse o al comprar?",
          "Brecha con controles en cada eslabón, en puntos porcentuales",
          "Usar internet: residentes de 18+. Comprar: usuarios de internet de 18+. Líneas: intervalo al 90 %.")
  })

  output$g_categorias <- graficar(calor(
    por_grupo(categorias, input$s1_grupo), "categoria",
    "¿Qué se compra en línea?",
    paste0("% de quienes compraron en línea que compró cada categoría, por ", tolower(input$s1_grupo)),
    "Respuesta múltiple: los porcentajes no suman 100."))

  # 2. ¿Quién queda fuera? ----------------------------------------------------
  output$g_brechas <- graficar({
    d <- brechas
    aj <- d[d$tipo == "Ajustada", ]
    d$comparacion <- factor(d$comparacion, aj$comparacion[order(abs(aj$estimacion))])
    d$tipo <- factor(d$tipo, c("Bruta", "Ajustada"))
    pesas(d, "comparacion", "tipo", c(Bruta = col$gris_claro, Ajustada = col$azul),
          "Brechas en la probabilidad de comprar en línea",
          "Gris: diferencia observada. Azul: con controles. Fondo durazno = desventaja.",
          "Usuarios de internet de 18+. Controles: sexo, edad, indígena, discapacidad, escolaridad,\nlocalidad y estrato. Líneas: intervalo al 90 %.")
  })

  output$g_perfiles <- graficar({
    d <- perfiles
    d$perfil <- factor(d$perfil, d$perfil[order(d$estimacion)])
    ggplot(d, aes(estimacion, perfil)) +
      geom_segment(aes(x = 0, xend = estimacion, yend = perfil), colour = col$borde, linewidth = 1.4) +
      geom_errorbarh(aes(xmin = li90, xmax = ls90), height = 0.2, colour = col$gris_claro) +
      punto(fill = col$azul, size = 11) +
      geom_text(aes(label = sprintf("%.0f", estimacion)), colour = "white",
                fontface = "bold", size = 3.6) +
      scale_x_continuous(limits = c(0, 100), labels = pct, expand = c(0, 2)) +
      labs(title = "Probabilidad de comprar, por perfil",
           subtitle = "Predicha por el modelo completo", x = NULL, y = NULL,
           caption = "Perfiles por definir (decisión 24).") +
      theme(panel.grid.major.y = element_blank())
  })

  # 3. ¿Por qué? --------------------------------------------------------------
  output$g_razones <- graficar(calor(
    por_grupo(razones, input$s3_grupo), "razon",
    "¿Por qué no compran en línea?",
    paste0("% de quienes no compraron que da cada razón, por ", tolower(input$s3_grupo)),
    "Respuesta múltiple: los porcentajes no suman 100. 87 % da una sola razón."))

  output$g_problemas <- graficar(calor(
    por_grupo(problemas, input$s3_grupo), "problema",
    "Problemas de quienes sí compran",
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
      p <- p + geom_polygon(aes(fill = region), colour = "white", linewidth = 0.35) +
        scale_fill_manual(values = c(Norte = col$azul, `Centro Norte` = col$verde,
                                     Centro = col$morado, Sur = col$ocre), name = NULL)
    }
    p + coord_quickmap(xlim = c(-117.5, -86.5), ylim = c(14.4, 32.8), expand = FALSE) +
      labs(title = "Compra en línea por entidad",
           subtitle = "% de usuarios de internet de 18+ que compró en línea",
           caption = "Más salmón = menos compra. Regionalización provisional (frente C).", x = NULL, y = NULL) +
      theme(axis.text = element_blank(), panel.grid.major = element_blank(), panel.border = element_blank())
  })

  output$mapa_detalle <- renderUI({
    h <- input$mapa_hover
    if (is.null(h)) return(p(class = "small text-muted mb-0", "Pasa el cursor sobre una entidad."))
    dist <- (entidades$long_c - h$x)^2 + (entidades$lat_c - h$y)^2
    e <- entidades[which.min(dist), ]
    if (min(dist) > 4) return(p(class = "small text-muted mb-0", "Pasa el cursor sobre una entidad."))
    p(class = "small mb-0", strong(e$entidad), sprintf(" · %.1f %% compra en línea (IC 90 %%: %.1f–%.1f) · región %s",
                                                    e$estimacion, e$li90, e$ls90, e$region))
  })

  output$g_regiones <- graficar({
    d <- regiones
    d$etiqueta <- paste(d$region, "vs.", d$referencia)
    d$tipo <- factor(d$tipo, c("Bruta", "Ajustada"))
    pesas(d, "etiqueta", "tipo", c(Bruta = col$gris_claro, Ajustada = col$azul),
          "Brecha por región", "Bruta (gris) y con controles (azul)",
          "Frente a la región de referencia.\nLíneas: intervalo al 90 %.", xlab = "pp")
  })

  # 5. ¿Qué hacer? ------------------------------------------------------------
  sim <- reactive({
    g <- simul[simul$grupo == input$sim_grupo, ]
    no_compran <- g$millones * (1 - g$pct_compra / 100)
    nuevos <- no_compran * (g$pct_no_sabe / 100 * input$sim_sabe / 100 +
                            g$pct_en_persona / 100 * input$sim_persona / 100)
    list(hoy = g$pct_compra, nuevo = g$pct_compra + 100 * nuevos / g$millones, nuevos = nuevos)
  })
  output$vb_hoy      <- renderText(sprintf("%.0f %%", sim()$hoy))
  output$vb_nuevo    <- renderText(sprintf("%.0f %%", sim()$nuevo))
  output$vb_personas <- renderText(sprintf("%.1f millones", sim()$nuevos))

  output$g_escenarios <- graficar({
    d <- escen
    ultimo <- d[d$escenario == "Observado" & d$anio == max(d$anio[d$escenario == "Observado"]), ]
    puentes <- do.call(rbind, lapply(c("Inercial", "Habilidades", "Omnicanal"), function(e)
      transform(ultimo, escenario = e, li90 = estimacion, ls90 = estimacion)))
    d <- rbind(d, puentes)
    d$escenario <- factor(d$escenario, c("Observado", "Inercial", "Habilidades", "Omnicanal"))
    colores <- c(Observado = col$tinta, Inercial = col$gris_claro, Habilidades = col$verde, Omnicanal = col$azul)
    final <- d[d$anio == max(d$anio), ]
    ggplot(d, aes(anio, estimacion, colour = escenario)) +
      annotate("rect", xmin = max(ultimo$anio), xmax = Inf, ymin = -Inf, ymax = Inf, fill = rampa[3], alpha = 0.6) +
      geom_ribbon(data = d[!is.na(d$li90), ], aes(ymin = li90, ymax = ls90, fill = escenario),
                  alpha = 0.18, colour = NA, show.legend = FALSE) +
      geom_line(linewidth = 1) +
      punto(data = d[!(d$anio == max(ultimo$anio) & d$escenario != "Observado"), ],
            aes(fill = escenario), size = 3.2) +
      geom_text(data = final, aes(label = paste0(escenario, " ", round(estimacion), " %")),
                hjust = 0, nudge_x = 0.25, colour = col$tinta, size = 3.6, show.legend = FALSE) +
      scale_colour_manual(values = colores, name = "Escenario") +
      scale_fill_manual(values = colores, guide = "none") +
      scale_x_continuous(breaks = 2019:2030, expand = expansion(add = c(0.3, 2.6))) +
      scale_y_continuous(labels = pct) +
      labs(title = "Escenarios 2030",
           subtitle = "% de usuarios de internet que compra en línea. Fondo crema = proyección.",
           caption = "Los escenarios son supuestos explícitos, no pronósticos. Bandas: rango del escenario.",
           x = NULL, y = NULL)
  })
}

shinyApp(ui, server)
