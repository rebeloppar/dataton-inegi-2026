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
entidades  <- merge(leer("entidades_mosaico.csv"), leer("s4_entidades.csv"), by = "cve_ent")
regiones   <- leer("s4_regiones.csv")
escen      <- leer("s5_escenarios.csv")
simul      <- leer("s5_simulador.csv")
calculo    <- leer("como_se_calculo.csv")
hallazgos  <- leer("hallazgos.csv")
propuesta  <- leer("propuesta.csv")

hay_prueba <- any(sapply(list(embudo, categorias, eslabon, brechas, perfiles, razones,
                              problemas, entidades, regiones, escen, simul),
                         function(d) isTRUE(any(d$prueba))))

# --- Apariencia --------------------------------------------------------------
azul <- "#3E6FA8"; azul_claro <- "#A9C4E4"; azul_medio <- "#6F96C6"; gris <- "#6B7280"
tema <- bs_theme(version = 5, primary = azul, bg = "#FFFFFF", fg = "#1F2937",
                 base_font = font_google("Inter", local = FALSE))
theme_set(theme_minimal(base_size = 14) +
            theme(panel.grid.minor = element_blank(),
                  plot.title.position = "plot", legend.position = "top"))
pct <- function(x) paste0(x, " %")

# --- Piezas reutilizables ----------------------------------------------------
# Tarjeta "Cómo se calculó": se llena desde como_se_calculo.csv
como_se_calculo <- function(id) {
  m <- calculo[calculo$grafica == id, ]
  if (nrow(m) == 0) return(NULL)
  accordion(open = FALSE, accordion_panel(
    "Cómo se calculó", icon = icon("circle-info"),
    tags$dl(class = "row small mb-0",
            tags$dt(class = "col-sm-3", "Fuente"),   tags$dd(class = "col-sm-9", m$fuente),
            tags$dt(class = "col-sm-3", "Universo"), tags$dd(class = "col-sm-9", m$universo),
            tags$dt(class = "col-sm-3", "Tipo"),     tags$dd(class = "col-sm-9", m$tipo),
            tags$dt(class = "col-sm-3", "Script"),   tags$dd(class = "col-sm-9", tags$code(m$script)),
            tags$dt(class = "col-sm-3", "Nota"),     tags$dd(class = "col-sm-9", m$nota))))
}

# "Lo que encontramos": se llena desde hallazgos.csv
lo_que_encontramos <- function(seccion) {
  h <- hallazgos[hallazgos$seccion == seccion, ]
  h <- h[order(h$orden), ]
  card(class = "border-primary border-start border-4 border-top-0 border-end-0 border-bottom-0",
       card_header(icon("lightbulb"), " Lo que encontramos"),
       tags$ol(class = "mb-0", lapply(h$texto, tags$li)))
}

# Encabezado de cada sección: la afirmación que sostiene, no el tema
afirmacion <- function(texto, apoyo) {
  div(class = "mb-3", h3(texto, class = "fw-semibold"), p(apoyo, class = "text-muted mb-0"))
}

selector_grupo <- function(id, opciones = c("Escolaridad", "Edad", "Sexo", "Localidad",
                                            "Estrato", "Región"), nacional = FALSE) {
  if (nacional) opciones <- c("Nacional", opciones)
  selectInput(id, "Ver por", opciones, selected = "Escolaridad")
}

grafica <- function(titulo, salida, alto, id_calculo) {
  card(card_header(titulo), plotOutput(salida, height = alto),
       card_footer(como_se_calculo(id_calculo)))
}

aviso_prueba <- if (hay_prueba) {
  div(class = "alert alert-warning py-2 mb-0 rounded-0 text-center",
      strong("Datos de prueba: "),
      "las gráficas usan cifras inventadas. Las cifras de los recuadros «Lo que encontramos» son del diagnóstico preliminar.")
}

# --- Interfaz ----------------------------------------------------------------
ui <- page_navbar(
  title = "¿Quién compra en línea en México?",
  theme = tema,
  header = aviso_prueba,
  fillable = FALSE,

  # Portada -------------------------------------------------------------------
  nav_panel("Inicio",
    div(class = "py-4",
      h2("Comprar en línea en México: quién se queda fuera, por qué y qué hacer",
         class = "fw-semibold"),
      p(class = "lead text-muted",
        "Diagnóstico con datos oficiales del INEGI (ENDUTIH 2025) y una propuesta para que más personas compren en línea.")),
    layout_columns(fill = FALSE,
      value_box("Usuarios de internet de 18+ que compran en línea", "43.7 %",
                showcase = icon("cart-shopping"), theme = "primary"),
      value_box("Brecha por escolaridad (superior vs. primaria)", "+42.5 pp",
                showcase = icon("graduation-cap"), theme = "light"),
      value_box("Brecha por edad (65+ vs. 18-24)", "−41.8 pp",
                showcase = icon("person-cane"), theme = "light"),
      value_box("Usuarios con primaria o menos que no compran", "11.3 millones",
                showcase = icon("users"), theme = "light")),
    lo_que_encontramos("inicio"),
    p(class = "text-muted small mt-3",
      "Cifras del diagnóstico preliminar; se actualizarán con las finales. Brechas con controles, intervalos al 90 %.")),

  # 1. Panorama ---------------------------------------------------------------
  nav_panel("1. Panorama",
    afirmacion("Del acceso a la compra, la gente se pierde en eslabones distintos",
               "Afirmación provisional. Frente A. Preguntas P1 y P3."),
    layout_sidebar(
      sidebar = sidebar(selector_grupo("s1_grupo", nacional = TRUE), open = "always", width = 220),
      navset_card_underline(
        nav_panel("Embudo",
          plotOutput("g_embudo", height = "460px"), como_se_calculo("s1_embudo")),
        nav_panel("¿Conectarse o comprar?",
          p(class = "text-muted small mt-2",
            "Brecha con controles en cada eslabón. ¿Dónde se pierde más gente de cada grupo? (no depende del filtro)"),
          plotOutput("g_eslabon", height = "380px"), como_se_calculo("s1_brechas_eslabon")),
        nav_panel("¿Qué se compra?",
          plotOutput("g_categorias", height = "460px"), como_se_calculo("s1_categorias")))),
    lo_que_encontramos("s1")),

  # 2. ¿Quién queda fuera? ----------------------------------------------------
  nav_panel("2. ¿Quién queda fuera?",
    afirmacion("La brecha es de escolaridad, edad y territorio",
               "Otras brechas se explican en buena parte por escolaridad. Frentes A y B. Pregunta P2."),
    layout_columns(col_widths = c(7, 5),
      grafica("Brecha en la probabilidad de comprar (puntos porcentuales)",
              "g_brechas", "440px", "s2_brechas"),
      grafica("Probabilidad de comprar en línea, por perfil",
              "g_perfiles", "440px", "s2_perfiles")),
    lo_que_encontramos("s2")),

  # 3. ¿Por qué? --------------------------------------------------------------
  nav_panel("3. ¿Por qué?",
    afirmacion("Unos no pueden (habilidades) y otros no quieren (prefieren lo presencial)",
               "Los medios de pago pesan poco. Frente B. Preguntas P4 y P5."),
    layout_sidebar(
      sidebar = sidebar(selector_grupo("s3_grupo"), open = "always", width = 220),
      navset_card_underline(
        nav_panel("Por qué no compran",
          p(class = "text-muted small mt-2", "% de quienes no compraron que da cada razón"),
          plotOutput("g_razones", height = "420px"), como_se_calculo("s3_razones")),
        nav_panel("Problemas de quienes sí compran",
          p(class = "text-muted small mt-2", "% de quienes compraron que tuvo cada problema"),
          plotOutput("g_problemas", height = "420px"), como_se_calculo("s3_problemas")))),
    lo_que_encontramos("s3")),

  # 4. ¿Dónde? ----------------------------------------------------------------
  nav_panel("4. ¿Dónde?",
    afirmacion("El territorio pesa aun con controles",
               "Afirmación provisional. Frente C."),
    layout_sidebar(
      sidebar = sidebar(open = "always", width = 220,
        radioButtons("s4_color", "Colorear el mapa por",
                     c("% que compra" = "estimacion", "Región" = "region"))),
      layout_columns(col_widths = c(7, 5),
        grafica("Usuarios de internet de 18+ que compran en línea, por entidad",
                "g_mapa", "440px", "s4_entidades"),
        grafica("Brecha por región, bruta y con controles (pp)",
                "g_regiones", "440px", "s4_regiones"))),
    lo_que_encontramos("s4")),

  # 5. ¿Qué hacer? ------------------------------------------------------------
  nav_panel("5. ¿Qué hacer?",
    afirmacion("La tienda como puerta de entrada a la compra en línea",
               "Propuesta en borrador. Frentes D y C."),
    navset_card_underline(
      nav_panel("La propuesta",
        layout_columns(col_widths = c(7, 5),
          div(class = "mt-2", tags$dl(lapply(seq_len(nrow(propuesta)), function(i)
            tagList(tags$dt(propuesta$campo[i]), tags$dd(class = "mb-3", propuesta$texto[i]))))),
          lo_que_encontramos("s5"))),
      nav_panel("Simulador",
        layout_sidebar(
          sidebar = sidebar(open = "always", width = 280,
            selectInput("sim_grupo", "Grupo objetivo", simul$grupo),
            sliderInput("sim_sabe", "De quienes dicen «no sé cómo», % que aprende y compra",
                        0, 100, 30, step = 5, post = " %"),
            sliderInput("sim_persona", "De quienes prefieren en persona, % que compra con recoger en tienda",
                        0, 100, 20, step = 5, post = " %")),
          layout_columns(fill = FALSE,
            value_box("Compran hoy", textOutput("vb_hoy"), showcase = icon("cart-shopping"), theme = "light"),
            value_box("Comprarían", textOutput("vb_nuevo"), showcase = icon("arrow-trend-up"), theme = "primary"),
            value_box("Personas nuevas que compran", textOutput("vb_personas"), showcase = icon("users"), theme = "light")),
          como_se_calculo("s5_simulador"))),
      nav_panel("Escenarios 2030",
        p(class = "text-muted small mt-2", "% de usuarios de internet que compra en línea"),
        plotOutput("g_escenarios", height = "420px"), como_se_calculo("s5_escenarios")))),

  nav_spacer(),
  nav_item(tags$a(icon("github"), "Código",
                  href = "https://github.com/rebeloppar/dataton-inegi-2026", target = "_blank"))
)

# --- Servidor ----------------------------------------------------------------
server <- function(input, output, session) {

  # Filtra un CSV por grupo_var y ordena los grupos
  por_grupo <- function(d, g) {
    d <- d[d$grupo_var == g, ]
    d$grupo <- factor(d$grupo, unique(d$grupo[order(d$orden)]))
    d
  }

  # Brechas tipo "pesas" (punto, línea e intervalo), reutilizable
  pesas <- function(d, y, color, valores, xlab) {
    ggplot(d, aes(estimacion, .data[[y]], colour = .data[[color]])) +
      geom_vline(xintercept = 0, colour = "grey70") +
      geom_line(aes(group = .data[[y]]), colour = "grey85", linewidth = 2) +
      geom_errorbarh(aes(xmin = li90, xmax = ls90), height = 0.2) +
      geom_point(size = 4) +
      scale_colour_manual(values = valores, name = NULL) +
      labs(x = xlab, y = NULL)
  }

  # Mapa de calor grupo × categoría, reutilizable (razones, problemas, categorías)
  calor <- function(d, fila) {
    prom <- tapply(d$estimacion, d[[fila]], mean)
    d[[fila]] <- factor(d[[fila]], names(sort(prom)))
    ggplot(d, aes(grupo, .data[[fila]], fill = estimacion)) +
      geom_tile(colour = "white", linewidth = 1) +
      geom_text(aes(label = sprintf("%.0f", estimacion),
                    colour = estimacion > max(estimacion) * 0.6), size = 4.5) +
      scale_fill_gradient(low = "#EEF3FA", high = azul, name = "%") +
      scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#1F2937"), guide = "none") +
      labs(x = NULL, y = NULL) +
      theme(panel.grid = element_blank(), legend.position = "right",
            axis.text.x = element_text(angle = 20, hjust = 1))
  }

  # 1. Panorama
  output$g_embudo <- renderPlot({
    d <- por_grupo(embudo, input$s1_grupo)
    d$eslabon <- factor(d$eslabon, c("Hogar con internet", "Usa internet", "Compra en línea"))
    ggplot(d, aes(eslabon, estimacion, fill = eslabon)) + geom_col(width = 0.7) +
      geom_errorbar(aes(ymin = li90, ymax = ls90), width = 0.2, colour = gris) +
      geom_text(aes(label = sprintf("%.0f %%", estimacion), y = estimacion / 2),
                colour = "white", fontface = "bold") +
      facet_wrap(~grupo) +
      scale_fill_manual(values = c(azul_claro, azul_medio, azul), guide = "none") +
      scale_y_continuous(limits = c(0, 100), labels = pct) +
      labs(x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  })

  output$g_eslabon <- renderPlot({
    d <- eslabon
    d$eslabon <- factor(d$eslabon, c("Usar internet", "Comprar en línea"))
    pesas(d, "comparacion", "eslabon",
          c(`Usar internet` = azul_claro, `Comprar en línea` = azul),
          "Diferencia en puntos porcentuales")
  })

  output$g_categorias <- renderPlot(calor(por_grupo(categorias, input$s1_grupo), "categoria"))

  # 2. ¿Quién queda fuera?
  output$g_brechas <- renderPlot({
    d <- brechas
    aj <- d[d$tipo == "Ajustada", ]
    d$comparacion <- factor(d$comparacion, aj$comparacion[order(abs(aj$estimacion))])
    d$tipo <- factor(d$tipo, c("Bruta", "Ajustada"))
    pesas(d, "comparacion", "tipo", c(Bruta = azul_claro, Ajustada = azul),
          "Diferencia en puntos porcentuales")
  })

  output$g_perfiles <- renderPlot({
    d <- perfiles
    d$perfil <- factor(d$perfil, d$perfil[order(d$estimacion)])
    ggplot(d, aes(perfil, estimacion)) + geom_col(fill = azul, width = 0.6) +
      geom_errorbar(aes(ymin = li90, ymax = ls90), width = 0.2, colour = gris) +
      coord_flip() + scale_y_continuous(limits = c(0, 100), labels = pct) +
      labs(x = NULL, y = NULL)
  })

  # 3. ¿Por qué?
  output$g_razones   <- renderPlot(calor(por_grupo(razones, input$s3_grupo), "razon"))
  output$g_problemas <- renderPlot(calor(por_grupo(problemas, input$s3_grupo), "problema"))

  # 4. ¿Dónde?
  output$g_mapa <- renderPlot({
    d <- entidades
    p <- ggplot(d, aes(col, -fila)) + coord_equal() + theme_void(base_size = 14) +
      theme(legend.position = "bottom")
    if (input$s4_color == "estimacion") {
      p <- p + geom_tile(aes(fill = estimacion), colour = "white", linewidth = 1.5) +
        scale_fill_gradient(low = "#EEF3FA", high = azul, name = "% que compra")
    } else {
      p <- p + geom_tile(aes(fill = region), colour = "white", linewidth = 1.5) +
        scale_fill_manual(values = c(azul, azul_medio, "#B9CFE8", "#E3ECF6"), name = NULL)
    }
    p + geom_text(aes(label = abrev), size = 3.4, fontface = "bold") +
      geom_text(aes(label = sprintf("%.0f %%", estimacion), y = -fila - 0.3), size = 2.8)
  })

  output$g_regiones <- renderPlot({
    d <- regiones
    d$etiqueta <- paste(d$region, "vs.", d$referencia)
    d$tipo <- factor(d$tipo, c("Bruta", "Ajustada"))
    pesas(d, "etiqueta", "tipo", c(Bruta = azul_claro, Ajustada = azul), "Puntos porcentuales")
  })

  # 5. Simulador: supuestos explícitos, una razón por persona
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

  output$g_escenarios <- renderPlot({
    d <- escen
    ultimo <- d[d$escenario == "Observado" & d$anio == max(d$anio[d$escenario == "Observado"]), ]
    puentes <- do.call(rbind, lapply(c("Inercial", "Habilidades", "Omnicanal"), function(e)
      transform(ultimo, escenario = e, li90 = estimacion, ls90 = estimacion)))
    d <- rbind(d, puentes)
    d$escenario <- factor(d$escenario, c("Observado", "Inercial", "Habilidades", "Omnicanal"))
    colores <- c(Observado = "#1F2937", Inercial = gris, Habilidades = azul_medio, Omnicanal = azul)
    ggplot(d, aes(anio, estimacion, colour = escenario, fill = escenario)) +
      geom_ribbon(data = d[!is.na(d$li90), ], aes(ymin = li90, ymax = ls90),
                  alpha = 0.15, colour = NA) +
      geom_line(linewidth = 1.2) + geom_point(size = 2) +
      scale_colour_manual(values = colores, name = NULL) +
      scale_fill_manual(values = colores, guide = "none") +
      scale_x_continuous(breaks = 2019:2030) +
      scale_y_continuous(labels = pct) +
      labs(x = NULL, y = NULL)
  })
}

shinyApp(ui, server)
