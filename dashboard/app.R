# =============================================================================
# Dashboard — Datatón INEGI 2026
# Cascarón: las 5 secciones del storyboard. Cada gráfica lee un CSV de
# dashboard/datos/. Hoy son datos de PRUEBA; cada frente los reemplaza por los
# finales con las mismas columnas (ver dashboard/README.md).
#
# Correr: abrir el proyecto en RStudio y ejecutar shiny::runApp("dashboard")
# =============================================================================

library(shiny)
library(bslib)
library(ggplot2)

# --- Datos -------------------------------------------------------------------
dir_datos <- if (dir.exists("datos")) "datos" else file.path("dashboard", "datos")
leer <- function(nombre) read.csv(file.path(dir_datos, nombre), fileEncoding = "UTF-8")

embudo    <- leer("s1_embudo.csv")
brechas   <- leer("s2_brechas.csv")
perfiles  <- leer("s2_perfiles.csv")
razones   <- leer("s3_razones.csv")
entidades <- merge(leer("entidades_mosaico.csv"), leer("s4_entidades.csv"), by = "cve_ent")
escen     <- leer("s5_escenarios.csv")
simul     <- leer("s5_simulador.csv")
calculo   <- leer("como_se_calculo.csv")

hay_prueba <- any(sapply(list(embudo, brechas, perfiles, razones, entidades, escen, simul),
                         function(d) isTRUE(any(d$prueba))))

# --- Apariencia --------------------------------------------------------------
azul <- "#3E6FA8"; azul_claro <- "#A9C4E4"; gris <- "#6B7280"
tema <- bs_theme(version = 5, primary = azul, bg = "#FFFFFF", fg = "#1F2937",
                 base_font = font_google("Inter", local = FALSE))
theme_set(theme_minimal(base_size = 14) +
            theme(panel.grid.minor = element_blank(),
                  plot.title.position = "plot", legend.position = "top"))

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

# Encabezado de cada sección: la afirmación que sostiene, no el tema
afirmacion <- function(texto, apoyo) {
  div(class = "mb-3", h3(texto, class = "fw-semibold"), p(apoyo, class = "text-muted mb-0"))
}

selector_grupo <- function(id, incluir_nacional = TRUE) {
  opciones <- c("Escolaridad", "Edad", "Sexo", "Localidad")
  if (incluir_nacional) opciones <- c("Nacional", opciones)
  selectInput(id, "Ver por", opciones, selected = opciones[2])
}

aviso_prueba <- if (hay_prueba) {
  div(class = "alert alert-warning py-2 mb-0 rounded-0 text-center",
      strong("Datos de prueba: "), "las cifras son inventadas y se reemplazarán por las finales.")
}

# --- Interfaz ----------------------------------------------------------------
ui <- page_navbar(
  title = "¿Quién compra en línea en México?",
  theme = tema,
  header = aviso_prueba,
  fillable = FALSE,

  # 1. Panorama ---------------------------------------------------------------
  nav_panel("1. Panorama",
    afirmacion("Del acceso a la compra, la gente se pierde en eslabones distintos",
               "Afirmación provisional. Frente A."),
    layout_sidebar(
      sidebar = sidebar(selector_grupo("s1_grupo"), open = "always", width = 220),
      card(card_header("Embudo: hogar con internet → usa internet → compra en línea"),
           plotOutput("g_embudo", height = "460px"),
           card_footer(como_se_calculo("s1_embudo"))))),

  # 2. ¿Quién queda fuera? ----------------------------------------------------
  nav_panel("2. ¿Quién queda fuera?",
    afirmacion("La brecha es de escolaridad, edad y territorio",
               "Otras brechas se explican en buena parte por escolaridad. Frentes A y B."),
    layout_columns(col_widths = c(7, 5),
      card(card_header("Brecha en la probabilidad de comprar (puntos porcentuales)"),
           plotOutput("g_brechas", height = "420px"),
           card_footer(como_se_calculo("s2_brechas"))),
      card(card_header("Probabilidad de comprar en línea, por perfil"),
           plotOutput("g_perfiles", height = "420px"),
           card_footer(como_se_calculo("s2_perfiles"))))),

  # 3. ¿Por qué? --------------------------------------------------------------
  nav_panel("3. ¿Por qué?",
    afirmacion("Unos no pueden (habilidades) y otros no quieren (prefieren lo presencial)",
               "Los medios de pago pesan poco. Frente B."),
    layout_sidebar(
      sidebar = sidebar(selector_grupo("s3_grupo"), open = "always", width = 220),
      card(card_header("Razones para no comprar en línea (% de quienes no compraron)"),
           plotOutput("g_razones", height = "420px"),
           card_footer(como_se_calculo("s3_razones"))))),

  # 4. ¿Dónde? ----------------------------------------------------------------
  nav_panel("4. ¿Dónde?",
    afirmacion("El territorio pesa aun con controles",
               "Afirmación provisional. Frente C."),
    layout_sidebar(
      sidebar = sidebar(open = "always", width = 220,
        radioButtons("s4_color", "Colorear por",
                     c("% que compra" = "estimacion", "Región" = "region"))),
      card(card_header("Usuarios de internet de 18+ que compran en línea, por entidad"),
           plotOutput("g_mapa", height = "460px"),
           card_footer(como_se_calculo("s4_entidades"))))),

  # 5. ¿Qué hacer? ------------------------------------------------------------
  nav_panel("5. ¿Qué hacer?",
    afirmacion("La tienda como puerta de entrada a la compra en línea",
               "Propuesta en construcción. Frentes D y C."),
    layout_sidebar(
      sidebar = sidebar(open = "always", width = 280, title = "Simulador",
        selectInput("sim_grupo", "Grupo objetivo", simul$grupo),
        sliderInput("sim_sabe", "De quienes dicen «no sé cómo», % que aprende y compra",
                    0, 100, 30, step = 5, post = " %"),
        sliderInput("sim_persona", "De quienes prefieren en persona, % que compra con recoger en tienda",
                    0, 100, 20, step = 5, post = " %")),
      layout_columns(fill = FALSE,
        value_box("Compran hoy", textOutput("vb_hoy"), showcase = icon("cart-shopping"), theme = "light"),
        value_box("Comprarían", textOutput("vb_nuevo"), showcase = icon("arrow-trend-up"), theme = "primary"),
        value_box("Personas nuevas que compran", textOutput("vb_personas"), showcase = icon("users"), theme = "light")),
      card(card_footer(como_se_calculo("s5_simulador"))),
      card(card_header("Escenarios 2030: % de usuarios de internet que compra en línea"),
           plotOutput("g_escenarios", height = "400px"),
           card_footer(como_se_calculo("s5_escenarios"))))),

  nav_spacer(),
  nav_item(tags$a(icon("github"), "Código",
                  href = "https://github.com/rebeloppar/dataton-inegi-2026", target = "_blank"))
)

# --- Servidor ----------------------------------------------------------------
server <- function(input, output, session) {

  # Barras con intervalo al 90 %, reutilizable
  barras_ic <- function(p) {
    p + geom_errorbar(aes(ymin = li90, ymax = ls90), width = 0.2, colour = gris, na.rm = TRUE)
  }

  output$g_embudo <- renderPlot({
    d <- embudo[embudo$grupo_var == input$s1_grupo, ]
    d$grupo   <- factor(d$grupo, unique(d$grupo[order(d$orden)]))
    d$eslabon <- factor(d$eslabon, c("Hogar con internet", "Usa internet", "Compra en línea"))
    barras_ic(ggplot(d, aes(eslabon, estimacion, fill = eslabon)) + geom_col(width = 0.7)) +
      geom_text(aes(label = sprintf("%.0f %%", estimacion), y = estimacion / 2),
                colour = "white", fontface = "bold") +
      facet_wrap(~grupo) +
      scale_fill_manual(values = c(azul_claro, "#6F96C6", azul), guide = "none") +
      scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, " %")) +
      labs(x = NULL, y = NULL) +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  })

  output$g_brechas <- renderPlot({
    d <- brechas
    orden <- d$comparacion[d$tipo == "Ajustada"][order(abs(d$estimacion[d$tipo == "Ajustada"]))]
    d$comparacion <- factor(d$comparacion, orden)
    d$tipo <- factor(d$tipo, c("Bruta", "Ajustada"))
    ggplot(d, aes(estimacion, comparacion, colour = tipo)) +
      geom_vline(xintercept = 0, colour = "grey70") +
      geom_line(aes(group = comparacion), colour = "grey80", linewidth = 2) +
      geom_errorbarh(aes(xmin = li90, xmax = ls90), height = 0.2) +
      geom_point(size = 4) +
      scale_colour_manual(values = c(Bruta = azul_claro, Ajustada = azul), name = NULL) +
      labs(x = "Diferencia en puntos porcentuales", y = NULL)
  })

  output$g_perfiles <- renderPlot({
    d <- perfiles
    d$perfil <- factor(d$perfil, d$perfil[order(d$estimacion)])
    barras_ic(ggplot(d, aes(perfil, estimacion)) + geom_col(fill = azul, width = 0.6)) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, " %")) +
      labs(x = NULL, y = NULL)
  })

  output$g_razones <- renderPlot({
    d <- razones[razones$grupo_var == input$s3_grupo, ]
    d$grupo <- factor(d$grupo, unique(d$grupo[order(d$orden)]))
    prom <- tapply(d$estimacion, d$razon, mean)
    d$razon <- factor(d$razon, names(sort(prom)))
    ggplot(d, aes(grupo, razon, fill = estimacion)) +
      geom_tile(colour = "white", linewidth = 1) +
      geom_text(aes(label = sprintf("%.0f", estimacion),
                    colour = estimacion > max(estimacion) * 0.6), size = 5) +
      scale_fill_gradient(low = "#EEF3FA", high = azul, name = "%") +
      scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = "#1F2937"), guide = "none") +
      labs(x = NULL, y = NULL) +
      theme(panel.grid = element_blank(), legend.position = "right")
  })

  output$g_mapa <- renderPlot({
    d <- entidades
    p <- ggplot(d, aes(col, -fila)) + coord_equal() + theme_void(base_size = 14) +
      theme(legend.position = "right")
    if (input$s4_color == "estimacion") {
      p <- p + geom_tile(aes(fill = estimacion), colour = "white", linewidth = 1.5) +
        scale_fill_gradient(low = "#EEF3FA", high = azul, name = "% que\ncompra")
    } else {
      p <- p + geom_tile(aes(fill = region), colour = "white", linewidth = 1.5) +
        scale_fill_manual(values = c("#3E6FA8", "#7FA3CF", "#B9CFE8", "#E3ECF6"), name = "Región")
    }
    p + geom_text(aes(label = abrev), size = 3.6, fontface = "bold") +
      geom_text(aes(label = sprintf("%.0f %%", estimacion), y = -fila - 0.3), size = 3)
  })

  # Simulador: supuestos explícitos, una razón por persona
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
    d$escenario <- factor(d$escenario, c("Observado", "Inercial", "Habilidades", "Omnicanal"))
    puente <- d[d$escenario == "Observado" & d$anio == max(d$anio[d$escenario == "Observado"]), ]
    puentes <- do.call(rbind, lapply(c("Inercial", "Habilidades", "Omnicanal"),
                                     function(e) transform(puente, escenario = e, li90 = estimacion, ls90 = estimacion)))
    d <- rbind(d, puentes)
    colores <- c(Observado = "#1F2937", Inercial = gris, Habilidades = "#7FA3CF", Omnicanal = azul)
    ggplot(d, aes(anio, estimacion, colour = escenario, fill = escenario)) +
      geom_ribbon(data = d[!is.na(d$li90), ], aes(ymin = li90, ymax = ls90),
                  alpha = 0.15, colour = NA) +
      geom_line(linewidth = 1.2) + geom_point(size = 2) +
      scale_colour_manual(values = colores, name = NULL) +
      scale_fill_manual(values = colores, guide = "none") +
      scale_x_continuous(breaks = 2019:2030) +
      scale_y_continuous(labels = function(x) paste0(x, " %")) +
      labs(x = NULL, y = NULL)
  })
}

shinyApp(ui, server)
