# =============================================================================
# Frente B — Funciones para escribir los CSV de la sección 3 del dashboard
# Las usan 01_razones_por_grupo.R (s3_razones.csv) y 03_problemas_compradores.R
# (s3_problemas.csv). Formato: dashboard/README.md (decisión 55).
# No se corre sola: cada script la carga con source() después de prep_endutih.R.
# =============================================================================

# Cortes del dashboard: nombre que ve el jurado (grupo_var) = variable del prep.
# Escolaridad: ESCOLARIDAD_S1 ("Primaria o menos" une sin escolaridad y
# primaria), igual que la sección 1 y el grupo objetivo (decisiones 65 y 83).
cortes_dashboard <- c(
  "Nacional"    = "NACIONAL",
  "Escolaridad" = "ESCOLARIDAD_S1",
  "Edad"        = "GRUPO_EDAD",
  "Sexo"        = "SEXO",
  "Localidad"   = "TLOC_ASC",
  "Estrato"     = "ESTRATO",
  "Región"      = "REGION")

# Agrega al diseño las dos variables que solo usa el dashboard:
#   NACIONAL = un solo grupo (la fila nacional)
#   TLOC_ASC = tamaño de localidad de la más chica a la más grande
preparar_cortes <- function(d) {
  update(d,
         NACIONAL = factor(rep("Nacional", length(SEXO))),
         TLOC_ASC = factor(TLOC, levels = rev(levels(TLOC))))
}

# % (con IC 90 %) de cada variable 0/1 de `vars`, por cada corte del dashboard.
# `etiquetas`: nombre que se ve en la gráfica, con los nombres de `vars`.
estimar_por_cortes <- function(d, vars, etiquetas) {
  f <- as.formula(paste("~", paste(vars, collapse = " + ")))
  do.call(rbind, lapply(names(cortes_dashboard), function(gv) {
    v <- cortes_dashboard[[gv]]
    r <- svyby(f, as.formula(paste0("~", v)), d, svymean,
               na.rm = TRUE, vartype = c("se", "cv"))
    grupos <- as.character(r[[v]])
    orden  <- match(grupos, levels(d$variables[[v]]))
    # Con una sola variable, svyby nombra las columnas "se" y "cv"
    col_se <- function(x) if (length(vars) == 1) "se" else paste0("se.", x)
    col_cv <- function(x) if (length(vars) == 1) "cv" else paste0("cv.", x)
    do.call(rbind, lapply(vars, function(x) {
      est <- r[[x]]; se <- r[[col_se(x)]]
      data.frame(grupo_var  = gv,
                 grupo      = grupos,
                 orden      = orden,
                 categoria  = unname(etiquetas[[x]]),
                 estimacion = round(100 * est, 1),
                 li90       = round(100 * pmax(0, est - z90 * se), 1),
                 ls90       = round(100 * pmin(1, est + z90 * se), 1),
                 cv_pct     = round(100 * r[[col_cv(x)]], 1),
                 prueba     = FALSE,
                 row.names  = NULL)
    }))
  }))
}

# Escribe el CSV en dashboard/datos/ con las columnas exactas del README.
# `col` = nombre de la columna de categorías ("razon" o "problema").
# Avisa en la consola qué celdas son poco precisas (CV > 30 %): el heatmap no
# las marca, así que no se citan en el texto.
escribir_dashboard <- function(tab, archivo, col) {
  imprecisas <- tab[is.na(tab$cv_pct) | tab$cv_pct > 30, ]
  names(tab)[names(tab) == "categoria"] <- col
  tab <- tab[, c("grupo_var", "grupo", "orden", col,
                 "estimacion", "li90", "ls90", "prueba")]
  ruta <- file.path("dashboard", "datos", archivo)
  write.csv(tab, ruta, row.names = FALSE, fileEncoding = "UTF-8")
  cat(sprintf("\nEscrito %s (%d filas).\n", ruta, nrow(tab)))
  if (nrow(imprecisas) > 0) {
    cat("Celdas poco precisas (CV > 30 %): se ven en el heatmap, pero no se citan.\n")
    print(imprecisas[, c("grupo_var", "grupo", "categoria", "estimacion", "cv_pct")],
          row.names = FALSE)
  }
  invisible(tab)
}
