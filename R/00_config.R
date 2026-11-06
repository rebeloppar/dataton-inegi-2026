# =============================================================================
# Datatón INEGI 2026 — Configuración común
# Lo carga R/prep_endutih.R. Paquetes, rutas y constantes compartidas.
# Los cambios se anotan en bitacoras/decisiones.md.
# =============================================================================

# --- Paquetes ------------------------------------------------------------------
# Primera vez: install.packages(c("survey", "marginaleffects"))
suppressMessages(library(survey))
options(survey.lonely.psu = "adjust")

# --- Directorio de trabajo: la raíz del repositorio ----------------------------
# Abre el proyecto con dataton-inegi-2026.Rproj y esto se cumple solo.
if (!file.exists("dataton-inegi-2026.Rproj")) stop(
  "No estás en la raíz del repositorio. Abre dataton-inegi-2026.Rproj en ",
  "RStudio (o usa setwd() hacia la carpeta del repositorio).\n",
  "Carpeta actual: ", getwd())

# --- Rutas ---------------------------------------------------------------------
# Los datos crudos NO se suben al repositorio: ver datos/README.md
rutas <- list(
  endutih = file.path("datos", "endutih_2025", "conjunto_de_datos")
  # enigh = file.path("datos", "enigh_2024", ...),   # frente C
  # eic   = file.path("datos", "eic_2025", ...)      # frente C
)

revisar_datos <- function(ruta) {
  if (!file.exists(ruta)) stop(
    "No encuentro: ", ruta, "\nDescarga los datos como indica datos/README.md")
  ruta
}

# --- Constantes ----------------------------------------------------------------
# IC al 90 % (como publica el INEGI): es bilateral, deja 5 % en cada cola,
# por eso el valor crítico es el percentil 95 de la normal (1.645), no el 90.
z90 <- qnorm(0.95)

entidades <- c("Aguascalientes", "Baja California", "Baja California Sur",
  "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
  "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
  "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
  "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
  "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", "Zacatecas")

dir.create("resultados", showWarnings = FALSE)
