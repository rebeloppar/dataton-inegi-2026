# =============================================================================
# Frente A — ¿Cómo ha cambiado lo que se compra? (pregunta 3 del reto)
# Solo cifras publicadas por el INEGI: no tenemos los microdatos de años
# anteriores. Universo del INEGI: personas usuarias de internet de 6+ que
# compraron en línea; % que compró cada categoría. Solo las categorías que
# aparecen en los cuatro años, con el mismo nombre.
# Fuentes:
#   2022: INEGI, comunicado de prensa de la ENDUTIH 2022 (junio de 2023)
#   2023: INEGI, comunicado de prensa 372/24, ENDUTIH 2023 (13 de junio de 2024)
#   2024: INEGI, presentación de resultados de la ENDUTIH 2024
#   2025: INEGI, reporte de resultados de la ENDUTIH 2025, gráfica 9; coincide
#         con nuestro cálculo con los microdatos (analisis/A_conexion/02_hogar_y_categorias.R)
# Escribe: dashboard/datos/s1_tendencia.csv
# =============================================================================

publicadas <- rbind(
  data.frame(categoria = "Uso e higiene personal (incl. ropa)", anio = 2022:2025, estimacion = c(74.3, 76.2, 77.9, 78.7)),
  data.frame(categoria = "Artículos para el hogar",             anio = 2022:2025, estimacion = c(40.7, 41.9, 45.8, 50.6)),
  data.frame(categoria = "Alimentos y bebidas",                 anio = 2022:2025, estimacion = c(37.0, 34.7, 33.0, 37.4)),
  data.frame(categoria = "Artículos de salud",                  anio = 2022:2025, estimacion = c(19.4, 19.0, 18.6, 23.7)))
print(reshape(publicadas, idvar = "categoria", timevar = "anio", direction = "wide"), row.names = FALSE)
write.csv(data.frame(publicadas, prueba = FALSE), file.path("dashboard", "datos", "s1_tendencia.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\nListo: dashboard/datos/s1_tendencia.csv\n")
