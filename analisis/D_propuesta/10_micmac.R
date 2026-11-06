# =============================================================================
# Frente D (con B) — MICMAC simplificado para los escenarios 2030 (decisiones 88 y 92)
# Cada impulsor sale de un hallazgo del diagnóstico. La matriz dice cuánto
# influye el impulsor de la FILA en el de la COLUMNA: 0 nada, 1 poco,
# 2 moderado, 3 fuerte. Es un juicio del equipo, no una estimación: se edita
# aquí y se vuelve a correr.
# Influencia = suma de la fila; dependencia = suma de la columna.
# Los dos ejes de los escenarios son los impulsores motrices (mucha influencia,
# poca dependencia) que además son inciertos (pueden ir para un lado o el otro).
# Envejecimiento de la población se quitó en la decisión 92.
# Escribe: dashboard/datos/s6_impulsores.csv, dashboard/datos/s6_ejes.csv
# =============================================================================

impulsores <- data.frame(
  clave = c("HAB", "TIE", "PAG", "ESC", "RUR", "MOV", "PUB", "EFE", "CON"),
  impulsor = c("Habilidades digitales de quienes no compran",
               "Tiendas que acompañan la compra en línea",
               "Práctica con pagos en línea",
               "Escolaridad de la población",
               "Tiendas de cadena fuera de las ciudades",
               "Conectividad móvil y precio de los datos",
               "Programas públicos de inclusión digital",
               "Pagar en efectivo una compra en línea",
               "Protección al consumidor y devoluciones"),
  categoria = c("Social", "Económico", "Social", "Social", "Económico",
                "Tecnológico", "Político", "Económico", "Legal"),
  hallazgo = c("«No sé cómo»: 22.7 % del grupo que no compra (hallazgos 12-13)",
               "18.5 % de compradores ya recoge en tienda; hay kioscos desde 2018 (decisión 70)",
               "Las mujeres pagan menos en línea: −5.8 pp con controles (hallazgo 36)",
               "Superior vs. primaria: +42.5 pp (hallazgo 1)",
               "Localidad rural: −11.2 pp con controles (hallazgo 30)",
               "85 % de los adultos ya usa internet; la brecha está en comprar (hallazgo 29)",
               "INEA, INAPAM y Centros de Inclusión Digital (mapa de actores)",
               "No tener tarjeta: 2.6 % de quienes no compran (hallazgo 15)",
               "Desconfianza en la seguridad: 10.8 % de quienes no compran"),
  incierto = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE))

# Fila influye en columna        HAB TIE PAG ESC RUR MOV PUB EFE CON
m <- matrix(c(
  0, 2, 3, 0, 0, 1, 1, 1, 1,   # HAB habilidades
  3, 0, 2, 0, 2, 0, 1, 2, 1,   # TIE tiendas que acompañan
  2, 1, 0, 0, 0, 0, 0, 1, 1,   # PAG pagos
  3, 1, 2, 0, 0, 1, 1, 0, 0,   # ESC escolaridad
  1, 2, 0, 0, 0, 1, 1, 1, 0,   # RUR tiendas fuera de ciudades
  1, 1, 1, 0, 0, 0, 0, 0, 0,   # MOV conectividad
  3, 1, 1, 1, 0, 1, 0, 0, 1,   # PUB programas públicos
  0, 1, 1, 0, 0, 0, 0, 0, 0,   # EFE efectivo
  1, 1, 2, 0, 0, 0, 0, 1, 0),  # CON protección al consumidor
  nrow = 9, byrow = TRUE, dimnames = list(impulsores$clave, impulsores$clave))
stopifnot(all(diag(m) == 0))

impulsores$influencia  <- rowSums(m)
impulsores$dependencia <- colSums(m)
ci <- mean(impulsores$influencia); cd <- mean(impulsores$dependencia)
impulsores$cuadrante <- with(impulsores, ifelse(influencia >= ci & dependencia < cd, "Motriz",
                                         ifelse(influencia >= ci, "De enlace",
                                         ifelse(dependencia >= cd, "Dependiente", "Autónomo"))))
print(impulsores[order(-impulsores$influencia), c("clave", "influencia", "dependencia", "cuadrante", "incierto")],
      row.names = FALSE)

# Ejes: los dos más influyentes entre los inciertos
cand <- impulsores[impulsores$incierto, ]
ejes <- head(cand[order(-cand$influencia, cand$dependencia), ], 2)
ejes <- ejes[order(match(ejes$clave, c("HAB", "TIE"))), ]   # eje 1 (vertical): habilidades
cat("\nEjes de los escenarios:", paste(ejes$impulsor, collapse = " | "), "\n")

write.csv(data.frame(impulsores[, c("impulsor", "categoria", "influencia", "dependencia")], prueba = FALSE),
          file.path("dashboard", "datos", "s6_impulsores.csv"), row.names = FALSE, fileEncoding = "UTF-8")
polos <- list(HAB = c("Aprenden y confían", "Siguen sin saber cómo"),
              TIE = c("La tienda acompaña la compra en línea", "La tienda sigue aparte"))
stopifnot(all(ejes$clave %in% names(polos)))   # si cambian los ejes, escribir sus polos
write.csv(data.frame(eje = 1:2,
                     nombre = c("Habilidades y confianza de quienes hoy no compran",
                                "Tiendas que acompañan la compra en línea")[match(ejes$clave, names(polos))],
                     polo_alto = sapply(polos[ejes$clave], `[`, 1), polo_bajo = sapply(polos[ejes$clave], `[`, 2),
                     prueba = FALSE),
          file.path("dashboard", "datos", "s6_ejes.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cat("Listo: s6_impulsores.csv y s6_ejes.csv\n")
