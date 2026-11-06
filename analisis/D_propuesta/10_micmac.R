# =============================================================================
# Frente D (con B) — MICMAC simplificado para los escenarios 2030 (decisiones 88, 92 y 103)
# Cada impulsor sale de un hallazgo del diagnóstico. La matriz dice cuánto
# influye el impulsor de la FILA en el de la COLUMNA: 0 nada, 1 poco,
# 2 moderado, 3 fuerte. Es un juicio del equipo, no una estimación: se edita
# aquí y se vuelve a correr.
# Influencia = suma de la fila; dependencia = suma de la columna.
# Los dos ejes de los escenarios son los impulsores motrices (mucha influencia,
# poca dependencia) que además son inciertos (pueden ir para un lado o el otro).
# Envejecimiento de la población se quitó en la decisión 92. La lista sale del
# PESTEL del equipo (decisión 103): sin MOV, CON pasa a SEG, RUR incluye las
# entregas, y entran PRE (la puerta 2) y EST (estructural, no incierto).
# Escribe: dashboard/datos/s6_impulsores.csv, dashboard/datos/s6_ejes.csv
# =============================================================================

impulsores <- data.frame(
  clave = c("HAB", "TIE", "PAG", "ESC", "RUR", "PUB", "EFE", "SEG", "PRE", "EST"),
  impulsor = c("Habilidades digitales de quienes no compran",
               "Tiendas que acompañan la compra en línea",
               "Práctica con pagos en línea",
               "Escolaridad de la población",
               "Tiendas y entregas fuera de las ciudades",
               "Programas públicos de inclusión digital",
               "Pagar en efectivo una compra en línea",
               "Confianza en la seguridad de comprar en línea",
               "Preferencia por lo presencial",
               "Ingreso y estrato socioeconómico"),
  categoria = c("Social", "Económico", "Social", "Social", "Económico",
                "Político", "Económico", "Legal", "Social", "Económico"),
  hallazgo = c("«No sé cómo»: 22.7 % del grupo que no compra (hallazgos 12-13)",
               "18.5 % de compradores ya recoge en tienda; hay kioscos desde 2018 (decisión 70)",
               "Las mujeres pagan menos en línea: −5.8 pp con controles (hallazgo 36)",
               "Superior vs. primaria: +42.5 pp (hallazgo 1)",
               "Localidad rural: −11.2 pp con controles (hallazgo 6); la entrega es el problema más común de quien compra (hallazgo 23)",
               "INEA, INAPAM y Centros de Inclusión Digital (mapa de actores)",
               "No tener tarjeta: 2.6 % de quienes no compran (hallazgo 21)",
               "Desconfía de la seguridad: 7.4 % (primaria o menos) a 13.6 % (superior) de quienes no compran (hallazgo 14)",
               "«Prefiero en persona»: 36 % en el estrato bajo y 52 % en el alto; quienes lo dicen casi no hacen transacciones (hallazgos 13 y 34)",
               "Estrato alto vs. bajo: +14.5 pp en ciudades, con controles (hallazgo 7)"),
  incierto = c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE))

# Fila influye en columna. Puntajes revisados por el equipo (decisión 107):
# nueve cambios sobre el borrador de la decisión 103.
#                                HAB TIE PAG ESC RUR PUB EFE SEG PRE EST
m <- matrix(c(
  0, 2, 3, 0, 0, 1, 1, 1, 2, 0,   # HAB habilidades
  2, 0, 1, 0, 2, 0, 2, 1, 2, 0,   # TIE tiendas que acompañan
  2, 1, 0, 0, 0, 0, 1, 1, 1, 0,   # PAG pagos
  3, 1, 2, 0, 0, 1, 0, 1, 2, 1,   # ESC escolaridad
  1, 2, 0, 0, 0, 1, 1, 0, 1, 0,   # RUR tiendas y entregas fuera de ciudades
  3, 1, 1, 1, 0, 0, 0, 1, 0, 0,   # PUB programas públicos
  0, 1, 1, 0, 0, 0, 0, 0, 1, 0,   # EFE efectivo
  1, 1, 1, 0, 0, 0, 1, 0, 1, 0,   # SEG confianza en la seguridad (antes CON)
  1, 1, 1, 0, 0, 0, 1, 0, 0, 0,   # PRE preferencia por lo presencial
  2, 1, 2, 0, 0, 0, 1, 1, 2, 0),  # EST ingreso y estrato
  nrow = 10, byrow = TRUE, dimnames = list(impulsores$clave, impulsores$clave))
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
