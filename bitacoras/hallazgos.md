# Bitácora de hallazgos — Datatón INEGI 2026

Qué encontramos, con su cifra y el script que lo produce. Universo: usuarios de internet de 18 años o más (ENDUTIH 2025), salvo que se indique otra cosa. Cifras ponderadas; IC al 90 %.

## 2026-10-25

| # | Hallazgo | Cifra clave | Fuente |
|---|---|---|---|
| 1 | Escolaridad y edad son los factores más asociados a comprar en línea | Superior vs. preescolar o primaria: +42.5 pp; 65+ vs. 18-24: −41.8 pp (modelo completo) | `analisis/diagnostico/04_regresion.R` |
| 2 | La escolaridad explica la mayor parte de la brecha indígena | Brecha indígena: −12.9 pp sin controles → −5.5 con escolaridad → −1.9 con todo | `analisis/diagnostico/04_regresion.R` |
| 3 | La brecha por discapacidad se explica casi toda por escolaridad (y edad) | −4.0 pp → −1.0 con escolaridad; no significativa en el modelo completo | `analisis/diagnostico/04_regresion.R` |
| 4 | Brecha de género pequeña pero persistente; no se explica por composición | −3.8 pp sin controles → −3.4 con todo | `analisis/diagnostico/04_regresion.R` |
| 5 | La brecha de género crece con la edad | Mujer − hombre: −2.4 pp (18-24) vs. −7.0 pp (65+) | `analisis/diagnostico/02_cruces.R` (tabla B) |
| 6 | Penalización rural persistente aun con controles | Menos de 2,500 hab. vs. 100 mil y más: −11.2 pp (modelo completo) | `analisis/diagnostico/04_regresion.R` |
| 7 | El efecto del estrato es real pero en parte era ruralidad | Alto vs. bajo: +17.7 pp nacional → +14.5 pp solo ciudades | `analisis/diagnostico/04_regresion.R` (robustez) |
| 8 | Escolaridad robusta: no cambia en ciudades | Superior vs. preescolar o primaria: +42.5 pp nacional vs. +41.9 pp solo ciudades | `analisis/diagnostico/04_regresion.R` (robustez) |
| 9 | Las razones para no comprar son de preferencia, no de medios de pago | Prefiere en persona ~45 %, no le interesa ~24 %; sin tarjeta ~3 %, sin recursos ~5 % | `analisis/diagnostico/02_cruces.R` (tabla C) |
| 10 | "No sabe cómo hacerlo" pesa más en mujeres | Mujeres 19.0 % vs. hombres 14.1 % de quienes no compran | `analisis/diagnostico/02_cruces.R` (tabla C) |
| 11 | Las conclusiones no dependen de cómo se agrupa la escolaridad | Brecha indígena con escolaridad: −5.5 (principal), −5.9 (INEGI tal cual), −5.4 (años); con todo: −1.9, −2.0, −1.8. Años de escolaridad: 16 vs. 6 años, +39.3 pp | `analisis/diagnostico/04_regresion.R` (robustez) |
| 12 | "No sabe cómo hacerlo" es la barrera que más cambia entre grupos: cae con la escolaridad y sube con la edad | Sin escolaridad 43.5 % → superior 6.9 %; 18-24: 7.0 % → 65+: 32.1 % (de quienes no compran) | `analisis/B_barreras/01_razones_por_grupo.R` |
| 13 | "Prefiere en persona" va en sentido contrario: sube con la escolaridad | Sin escolaridad 33.2 % → superior 52.0 %; por edad varía poco (41-49 %) | `analisis/B_barreras/01_razones_por_grupo.R` |
| 14 | La desconfianza en la seguridad es secundaria pero no marginal en escolaridad alta | 13.6 % en superior (segunda razón concreta tras "en persona"); 7.5 % en primaria | `analisis/B_barreras/01_razones_por_grupo.R` |
| 15 | Limitación: los datos no permiten saber qué esconde "prefiere en persona" | 83 % la da como única razón, pero 87 % de quienes no compran dan una sola razón, sea cual sea: es el patrón de respuesta, no algo propio de esa opción | `analisis/B_barreras/02_coocurrencia_en_persona.R` |
| 16 | Alcance de las brechas candidatas (usuarios de 18+ que no compran en línea, en millones) | Mujeres 25.3 · indígenas 15.5 · primaria o menos 11.3 · rural (< 2,500 hab.) 10.9 · 65+ 5.6 · discapacidad 4.5. Total de usuarios de 18+: 81.4 M | `analisis/D_propuesta/01_alcance.R` |
| 17 | La baja escolaridad no es solo un tema de personas mayores | De 11.3 M de usuarios de 18+ con primaria o menos que no compran, 22 % tiene 65+ y 56 % menos de 55; "no sé cómo" ronda 20 % aun en 18-44 | `analisis/D_propuesta/03_baja_escolaridad_por_edad.R` |
_Cifras 1-8 y 11 actualizadas el 2026-10-25 con la escolaridad según el INEGI (decisiones 34-37)._

**Hallazgo principal (borrador):** Entre usuarios adultos de internet, la escolaridad y la edad son los factores más asociados a comprar en línea (~40 pp entre extremos, con todo lo demás igual). La escolaridad además explica la mayor parte de las brechas indígena y por discapacidad, y su efecto es prácticamente el mismo a nivel nacional que al analizar únicamente las ciudades de 100 mil habitantes o más, y con cualquiera de las tres formas de medir la escolaridad.

**Lectura de las barreras (hallazgos 12-15):** parece haber dos tipos de quienes no compran: quienes **no pueden** (habilidades; personas mayores y de baja escolaridad) y quienes **no quieren** (preferencia por lo presencial; más común con mayor escolaridad). La ENDUTIH no permite separarlo del todo: se presenta como interpretación y limitación.

**Punto ciego:** todo lo anterior es entre quienes ya usan internet. Falta ver quién no se conecta.
