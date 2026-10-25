# Bitácora de hallazgos — Datatón INEGI 2026

Qué encontramos, con su cifra y el script que lo produce. Universo: usuarios de internet de 18 años o más (ENDUTIH 2025), salvo que se indique otra cosa. Cifras ponderadas; IC al 90 %.

## 2026-10-25

| # | Hallazgo | Cifra clave | Fuente |
|---|---|---|---|
| 1 | Escolaridad y edad son los factores más asociados a comprar en línea | Superior vs. primaria: +42.7 pp; 65+ vs. 18-24: −41.3 pp (modelo completo) | `analisis/diagnostico/04_regresion.R` |
| 2 | La escolaridad explica la mayor parte de la brecha indígena | Brecha indígena: −12.9 pp sin controles → −5.6 con escolaridad → −1.9 con todo | `analisis/diagnostico/04_regresion.R` |
| 3 | La brecha por discapacidad se explica casi toda por escolaridad (y edad) | −4.0 pp → −0.9 con escolaridad; no significativa en el modelo completo | `analisis/diagnostico/04_regresion.R` |
| 4 | Brecha de género pequeña pero persistente; no se explica por composición | −3.8 pp sin controles → −3.2 con todo | `analisis/diagnostico/04_regresion.R` |
| 5 | La brecha de género crece con la edad | Mujer − hombre: −2.4 pp (18-24) vs. −7.0 pp (65+) | `analisis/diagnostico/02_cruces.R` (tabla B) |
| 6 | Penalización rural persistente aun con controles | Menos de 2,500 hab. vs. 100 mil y más: −11.1 pp (modelo completo) | `analisis/diagnostico/04_regresion.R` |
| 7 | El efecto del estrato es real pero en parte era ruralidad | Alto vs. bajo: +18.1 pp nacional → +14.7 pp solo ciudades | `analisis/diagnostico/04_regresion.R` (robustez) |
| 8 | Escolaridad robusta: no cambia en ciudades | +42.7 pp nacional vs. +42.4 pp solo ciudades | `analisis/diagnostico/04_regresion.R` (robustez) |
| 9 | Las razones para no comprar son de preferencia, no de medios de pago | Prefiere en persona ~45 %, no le interesa ~24 %; sin tarjeta ~3 %, sin recursos ~5 % | `analisis/diagnostico/02_cruces.R` (tabla C) |
| 10 | "No sabe cómo hacerlo" pesa más en mujeres | Mujeres 19.0 % vs. hombres 14.1 % de quienes no compran | `analisis/diagnostico/02_cruces.R` (tabla C) |

**Hallazgo principal (borrador):** Entre usuarios adultos de internet, la escolaridad y la edad son los factores más asociados a comprar en línea (~40 pp entre extremos, con todo lo demás igual). La escolaridad además explica la mayor parte de las brechas indígena y por discapacidad, y su efecto se sostiene solo en ciudades.

**Punto ciego:** todo lo anterior es entre quienes ya usan internet. Falta ver quién no se conecta.
