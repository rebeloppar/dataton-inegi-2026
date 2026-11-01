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
| 18 | Una parte de quienes ya compran en línea recoge en punto de venta, a todos los niveles de escolaridad | 18.5 % de compradores de 18+ recogió en punto de venta (15-19 % por escolaridad, salvo "sin escolaridad", imprecisa); 28 % recibió entrega directa del vendedor (32 % con primaria vs. 25.5 % con superior) | `analisis/D_propuesta/04_medios_entrega.R` |
_Cifras 1-8 y 11 actualizadas el 2026-10-25 con la escolaridad según el INEGI (decisiones 34-37)._

**Hallazgo principal (borrador):** Entre usuarios adultos de internet, la escolaridad y la edad son los factores más asociados a comprar en línea (~40 pp entre extremos, con todo lo demás igual). La escolaridad además explica la mayor parte de las brechas indígena y por discapacidad, y su efecto es prácticamente el mismo a nivel nacional que al analizar únicamente las ciudades de 100 mil habitantes o más, y con cualquiera de las tres formas de medir la escolaridad.

**Lectura de las barreras (hallazgos 12-15):** parece haber dos tipos de quienes no compran: quienes **no pueden** (habilidades; personas mayores y de baja escolaridad) y quienes **no quieren** (preferencia por lo presencial; más común con mayor escolaridad). La ENDUTIH no permite separarlo del todo: se presenta como interpretación y limitación.

**Punto ciego:** todo lo anterior es entre quienes ya usan internet. Falta ver quién no se conecta.

## 2026-10-30

| # | Hallazgo | Cifra clave | Fuente |
|---|---|---|---|
| 19 | La lectura "no pueden / no quieren" se sostiene por estrato, pero es más débil que por escolaridad | "No sabe cómo": bajo 24.7 % → alto 11.2 % (13.5 pp; por escolaridad son 36.6 pp); "prefiere en persona": bajo 36.1 % → alto 51.5 % (de quienes no compran) | `analisis/B_barreras/01_razones_por_grupo.R` |
| 20 | Por región se repite en los extremos: el Sur se parece a los que "no pueden" y el Norte a los que "no quieren" (preliminar, región temporal) | Sur: "no sabe cómo" 22.1 %, "en persona" 37.4 %; Norte: 14.9 % y 55.3 %. El Centro tiene la mayor desconfianza en la seguridad (13.2 %) | `analisis/B_barreras/01_razones_por_grupo.R` (región TEMPORAL, decisión 48) |
| 21 | Los medios de pago son una barrera marginal en todos los estratos y regiones | Sin tarjeta o banca en línea ≤ 4.0 %; falta de recursos ≤ 7.3 % (máximo en estrato bajo) | `analisis/B_barreras/01_razones_por_grupo.R` |
| 22 | Pocos compradores tienen problemas, y los grupos rezagados que compran no tienen más que los demás | 8.8 % de compradores de 18+ tuvo algún problema (IC 90 %: 8.2–9.4). Sin diferencias claras por edad, sexo ni localidad; la única clara va al revés: superior +3.1 pp vs. primaria o menos | `analisis/B_barreras/03_problemas_compradores.R` (secciones 1 y 3a) |
| 23 | El problema principal al comprar es la entrega, no la seguridad del pago, y pesa más en lo rural | Entrega 58.7 %, calidad 33.1 %, estado del producto 25.3 %, seguridad del pago 17.8 % (de quienes tuvieron problema). Entrega en < 2,500 hab.: 73.3 % vs. 57.1 % (+16.1 pp, clara) | `analisis/B_barreras/03_problemas_compradores.R` (secciones 2 y 3b) |
| 24 | El tipo de problema cambia con la edad y el sexo: los jóvenes reportan más entrega; 35-54, más calidad; las mujeres, menos problemas con el producto | Entrega: 18-34 65.8 % → 35-54 54.9 % → 55+ 36.9 % (claras); calidad: 18-34 28.3 % vs. 35-54 38.6 % (clara). Mujeres vs. hombres: calidad −6.6 pp, estado del producto −7.9 pp (claras) | `analisis/B_barreras/03_problemas_compradores.R` (sección 3b) |
| 25 | Limitación: P5 describe a quienes ya compran, un grupo seleccionado | P7_26 solo incluye a quienes compraron en los últimos 12 meses: los rezagados que compran ya superaron la barrera de habilidades, y quien dejó de comprar por una mala experiencia no aparece. Tipos de problema en "primaria o menos": 49 casos, solo indicios | `analisis/B_barreras/03_problemas_compradores.R` |
| 26 | En 55+, las mujeres que no compran dicen más "no sé cómo" que los hombres, y no se explica por escolaridad, edad ni territorio | "No sabe cómo": mujeres 30.0 % vs. hombres 23.7 % (+6.3 pp, clara); con controles del modelo completo: +5.9 pp (IC 90 %: 3.7–8.0). En 55+, las mujeres prefieren algo menos lo presencial: −4.0 pp (clara) | `analisis/B_barreras/04_genero_55mas.R` (secciones 1 y 3) |
| 27 | La brecha de género en "no sé cómo" está en todas las edades: no es mayor en 55+ que en 18-54 | Con controles: 18-54 +4.0 pp (IC 90 %: 2.7–5.2); todos 18+ +4.5 pp (3.4–5.6). Brecha 55+ menos brecha 18-54: +2.3 pp (IC 90 %: −0.3 a 4.9, no clara). Medios de pago sin diferencia por sexo en 55+ | `analisis/B_barreras/04_genero_55mas.R` (secciones 2 y 3) |
| 28 | Con las regiones del CEEY (decisión 62) el hallazgo 20 se sostiene igual: Norte, Centro y Sur no cambian de estados. Las dos regiones nuevas se parecen más al Sur que al Norte: "no sabe cómo" alto y "en persona" medio | "No sabe cómo": Sur 22.1 %, Norte-occidente 20.8 %, Centro-norte 19.5 %, Centro 12.8 %, Norte 14.9 %. "En persona": Norte 55.3 %, Centro 47.1 %, Norte-occidente 45.0 %, Centro-norte 43.2 %, Sur 37.4 % | `analisis/B_barreras/01_razones_por_grupo.R` (actualizado por Rebeca) |

**Lectura de P5 (hallazgos 22-25):** la experiencia de compra no explica por qué los grupos rezagados compran menos: quienes ya compran tienen pocos problemas y parecidos entre grupos. La barrera está **antes** de la primera compra (habilidades, preferencia). El problema que sí aparece es **logístico** (entrega), sobre todo en lo rural, lo que conecta con la penalización rural (hallazgo 6) y con la recolección en punto de venta (hallazgo 18).

**Lectura de género (hallazgos 26-27):** la brecha de género en "no sé cómo" es real (se sostiene con controles) y está en todas las edades, no solo en personas mayores. Matiza la decisión 47: la brecha en **comprar** crece con la edad (hallazgo 5), pero la brecha en la **razón** "no sé cómo" no. Para la propuesta, el enfoque de género en habilidades debe llegar también a mujeres menores de 55.