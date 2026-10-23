# reference/hungary — valores y fracciones publicados (verdad para `--stage verify`)

Generado por `tools/build_reference.R` el 2026-10-23 10:42 con R version 4.6.1 (2026-06-24) (readr 2.2.0, dplyr 1.2.1, xml2 1.6.0, png 0.1.9) para **Hungary** (OCT15_v9 / figuras finales 2026-10-16).
Nada de esta carpeta se edita a mano: se regenera con

    Rscript src/src_reports/tools/build_reference.R

con `reference_src_dir` (finales/datos), `published_dir` (figuras publicadas) y `path2EU` (data4web) en `config/local.yml`.

## Convenciones (contratos §4.4 y §4.6)

- `Figure_NN_slug.csv`: `order, kind, grouping, label, value, value2, text, text2, frac, frac2, tol` (+ `tol2` en la 3; `tol_xy, x, y` en la red; `lo, hi` en las tarjetas; `n` en el scatter y el upset). `text`/`text2` es exactamente lo impreso; `frac`/`frac2` la fracción efectivamente dibujada en la figura publicada (NA si no hay barra medible): barras = valor/escala de la pista (100, o 10 en la 4); embudos (8, 34) = barra/la más ancha; columnas (30, 37) = valor/vmax, con vmax el máximo de las dos series de la figura (`report_charts.R:259-266`).
- Tolerancias: `tol` = de `value` por fila: 0.6 pp; 0.06 en escala 0-10 (fig 4); 0.05 en lifts (aristas de la 5). `tol2` = de `value2` (si falta o es NA, la `tol` de la fila): 0.06 en la severidad 0-10 de la 3. `tol_xy` = de `x` e `y` (si falta o es NA, 0.01): 0.01 en los nodos de la 5. `lo`, `hi` y `n`, si están, se comparan exactos.
- Niveles: **texto_publicado** = CSV de `finales/datos` (valores y fracciones de la figura publicada; en las figuras con filas en `00-verificacion_valores.csv`, `value` es la columna `data` = valor impreso como número, y donde falta `v1_frac·escala`); en las tarjetas (32, 41, 42), valores de A03/A04/A05 que coinciden con `finales/datos` (2 decimales, |Δ| ≤ 0.005) y textos y deltas según `charts5.py`; **tabla_analisis** = tablas de `analisis adicionales` (F01, F02, F03, F08 + R04); **svg** = leído del SVG (`cooccurrence_network.svg`: coordenadas invertidas del lienzo de ggplot y lifts invertidos del ancho de trazo); **transcrito** = tarjetas sin `finales/datos` con que cotejarlas (textos según `charts5.py`, cotejados a ojo con el PNG) → **revisar**.
- grouped_bars/dumbbell: `grouping` = "" en `avg`, nombre del grupo en `header` y en sus `row`. sankey: `grouping` = columna ("1", "2", "3"), `label` = clave del nodo de `sankey_def`. cards: `grouping` = fila impresa, `label` = columna impresa, `value2` = valor crudo de la tarjeta de referencia (NA en ella), `text2` = delta de valores redondeados con signo "+", "−" (U+2212) o "±" y sufijo " points" ("" en la tarjeta de referencia), `lo`, `hi` = min/max de round(value) de la figura. matrix: `grouping` = fila ("People who also experienced"), `label` = columna ("Among people who experienced problems with"), `text2` = banda. network: nodos con `x`, `y` = coordenadas de datos del layout de `network_graph.R` tras pad, min_dist y hstretch, sin renormalizar (x ≈ 0.5 ± 0.575, y 0-1; algunos nodos caen fuera de [0, 1]); `label` de arista = "from|to" en el orden de `edges_all` (i < j en `cats`); sin `n`. upset: `label` de intersección = sus conjuntos en el orden fijo (Access to Assistance, Access to Information, Resolution Process, Problem Resolved) unidos por " + "; intersecciones de izquierda a derecha como las dibuja ComplexUpset (`upset_data()` sobre réplicas round(pct·10): tamaño descendente con su desempate); conjuntos de arriba abajo como la matriz (orden fijo invertido). scatter: `grouping` = faceta, `value` = prevalencia, `value2` = severidad. severity_bars: `grouping` = serie (país | EU Average; el país primero), `label` = severidad.
- `eu_values.csv`: medias UE de data4web (`level == "eu"`, `Total Sample`) de los ids de la columna `eu_id` del manifiesto en las figuras con `metodo_ue` data4web; `value` en 0-100 sin redondear; `rp_time` está duplicado en el origen (`distinct`).
- `published/`: PNG publicados remuestreados a 96 dpi para `--stage compare` con `resample_to_dpi()` de `R/08_compare.R` (fondo blanco, bloques 3 × 3 y vecino más cercano hasta round(px · 96/300)), el mismo remuestreo que compare aplica a la figura nueva; mismo tamaño en macOS y Linux.
- Guarda de país: antes de escribir en `reference/<slug>/`, la herramienta comprueba que las fuentes de `config/local.yml` son del país (carpeta `output/viz/<país>`, `<país>_jg_upset.svg`, columna del país en F03.csv, títulos de `00-INDICE.csv`) y si no se detiene.
- Lectura de los CSV: `read_csv(f, na = "NA", col_types = cols(.default = col_character()))` y convertir a numérico `value, value2, frac, frac2, tol` (y `tol2, tol_xy, x, y, n, lo, hi`): con tipos automáticos las columnas vacías pasan a NA lógico y los `grouping` "1"/"2"/"3" a número. `""` (vacío) y `NA` significan lo mismo en `text`/`value` (nada impreso / suprimido).

## Figuras

| fig | slug | nivel | fuente(s) | filas | tol | observaciones |
|---|---|---|---|---|---|---|
| 01 | prevalence_nontrivial_problems | texto_publicado | finales/datos/Figure_01_prevalence_nontrivial_problems.csv; finales/datos/00-verificacion_valores.csv | 18 | 0.6 | 14 valores de 00-verificacion (\|data − frac·100\| máx 0.45); resto v1_frac·100 |
| 02 | prevalence_by_category_hu_eu | texto_publicado | finales/datos/Figure_02_prevalence_by_category_hu_eu.csv; finales/datos/00-verificacion_valores.csv | 12 | 0.6 | 24 valores de 00-verificacion (\|data − frac·100\| máx 0.46); resto v1_frac·100 |
| 03 | prevalence_severity_by_area | tabla_analisis | analisis adicionales/F02.csv | 24 | 0.6 · tol2 0.06 | 24 puntos; 3 sin severidad (value2 NA = suprimido); value2 con tol2 0.06 |
| 04 | cooccurrence_number_of_problems | texto_publicado | finales/datos/Figure_04_cooccurrence_number_of_problems.csv; finales/datos/00-verificacion_valores.csv | 17 | 0.06 | 13 valores de 00-verificacion (\|data − frac·10\| máx 0.04); resto v1_frac·10 |
| 05 | cooccurrence_network | svg | analisis adicionales/cooccurrence_network.svg | 78 | 0.05 / 0.60 · tol_xy 0.01 | 12 nodos + 66 aristas (todas las parejas: sí); lift 1.57-11.78; x, y del layout sin renormalizar (tol_xy 0.01) |
| 06 | cooccurrence_conditional_matrix | tabla_analisis | analisis adicionales/F01.csv | 144 | 0.6 | 144 celdas: 121 con valor, 11 N/A, 12 diagonal |
| 07 | severity_distribution_hu_eu | tabla_analisis | analisis adicionales/F03.csv | 22 | 0.6 | 22 barras sin texto impreso |
| 08 | justice_pathway_stages | texto_publicado | finales/datos/Figure_08_justice_pathway_stages.csv | 3 | 0.6 | etiquetas con salto de línea como se imprimen |
| 09 | access_information | texto_publicado | finales/datos/Figure_09_access_information.csv; finales/datos/00-verificacion_valores.csv | 36 | 0.6 | 26 valores de 00-verificacion (\|data − frac·100\| máx 0.50); resto v1_frac·100 |
| 10 | access_information_perception_experience | texto_publicado | finales/datos/Figure_10_access_information_perception_experience.csv; finales/datos/00-verificacion_valores.csv | 17 | 0.6 | 25 valores de 00-verificacion (\|data − frac·100\| máx 0.50); resto v1_frac·100 |
| 11 | pathway_assistance_sankey | texto_publicado | finales/datos/Figure_11_pathway_assistance_sankey.csv | 10 | 0.6 | definición assistance (standard_figures.R:128-177); value = texto publicado como número |
| 12 | access_assistance | texto_publicado | finales/datos/Figure_12_access_assistance.csv; finales/datos/00-verificacion_valores.csv | 36 | 0.6 | 22 valores de 00-verificacion (\|data − frac·100\| máx 0.50); resto v1_frac·100 |
| 13 | advisers_contacted | texto_publicado | finales/datos/Figure_13_advisers_contacted.csv | 8 | 0.6 | sin filas en 00-verificacion: value = v1_frac·100 |
| 14 | barriers_assistance | texto_publicado | finales/datos/Figure_14_barriers_assistance.csv | 8 | 0.6 | sin filas en 00-verificacion: value = v1_frac·100 |
| 15 | access_assistance_perception_experience | texto_publicado | finales/datos/Figure_15_access_assistance_perception_experience.csv; finales/datos/00-verificacion_valores.csv | 17 | 0.6 | 24 valores de 00-verificacion (\|data − frac·100\| máx 0.50); resto v1_frac·100 |
| 16 | pathway_drm_sankey | texto_publicado | finales/datos/Figure_16_pathway_drm_sankey.csv | 9 | 0.6 | definición drm (standard_figures.R:128-177); value = texto publicado como número |
| 17 | access_drm | texto_publicado | finales/datos/Figure_17_access_drm.csv; finales/datos/00-verificacion_valores.csv | 23 | 0.6 | 15 valores de 00-verificacion (\|data − frac·100\| máx 0.49); resto v1_frac·100 |
| 18 | drm_contacted | texto_publicado | finales/datos/Figure_18_drm_contacted.csv | 7 | 0.6 | sin filas en 00-verificacion: value = v1_frac·100 |
| 19 | barriers_drm | texto_publicado | finales/datos/Figure_19_barriers_drm.csv | 5 | 0.6 | sin filas en 00-verificacion: value = v1_frac·100 |
| 20 | timeliness | texto_publicado | finales/datos/Figure_20_timeliness.csv; finales/datos/00-verificacion_valores.csv | 23 | 0.6 | 17 valores de 00-verificacion (\|data − frac·100\| máx 0.49); resto v1_frac·100 |
| 21 | perceived_speed | texto_publicado | finales/datos/Figure_21_perceived_speed.csv; finales/datos/00-verificacion_valores.csv | 22 | 0.6 | 16 valores de 00-verificacion (\|data − frac·100\| máx 0.48); resto v1_frac·100 |
| 22 | perceived_fairness | texto_publicado | finales/datos/Figure_22_perceived_fairness.csv; finales/datos/00-verificacion_valores.csv | 23 | 0.6 | 17 valores de 00-verificacion (\|data − frac·100\| máx 0.50); resto v1_frac·100 |
| 23 | cost_coverage | texto_publicado | finales/datos/Figure_23_cost_coverage.csv; finales/datos/00-verificacion_valores.csv | 23 | 0.6 | 17 valores de 00-verificacion (\|data − frac·100\| máx 0.43); resto v1_frac·100 |
| 24 | perceived_costliness | texto_publicado | finales/datos/Figure_24_perceived_costliness.csv; finales/datos/00-verificacion_valores.csv | 22 | 0.6 | 16 valores de 00-verificacion (\|data − frac·100\| máx 0.49); resto v1_frac·100 |
| 25 | outcome | texto_publicado | finales/datos/Figure_25_outcome.csv; finales/datos/00-verificacion_valores.csv | 23 | 0.6 | 17 valores de 00-verificacion (\|data − frac·100\| máx 0.43); resto v1_frac·100 |
| 26 | satisfaction | texto_publicado | finales/datos/Figure_26_satisfaction.csv; finales/datos/00-verificacion_valores.csv | 23 | 0.6 | 13 valores de 00-verificacion (\|data − frac·100\| máx 0.49); resto v1_frac·100 |
| 27 | outcome_by_access | texto_publicado | finales/datos/Figure_27_outcome_by_access.csv | 9 | 0.6 | sin filas en 00-verificacion: value = v1_frac·100 |
| 28 | justice_gap | texto_publicado | finales/datos/Figure_28_justice_gap.csv; finales/datos/00-verificacion_valores.csv | 18 | 0.6 | 14 valores de 00-verificacion (\|data − frac·100\| máx 0.50); resto v1_frac·100 |
| 29 | unmet_needs_upset | tabla_analisis | analisis adicionales/F08.csv; analisis adicionales/R04_fig24_conjuntos.csv | 19 | 0.6 | 15 intersecciones en el orden de ComplexUpset 1.3.3 (2 fuera del orden de F08.csv) + 4 conjuntos de arriba abajo; textos fmt_num(·, 1) |
| 30 | hardships_hu_eu | texto_publicado | finales/datos/Figure_30_hardships_hu_eu.csv; finales/datos/00-verificacion_valores.csv | 4 | 0.6 | 8 valores de 00-verificacion (\|data − frac·100\| máx 0.41); resto v1_frac·100; frac = v_frac/vmax (vmax 0.3626, altura dibujada) |
| 31 | at_least_one_hardship | texto_publicado | finales/datos/Figure_31_at_least_one_hardship.csv; finales/datos/00-verificacion_valores.csv | 23 | 0.6 | 18 valores de 00-verificacion (\|data − frac·100\| máx 0.48); resto v1_frac·100 |
| 32 | hardships_by_outcome | texto_publicado | analisis adicionales/A03.csv; finales/datos/Figure_32_hardships_by_outcome.csv | 10 | 0.6 | 10 tarjetas = finales/datos (\|Δ\| máx 0.0045); textos y deltas (de valores redondeados) según charts5.py; lo/hi = 2/62 |
| 33 | knowledge_rights | texto_publicado | finales/datos/Figure_33_knowledge_rights.csv; finales/datos/00-verificacion_valores.csv | 18 | 0.6 | 14 valores de 00-verificacion (\|data − frac·100\| máx 0.50); resto v1_frac·100 |
| 34 | legal_capability | texto_publicado | finales/datos/Figure_34_legal_capability.csv | 3 | 0.6 | etiquetas con salto de línea como se imprimen |
| 35 | expert_support | texto_publicado | finales/datos/Figure_35_expert_support.csv; finales/datos/00-verificacion_valores.csv | 18 | 0.6 | 14 valores de 00-verificacion (\|data − frac·100\| máx 0.45); resto v1_frac·100 |
| 36 | perceived_causes | texto_publicado | finales/datos/Figure_36_perceived_causes.csv | 10 | 0.6 | sin filas en 00-verificacion: value = v1_frac·100 |
| 37 | legal_documentation | texto_publicado | finales/datos/Figure_37_legal_documentation.csv; finales/datos/00-verificacion_valores.csv | 3 | 0.6 | 6 valores de 00-verificacion (\|data − frac·100\| máx 0.36); resto v1_frac·100; frac = v_frac/vmax (vmax 0.9916, altura dibujada) |
| 38 | trust_judges | texto_publicado | finales/datos/Figure_38_trust_judges.csv; finales/datos/00-verificacion_valores.csv | 24 | 0.6 | 18 valores de 00-verificacion (\|data − frac·100\| máx 0.49); resto v1_frac·100 |
| 39 | trust_prosecutors | texto_publicado | finales/datos/Figure_39_trust_prosecutors.csv; finales/datos/00-verificacion_valores.csv | 24 | 0.6 | 18 valores de 00-verificacion (\|data − frac·100\| máx 0.46); resto v1_frac·100 |
| 40 | trust_public_defense | texto_publicado | finales/datos/Figure_40_trust_public_defense.csv; finales/datos/00-verificacion_valores.csv | 24 | 0.6 | 18 valores de 00-verificacion (\|data − frac·100\| máx 0.45); resto v1_frac·100 |
| 41 | trust_by_exposure | texto_publicado | analisis adicionales/A04.csv; finales/datos/Figure_41_trust_by_exposure.csv | 12 | 0.6 | 12 tarjetas = finales/datos (\|Δ\| máx 0.0045); textos y deltas (de valores redondeados) según charts5.py; lo/hi = 29/55 |
| 42 | trust_by_hardship | texto_publicado | analisis adicionales/A05.csv; finales/datos/Figure_42_trust_by_hardship.csv | 9 | 0.6 | 9 tarjetas = finales/datos (\|Δ\| máx 0.0049); textos y deltas (de valores redondeados) según charts5.py; lo/hi = 30/55 |

## Medias UE (`eu_values.csv`)

Fuente: `reports/eu-thematic-reports/data-viz/output/data4web_gpp.csv` (md5 `1c9cd88849b1a992e11f23639988d257`, fecha 2025-08-20).

| id | value | redondeado | nota |
|---|---|---|---|
| prevalence2 | 49.1521 | 49 | level = eu (European Union), demographic = Total Sample; valor del CSV (0-1) x 100 |
| access2info | 61.1297 | 61 | level = eu (European Union), demographic = Total Sample; valor del CSV (0-1) x 100 |
| access2drm | 40.1829 | 40 | level = eu (European Union), demographic = Total Sample; valor del CSV (0-1) x 100 |
| rp_time | 73.9550 | 74 | level = eu (European Union), demographic = Total Sample; valor del CSV (0-1) x 100; fila duplicada en el origen (distinct) |
| rp_fair | 53.5229 | 54 | level = eu (European Union), demographic = Total Sample; valor del CSV (0-1) x 100 |
| rp_cost | 82.1040 | 82 | level = eu (European Union), demographic = Total Sample; valor del CSV (0-1) x 100 |
| rp_outcome | 72.0026 | 72 | level = eu (European Union), demographic = Total Sample; valor del CSV (0-1) x 100 |
| TRT_judges | 57.2053 | 57 | level = eu (European Union), demographic = Total Sample; valor del CSV (0-1) x 100 |
| TRT_prosecutors | 57.4226 | 57 | level = eu (European Union), demographic = Total Sample; valor del CSV (0-1) x 100 |
| TRT_pda | 51.9890 | 52 | level = eu (European Union), demographic = Total Sample; valor del CSV (0-1) x 100 |

## Figuras publicadas (`published/`)

Ancho a 96 dpi = round(w_in·96) del manifiesto en 42 de 42.

| fig | archivo (300 dpi) | px 300 dpi | px 96 dpi | md5 (300 dpi) |
|---|---|---|---|---|
| 01 | Figure_01_prevalence_nontrivial_problems.png | 1821x1296 | 583x415 | bba811aeadb294f41fb76ae321427362 |
| 02 | Figure_02_prevalence_by_category_hu_eu.png | 2520x1150 | 806x368 | 12563703d1c2ba5c91ecc810e0c9cf72 |
| 03 | Figure_03_prevalence_severity_by_area.png | 2215x1034 | 709x331 | 7ec5dff7f8936f75d893b9c8f8f69ca5 |
| 04 | Figure_04_cooccurrence_number_of_problems.png | 1821x1231 | 583x394 | be4380ae010d6f4527dd77a6205a0be4 |
| 05 | Figure_05_cooccurrence_network.png | 1950x1761 | 624x564 | 2a10fb9eb977570c6ded8996208d7588 |
| 06 | Figure_06_cooccurrence_conditional_matrix.png | 2520x2268 | 806x726 | d24d0f1610df014945f84fdef9773439 |
| 07 | Figure_07_severity_distribution_hu_eu.png | 1908x818 | 611x262 | bfbe9f8e5f55d494b4d129c5a2210e6c |
| 08 | Figure_08_justice_pathway_stages.png | 1950x683 | 624x219 | d73de86a855afe881a28499054a1ef86 |
| 09 | Figure_09_access_information.png | 1821x2512 | 583x804 | 43ba625bf769149eaed9cc316de6d00f |
| 10 | Figure_10_access_information_perception_experience.png | 1950x1575 | 624x504 | 8d543fa8c39a2466d1542f926aaad2db |
| 11 | Figure_11_pathway_assistance_sankey.png | 2520x1550 | 806x496 | 08b77af1192b359435184bd5a9026a4a |
| 12 | Figure_12_access_assistance.png | 1821x2512 | 583x804 | a1b3a64499a50bf3848e408d32e7757c |
| 13 | Figure_13_advisers_contacted.png | 2183x833 | 699x267 | a8fedf24dd9910be2d6ba02d3694d8a8 |
| 14 | Figure_14_barriers_assistance.png | 2183x833 | 699x267 | eb621b4cb34060a5fdbe1454e2dfd779 |
| 15 | Figure_15_access_assistance_perception_experience.png | 1950x1575 | 624x504 | 36a9dea2943cdf73d3994cc9401f24df |
| 16 | Figure_16_pathway_drm_sankey.png | 2520x1550 | 806x496 | 3330e274f8b6ee90099c565c406a6e3a |
| 17 | Figure_17_access_drm.png | 1821x1646 | 583x527 | 3b85f904be63ea9c5df406ba91c800bd |
| 18 | Figure_18_drm_contacted.png | 2183x742 | 699x237 | b143395d78c7ab0b5e0e3084be186e66 |
| 19 | Figure_19_barriers_drm.png | 2183x558 | 699x179 | 3ad0d54688308d1769e5ddf708f1aeac |
| 20 | Figure_20_timeliness.png | 1821x1646 | 583x527 | c458c7a166f3fc2e4f211b84bb3eeca4 |
| 21 | Figure_21_perceived_speed.png | 1821x1581 | 583x506 | 06e36ff879560c7b5f696ada17e87a72 |
| 22 | Figure_22_perceived_fairness.png | 1821x1646 | 583x527 | bfb3b4d2ad74e3052598285944c5a9c9 |
| 23 | Figure_23_cost_coverage.png | 1821x1646 | 583x527 | 2c65dcc40eb6ddee758456cf354b1426 |
| 24 | Figure_24_perceived_costliness.png | 1821x1581 | 583x506 | 5d2ed2208cf543df9757e6e9acf018e0 |
| 25 | Figure_25_outcome.png | 1821x1646 | 583x527 | 20f2a8954dc93f498e36a0bb8b3b4cf3 |
| 26 | Figure_26_satisfaction.png | 1821x1673 | 583x535 | f1265d47effda8cde8b5ec4c7268977b |
| 27 | Figure_27_outcome_by_access.png | 1950x900 | 624x288 | fb19336ef0a7e1e44a22302f3b330bf6 |
| 28 | Figure_28_justice_gap.png | 1821x1296 | 583x415 | c14e88a73c7c4316e1489e75d2e97ef5 |
| 29 | Figure_29_unmet_needs_upset.png | 1950x1620 | 624x518 | bb0cb556ee4152b0849f5bfafeea6f2c |
| 30 | Figure_30_hardships_hu_eu.png | 1950x1002 | 624x321 | 5d98286fc4d98853547d2cb4209b353e |
| 31 | Figure_31_at_least_one_hardship.png | 1821x1646 | 583x527 | bae5d2f1811113d305e640e7e4e6032f |
| 32 | Figure_32_hardships_by_outcome.png | 1950x1222 | 624x391 | f138110bd6e5bdc6e6881320c2f81b78 |
| 33 | Figure_33_knowledge_rights.png | 1821x1296 | 583x415 | 3fefe27e94693587b3ea130701be0dba |
| 34 | Figure_34_legal_capability.png | 1950x683 | 624x219 | c86e19aaee504c0907e4a92dcc1fdb7b |
| 35 | Figure_35_expert_support.png | 1821x1296 | 583x415 | e4b3374848d10ee56a35215e5813983f |
| 36 | Figure_36_perceived_causes.png | 2183x1017 | 699x325 | 83d4545222b12154c6be676aeeedadbf |
| 37 | Figure_37_legal_documentation.png | 2183x1002 | 699x321 | 574accc5188d51d63a178a3608c6565c |
| 38 | Figure_38_trust_judges.png | 1821x1738 | 583x556 | ae48fa4e23b90e61d696c68fb5961eb7 |
| 39 | Figure_39_trust_prosecutors.png | 1821x1738 | 583x556 | 53fa2c1e6641ab76358c2808751d4b60 |
| 40 | Figure_40_trust_public_defense.png | 1821x1738 | 583x556 | 42405b9fd98a2106dfff9e7cee031992 |
| 41 | Figure_41_trust_by_exposure.png | 1950x1052 | 624x337 | 8f1170ff777a50c1780c29ac933048cd |
| 42 | Figure_42_trust_by_hardship.png | 1950x827 | 624x265 | fcb345f5f33e8cb546d73b656f3cf9a4 |

## Insumos

| Archivo | md5 | Fecha |
|---|---|---|
| reports/eu-thematic-reports/data-viz/output/data4web_gpp.csv | 1c9cd88849b1a992e11f23639988d257 | 2025-08-20 13:21 |
| finales/datos/00-verificacion_valores.csv | 0c632cdae1ed09e728c469b65803306c | 2026-10-16 12:44 |
| finales/datos/Figure_01_prevalence_nontrivial_problems.csv | 11e9d0261bad6d56830d19287de535af | 2026-10-16 12:44 |
| finales/datos/Figure_02_prevalence_by_category_hu_eu.csv | 2ba1a581968988f94b5a01587464fb2d | 2026-10-16 12:44 |
| finales/datos/Figure_04_cooccurrence_number_of_problems.csv | 7aa04b6b18b87ef06962c047adf4f07b | 2026-10-16 12:44 |
| finales/datos/Figure_08_justice_pathway_stages.csv | fa359f62d1a03e41028206d3af17622b | 2026-10-16 12:44 |
| finales/datos/Figure_09_access_information.csv | b117a2eed2659a469acacfaca0decab2 | 2026-10-16 12:44 |
| finales/datos/Figure_10_access_information_perception_experience.csv | 5733426a2f9fec8181c424496a9bfd3e | 2026-10-16 12:44 |
| finales/datos/Figure_11_pathway_assistance_sankey.csv | ff1c0920cd5cd5cf16815d5aafd0401a | 2026-10-16 12:44 |
| finales/datos/Figure_12_access_assistance.csv | f3054efa4f5783c61e3ae4527da13df1 | 2026-10-16 12:44 |
| finales/datos/Figure_13_advisers_contacted.csv | 11740c7e9c4ddb42e2e9769ef4863728 | 2026-10-16 12:44 |
| finales/datos/Figure_14_barriers_assistance.csv | 3988b3d70d3deeb14cb01633b5dd170f | 2026-10-16 12:44 |
| finales/datos/Figure_15_access_assistance_perception_experience.csv | d888983d52b74a7f3602def1bcc82a71 | 2026-10-16 12:44 |
| finales/datos/Figure_16_pathway_drm_sankey.csv | ef5015e066ec8f5e5a984bf26c641995 | 2026-10-16 12:44 |
| finales/datos/Figure_17_access_drm.csv | 195d5fd8be03f0a69aee3786135b5fd3 | 2026-10-16 12:44 |
| finales/datos/Figure_18_drm_contacted.csv | 2285387b045736ef0f588c6331221b49 | 2026-10-16 12:44 |
| finales/datos/Figure_19_barriers_drm.csv | a343e4ea14feddd6beb169ae7bdf492b | 2026-10-16 12:44 |
| finales/datos/Figure_20_timeliness.csv | 7643ee8516a0dcd0f9a4e3655d386e09 | 2026-10-16 12:44 |
| finales/datos/Figure_21_perceived_speed.csv | 33cbe40a5df26ca801b52250ef1753d9 | 2026-10-16 12:44 |
| finales/datos/Figure_22_perceived_fairness.csv | 5d97bd2bc83b7607446c668d9008ab31 | 2026-10-16 12:44 |
| finales/datos/Figure_23_cost_coverage.csv | 1e1304802971f0835b73c25d650e316e | 2026-10-16 12:44 |
| finales/datos/Figure_24_perceived_costliness.csv | 91a7c8a1e1d6cc7c408f326e166d0f78 | 2026-10-16 12:44 |
| finales/datos/Figure_25_outcome.csv | b5a025acb4f3269d9571f3e63eec1ca6 | 2026-10-16 12:44 |
| finales/datos/Figure_26_satisfaction.csv | b3a306954f89ec0f6903def35f67984f | 2026-10-16 12:44 |
| finales/datos/Figure_27_outcome_by_access.csv | 0e4921ad429fcdf510ed2d3ef5a4ed6a | 2026-10-16 12:44 |
| finales/datos/Figure_28_justice_gap.csv | 0186dc1a4d4900e7109107b0564aace6 | 2026-10-16 12:44 |
| finales/datos/Figure_30_hardships_hu_eu.csv | 8e807bf912dbab429bcf321b27f27274 | 2026-10-16 12:44 |
| finales/datos/Figure_31_at_least_one_hardship.csv | 6994c4e46e0c08e7fdd38b4263803a1b | 2026-10-16 12:44 |
| finales/datos/Figure_32_hardships_by_outcome.csv | cc875eb0ec7ee9fedf88d0669ffc3c59 | 2026-10-16 12:44 |
| finales/datos/Figure_33_knowledge_rights.csv | 39ef0be0ae35c67b07002b83c398ab9a | 2026-10-16 12:44 |
| finales/datos/Figure_34_legal_capability.csv | aec0654b643df908096a87a89a58bddd | 2026-10-16 12:44 |
| finales/datos/Figure_35_expert_support.csv | 3767ef5caa069bd08be9d1254e48431c | 2026-10-16 12:44 |
| finales/datos/Figure_36_perceived_causes.csv | bfaadec2b07093bb65fe799d521fc4b3 | 2026-10-16 12:44 |
| finales/datos/Figure_37_legal_documentation.csv | a17bce10dfd3d055e7d595be9e3a7f14 | 2026-10-16 12:44 |
| finales/datos/Figure_38_trust_judges.csv | 9838ecfa65fcaffbc931845520424888 | 2026-10-16 12:44 |
| finales/datos/Figure_39_trust_prosecutors.csv | 1385c7034bd2fa0625bc68f46e4098b0 | 2026-10-16 12:44 |
| finales/datos/Figure_40_trust_public_defense.csv | 32d25e5c1e0ba4c1a382a8faef030138 | 2026-10-16 12:44 |
| finales/datos/Figure_41_trust_by_exposure.csv | 438da4ad2748a3750627dfc767b9e388 | 2026-10-16 12:44 |
| finales/datos/Figure_42_trust_by_hardship.csv | 133b37f5f1f8ed0ed10f49f06577387a | 2026-10-16 12:44 |
| analisis adicionales/A03.csv | 06d5f1de792337ab3e5c0d8f6104c12b | 2026-09-28 15:10 |
| analisis adicionales/A04.csv | 595477bb9c945b283b9d3643e415083f | 2026-09-28 15:10 |
| analisis adicionales/A05.csv | 928dddcf2ae2b3ab06b8083bdf93d420 | 2026-09-28 15:11 |
| analisis adicionales/F01.csv | 78460edf20a5e12c67c7dab5ed910803 | 2026-09-28 15:10 |
| analisis adicionales/F02.csv | 66170f03d16efda706238fc878cfeda1 | 2026-09-28 15:10 |
| analisis adicionales/F03.csv | 9526a161344d420695d6ed7187f196e7 | 2026-09-28 15:10 |
| analisis adicionales/F08.csv | 5445f9ad0380dd754b0577f318352c04 | 2026-09-28 15:10 |
| analisis adicionales/R04_fig24_conjuntos.csv | 4834d2d52b43796e63a6fa5fdd71bc92 | 2026-09-28 15:10 |
| analisis adicionales/cooccurrence_network.svg | c8f9ff445ec2eec6a71d03b7174d1b2c | 2026-09-28 15:10 |
| Hungary_jg_upset.svg (consultado: sin <text>, glifos en trazos) | df51cf39a64c305307252fd3c6beb2bc | 2026-07-09 09:37 |
| Hungria/00-INDICE.csv (guarda de país: títulos) | 48fd9a49dbccd5f44e48cf3e21c19bd3 | 2026-10-16 12:44 |

## Tiempos

- Tablas: 0.4 s (42 figuras). PNG a 96 dpi: 6.1 s. Medias UE: 1.1 s. Total: 9.3 s.

