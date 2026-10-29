## 05_charts.R — filas (df, §4.4) y dibujo (chart_out, §4.5) por tipo de figura ------------------------------------
## Origen: src/hungary/finales/report_charts.R:127-145 (chart_grouped, cuerpo literal; la entrada es el df de §4.4 en
## vez de la spec con v1_frac / v1_text); report_charts.R:180-217 (chart_dumbbell, cuerpo literal; v1 = percepción,
## v2 = experiencia) con la pista de standard_figures.R:469-471; report_charts.R:311-382 y standard_figures.R:126-171
## (bloque sankey: chart_sankey, sankey_def, rows_sankey); figure_specs.R:186 (formato de los valores impresos:
## formatC(round(x, decimals), format = "f", digits = decimals)). Los demás bloques (report_charts.R:150-178, 220-310,
## 383-431) se portan en la Etapa 3: aquí solo el encabezado con el mapeo de columnas y stop("pendiente").
## Convención: rows_<tipo>(…) devuelve el df de §4.4 — order, kind, grouping, label, value, value2, text, text2 — con
## todo número impreso en text/text2 vía fmt_num(); chart_<tipo>(df, spec, …) dibuja df$text tal cual (no formatea ni
## recalcula), toma L de LABEL_COL[[spec$label_col]] y W de spec$w_in, y devuelve chart_out (04_svg.R) con drawn = lo
## efectivamente dibujado. Las etiquetas, los textos y el orden exactos que compara verify están en reference/<slug>/
## Figure_NN_slug.csv (la verdad de cada figura); el encabezado de cada bloque (grep -n "^## Bloque") resume su mapeo.
## Revisión del 23-oct (# corregido: en cada sitio): fmt_num() robusto (texto "NA" / "", factores, cero negativo, Inf);
## rows_grouped() ordena los grupos por los niveles del factor; chart_grouped() admite fill_w y comprueba que el diseño
## mide el ancho de la figura; encabezados de bloque alineados con las referencias. Solo define funciones: nada se
## ejecuta al cargar.


## Formato de los números impresos ------------------------------------------------------------------------------------

# Contrato §5.C: "61%" (decimals 0), "12.3%" (1), "6.0" (decimals 1, pct = FALSE); NA → "N/A". Vectorizado; round()
# de R (medio al par). origen del formato: figure_specs.R:186
# corregido: los NA se marcan sobre el valor ya convertido (antes sobre x: el texto "NA" o "" salía "NA%"). Un factor
# se lee por su texto (no por el código del nivel); "", "NA", "N/A" y "NaN" (tablas leídas con col_character, §7) son
# NA sin aviso (otro texto no numérico sí avisa); + 0 quita el cero negativo ("-0%" → "0%") y un valor no finito se
# imprime "N/A" (antes " Inf%").
fmt_num <- function(x, decimals = 0, pct = TRUE) {
  if (length(x) == 0) return(character(0))
  if (!is.numeric(x)) {
    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "N/A", "NaN")] <- NA
  }
  v <- as.numeric(x)
  out <- paste0(formatC(round(v, decimals) + 0, format = "f", digits = decimals), if (pct) "%")
  out[!is.finite(v)] <- "N/A"
  out
}


## Bloque grouped_bars — figuras 1, 4, 9, 12, 17, 20-28, 31, 33, 35, 38-40 ------------------------------------------
## Mapeo (§4.4): kind avg | header | row; grouping "" en avg, nombre del grupo en header y row; label "National Average"
## / "EU Average", nombre del grupo, categoría; value % 0-100 (media 0-10 en la 4; NA si n < 30); value2 NA; text
## "61%", "N/A", "6.0" ("" en header); text2 "". Orden: avg National, avg EU (si hay), luego cada grupo en el orden de
## clabels de get_results_table (header + filas; los niveles del factor grouping) y, dentro del grupo, las categorías
## en orden alfabético inverso de arriba abajo (arrange(desc(label), .locale = "C")). Medidas STD (pista 250 pt) con L =
## LABEL_COL[["std"]] = 149: el diseño mide 437 pt = 6.069 in (spec$w_in). Referencia: reference/<slug>/
## Figure_09_access_information.csv (y la de cada figura del bloque).
## Excepción, figura 27 (decisión de la revisión): sin filas avg ni orden alfabético. fig_27 arma el df desde
## diff_stratified() (03_estimators.R ← A01.R): por etapa (Information and advice, Appropriate assistance, Dispute
## resolution mechanism) su header y las filas "With access" y "Without access", en ese orden, sin rows_grouped(); lo
## dibuja chart_grouped(df, spec, st = ST_A01, fill_w = spec$w_in * 72): la pista (281 pt) llena los 6.5 in, como el
## fill_w = W_CALLOUT del original (standard_figures.R:277, 471).
## Origen del dibujo: report_charts.R:131-145 chart_grouped ← src/viz/grouped_bars.R gen_grouped_bars (worker-viz.R:178).

# tbl = salida de get_results_table(ctx, …) (03_estimators.R): tibble(grouping, category, value) con la primera fila
# grouping "National" / category "National Average", los grupos en el orden de clabels y n < 30 → NA. eu = media UE
# (escalar; NULL si la figura no la lleva). scale = valor de la barra llena: 100 (%) o 10 (medias 0-10, figura 4);
# text lleva "%" solo con scale 100. Los grupos que se omiten (p. ej. "Category") se quitan antes, en
# get_results_table(omit = …). frac no va en el df (lo calcula chart_grouped para drawn).
rows_grouped <- function(tbl, eu = NULL, decimals = 0, scale = 100) {
  need <- c("grouping", "category", "value")
  if (!all(need %in% names(tbl))) stop("rows_grouped: tbl debe tener columnas ", paste(need, collapse = ", "), call. = FALSE)
  if (anyNA(tbl$grouping)) stop("rows_grouped: tbl trae filas con grouping NA", call. = FALSE)   # nuevo
  nat <- filter(tbl, grouping == "National")
  if (nrow(nat) != 1) stop("rows_grouped: tbl debe tener una sola fila grouping == 'National' (hay ", nrow(nat), ")", call. = FALSE)
  pct <- scale == 100
  avg <- tibble(kind = "avg", grouping = "", label = "National Average", value = as.numeric(nat$value))
  if (!is.null(eu)) avg <- bind_rows(avg, tibble(kind = "avg", grouping = "", label = "EU Average", value = as.numeric(eu)))
  # corregido: los grupos van en el orden de los niveles del factor (= clabels de get_results_table, §7) y no en el de
  # aparición de las filas, que un arrange() o bind_rows() previo cambia; con grouping character, el de aparición
  present <- unique(as.character(tbl$grouping[tbl$grouping != "National"]))
  groups <- if (is.factor(tbl$grouping)) intersect(setdiff(levels(tbl$grouping), "National"), present) else present
  body <- bind_rows(lapply(groups, function(g) {
    rows <- filter(tbl, grouping == g) %>%
      transmute(kind = "row", grouping = g, label = as.character(category), value = as.numeric(value)) %>%
      arrange(desc(label), .locale = "C")
    bind_rows(tibble(kind = "header", grouping = g, label = g, value = NA_real_), rows)
  }))
  bind_rows(avg, body) %>%
    mutate(order = row_number(), value2 = NA_real_,
           text = if_else(kind == "header", "", fmt_num(value, decimals, pct)), text2 = "") %>%
    select(order, kind, grouping, label, value, value2, text, text2)
}

# Columna de etiquetas (pt) según spec$label_col: std | dumb | hbars → LABEL_COL; "own" → la de las propias etiquetas
# con label_column() (origen: standard_figures.R:469). rows: tibble con kind y label.
label_col_pt <- function(spec, rows, st = STD) {
  col <- spec$label_col %||% "std"
  if (identical(col, "own")) return(label_column(list(rows), st))
  if (!col %in% names(LABEL_COL)) {
    stop("label_col '", col, "' sin columna de etiquetas (LABEL_COL: ", paste(names(LABEL_COL), collapse = ", "), ")", call. = FALSE)
  }
  LABEL_COL[[col]]
}

# origen: report_charts.R:128-145 (cuerpo literal) + standard_figures.R:471 (fill_w). track = FALSE y value_at_end =
# TRUE reproducen gen_grouped_bars(perc = FALSE): sin pista gris y el valor justo después de la barra (figura 4).
# Adaptación al contrato §5.C: entra el df de §4.4 (spec_rows: v1_frac = value / scale, v1_text = text, dibujado tal
# cual); L = LABEL_COL[[spec$label_col]]; W = spec$w_in * 72 (el original: xv + st$value_w = 437 pt con L = 149, que
# declara el mismo "6.069in"); el alto se calcula como el original (rows$y + 4) y no se fuerza a spec$h_in (06_export.R
# lo comprueba). drawn$frac = fracción de barra dibujada (NA en encabezados y valores suprimidos); scale = valor de la
# barra llena (10 en la figura 4). fill_w (pt; NULL = la pista de st): ancho que llena la figura, con st$track = fill_w −
# L − gap_label − gap_value − value_w (figura 27: fill_w = spec$w_in * 72). El diseño (L + gap_label + pista +
# gap_value + value_w) debe medir W con ±0.05 pt (std: 437 frente a 436.97) o la función se detiene.
chart_grouped <- function(df, spec, track = TRUE, value_at_end = FALSE, scale = 100, st = STD, fill_w = NULL) {
  df <- df[order(df$order), ]
  spec_rows <- tibble(kind = df$kind, label = df$label, v1_frac = as.numeric(df$value) / scale,
                      v1_text = coalesce(as.character(df$text), ""))
  L <- label_col_pt(spec, spec_rows, st)
  # corregido: faltaba el fill_w del original (standard_figures.R:471): con ST_A01 la pista de la 27 medía 250 pt y no
  # 281, y como drawn$frac es relativa a la pista, verify no lo veía
  if (!is.null(fill_w)) st$track <- fill_w - L - st$gap_label - st$gap_value - st$value_w
  x0 <- L + st$gap_label; xv <- x0 + st$track + st$gap_value
  W <- spec$w_in * 72
  if (length(W) != 1 || !is.finite(W)) stop("chart_grouped: spec$w_in debe ser un número (pulgadas)", call. = FALSE)
  if (abs(xv + st$value_w - W) > 0.05) {                # nuevo: el diseño debe medir el ancho del manifiesto
    stop("chart_grouped: el diseño mide ", r2(xv + st$value_w), " pt y la figura ", r2(W), " pt (w_in ", spec$w_in,
         "); revise st$track o pase fill_w = spec$w_in * 72", call. = FALSE)
  }
  rows <- layout_rows(spec_rows, L, 2, function(r, y) {
    cy <- y + st$pitch / 2
    els <- label_el(r, L, mid_base(cy, st$size_label), st)
    if (!is.na(r$v1_frac)) {
      if (track) els <- c(els, rect_el(x0, cy - st$bar / 2, st$track, st$bar, st$col$track))
      els <- c(els, rect_el(x0, cy - st$bar / 2, st$track * min(max(r$v1_frac, 0), 1), st$bar, st$col$main))
    }
    x_val <- if (value_at_end && !is.na(r$v1_frac)) x0 + st$track * r$v1_frac + st$gap_value else xv
    els <- c(els, value_el(r$v1_text, x_val, mid_base(cy, st$size_value), st$col$main, st = st))
    list(els = els, h = st$pitch)
  }, st)
  is_bar <- df$kind != "header" & !is.na(spec_rows$v1_frac)
  drawn <- tibble(order = df$order, kind = df$kind, grouping = df$grouping, label = df$label,
                  frac = ifelse(is_bar, pmin(pmax(spec_rows$v1_frac, 0), 1), NA_real_), frac2 = NA_real_,
                  text = ifelse(df$kind == "header", "", spec_rows$v1_text), text2 = df$text2)
  chart_out(W, rows$y + 4, rows$els, drawn, st$size_label)
}


## Bloque paired_bars — figura 2 ------------------------------------------------------------------------------------
## Mapeo (§4.4): kind row; grouping ""; label categoría; value % país; value2 % UE; text "61%"; text2 "49%"; drawn frac
## = value / 100 y frac2 = value2 / 100. Orden de la referencia (% país descendente; F04.R). label_col own (la figura
## mide su propia columna con label_column()); medidas ST_WIDE2; la pista llena el ancho de la figura: fill_w =
## spec$w_in * 72 (8.4 in; standard_figures.R:233, 471), como en chart_grouped. Leyenda c(spec$country, "EU Average"):
## spec$country = cfg$country lo añade read_manifest (plot() no lee ctx ni cfg; nunca un literal de país). Referencia:
## reference/<slug>/Figure_02_*.csv. Origen: report_charts.R:150-178 chart_paired(spec, L, legend, st). Pendiente
## (Etapa 3).
rows_paired <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_paired <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque dumbbell — figuras 10, 15 ---------------------------------------------------------------------------------
## Mapeo (§4.4): kind avg | header | row; grouping "" en avg, nombre del grupo en header y row; label "National Average",
## nombre del grupo, categoría; value percepción % (población general); value2 experiencia % (con problema no trivial);
## NA si n < 30; text / text2 "27%" / "61%" o "" si el valor falta: en los dumbbells un valor suprimido no lleva punto
## ni texto (nunca "N/A"). Orden: National Average, luego cada corte en el orden de tbl (Gender, Age Group, Financial
## Situation, Area of Residence; F06.R:4) con su encabezado y, dentro, alfabético inverso (arrange(desc(label),
## .locale = "C")), como las agrupadas. Medidas ST_DUMB (pitch 18); label_col dumb (84 pt); la pista llena el ancho de
## la figura. Referencia: reference/<slug>/Figure_10_*.csv (17 filas: avg, 4 header, 12 row).
## Origen del dibujo: report_charts.R:180-217 chart_dumbbell ← src/hungary/viz_helpers.R:92 report_dumbbell
## (F06.R:26-27); ancho y pista: standard_figures.R:38 (ST_DUMB), 246 y 258 (column dumb, fill_w = W_CALLOUT), 469-478.

# tbl = salida de perception_experience(ctx, jse, exp) (03_estimators.R ← F06.R:3-15): tibble(grouping, category,
# perception, experience, n_p, n_e) con la fila nacional grouping "" / category "National Average", los cortes en el
# orden de F06.R:4 y n < 30 → NA. Devuelve el df de §4.4 (n_p / n_e no pasan: no se imprimen).
rows_dumbbell <- function(tbl, decimals = 0) {
  need <- c("grouping", "category", "perception", "experience")
  if (!all(need %in% names(tbl))) stop("rows_dumbbell: tbl debe tener columnas ", paste(need, collapse = ", "), call. = FALSE)
  nat <- filter(tbl, grouping == "", category == "National Average")
  if (nrow(nat) != 1) {
    stop("rows_dumbbell: tbl debe tener una sola fila grouping == \"\" / category == \"National Average\" (hay ", nrow(nat), ")",
         call. = FALSE)
  }
  shown <- function(x) if_else(is.na(x), "", fmt_num(x, decimals))           # suprimido → "" (sin "N/A")
  avg <- tibble(kind = "avg", grouping = "", label = "National Average",
                value = as.numeric(nat$perception), value2 = as.numeric(nat$experience))
  groups <- unique(as.character(tbl$grouping[tbl$grouping != ""]))           # orden de los cortes (F06.R:4)
  body <- bind_rows(lapply(groups, function(g) {
    rows <- filter(tbl, grouping == g) %>%
      transmute(kind = "row", grouping = g, label = as.character(category),
                value = as.numeric(perception), value2 = as.numeric(experience)) %>%
      arrange(desc(label), .locale = "C")
    bind_rows(tibble(kind = "header", grouping = g, label = g, value = NA_real_, value2 = NA_real_), rows)
  }))
  bind_rows(avg, body) %>%
    mutate(order = row_number(), text = shown(value), text2 = shown(value2)) %>%
    select(order, kind, grouping, label, value, value2, text, text2)
}

# origen: report_charts.R:182-217 (cuerpo literal) + standard_figures.R:469-471 (L y pista). Adaptación al contrato
# §5.C: entra el df de §4.4 (spec_rows: v1 = percepción, v2 = experiencia; v*_frac = value / 100, v*_text = text /
# text2 dibujados tal cual); L = LABEL_COL[[spec$label_col]] (dumb = 84); fill_w = spec$w_in * 72 (el original:
# W_CALLOUT = 6.5 in) y la pista llena ese ancho, así que el documento (xT + gap_value + value_w) mide spec$w_in; el
# alto se calcula como el original (rows$y + 4) y no se fuerza a spec$h_in (06_export.R lo comprueba). legend = las
# dos líneas de la leyenda (texto fijo del diseño; lo pasa la figura). Los rótulos del eje (0%-100%) son del diseño.
# Fila sin ningún valor: solo la etiqueta (figura 15, 18-24); con uno: un punto y su texto a la derecha. drawn: frac
# = value / 100 y frac2 = value2 / 100 de los puntos dibujados (NA sin punto y en encabezados), text/text2 lo impreso.
chart_dumbbell <- function(df, spec, legend, st = ST_DUMB) {
  df <- df[order(df$order), ]
  spec_rows <- tibble(kind = df$kind, label = df$label,
                      v1_frac = as.numeric(df$value) / 100, v1_text = coalesce(as.character(df$text), ""),
                      v2_frac = as.numeric(df$value2) / 100, v2_text = coalesce(as.character(df$text2), ""))
  L <- label_col_pt(spec, spec_rows, st)
  fill_w <- spec$w_in * 72
  st$track <- fill_w - L - st$gap_label - st$gap_value - st$value_w          # origen: standard_figures.R:471
  x0 <- L + st$gap_label; xT <- x0 + st$track
  xs <- function(frac) x0 + st$track * frac
  # leyenda (dos líneas) y eje arriba
  leg <- c(
    circle_el(x0 + st$dot_r, 10 - 3.4, st$dot_r, st$col$main),
    text_path(legend[1], x0 + 2 * st$dot_r + 5, 10, st$size_legend, fill = st$col$text),
    circle_el(x0 + st$dot_r, 24 - 3.4, st$dot_r, st$col$exp),
    text_path(legend[2], x0 + 2 * st$dot_r + 5, 24, st$size_legend, fill = st$col$text)
  )
  ax_base <- 44; ax_line <- 49
  axis <- c(
    vapply(c(0, 0.25, 0.5, 0.75, 1), function(t) {
      text_path(paste0(round(100 * t), "%"), xs(t), ax_base, st$size_axis, anchor = "middle", fill = st$col$text)
    }, ""),
    line_el(x0, ax_line, xT, ax_line, st$col$axis, 0.75)
  )
  rows <- layout_rows(spec_rows, L, ax_line + 3, function(r, y) {
    cy <- y + st$pitch / 2
    els <- label_el(r, L, mid_base(cy, st$size_label), st)
    pts <- tibble::tibble(frac = c(r$v1_frac, r$v2_frac), txt = c(r$v1_text, r$v2_text),
                          col = c(st$col$main, st$col$exp)) %>% dplyr::filter(!is.na(frac))
    if (nrow(pts) == 2) els <- c(els, line_el(xs(min(pts$frac)), cy, xs(max(pts$frac)), cy, st$col$track, st$segment))
    if (nrow(pts) > 0) {
      left <- if (nrow(pts) == 2) which.min(pts$frac) else 0
      for (k in seq_len(nrow(pts))) {
        els <- c(els, circle_el(xs(pts$frac[k]), cy, st$dot_r, pts$col[k]))
        on_left <- k == left
        els <- c(els, value_el(pts$txt[k], xs(pts$frac[k]) + if (on_left) -(st$dot_r + 3) else st$dot_r + 3,
                               mid_base(cy, st$size_value), pts$col[k], anchor = if (on_left) "end" else "start", st = st))
      }
    }
    list(els = els, h = st$pitch)
  }, st)
  dot1 <- df$kind != "header" & !is.na(spec_rows$v1_frac)
  dot2 <- df$kind != "header" & !is.na(spec_rows$v2_frac)
  drawn <- tibble(order = df$order, kind = df$kind, grouping = df$grouping, label = df$label,
                  frac = ifelse(dot1, spec_rows$v1_frac, NA_real_), frac2 = ifelse(dot2, spec_rows$v2_frac, NA_real_),
                  text = ifelse(dot1, spec_rows$v1_text, ""), text2 = ifelse(dot2, spec_rows$v2_text, ""))
  chart_out(xT + st$gap_value + st$value_w, rows$y + 4, c(leg, axis, rows$els), drawn, st$size_label)
}


## Bloque hbars — figuras 13, 14, 18, 19, 36 ------------------------------------------------------------------------
## Mapeo (§4.4): kind row; grouping ""; label categoría; value %; value2 NA; text "54%"; text2 ""; drawn frac = value /
## 100. Medidas ST_HBARS; label_col hbars (172 pt): W = L + 12 + track + value_w + 6 = 524 pt = 7.278 in (W_hbars,
## standard_figures.R:463). Referencia: reference/<slug>/Figure_13_*.csv (y 14, 18, 19, 36). Origen:
## report_charts.R:224-242 chart_hbars_axis(spec, L, st) ← src/viz/horizontal_bars.R gen_horizontal_bars (eje 0-100 %).
## rows_hbars: tbl = tibble(category, value) de contacted_shares() o reason_shares() (R/03_estimators.R). Orden: más
## citado primero (arrange(desc(value)) estable: los empates conservan el orden de tbl; NA al final), como
## gen_horizontal_bars (horizontal_bars.R:10) y spec_hbars (standard_figures.R:336); text = fmt_num(value, decimals)
## ("54%"; el original paste0(round(x), "%") imprimía "NA%" donde el contrato pone "N/A"). chart_hbars_axis
## (respaldo svg_r) sigue pendiente: dibuja wjpr_hbars.
# corregido (integración del lote 8-19, 2026-10-28): la implementación de hbars_rows() del agente L2, que vivía en
# 03_estimators.R mientras este esqueleto seguía en stop("pendiente")
rows_hbars <- function(tbl, decimals = 0) {
  need <- c("category", "value")
  if (!all(need %in% names(tbl))) stop("rows_hbars: tbl debe tener columnas category y value", call. = FALSE)
  if (nrow(tbl) == 0) stop("rows_hbars: tbl sin filas (ningún encuestado en la base de la figura)", call. = FALSE)
  out <- tbl %>%
    arrange(desc(value)) %>%
    transmute(order = row_number(), kind = "row", grouping = "", label = as.character(category),
              value = as.numeric(value), value2 = NA_real_, text = fmt_num(value, decimals), text2 = "")
  log_msg("rows_hbars: ", nrow(out), " filas · ", paste(sprintf("%s %.2f", out$label, out$value), collapse = " / "))
  out
}
chart_hbars_axis <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque columns — figuras 30, 37 ----------------------------------------------------------------------------------
## Mapeo (§4.4): kind row; grouping ""; label categoría; value % país; value2 % UE; text "32%"; text2 "45%". Alturas
## relativas a la columna más alta: drawn frac = value / vmax y frac2 = value2 / vmax, con vmax = máximo de las dos
## series de la figura (report_charts.R:259, 266; la referencia guarda esa fracción dibujada). Medidas ST_COLS; W =
## spec$w_in * 72 (30: 6.5 in = W_CALLOUT; 37: 7.278 in, el ancho de las barras horizontales de la 36, que comparte su
## página; standard_figures.R:283, 296, 463). Leyenda c(spec$country, "EU Average"), como en paired. Referencia:
## reference/<slug>/Figure_30_*.csv y Figure_37_*.csv. Origen: report_charts.R:247-279 chart_columns(spec, W, legend,
## st). Pendiente (Etapa 3).
rows_columns <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_columns <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque funnel — figuras 8, 34 ------------------------------------------------------------------------------------
## Mapeo (§4.4): kind stage; grouping ""; label = la etapa tal como se imprime, con "\n" donde la figura corta la línea
## (p. ej. "Obtained appropriate assistance\nwhen they needed it"; verify compara la etiqueta exacta de reference/
## <slug>/Figure_08_*.csv y Figure_34_*.csv); value %; value2 NA; text "61%"; text2 "". chart_funnel divide la etiqueta
## por "\n" y, sin "\n", la ajusta a funnel_label_w con wrap_text() (report_charts.R:285-287): text_width() y
## text_path() no aceptan "\n". drawn frac = value / max(value), el ancho de la barra relativo a la más ancha (la primera
## etapa: frac 1 en la referencia). Medidas ST_FUNNEL; W = spec$w_in * 72. Origen: report_charts.R:284-309
## chart_funnel(rows, W, st) (diseño de charts5.py); datos journey(ctx) (03_estimators.R ← R03.R).
## rows_funnel: values = vector con nombre (nombre = la etapa tal como se imprime, valor = %), en el orden del diseño
## (de arriba abajo); frac no va en el df: la calcula el adaptador (wjpr_funnel). chart_funnel (respaldo svg_r) sigue
## pendiente: dibuja wjpr_funnel.
# origen: standard_figures.R:108-119 (funnel_rows). La usan fig_08 y fig_34. corregido (integración del lote 8-19,
# 2026-10-28): la propuesta del agente L1, que armaba el df en fig_08 mientras este esqueleto seguía pendiente
rows_funnel <- function(values, decimals = 0) {
  v <- unname(as.numeric(values))
  tibble(order = seq_along(v), kind = "stage", grouping = "", label = names(values), value = v, value2 = NA_real_,
         text = fmt_num(v, decimals), text2 = "")
}
chart_funnel <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque sankey — figuras 11, 16 -----------------------------------------------------------------------------------
## Mapeo (§4.4): kind node; grouping "1" | "2" | "3" (columna); label = clave del nodo de sankey_def(), por columna: en
## la 11 contacted, not_contacted | appropriate, non_appropriate, needed_no, no_need, unknown | access, no_access,
## excluded; en la 16 contacted, not_contacted | turned, needed_no, no_need, unknown | access, no_access, excluded;
## value %; value2 NA; text "29%"; text2 "". Orden: columna 1, 2 y 3 y, dentro de cada una, de arriba abajo (order1,
## order2, order3 de sankey_def). Método publicado
## (standard_figures.R:126-171): columnas 1 y 2 = partes de journey(ctx) a 1 decimal (not_contacted = 100 − contacted);
## columna 3: access = round(rep_c_published | drm_c_published), no_access = 100 − access, excluded = round(no_need_r1 +
## unknown_r1) (16: round(47.1 + 8.4) = 56, decisión del 22-oct; sin redondear las partes daría 55); text = fmt_num(value,
## 0). Medidas ST_SANKEY; W = spec$w_in * 72 (8.4 in = MAX_W_FULL_IN). Rótulos, encabezados y notas: texto fijo del
## diseño en sankey_def(). Referencia: reference/<slug>/Figure_11_*.csv y Figure_16_*.csv (= PUBLISHED_SANKEY,
## standard_figures.R:174-177; frac NA: el sankey no tiene barra medible). Origen del dibujo: report_charts.R:311-382
## chart_sankey(s, W, st) ← legacy_python/charts5.py:79 sankey; datos R03.R:4-38 (journey_vars, vía journey(ctx)).

# origen: standard_figures.R:126-171 (sankey_def; rótulos, columnas, flujos y encabezados literales). Adaptado: sin las
# lecturas de R03.csv (r03()) ni los valores (C1, valores de C2, VAL3), que calcula rows_sankey(); C2 guarda solo el nodo
# de la columna 1 del que sale cada nodo de la columna 2 (el original: c(valor, padre)); order1 = names(C1) del
# original. M: nodo de la columna 3 al que va cada nodo de la columna 2; NOTE3: nota bajo el valor de la columna 3.
sankey_def <- function(which) {
  # nuevo: el original tomaba por drm todo lo que no fuera assistance
  if (!isTRUE(which %in% c("assistance", "drm"))) stop("sankey_def: which debe ser 'assistance' o 'drm'", call. = FALSE)
  if (which == "assistance") {
    list(
      order1 = c("contacted", "not_contacted"),
      LAB1 = list(contacted = "Contacted an adviser:", not_contacted = "Did not contact an adviser:"),
      C2 = list(appropriate = "contacted", non_appropriate = "contacted", needed_no = "not_contacted",
                no_need = "not_contacted", unknown = "not_contacted"),
      LAB2 = list(appropriate = "Appropriate adviser:", non_appropriate = "Non-appropriate adviser:",
                  needed_no = c("Needed but did not have", "access to an adviser:"), no_need = "Did not need an adviser:",
                  unknown = "Unknown reason:"),
      order2 = c("appropriate", "non_appropriate", "needed_no", "no_need", "unknown"),
      M = list(appropriate = "access", non_appropriate = "no_access", needed_no = "no_access", no_need = "excluded", unknown = "excluded"),
      order3 = c("access", "no_access", "excluded"),
      LAB3 = list(access = "Had access:", no_access = "Had no access:", excluded = c("Did not need or", "unknown reason:")),
      NOTE3 = list(excluded = "of all respondents"),
      headers = list(c("Contacted", "an adviser"), c("Type of adviser or", "reason for no contact"),
                     c("Access among those", "who needed assistance", "(SDG 16.3.3)")))
  } else {
    list(
      order1 = c("contacted", "not_contacted"),
      LAB1 = list(contacted = "Contacted a DRM:", not_contacted = "Did not contact a DRM:"),
      C2 = list(turned = "contacted", needed_no = "not_contacted", no_need = "not_contacted", unknown = "not_contacted"),
      LAB2 = list(turned = "Turned to a DRM:", needed_no = c("Needed but did not have", "access to a DRM:"),
                  no_need = c("Did not need to", "turn to a DRM:"), unknown = "Unknown reason:"),
      order2 = c("turned", "needed_no", "no_need", "unknown"),
      M = list(turned = "access", needed_no = "no_access", no_need = "excluded", unknown = "excluded"),
      order3 = c("access", "no_access", "excluded"),
      LAB3 = list(access = "Had access:", no_access = "Had no access:", excluded = c("Did not need or", "unknown reason:")),
      NOTE3 = list(excluded = "of all respondents"),
      headers = list(c("Contacted", "a DRM"), c("Reason for", "no contact"), c("Access among those", "who needed a DRM", "(SDG 16.3.3)")))
  }
}

# origen: standard_figures.R:129-133, 147-148 (assistance) y 153-154, 166-167 (drm): los valores de sankey_def, con
# jv(k) = journey(ctx)[[k]] en lugar de r03(k) (R03.csv; journey() estima las mismas variables de R03.R:4-38). value =
# la cifra del método publicado (las partes a 1 decimal son las que se dibujan); excluded = round(no_need_r1 +
# unknown_r1), que es el round(v2[["no_need"]] + v2[["unknown"]]) del original; text = fmt_num(value, 0) (el original:
# paste0(round(x), "%")). j = journey(ctx) (03_estimators.R).
rows_sankey <- function(j, which) {
  s <- sankey_def(which)
  jv <- function(k) j[[k]] %||% stop("rows_sankey: journey(ctx) no trae '", k, "'", call. = FALSE)
  r1 <- function(v) round(v, 1)
  if (which == "assistance") {
    c1 <- r1(jv("adv_contacted"))
    v2 <- c(appropriate = r1(jv("adv_s_appropriate")), non_appropriate = r1(jv("adv_s_nonappr")),
            needed_no = r1(jv("adv_s_needed_no")), no_need = r1(jv("adv_s_noneed")), unknown = r1(jv("adv_s_unknown")))
    access <- round(jv("rep_c_published")); excluded <- round(jv("no_need_r1") + jv("unknown_r1"))
  } else {
    c1 <- r1(jv("drm_s_turned"))
    v2 <- c(turned = c1, needed_no = r1(jv("drm_s_needed_no")), no_need = r1(jv("drm_s_noneed")), unknown = r1(jv("drm_s_unknown")))
    access <- round(jv("drm_c_published")); excluded <- round(jv("drm_no_need_r1") + jv("drm_unknown_r1"))
  }
  value <- unname(c(c1, 100 - c1, v2[s$order2], access, 100 - access, excluded))
  tibble(order = seq_along(value), kind = "node",
         grouping = rep(c("1", "2", "3"), c(length(s$order1), length(s$order2), length(s$order3))),
         label = c(s$order1, s$order2, s$order3), value = value, value2 = NA_real_,
         text = fmt_num(value, 0), text2 = "")
}

# origen: report_charts.R:311-382 (chart_sankey, cuerpo literal). Adaptado al contrato §5.C: entra el df de §4.4
# (rows_sankey) y la definición fija sankey_def(which); los altos de las columnas 1 y 2 son df$value y la columna 3
# suma, como el original, las partes de la columna 2 que van a cada resultado (s$M); los valores impresos son df$text
# tal cual (el original: paste0(round(x), "%") y VAL3). W = spec$w_in * 72 (el original: MAX_W_FULL_IN * 72,
# standard_figures.R:515); alto BOT + 10 = 372 pt (5.167 in). drawn: una fila por nodo (orden de df) con frac = alto
# dibujado / alto del 100 % (columnas 1 y 2: value / 100; columna 3: la suma de sus partes / 100) y text = lo impreso.
chart_sankey <- function(df, spec, which, st = ST_SANKEY) {
  s <- sankey_def(which)
  df <- df[order(df$order), ]
  node_vals <- function(g, keys, col) {                  # nuevo: valores de una columna del sankey, por clave de nodo
    r <- df[df$grouping == g, ]; i <- match(keys, r$label)
    if (anyNA(i)) stop("chart_sankey: faltan en df los nodos ", paste(keys[is.na(i)], collapse = ", "), call. = FALSE)
    setNames(r[[col]][i], keys)
  }
  C1 <- node_vals("1", s$order1, "value"); val2 <- node_vals("2", s$order2, "value")
  if (anyNA(c(C1, val2))) stop("chart_sankey: nodo sin valor (n < 30) en las columnas 1-2", call. = FALSE)
  txt <- list(node_vals("1", s$order1, "text"), node_vals("2", s$order2, "text"), node_vals("3", s$order3, "text"))
  W <- spec$w_in * 72
  out_col <- c(access = st$col$main, no_access = st$col$eu, excluded = st$col$excl)
  TOP <- st$sankey_top; BOT <- TOP + st$sankey_h; GAP <- 9; BAR <- 11
  X1 <- st$sankey_margin; X3 <- W - st$sankey_margin - BAR; X2 <- (X1 + X3) / 2
  order1 <- s$order1
  val3 <- vapply(s$order3, function(o) sum(val2[s$order2[unlist(s$M[s$order2]) == o]]), 0)
  scale <- (st$sankey_h - GAP * (length(s$order2) - 1)) / 100
  layout <- function(values, order) {
    used <- sum(values[order]) * scale + GAP * (length(order) - 1)
    y <- TOP + (st$sankey_h - used) / 2; pos <- list()
    for (k in order) { pos[[k]] <- c(y, y + values[[k]] * scale); y <- y + values[[k]] * scale + GAP }
    pos
  }
  p1 <- layout(C1, order1); p2 <- layout(val2, s$order2); p3 <- layout(val3, s$order3)
  band <- function(xa, ya0, ya1, xb, yb0, yb1, fill, op) {
    xm <- (xa + xb) / 2
    path_el(sprintf("M%s %sC%s %s %s %s %s %sL%s %sC%s %s %s %s %s %sZ", r2(xa), r2(ya0), r2(xm), r2(ya0), r2(xm), r2(yb0), r2(xb), r2(yb0),
                    r2(xb), r2(yb1), r2(xm), r2(yb1), r2(xm), r2(ya1), r2(xa), r2(ya1)), fill, op)
  }
  els <- character()
  cur1 <- vapply(p1, `[`, 0, 1)
  for (k in s$order2) {
    h <- val2[[k]] * scale; par <- s$C2[[k]]              # adaptado: C2 = nodo padre (el original: s$C2[[k]][2])
    els <- c(els, band(X1 + BAR, cur1[[par]], cur1[[par]] + h, X2, p2[[k]][1], p2[[k]][2], out_col[[s$M[[k]]]], 0.30))
    cur1[[par]] <- cur1[[par]] + h
  }
  cur3 <- vapply(p3, `[`, 0, 1)
  for (k in s$order2) {
    o <- s$M[[k]]; h <- val2[[k]] * scale
    els <- c(els, band(X2 + BAR, p2[[k]][1], p2[[k]][2], X3, cur3[[o]], cur3[[o]] + h, out_col[[o]], 0.45))
    cur3[[o]] <- cur3[[o]] + h
  }
  for (k in order1) els <- c(els, rect_el(X1, p1[[k]][1], BAR, diff(p1[[k]]), st$col$node))
  for (k in s$order2) els <- c(els, rect_el(X2, p2[[k]][1], BAR, diff(p2[[k]]), out_col[[s$M[[k]]]]))
  for (o in s$order3) els <- c(els, rect_el(X3, p3[[o]][1], BAR, diff(p3[[o]]), out_col[[o]]))
  hs <- st$sankey_head
  for (j in 1:3) {
    xc <- c(X1, X2, X3)[j] + BAR / 2
    els <- c(els, vapply(seq_along(s$headers[[j]]), function(i) {
      text_path(s$headers[[j]][i], xc, 14 + hs * 1.25 * (i - 1), hs, 700, TRUE, "middle", st$col$main)
    }, ""))
  }
  box <- function(xc, yc, lines, val, note = NULL, val_fill = st$col$main) {
    size <- st$size_label; lh <- size * 1.22; ns <- st$sankey_note
    rows <- c(lines, val, note); kinds <- c(rep("lab", length(lines)), "val", rep("note", length(note)))
    wd <- mapply(function(t, k) switch(k, lab = text_width(t, size), val = text_width(t, size, 600, TRUE), note = text_width(t, ns, 400, TRUE)), rows, kinds)
    w <- max(wd) + 14; h <- lh * length(rows) + 6
    x0 <- xc - w / 2; y0 <- yc - h / 2
    e <- rect_el(x0, y0, w, h, st$col$box, 2.5, st$col$text, 0.6)
    y <- y0 + 3 + lh * 0.8
    for (i in seq_along(rows)) {
      e <- c(e, switch(kinds[i],
        lab  = text_path(rows[i], xc, y, size, anchor = "middle", fill = st$col$text),
        val  = text_path(rows[i], xc, y, size, 600, TRUE, "middle", val_fill),
        note = text_path(rows[i], xc, y, ns, 400, TRUE, "middle", st$col$note)))
      y <- y + lh
    }
    e
  }
  # adaptado: el valor de cada caja es df$text (el original: paste0(round(s$C1[[k]]), "%"), paste0(round(val2[[k]]), "%")
  # y s$VAL3[[o]])
  for (k in order1) els <- c(els, box(X1 + BAR / 2, mean(p1[[k]]), s$LAB1[[k]], txt[[1]][[k]]))
  for (k in s$order2) els <- c(els, box(X2 + BAR / 2, mean(p2[[k]]), s$LAB2[[k]], txt[[2]][[k]]))
  for (o in s$order3) {
    els <- c(els, box(X3 + BAR / 2, mean(p3[[o]]), s$LAB3[[o]], txt[[3]][[o]], s$NOTE3[[o]],
                      if (o == "excluded") st$col$eu_text else st$col$main))
  }
  # nuevo (§4.5): lo dibujado por nodo, en el orden de df
  key <- paste(rep(c("1", "2", "3"), c(length(order1), length(s$order2), length(s$order3))), c(order1, s$order2, s$order3))
  i <- match(paste(df$grouping, df$label), key)
  if (anyNA(i)) stop("chart_sankey: df trae nodos que no están en sankey_def('", which, "')", call. = FALSE)
  frac <- c(C1[order1], val2[s$order2], val3[s$order3]) / 100      # alto dibujado (valor × scale) / alto del 100 %
  printed <- c(txt[[1]][order1], txt[[2]][s$order2], txt[[3]][s$order3])
  drawn <- tibble(order = df$order, kind = df$kind, grouping = df$grouping, label = df$label,
                  frac = unname(frac[i]), frac2 = NA_real_, text = unname(printed[i]), text2 = df$text2)
  chart_out(W, BOT + 10, els, drawn, st$size_label)
}


## Bloque cards — figuras 32, 41, 42 ----------------------------------------------------------------------------------
## Mapeo (§4.4): kind card; grouping = fila impresa; label = columna impresa (textos exactos en reference/<slug>/
## Figure_32_*.csv, _41_ y _42_); value %; value2 = % sin redondear de la tarjeta de referencia (NA en la propia
## tarjeta de referencia); text "41%"; text2 = delta de los valores REDONDEADOS, round(value) − round(value2), con signo
## "+", "−" (U+2212) o "±" (cero) y sufijo " points" ("+16 points", "−3 points"; report_charts.R:422-423), "" en la
## tarjeta de referencia. Tarjeta de referencia: en la 32 la primera columna de cada fila (ref = "col", Fully
## resolved); en la 41 y la 42 la primera fila de cada columna (ref = "row", Without a non-trivial problem). Extras lo,
## hi = rango de la escala de color POR FIGURA, como lo publicado (32: 2/62, 41: 29/55, 42: 30/55; no es común a 41 y
## 42). Medidas ST_CARDS. Origen: report_charts.R:389-431 mix_col, chart_scorecards(row_labels, col_labels, vals, ref,
## W, lohi, st); datos A03.R / A04.R / A05.R (add_report_vars + estimación). Pendiente (Etapa 3).
rows_cards <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_scorecards <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque network — figura 5 ----------------------------------------------------------------------------------------
## Mapeo (§4.4): kind node | edge; grouping ""; label problema / "A|B" (aristas en el orden de edges_all de
## network_graph.R, A antes que B, como la referencia); value prevalencia % (nodo) / lift (arista); value2 NA; text
## "(40%)" / ""; text2 ""; extras x, y solo en los nodos (NA en las aristas) = coordenadas de datos del layout de
## network_graph.R:148-199 (layout_with_fr + pad 0.16, min_dist 0.28, hstretch 1.15) sin renormalizar: x ≈ 0.5 ± 0.575
## (sale de [0, 1]), y 0-1; verify compara x, y con tol_xy 0.01 y el lift con 0.05. Sin columna n. Referencia:
## reference/<slug>/Figure_05_*.csv. Origen: nuevo (manifiesto: chart_network (nuevo)); la publicada solo se reescaló
## (standard_figures.R:16) desde analisis adicionales/cooccurrence_network.svg; layout igraph (deps.csv: exacta).
## Pendiente (Etapa 3).
rows_network <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_network <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloques ggplot — figuras 3 (scatter), 6 (matrix), 7 (severity_bars), 29 (upset) ------------------------------------
## plot(df, spec) devuelve un ggplot; 06_export.R lo guarda con svglite, contornea el texto (outline_svg_text: error si
## Inter Tight no tiene un glifo) y fija el ancho (set_size). Mapeo (§4.4): scatter: kind point, grouping faceta (Rural
## / Urban), label categoría, value prevalencia % (x), value2 severidad 0-10 (y; tol2 0.06), text/text2 "", extra n.
## matrix: kind cell, grouping categoría fila, label categoría columna, value % o NA, text "62%" | "N/A" | "–", text2
## banda ("50% or more", …). severity_bars: kind bar, grouping serie (ctx$country / "EU Average"), label severidad
## "0".."10", value %. upset: kind row | set; label de row = la combinación con separador " + " y los conjuntos en el
## orden fijo de la referencia ("Access to Assistance + Access to Information"), filas en el orden en que ComplexUpset
## dibuja las intersecciones, value %, text "12.3%", extra n; set: label = conjunto, text "36.5%". Referencias:
## reference/<slug>/Figure_03_*.csv, _06_, _07_ y _29_. Origen: render_figure.R, src/viz/jg_upset.R, module_1.R.
## Pendiente.
rows_scatter <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
rows_matrix <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
rows_severity_bars <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
rows_upset <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Prueba -----------------------------------------------------------------------------------------------------------
## cd /tmp && Rscript --vanilla SCRATCH/agente_C/prueba.R (la misma corrida que 04_svg.R; 29/29 ok).
##  - rows_grouped() sobre una tbl al estilo get_results_table (fila National + 28 filas barajadas dentro de 6 grupos,
##    eu = 61.13) reproduce order, kind, grouping, label y text del CSV publicado de la figura 9 (36 filas, 4 "N/A");
##    sin eu: 35 filas; scale = 10 y decimals = 1: "6.1", "6.0", "N/A", encabezados "".
##  - fmt_num(): "61%", "N/A", "100%", "0%", "6.0", "12.3%", character(0).
##  - chart_grouped(df de la figura 9, spec 9): chart_out con width_in 6.069, height_in 8.375, text_pt 10 y drawn (36
##    filas; frac NA en 6 encabezados y 4 N/A; text = df$text); rectángulos y textos idénticos a la figura publicada
##    redibujada (ver 04_svg.R); 0.16 s. track = FALSE, value_at_end = TRUE, scale = 10: sin pista gris, frac =
##    value / 10. label_col sin columna en LABEL_COL → error legible.
##  - Bloque dumbbell (Etapa 2): cd /tmp && Rscript --vanilla SCRATCH/etapa2_D/pruebas/prueba_dumbbell.R → 19/19 ok, y
##    Rscript main.R --stage all --figs 10 → figures ok (17 filas · 6.500×5.250 in · 1950×1575 px · 0 <text>),
##    verify ok 90/90 (value/value2 a ≤ 0.50 pp de la referencia redondeada; frac/frac2 a ≤ 0.00002), compare ok.
##  - rows_dumbbell(perception_experience(ctx, "JSE_access2info", "access2info")): kind, grouping, label, text y text2
##    idénticos a reference/hungary/Figure_10_*.csv (18-24: percepción NA → text "", experiencia "68%"); con
##    ("JSE_access2assis", "rep_c_published") idénticos a los de la figura 15 (18-24 sin ningún valor). tbl sintética:
##    orden alfabético inverso con locale C, "" (nunca "N/A") si NA, decimals = 1 → "10.2%"; sin fila nacional o sin
##    columna → error legible.
##  - chart_dumbbell(): figuras 10 y 15 a 6.500 × 5.250 in (pista 346 pt con L = 84); contra output/finales/Hungria/
##    figuras/Figure_10_*.svg y Figure_15_*.svg (mismo código de origen): mismos 1 rect, 27/26 circle, 13 line, 49/48
##    path y 0 <text>; diferencia máxima de posición 0.01 pt (solo los puntos y valores: la publicada tomó las
##    fracciones de la geometría del SVG ggplot, aquí value / 100). Fila sin valores: solo la etiqueta; con uno: un
##    punto y su texto a la derecha; drawn frac/frac2 NA donde no hay punto.
##  - Bloque sankey (Etapa 2): cd /tmp && Rscript --vanilla SCRATCH/etapa2_S/pruebas/prueba_sankey.R → 25/25 ok, y
##    main.R --stage all --figs 11 → verify "ok · 26/26 ok". rows_sankey(journey(ctx), "assistance"): 10 nodos con los
##    textos de la referencia (29% 71% 21% 8% 38% 25% 8% 32% 68% 33%; |value − ref| máx 0.40 pp); "drm": 9 nodos (13% 87%
##    13% 32% 47% 8% 28% 72% 56%; excluded = round(47.1 + 8.4)). chart_sankey(): 8.400 x 5.167 in, 0 <text>, PNG 2520 x
##    1550 px; los SVG de 11 y 16 son idénticos byte a byte (md5) a los publicados en output/finales/Hungria/figuras (21
##    rect + 40 path; 19 rect + 37 path). Errores legibles: which desconocido, campo de journey ausente, nodo faltante o
##    ajeno a sankey_def, valor NA en las columnas 1-2; df barajado → mismo SVG.
##  - Revisión del 23-oct (grupo G3; la misma prueba_G3.R de 04_svg.R, 67/67 ok): chart_grouped(df, spec, st = ST_A01,
##    fill_w = spec$w_in * 72) → figura 27 de 6.5 x 3 in con pista 281 pt: 12 rect y 9 path de texto idénticos a
##    output/finales/Hungria/figuras/Figure_27_*.svg (sin fill_w se detiene: el diseño mide 437 pt y la figura 468);
##    figuras 4 y 9: mismo SVG que antes. rows_grouped(): grupos en el orden de los niveles del factor aunque se
##    reordenen las filas; grouping NA → error; cadena real de la 9 = referencia. fmt_num(): "NA" / "" / "N/A" → "N/A",
##    factor por su texto, "-0%" → "0%", Inf → "N/A"; números: mismo texto. Encabezados de bloque contrastados con las
##    referencias (tarjetas, upset, red, embudo, sankey) y con standard_figures.R y network_graph.R (anchos y citas).
