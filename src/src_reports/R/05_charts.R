## 05_charts.R — filas (df, §4.4) y dibujo (chart_out, §4.5) por tipo de figura ------------------------------------
## Origen: src/hungary/finales/report_charts.R:127-145 (chart_grouped, cuerpo literal; la entrada es el df de §4.4 en
## vez de la spec con v1_frac / v1_text); figure_specs.R:186 (formato de los valores impresos: formatC(round(x,
## decimals), format = "f", digits = decimals)). Los demás bloques (report_charts.R:150-431) se portan en las Etapas
## 2/3: aquí solo el encabezado con el mapeo de columnas y stop("pendiente").
## Convención: rows_<tipo>(…) devuelve el df de §4.4 — order, kind, grouping, label, value, value2, text, text2 — con
## todo número impreso en text/text2 vía fmt_num(); chart_<tipo>(df, spec, …) dibuja df$text tal cual (no formatea ni
## recalcula), toma L de LABEL_COL[[spec$label_col]] y W de spec$w_in, y devuelve chart_out (04_svg.R) con drawn = lo
## efectivamente dibujado. Solo define funciones: nada se ejecuta al cargar.


## Formato de los números impresos ------------------------------------------------------------------------------------

# Contrato §5.C: "61%" (decimals 0), "12.3%" (1), "6.0" (decimals 1, pct = FALSE); NA → "N/A". Vectorizado; round()
# de R (medio al par). origen del formato: figure_specs.R:186
fmt_num <- function(x, decimals = 0, pct = TRUE) {
  if (length(x) == 0) return(character(0))
  out <- paste0(formatC(round(as.numeric(x), decimals), format = "f", digits = decimals), if (pct) "%")
  out[is.na(x)] <- "N/A"
  out
}


## Bloque grouped_bars — figuras 1, 4, 9, 12, 17, 20-28, 31, 33, 35, 38-40 ------------------------------------------
## Mapeo (§4.4): kind avg | header | row; grouping "" en avg, nombre del grupo en header y row; label "National Average"
## / "EU Average", nombre del grupo, categoría; value % 0-100 (media 0-10 en la 4; NA si n < 30); value2 NA; text
## "61%", "N/A", "6.0" ("" en header); text2 "". Orden: avg National, avg EU (si hay), luego cada grupo en el orden de
## clabels de get_results_table (header + filas) y, dentro del grupo, las categorías en orden alfabético inverso de
## arriba abajo (arrange(desc(label), .locale = "C")). Referencia: finales/datos/Figure_09_access_information.csv.
## Origen del dibujo: report_charts.R:131-145 chart_grouped ← src/viz/grouped_bars.R gen_grouped_bars (worker-viz.R:178).

# tbl = salida de get_results_table(ctx, …) (03_estimators.R): tibble(grouping, category, value) con la primera fila
# grouping "National" / category "National Average", los grupos en el orden de clabels y n < 30 → NA. eu = media UE
# (escalar; NULL si la figura no la lleva). scale = valor de la barra llena: 100 (%) o 10 (medias 0-10, figura 4);
# text lleva "%" solo con scale 100. Los grupos que se omiten (p. ej. "Category") se quitan antes, en
# get_results_table(omit = …). frac no va en el df (lo calcula chart_grouped para drawn).
rows_grouped <- function(tbl, eu = NULL, decimals = 0, scale = 100) {
  need <- c("grouping", "category", "value")
  if (!all(need %in% names(tbl))) stop("rows_grouped: tbl debe tener columnas ", paste(need, collapse = ", "), call. = FALSE)
  nat <- filter(tbl, grouping == "National")
  if (nrow(nat) != 1) stop("rows_grouped: tbl debe tener una sola fila grouping == 'National' (hay ", nrow(nat), ")", call. = FALSE)
  pct <- scale == 100
  avg <- tibble(kind = "avg", grouping = "", label = "National Average", value = as.numeric(nat$value))
  if (!is.null(eu)) avg <- bind_rows(avg, tibble(kind = "avg", grouping = "", label = "EU Average", value = as.numeric(eu)))
  groups <- unique(as.character(tbl$grouping[tbl$grouping != "National"]))      # orden de clabels
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

# origen: report_charts.R:128-145 (cuerpo literal). track = FALSE y value_at_end = TRUE reproducen gen_grouped_bars(
# perc = FALSE): sin pista gris y el valor justo después de la barra (figura 4). Adaptación al contrato §5.C: entra el
# df de §4.4 (spec_rows: v1_frac = value / scale, v1_text = text, dibujado tal cual); L = LABEL_COL[[spec$label_col]];
# W = spec$w_in * 72 (el original: xv + st$value_w = 437 pt con L = 149, que declara el mismo "6.069in"); el alto se
# calcula como el original (rows$y + 4) y no se fuerza a spec$h_in (06_export.R lo comprueba). drawn$frac = fracción
# de barra dibujada (NA en encabezados y valores suprimidos); scale = valor de la barra llena (10 en la figura 4).
chart_grouped <- function(df, spec, track = TRUE, value_at_end = FALSE, scale = 100, st = STD) {
  df <- df[order(df$order), ]
  spec_rows <- tibble(kind = df$kind, label = df$label, v1_frac = as.numeric(df$value) / scale,
                      v1_text = coalesce(as.character(df$text), ""))
  L <- label_col_pt(spec, spec_rows, st)
  x0 <- L + st$gap_label; xv <- x0 + st$track + st$gap_value
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
  chart_out(spec$w_in * 72, rows$y + 4, rows$els, drawn, st$size_label)
}


## Bloque paired_bars — figura 2 ------------------------------------------------------------------------------------
## Mapeo (§4.4): kind row; grouping ""; label categoría; value % país; value2 % UE; text "61%"; text2 "49%".
## label_col own (la figura mide su propia columna). Origen: report_charts.R:150-178 chart_paired(spec, L, legend, st);
## leyenda ctx$country / "EU Average" (sin literal de país). Pendiente (Etapa 2/3).
rows_paired <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_paired <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque dumbbell — figuras 10, 15 ---------------------------------------------------------------------------------
## Mapeo (§4.4): kind avg | header | row; grouping "" / grupo; label "National Average", grupo, categoría; value
## percepción %; value2 experiencia %; text "27%" o ""; text2 "61%" o "". Medidas ST_DUMB (pista: llena el ancho);
## label_col dumb. Origen: report_charts.R:182-218 chart_dumbbell(spec, L, legend, st); datos perception_experience()
## (03_estimators.R ← F06.R). Pendiente (Etapa 2/3).
rows_dumbbell <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_dumbbell <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque hbars — figuras 13, 14, 18, 19, 36 ------------------------------------------------------------------------
## Mapeo (§4.4): kind row; grouping ""; label categoría; value %; value2 NA; text "54%"; text2 "". Medidas ST_HBARS;
## label_col hbars. Origen: report_charts.R:224-242 chart_hbars_axis(spec, L, st) ← src/viz/horizontal_bars.R
## gen_horizontal_bars (eje 0-100 %). Pendiente (Etapa 2/3).
rows_hbars <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_hbars_axis <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque columns — figuras 30, 37 ----------------------------------------------------------------------------------
## Mapeo (§4.4): kind row; grouping ""; label categoría; value % país; value2 % UE; text "32%"; text2 "45%". Medidas
## ST_COLS. Origen: report_charts.R:247-279 chart_columns(spec, W, legend, st). Pendiente (Etapa 2/3).
rows_columns <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_columns <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque funnel — figuras 8, 34 ------------------------------------------------------------------------------------
## Mapeo (§4.4): kind stage; grouping ""; label etapa; value %; value2 NA; text "61%"; text2 "". Medidas ST_FUNNEL.
## Origen: report_charts.R:284-309 chart_funnel(rows, W, st) (diseño de charts5.py); datos journey(ctx)
## (03_estimators.R ← R03.R). Pendiente (Etapa 2/3).
rows_funnel <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_funnel <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque sankey — figuras 11, 16 -----------------------------------------------------------------------------------
## Mapeo (§4.4): kind node; grouping "1" | "2" | "3" (columna); label clave del nodo (contacted, not_contacted,
## appropriate, excluded, …); value %; value2 NA; text "29%"; text2 "". Medidas ST_SANKEY; textos publicados:
## standard_figures.R:128-177 (PUBLISHED_SANKEY; 16 excluded = "56%"). Origen: report_charts.R:315-383
## chart_sankey(s, W, st); datos journey(ctx). Pendiente (Etapa 2/3).
rows_sankey <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_sankey <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque cards — figuras 32, 41, 42 ----------------------------------------------------------------------------------
## Mapeo (§4.4): kind card; grouping fila impresa; label columna impresa; value %; value2 % de referencia; text "41%";
## text2 "+12" (delta); extras lo, hi. Medidas ST_CARDS. Origen: report_charts.R:389-431 mix_col,
## chart_scorecards(row_labels, col_labels, vals, ref, W, lohi, st); datos A03.R / A04.R / A05.R (add_report_vars +
## estimación). Pendiente (Etapa 2/3).
rows_cards <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_scorecards <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloque network — figura 5 ----------------------------------------------------------------------------------------
## Mapeo (§4.4): kind node | edge; grouping ""; label problema / "A|B"; value prevalencia % / lift; value2 NA; text
## "(40%)" / ""; text2 ""; extras x, y (0-1), n. Origen: nuevo (manifiesto: chart_network (nuevo)); la publicada solo
## se reescaló (standard_figures.R:16) desde analisis adicionales/cooccurrence_network.svg; layout igraph (deps.csv:
## exacta). Pendiente (Etapa 2/3).
rows_network <- function(...) stop("pendiente (Etapa 2/3)", call. = FALSE)
chart_network <- function(df, spec) stop("pendiente (Etapa 2/3)", call. = FALSE)


## Bloques ggplot — figuras 3 (scatter), 6 (matrix), 7 (severity_bars), 29 (upset) ------------------------------------
## plot(df, spec) devuelve un ggplot; 06_export.R lo guarda con svglite, contornea el texto (outline_svg_text) y fija el
## ancho (set_size). Mapeo (§4.4): scatter: kind point, grouping faceta (Rural / Urban), label categoría, value
## prevalencia % (x), value2 severidad 0-10 (y), text/text2 "", extra n. matrix: kind cell, grouping categoría fila,
## label categoría columna, value % o NA, text "62%" | "N/A" | "–", text2 banda. severity_bars: kind bar, grouping
## serie ({country} / "EU Average"), label severidad "0".."10", value %. upset: kind row | set, label combinación "A+B"
## (orden fijo) / conjunto, value %, text "12.3%". Origen: render_figure.R, src/viz/jg_upset.R, module_1.R. Pendiente.
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
