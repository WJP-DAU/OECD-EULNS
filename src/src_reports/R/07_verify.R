## 07_verify.R — verificación de la tabla, el dibujo y los archivos de cada figura contra reference/<slug> ---------
## Origen: nuevo para src_reports. Criterio de tolerancias tomado de src/hungary/finales/figure_specs.R:166-198
## (check_spec: texto impreso exacto; barra dentro de tol puntos de la escala; "suprimido" = ni dato ni texto).
## Usa de 00_setup.R: figure_stem(), figure_file(), log_msg(), first_line(), deps_exact_mismatch(); de 01_load.R:
## rel_path(), read_key(), secs(); de 04_svg.R: svg_size(path) → list(width_in, height_in) y count_live_text(path) → n
## de <text>; de 09_wjpr.R: wjpr_similarity() (metodo wjpr), wjpr_norm_text() y WJPR_DRAWN_OPT (contrato por fila).
##
##   run_verify(specs, cfg)           → tibble(fig, metodo, estado, verificacion, archivo) (§4.7), una fila por spec en
##                                      el orden del manifiesto; actualiza output/<slug>/tables/00-verificacion.csv (fig,
##                                      slug, kind, grouping, label, campo, esperado, obtenido, tol, resultado, nota): las
##                                      filas de las figuras verificadas en la corrida se reemplazan y las de las demás se
##                                      conservan (upsert por fig: una corrida con --figs no borra el resto); registra una
##                                      línea por figura con log_msg().
##   verify_figure(spec, cfg, status) → list(estado, verificacion, archivo, detalle); status = última fila de
##                                      log/figures_status.csv para la figura (lista) o NULL.
##
## Estados de la figura: ok | fallo | sin dibujo | error | sin salida | sin referencia | desactualizada | estatica |
## omitida. "sin dibujo" (metodo wjpr): nada falla pero el parecido con la publicada no se pudo medir (sin PNG nuevo
## por --no-png o sin Chrome, o sin PNG publicado): el dibujo no está verificado, no cuenta como verificada en la
## línea de cierre y --strict sale con 1.
## Resultado por fila del detalle: ok | fallo | conocida | suprimido | faltante | extra.
## Campos: figures | text | text2 | value | value2 | x | y | lo | hi | n | frac | frac2 | label_drawn | grouping_drawn |
## val | val2 | orden | parecido | parecido_local | textos | ancho | alto | png | texto_vivo | entorno.
## Pasos de verify_figure: (1) estados previos: figures dio "error" (verificacion "figures: <motivo de figures_status>"),
## sin tabla, static, sin referencia, desactualizada (también, con metodo wjpr, figures_status.csv sin wjpr_drawn o
## drawn.csv sin las columnas del contrato extendido: «corra --stage figures: … anteriores al contrato por fila»);
## (2) unión por (kind, grouping, label): faltante / extra; (3) orden
## de las filas comunes; (4) por fila: text/text2 exactos si la referencia los trae, en la tabla y en drawn.csv (lo
## dibujado: svg_r, y wjpr cuando el adaptador lo expone); |value − ref| ≤ tol (tol de la fila o cfg$verify$tol_pp;
## ambos NA → suprimido); value2 con tol2 de la fila (NA → la de value); x, y con tol_xy de la fila (NA → 0.01) y lo, hi,
## n exactos (|Δ| ≤ 1e-9) cuando la referencia los trae (decisión 1 de la revisión); (5) drawn.csv (drawn_ref_rows):
## frac/frac2 con cfg$verify$tol_frac y mismo orden (svg_r: drawn de chart_out; wjpr: attr(p, "wjpr")$drawn leído de
## ggplot_build, con order = posición visual; en los adaptadores wjpr que no lo exponen, fila frac "suprimido") y, con el
## contrato extendido, label_drawn/grouping_drawn exactos frente a label/grouping de la referencia y val/val2 frente a
## value/value2 con la tol de la fila (lo que drawn.csv trae NA no se compara);
## con metodo wjpr además: parecido = f1 de tinta del PNG nuevo frente a reference/<slug>/published/Figure_NN_slug.png
## ≥ cfg$verify$min_f1 y parecido_local = peor ventana de local_block_px px con píxeles distintos ≤ cfg$verify$max_local
## (wjpr_similarity a 96 dpi; sin uno de los dos PNG, suprimidos y la figura "sin dibujo") y textos = la comparación de
## los textos dibujados con df que hizo figures (wjpr_textos en figures_status; "sin textos que comparar" →
## suprimido); una salida sin wjpr_build en figures_status (dibujada con otro metodo) está desactualizada;
## (6) archivo: ancho exacto (±0.001) y alto ± size_tol_in, PNG = round(w_in·dpi) × round(h_in·dpi), 0 <text>,
## versiones 'exacta' de deps.csv (aviso, campo entorno); un PNG que falta da obtenido "ausente"; figures en texto,
## dibujo, tamaño o png → fila "figures" fallida y verificacion "figures: <motivo>"; (7) conocida: una fila fallida cuyo label y text obtenido
## aparezcan en spec$nota como [conocida: <label>=<text>] pasa a "conocida" (ni ok ni fallo; una figura solo con
## conocidas queda "ok" y su verificacion empieza por "conocida").
## Decisiones: value2, x, y, lo, hi, n y frac/frac2 solo se comparan en una fila cuando alguna de las dos tablas trae el
## dato (value2: en toda la figura); x, y, lo, hi, n además solo si la referencia trae alguno, y frac/frac2 solo si la
## referencia trae alguna fracción (sankey 11/16: sin barra medible); las claves repetidas se numeran en el orden de
## `order`, no en el orden físico del CSV; el texto de drawn.csv no añade filas: la fila text/text2 falla (nota
## "drawn.csv") si lo dibujado no es la referencia aunque la tabla lo sea; sin PNG → "suprimido" (omitido con --no-png o
## sin Chrome) salvo que figures_status diga estado "png" → fallo; el PNG se acepta con ±1 px de ancho y
## ±round(size_tol_in·dpi) px de alto (= 3) porque el alto real del SVG puede diferir del h_in del manifiesto dentro de
## size_tol_in. Los PNG publicados miden exactamente round(w_in·300) × round(h_in·300) (fig 9: 1821×2512; 2513 =
## ceiling(8.375·300) es la ventana de Chrome antes del recorte, no el PNG).

KEY_COLS <- c("kind", "grouping", "label", ".dup")   # clave de unión (+ índice de repetición)
XY_TOL   <- 0.01                                     # tol de x, y cuando la referencia no trae tol_xy (decisión 1)
MIN_F1   <- 0.995                                    # parecido mínimo (metodo wjpr) si el yml no trae verify$min_f1
# Diferencia local máxima con la publicada (metodo wjpr) y parámetros de wjpr_similarity() si el yml no los trae
# (calibrados el 23-oct: ver config/hungary.yml, verify$max_local)
VERIFY_LOCAL <- list(max_local = 0.5, local_block_px = 12, local_delta = 0.2, local_delta_flat = 0.05)
# Estados de figures que no son ok ni error: verify da "fallo · figures: <motivo>" aunque lo demás pase (revisión 4)
FIGURES_NOT_OK <- c("texto", "dibujo", "tamaño", "png")


## Utilidades ------------------------------------------------------------------------------------------------------

# Esquema de 00-verificacion.csv (vacío); verify_rows_empty() es lo mismo sin fig/slug (se añaden al final)
verify_empty <- function() tibble(
  fig = character(), slug = character(), kind = character(), grouping = character(), label = character(),
  campo = character(), esperado = character(), obtenido = character(), tol = numeric(), resultado = character(),
  nota = character()
)
verify_rows_empty <- function() verify_empty()[, setdiff(names(verify_empty()), c("fig", "slug"))]

# Una fila del detalle (sin fig/slug)
vrow <- function(campo, esperado, obtenido, resultado, tol = NA_real_, kind = "", grouping = "", label = "", nota = "") {
  tibble(kind = kind, grouping = grouping, label = label, campo = campo, esperado = as.character(esperado),
         obtenido = as.character(obtenido), tol = as.numeric(tol), resultado = resultado, nota = nota)
}

# Número → texto para esperado/obtenido ("NA" si falta); hasta 6 cifras significativas, sin ceros de relleno
fmt_val <- function(x) {
  if (is.null(x) || length(x) == 0) return("NA")
  x <- suppressWarnings(as.numeric(x))[1]
  if (is.na(x)) "NA" else trimws(formatC(x, digits = 6, format = "fg"))
}

within_tol <- function(a, b, tol) isTRUE(abs(as.numeric(a) - as.numeric(b)) <= as.numeric(tol) + 1e-9)


# Lee una tabla (salida, drawn o referencia) como texto y asegura las columnas del contrato: faltantes → NA / "".
# Numéricas: value, value2, frac, frac2, tol y (decisión 1 de la revisión) tol2, tol_xy, x, y, lo, hi, n.
# corregido (revisión 4): también las columnas opcionales de drawn.csv (contrato extendido): val, val2 numéricas y
# label_drawn, grouping_drawn texto con NA (ausentes o escritas "NA" = no expuestas; "" = nada impreso)
read_table_csv <- function(f) {
  num <- c("value", "value2", "frac", "frac2", "tol", "tol2", "tol_xy", "x", "y", "lo", "hi", "n", "val", "val2")
  chr <- c("kind", "grouping", "label", "text", "text2")
  opt <- c("label_drawn", "grouping_drawn")
  x <- read_csv(f, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE, progress = FALSE)
  for (c in c("order", num, opt)) if (!c %in% names(x)) x[[c]] <- NA_character_
  for (c in chr) if (!c %in% names(x)) x[[c]] <- ""
  x %>% mutate(order = suppressWarnings(as.integer(order)),
               across(all_of(num), ~ suppressWarnings(as.numeric(.x))),
               across(all_of(chr), ~ coalesce(.x, "")),
               across(all_of(opt), ~ if_else(.x %in% "NA", NA_character_, .x)))
}

# ¿drawn.csv escrito con el contrato extendido (revisión 4)? Su encabezado trae las columnas opcionales
# (WJPR_DRAWN_OPT, 09_wjpr.R), que figures escribe siempre (NA si no se exponen)
drawn_has_contract <- function(f) {
  hdr <- tryCatch(names(read_csv(f, n_max = 0, col_types = cols(.default = col_character()), show_col_types = FALSE,
                                 progress = FALSE)), error = function(e) character())
  all(WJPR_DRAWN_OPT %in% hdr)
}

# Filas del detalle de lo dibujado por fila frente a la referencia (sección 5 de verify_figure; la galería usa las
# mismas): orden visual, frac/frac2 ± tol_frac donde la referencia trae barra (has_frac) y, con el contrato extendido,
# label_drawn y grouping_drawn exactos (wjpr_norm_text) frente a label y grouping de la referencia, val ± la tol de la
# fila (o tol_pp) frente a value y val2 ± tol2 (o la de value) frente a value2; lo que drawn.csv trae NA no se compara
# y un val finito donde la referencia trae NA es fallo. ref_k y d_k con add_key().
drawn_ref_rows <- function(ref_k, d_k, tol_frac, tol_pp, has_frac) {
  out <- list(); add <- function(r) out[[length(out) + 1L]] <<- r
  # sufijo _dib: con _drawn, dplyr nombraba label_drawn de drawn.csv "label_drawn_drawn_drawn"
  bd <- inner_join(ref_k, d_k, by = KEY_COLS, suffix = c("_ref", "_dib")) %>% arrange(order_ref)
  if (nrow(bd) > 1) add(order_row(bd$order_ref, bd$order_dib, bd, nota = "drawn.csv"))
  txt_row <- function(campo, esperado, obtenido, meta) {
    vrow(campo, esperado, obtenido, if (identical(wjpr_norm_text(esperado), wjpr_norm_text(obtenido))) "ok" else "fallo", tol = 0,
         kind = meta$kind, grouping = meta$grouping, label = meta$label,
         nota = paste0("drawn.csv: nombre impreso en la posición de la fila (", if (campo == "label_drawn") "etiqueta" else "encabezado", ")"))
  }
  num_row <- function(campo, esperado, obtenido, tol, meta) {
    if (is.na(obtenido)) return(NULL)                    # no codificado o no expuesto
    cmp_num(campo, esperado, obtenido, tol, meta, nota = "drawn.csv: valor que codifica la geometría")
  }
  for (i in seq_len(nrow(bd))) {
    b <- bd[i, ]; meta <- list(kind = b$kind, grouping = b$grouping, label = b$label)
    tol_i <- if (is.na(b$tol_ref)) tol_pp else b$tol_ref
    tol2_i <- if (is.na(b$tol2_ref)) tol_i else b$tol2_ref
    if (has_frac && (!is.na(b$frac_ref) || !is.na(b$frac_dib))) add(cmp_num("frac", b$frac_ref, b$frac_dib, tol_frac, meta, nota = "drawn.csv"))
    if (has_frac && (!is.na(b$frac2_ref) || !is.na(b$frac2_dib))) add(cmp_num("frac2", b$frac2_ref, b$frac2_dib, tol_frac, meta, nota = "drawn.csv"))
    if (!is.na(b$label_drawn_dib)) add(txt_row("label_drawn", b$label, b$label_drawn_dib, meta))
    if (!is.na(b$grouping_drawn_dib)) add(txt_row("grouping_drawn", b$grouping, b$grouping_drawn_dib, meta))
    add(num_row("val", b$value_ref, b$val_dib, tol_i, meta))
    add(num_row("val2", b$value2_ref, b$val2_dib, tol2_i, meta))
  }
  md <- anti_join(filter(ref_k, !is.na(frac) | !is.na(frac2)), d_k, by = KEY_COLS) %>% arrange(order)
  for (i in seq_len(nrow(md))) {
    add(vrow("frac", fmt_val(coalesce(md$frac[i], md$frac2[i])), "", "fallo", tol = tol_frac, kind = md$kind[i],
             grouping = md$grouping[i], label = md$label[i], nota = "barra de la referencia sin fila en drawn.csv"))
  }
  # corregido (revisión 4, G3): una marca dibujada sin fila en la referencia (un nodo o una arista de más, un rótulo sin
  # punto, una barra en una posición sin categoría) da su propia fila "extra"; antes solo la veía figures y verify
  # fallaba por la fila «figures». Cuenta si codifica un valor o imprime algo (texto o nombre)
  ex <- anti_join(filter(d_k, is.finite(val) | is.finite(val2) | nzchar(text) | nzchar(text2) |
                            (!is.na(label_drawn) & nzchar(label_drawn))), ref_k, by = KEY_COLS) %>% arrange(order)
  for (i in seq_len(nrow(ex))) {
    shown <- c(fmt_val(coalesce(ex$val[i], ex$val2[i])), ex$text[i], ex$label_drawn[i])
    add(vrow("val", "", paste(shown[!is.na(shown) & nzchar(shown) & shown != "NA"], collapse = " · "), "extra",
             kind = ex$kind[i], grouping = ex$grouping[i], label = ex$label[i], nota = "drawn.csv: marca dibujada sin fila en la referencia"))
  }
  Filter(Negate(is.null), out)
}

# Clave (kind, grouping, label) + índice de repetición. Las claves duplicadas se numeran en el orden de `order`: en el
# orden físico del CSV, el mismo contenido escrito en otro orden (validate_df lo admite) fallaba
add_key <- function(x) x %>% arrange(order) %>% group_by(kind, grouping, label) %>% mutate(.dup = row_number()) %>% ungroup()

# Compara un número de la fila: ambos NA → suprimido; uno solo NA → fallo; |esperado − obtenido| ≤ tol → ok
cmp_num <- function(campo, esperado, obtenido, tol, meta, nota = "") {
  res <- if (is.na(esperado) && is.na(obtenido)) "suprimido"
         else if (is.na(esperado) || is.na(obtenido)) "fallo"
         else if (within_tol(esperado, obtenido, tol)) "ok" else "fallo"
  vrow(campo, fmt_val(esperado), fmt_val(obtenido), res, tol = tol, kind = meta$kind, grouping = meta$grouping,
       label = meta$label, nota = nota)
}

# Compara un texto impreso: el de la tabla y, si hay drawn.csv (svg_r), el dibujado deben ser el de la referencia
# (§4.5: drawn$text = df$text). dibujado = NULL si no hay drawn que mirar, NA si drawn.csv no tiene la fila.
cmp_text <- function(campo, esperado, tabla, dibujado, meta) {
  row <- function(obtenido, res, nota = "") vrow(campo, esperado, obtenido, res, tol = 0, kind = meta$kind,
                                                 grouping = meta$grouping, label = meta$label, nota = nota)
  if (!identical(esperado, tabla)) {
    otro <- !is.null(dibujado) && !is.na(dibujado) && !identical(dibujado, tabla)
    return(row(tabla, "fallo", if (otro) paste0("drawn.csv dibuja ", dibujado) else ""))
  }
  if (is.null(dibujado)) return(row(tabla, "ok"))
  if (is.na(dibujado)) return(row("", "fallo", "drawn.csv: la fila no está (texto sin dibujar)"))
  if (!identical(esperado, dibujado)) return(row(dibujado, "fallo", "drawn.csv: lo dibujado no es el texto de la tabla"))
  row(tabla, "ok")
}

# Fila "orden": posición de cada fila común en la referencia vs en la salida; primera discrepancia → fallo
order_row <- function(o_ref, o_out, rows, nota) {
  r_ref <- rank(o_ref, ties.method = "first", na.last = TRUE); r_out <- rank(o_out, ties.method = "first", na.last = TRUE)
  bad <- which(r_ref != r_out)
  if (length(bad) == 0) return(vrow("orden", paste0("1..", length(o_ref)), paste0("1..", length(o_ref)), "ok", nota = nota))
  i <- bad[1]
  vrow("orden", r_ref[i], r_out[i], "fallo", kind = rows$kind[i], grouping = rows$grouping[i], label = rows$label[i],
       nota = paste0(nota, ": posición esperada→obtenida de la primera fila fuera de orden"))
}

# Motivo de una fila (columna verificacion): "campo label: esperado→obtenido" | "faltante label" | "extra label"
motivo <- function(r) {
  res <- if (r$resultado == "conocida") sub("^conocida \\(era ([a-z]+)\\).*$", "\\1", r$nota) else r$resultado
  if (res %in% c("faltante", "extra")) return(paste(res, r$label))
  sprintf("%s%s: %s→%s", r$campo, if (nzchar(r$label)) paste0(" ", r$label) else "", r$esperado, r$obtenido)
}

# [conocida: <label>=<text>] en spec$nota → tibble(label, text)
known_pairs <- function(nota) {
  none <- tibble(label = character(), text = character())
  if (is.null(nota) || length(nota) == 0 || is.na(nota) || !nzchar(nota)) return(none)
  m <- str_match_all(nota, "\\[conocida:\\s*([^\\]=]*?)\\s*=\\s*([^\\]]*?)\\s*\\]")[[1]]
  if (nrow(m) == 0) none else tibble(label = m[, 2], text = m[, 3])
}


## figures_status.csv, caché, PNG y entorno ------------------------------------------------------------------------

# log/figures_status.csv completo (texto) o NULL si no existe
read_figures_status <- function(cfg) {
  f <- file.path(cfg$paths$output, "log", "figures_status.csv")
  if (!file.exists(f)) return(NULL)
  read_csv(f, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE, progress = FALSE)
}

# Última fila (lista) de figures_status para una figura, o NULL
last_status <- function(status_tbl, fig) {
  if (is.null(status_tbl) || nrow(status_tbl) == 0 || !"fig" %in% names(status_tbl)) return(NULL)
  f <- suppressWarnings(as.integer(status_tbl$fig))
  rows <- status_tbl[!is.na(f) & f == fig, ]
  if (nrow(rows) == 0) NULL else as.list(rows[nrow(rows), ])
}

# Campo de status como texto; NA si no existe o está vacío ("" o "NA")
sfield <- function(status, name) {
  v <- status[[name]]
  if (is.null(v) || length(v) == 0 || is.na(v[1]) || v[1] %in% c("", "NA")) NA_character_ else as.character(v[1])
}

# "desactualizada": md5 de la tabla, del SVG o de drawn.csv ≠ figures_status, o ctx_key ≠ cache/ctx_<slug>.key (si
# existe). figures_status guarda la clave del ctx (decisión 6 de la revisión: ctx$meta$ctx_key = ctx_key(cfg,
# master_key), el contenido de ctx_<slug>.key); se acepta además la igualdad con master.key o como prefijo de
# ctx_<slug>.key (filas escritas con la clave del master). corregido (V3-02): drawn.csv entra porque verify compara con
# él lo dibujado por fila también en metodo wjpr; un drawn.csv tocado después de figures ya no pasa por bueno.
is_stale <- function(status, f_table, f_svg, cfg, f_drawn = NULL) {
  no <- list(stale = FALSE, why = "")
  if (is.null(status)) return(no)
  md5 <- function(f) unname(tools::md5sum(f))
  m_t <- sfield(status, "md5_table")
  if (!is.na(m_t) && !identical(md5(f_table), m_t)) return(list(stale = TRUE, why = "md5 de la tabla ≠ figures_status"))
  m_s <- sfield(status, "md5_svg")
  if (!is.na(m_s) && file.exists(f_svg) && !identical(md5(f_svg), m_s)) return(list(stale = TRUE, why = "md5 del SVG ≠ figures_status"))
  m_d <- sfield(status, "md5_drawn")
  if (!is.null(f_drawn) && !is.na(m_d) && file.exists(f_drawn) && !identical(md5(f_drawn), m_d)) {
    return(list(stale = TRUE, why = "md5 de drawn.csv ≠ figures_status"))
  }
  sk <- sfield(status, "ctx_key")
  f_ckey <- file.path(cfg$paths$cache, paste0("ctx_", cfg$slug, ".key"))
  if (!is.na(sk) && file.exists(f_ckey)) {
    rk <- function(f) { k <- read_key(f); if (length(k) == 0 || is.na(k)) "" else k }
    fk <- rk(f_ckey); mk <- rk(file.path(cfg$paths$cache, "master.key"))
    if (!(identical(sk, fk) || identical(sk, mk) || startsWith(fk, paste0(sk, "|")))) {
      return(list(stale = TRUE, why = "ctx_key ≠ cache/ctx_<slug>.key"))
    }
  }
  no
}

# Paquetes 'exacta' de deps.csv con build instalada distinta (tibble paquete, version, instalada) o NULL: versión y, si
# origen fija un commit ("…/WJPr@382aee9"), también el commit ("1.1.0@local" ≠ "1.1.0@382aee9"; deps_exact_mismatch)
deps_mismatch <- function(cfg) {
  f <- cfg$paths$deps
  if (is.null(f) || is.na(f) || !file.exists(f)) return(NULL)
  deps <- read_csv(f, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE, progress = FALSE)
  dm <- deps_exact_mismatch(deps)
  if (nrow(dm) == 0) NULL else transmute(dm, paquete, version = esperada, instalada = coalesce(instalada, "-"))
}

# Umbral y parámetros de la diferencia local (metodo wjpr) de cfg$verify, con los defectos de VERIFY_LOCAL; error
# legible si uno no es válido (check_cfg, 00_setup.R, no los conoce: un umbral mal escrito no se cambia en silencio)
verify_local_params <- function(cfg) {
  v <- cfg$verify %||% list()
  p <- setNames(lapply(names(VERIFY_LOCAL), function(k) v[[k]] %||% VERIFY_LOCAL[[k]]), names(VERIFY_LOCAL))
  num <- function(x, lo, hi) is.numeric(x) && length(x) == 1 && !is.na(x) && x >= lo && x <= hi
  bad <- c(if (!num(p$max_local, 0, 1)) "verify$max_local (entre 0 y 1)",
           if (!num(p$local_block_px, 2, 200) || p$local_block_px != round(p$local_block_px)) "verify$local_block_px (entero de 2 a 200)",
           if (!num(p$local_delta, 0, 1)) "verify$local_delta (entre 0 y 1)",
           if (!num(p$local_delta_flat, 0, 1)) "verify$local_delta_flat (entre 0 y 1)")
  if (length(bad)) stop(cfg$slug %||% "config", ".yml: no son válidas las claves ", paste(bad, collapse = ", "), call. = FALSE)
  p
}

# metodo wjpr: parecido del PNG nuevo con el publicado (wjpr_similarity, 09_wjpr.R, a 96 dpi) en dos filas: parecido =
# f1 de tinta (global; 1 = mismo dibujo) ≥ min_f1, y parecido_local = la peor ventana de lp$local_block_px px (fracción
# de píxeles distintos; 0 = igual) ≤ lp$max_local. Una build de WJPr que cambia el dibujo (orden de las categorías, una
# leyenda por defecto) baja el f1 aunque la tabla y los textos sean los mismos; un error localizado (una barra más
# larga, colores de banda cambiados) apenas mueve el f1 y lo ve parecido_local. Sin PNG nuevo o sin publicada: las dos
# suprimidas (verify_figure deja la figura "sin dibujo").
# corregido (V3-01): antes solo el f1, que daba ok con una barra 10 pp más larga o los colores de banda invertidos.
parecido_rows <- function(f_png, f_pub, min_f1, lp, status, root) {
  esp <- paste0("≥ ", fmt_val(min_f1)); esp_l <- paste0("≤ ", fmt_val(lp$max_local))
  dib <- if (is.na(sfield(status, "wjpr_build"))) "" else paste0("; dibujada con WJPr ", sfield(status, "wjpr_build"))
  none <- function(why) list(vrow("parecido", esp, "", "suprimido", nota = paste0(why, ": no se mide", dib)),
                             vrow("parecido_local", esp_l, "", "suprimido", nota = paste0(why, ": no se mide")))
  if (!file.exists(f_png)) return(none("sin PNG nuevo (--no-png o sin Chrome)"))
  if (!file.exists(f_pub)) return(none(paste0("sin PNG publicado (", rel_path(f_pub, root), ")")))
  sm <- tryCatch(wjpr_similarity(f_png, f_pub, block = lp$local_block_px, delta = lp$local_delta,
                                 delta_flat = lp$local_delta_flat), error = function(e) e)
  if (inherits(sm, "error")) {
    why <- paste("wjpr_similarity:", first_line(conditionMessage(sm)))
    return(list(vrow("parecido", esp, "", "fallo", nota = why), vrow("parecido_local", esp_l, "", "fallo", nota = why)))
  }
  list(
    vrow("parecido", esp, sprintf("%.4f", sm$f1), if (isTRUE(sm$f1 >= min_f1 - 1e-9)) "ok" else "fallo",
         nota = sprintf("f1 de tinta a 96 dpi frente a %s (precision %.4f, recall %.4f, mad %.4f)%s", rel_path(f_pub, root),
                        sm$precision, sm$recall, sm$mad, dib)),
    vrow("parecido_local", esp_l, sprintf("%.3f", sm$local), if (isTRUE(sm$local <= lp$max_local + 1e-9)) "ok" else "fallo",
         nota = paste0(sprintf("fracción de píxeles distintos (|Δ| RGB > %s tolerando 1 px, o > %s donde las dos son planas) en la peor ventana de %d×%d px a 96 dpi",
                               fmt_val(lp$local_delta), fmt_val(lp$local_delta_flat), as.integer(lp$local_block_px), as.integer(lp$local_block_px)),
                       if (sm$local_px > 0) sprintf(", centrada en (%.2f, %.2f) in desde arriba a la izquierda; %d px distintos en la figura",
                                                    sm$local_x_in, sm$local_y_in, sm$local_px) else "; ningún píxel distinto"))
  )
}

# metodo wjpr: textos dibujados frente a df (columnas attr(p, "wjpr")$compara y rótulos del adaptador), comparados en
# figures con el plot vivo (drawn_text_diff_wjpr, 06_export.R) y guardados en figures_status (wjpr_textos). Es el
# multiconjunto: con lo dibujado por fila (por_fila) el texto de cada fila ya se compara en las filas text/text2.
# corregido (V3-07): "sin textos que comparar" (df sin textos impresos y ningún valor dibujado: figs 3 y 7) era
# "iguales" y contaba como comprobado; ahora es suprimido con su nota
textos_row <- function(status, por_fila = FALSE) {
  t <- sfield(status, "wjpr_textos")
  if (is.na(t)) return(vrow("textos", "iguales", "", "suprimido", nota = "sin wjpr_textos en figures_status: no se comprueba"))
  if (identical(t, "sin textos que comparar")) {
    return(vrow("textos", "iguales", t, "suprimido",
                nota = "sin textos que comparar: df no trae textos impresos (columnas compara), el adaptador no declara rótulos y no se dibuja ningún valor"))
  }
  vrow("textos", "iguales", t, if (identical(t, "iguales")) "ok" else "fallo",
       nota = paste0("multiconjunto de textos dibujados (wjpr_texts) frente a df$text/text2, medido en figures",
                     if (por_fila) "; el de cada fila, en las filas text/text2 (drawn.csv)" else
                       "; sin la fila de cada texto (el adaptador no expone lo dibujado por fila)"))
}

# Dimensiones (ancho, alto) en px de un PNG y dpi declarados, como png_props() de build_finales.R:154-157 pero con
# native = TRUE (una matriz de enteros en vez de alto × ancho × canales en doble precisión)
png_dims <- function(f) {
  x <- png::readPNG(f, native = TRUE, info = TRUE)
  info <- attr(x, "info")
  list(px = c(ncol(x), nrow(x)), dpi = if (!is.null(info$dpi)) round(info$dpi[1]) else NA_real_)
}


## Verificación de una figura ----------------------------------------------------------------------------------------

verify_figure <- function(spec, cfg, status = NULL) {
  stem <- figure_stem(spec); root <- cfg$paths$root; fig <- sprintf("%02d", spec$fig)
  f_table <- file.path(cfg$paths$output, "tables", paste0(stem, ".csv"))
  f_drawn <- file.path(cfg$paths$output, "tables", paste0(stem, ".drawn.csv"))
  f_svg   <- file.path(cfg$paths$output, "figures", paste0(stem, ".svg"))
  f_png   <- file.path(cfg$paths$output, "figures", paste0(stem, ".png"))
  f_ref   <- file.path(cfg$paths$reference, paste0(stem, ".csv"))
  archivo <- if (file.exists(f_svg)) rel_path(f_svg, root) else if (file.exists(f_table)) rel_path(f_table, root) else ""
  done <- function(estado, verificacion = "", detalle = verify_empty()) {
    list(estado = estado, verificacion = verificacion, archivo = archivo, detalle = detalle)
  }

  # (1) estados previos, en este orden. El error de figures va primero (decisión 2 de la revisión): export_figure borra
  # las salidas al empezar, así que un data() que fallaba se veía como "sin salida" y su mensaje solo quedaba en el log
  if (identical(sfield(status, "estado"), "error")) {
    m <- sfield(status, "motivo")
    if (!nzchar(archivo)) archivo <- rel_path(figure_file(spec, cfg), root)
    return(done("error", paste0("figures: ", if (is.na(m)) "error (ver log)" else first_line(m, 60))))
  }
  if (!file.exists(f_table)) return(done("sin salida"))
  if (identical(spec$metodo, "static")) return(done("estatica", "SVG estático: no se verifica"))
  if (!file.exists(f_ref)) return(done("sin referencia", "corra tools/build_reference.R"))
  st <- is_stale(status, f_table, f_svg, cfg, f_drawn)
  if (st$stale) return(done("desactualizada", st$why))
  wjpr <- identical(spec$metodo, "wjpr")
  if (wjpr && !is.null(status) && is.na(sfield(status, "wjpr_build"))) {
    return(done("desactualizada", "salida dibujada sin WJPr (otro metodo): corra --stage figures"))
  }
  # corregido (revisión 4): un figures_status.csv sin la columna wjpr_drawn o un drawn.csv sin las columnas del
  # contrato extendido son de una corrida anterior al contrato por fila; antes verify afirmaba que el adaptador «no
  # expone lo dibujado por fila», lo que no es cierto (lo expone y no se escribió)
  if (wjpr && ((!is.null(status) && !"wjpr_drawn" %in% names(status)) || (file.exists(f_drawn) && !drawn_has_contract(f_drawn)))) {
    return(done("desactualizada", "corra --stage figures: figures_status.csv o drawn.csv anteriores al contrato por fila"))
  }

  tol_pp <- cfg$verify$tol_pp %||% 0.6; tol_frac <- cfg$verify$tol_frac %||% 0.006
  size_tol <- cfg$verify$size_tol_in %||% 0.01; dpi <- cfg$layout$dpi %||% 300
  lp <- verify_local_params(cfg)
  out <- read_table_csv(f_table); ref <- read_table_csv(f_ref)
  # lo dibujado por fila: drawn.csv de svg_r y, con metodo wjpr, el del adaptador que lo expone (wjpr_drawn en
  # figures_status: número de filas; NA si no lo expone); metodo ggplot no dibuja filas
  n_drawn <- suppressWarnings(as.integer(sfield(status, "wjpr_drawn")))
  wjpr_rows <- wjpr && isTRUE(n_drawn > 0)
  d_k <- if ((!spec$metodo %in% c("ggplot", "wjpr") || wjpr_rows) && file.exists(f_drawn)) add_key(read_table_csv(f_drawn)) else NULL
  det <- list()
  add <- function(row) det[[length(det) + 1L]] <<- row
  if (wjpr_rows && (is.null(d_k) || nrow(d_k) == 0)) {
    add(vrow("frac", n_drawn, if (is.null(d_k)) "" else "0", "fallo", tol = tol_frac,
             nota = paste0("figures registró ", n_drawn, " filas dibujadas y ", basename(f_drawn), " no las tiene")))
    d_k <- NULL
  }

  # (2) unión por (kind, grouping, label): fila de la referencia sin tabla → faltante; de la tabla sin referencia → extra
  ref_k <- add_key(ref); out_k <- add_key(out)
  if (any(ref_k$.dup > 1) || any(out_k$.dup > 1)) log_msg("fig ", fig, ": claves (kind, grouping, label) repetidas; se emparejan en el orden de order")
  both  <- inner_join(ref_k, out_k, by = KEY_COLS, suffix = c("_ref", "_out")) %>% arrange(order_ref)
  miss  <- anti_join(ref_k, out_k, by = KEY_COLS) %>% arrange(order)
  extra <- anti_join(out_k, ref_k, by = KEY_COLS) %>% arrange(order)
  shown <- function(text, value) if (nzchar(text)) text else fmt_val(value)
  for (i in seq_len(nrow(miss))) {
    add(vrow("value", shown(miss$text[i], miss$value[i]), "", "faltante", kind = miss$kind[i], grouping = miss$grouping[i],
             label = miss$label[i], nota = "fila de la referencia sin equivalente en la tabla"))
  }
  for (i in seq_len(nrow(extra))) {
    add(vrow("value", "", shown(extra$text[i], extra$value[i]), "extra", kind = extra$kind[i], grouping = extra$grouping[i],
             label = extra$label[i], nota = "fila de la tabla sin equivalente en la referencia"))
  }

  # (3) orden: la secuencia de order de las filas comunes debe ser la misma
  if (nrow(both) > 1) add(order_row(both$order_ref, both$order_out, both, nota = "tabla"))

  # (4) por fila común: text/text2 exactos si la referencia los trae (en la tabla y en drawn.csv); value con la tol de
  # la fila; value2 con tol2 (NA → la de value); x, y con tol_xy (NA → 0.01) y lo, hi, n exactos si la referencia los
  # trae. Antes value2 usaba siempre la tol de value (fig 3: 0.6 en vez de 0.06) y x, y, lo, hi, n no se comparaban
  if (!is.null(d_k)) both <- left_join(both, select(d_k, all_of(KEY_COLS), text_drawn = text, text2_drawn = text2), by = KEY_COLS)
  uses_value2 <- any(!is.na(both$value2_ref)) || any(!is.na(both$value2_out))
  xtra <- c("x", "y", "lo", "hi", "n")
  xtra <- xtra[vapply(xtra, function(v) any(!is.na(ref[[v]])), logical(1))]
  for (i in seq_len(nrow(both))) {
    b <- both[i, ]; meta <- list(kind = b$kind, grouping = b$grouping, label = b$label)
    tol_i <- if (is.na(b$tol_ref)) tol_pp else b$tol_ref
    tol2_i <- if (is.na(b$tol2_ref)) tol_i else b$tol2_ref
    if (nzchar(b$text_ref)) add(cmp_text("text", b$text_ref, b$text_out, if (is.null(d_k)) NULL else b$text_drawn, meta))
    if (nzchar(b$text2_ref)) add(cmp_text("text2", b$text2_ref, b$text2_out, if (is.null(d_k)) NULL else b$text2_drawn, meta))
    add(cmp_num("value", b$value_ref, b$value_out, tol_i, meta))
    if (uses_value2) add(cmp_num("value2", b$value2_ref, b$value2_out, tol2_i, meta))
    for (v in xtra) {
      e <- b[[paste0(v, "_ref")]]; o <- b[[paste0(v, "_out")]]
      if (is.na(e) && is.na(o)) next
      tol_v <- if (v %in% c("x", "y")) (if (is.na(b$tol_xy_ref)) XY_TOL else b$tol_xy_ref) else 0
      add(cmp_num(v, e, o, tol_v, meta))
    }
  }

  # (5) dibujo: drawn.csv vs frac/frac2 de la referencia (solo donde alguna trae barra) y mismo orden; ggplot no dibuja.
  # Si la referencia no trae ninguna fracción (sankey 11/16: sin barra medible) las de drawn.csv no se comparan.
  # metodo wjpr: lo mismo con el drawn.csv del adaptador cuando lo expone (order = posición visual: "orden" mira el
  # orden dibujado de arriba abajo) y, siempre, parecido + parecido_local con la publicada y textos; sin PNG que
  # comparar la figura queda "sin dibujo".
  # corregido (V3-01, V3-02, V3-03): con wjpr solo había parecido (f1 global, que da ok con una barra 10 pp más larga o
  # los colores de banda invertidos) y textos (multiconjunto, que no ve un valor en la fila equivocada), y sin PNG la
  # figura quedaba ok sin ninguna comprobación del dibujo. No es cierto que WJPr no exponga las barras: ggplot_build()
  # las da (wjpr_drawn, 09_wjpr.R).
  f1 <- NA_real_; loc <- NA_real_; sin_dibujo <- NA_character_
  has_frac <- any(!is.na(ref$frac)) || any(!is.na(ref$frac2))
  # corregido (revisión 4): además de frac y orden, el nombre impreso (label_drawn, grouping_drawn) y el valor que
  # codifica la geometría (val, val2) de drawn.csv frente a la referencia (drawn_ref_rows, la regla de la galería)
  if (!is.null(d_k)) {
    for (r in drawn_ref_rows(ref_k, d_k, tol_frac, tol_pp, has_frac)) add(r)
    # corregido (revisión 4, G1): solo si drawn.csv trae fracciones (antes también «0 fracciones … sin comparar»)
    n_fr <- sum(!is.na(d_k$frac) | !is.na(d_k$frac2))
    if (!has_frac && n_fr > 0) log_msg("fig ", fig, ": la referencia no trae frac/frac2; ", n_fr, " fracciones de drawn.csv sin comparar")
  } else if (!spec$metodo %in% c("ggplot", "wjpr") && has_frac) {
    add(vrow("frac", sum(!is.na(ref$frac) | !is.na(ref$frac2)), "", "fallo", tol = tol_frac, nota = paste0("no existe ", basename(f_drawn))))
  }
  if (wjpr) {
    if (is.null(d_k) && !wjpr_rows) {
      add(vrow("frac", "", "", "suprimido", nota = paste0(spec$funcion %||% "el adaptador", " no expone lo dibujado por fila",
                                                          " (attr(p, \"wjpr\")$drawn): el dibujo lo miden parecido y parecido_local")))
    }
    pr <- parecido_rows(f_png, file.path(cfg$paths$reference, "published", paste0(stem, ".png")),
                        cfg$verify$min_f1 %||% MIN_F1, lp, status, root)
    if (pr[[1]]$resultado == "ok") f1 <- as.numeric(pr[[1]]$obtenido)
    if (pr[[2]]$resultado == "ok") loc <- as.numeric(pr[[2]]$obtenido)
    if (pr[[1]]$resultado == "suprimido") sin_dibujo <- sub(": no se mide.*$", "", pr[[1]]$nota)
    for (r in pr) add(r)
    add(textos_row(status, por_fila = !is.null(d_k)))
  }

  # (6) archivo: tamaño del SVG, texto vivo, PNG y entorno
  if (file.exists(f_svg)) {
    sz <- tryCatch(svg_size(f_svg), error = function(e) e)
    if (inherits(sz, "error")) {
      why <- paste("svg_size:", first_line(conditionMessage(sz)))
      add(vrow("ancho", fmt_val(spec$w_in), "", "fallo", tol = 0.001, nota = why))
      add(vrow("alto", fmt_val(spec$h_in), "", "fallo", tol = size_tol, nota = why))
    } else {
      add(vrow("ancho", fmt_val(spec$w_in), fmt_val(sz$width_in), if (within_tol(sz$width_in, spec$w_in, 0.001)) "ok" else "fallo", tol = 0.001))
      add(vrow("alto", fmt_val(spec$h_in), fmt_val(sz$height_in), if (within_tol(sz$height_in, spec$h_in, size_tol)) "ok" else "fallo", tol = size_tol))
    }
    nt <- tryCatch(count_live_text(f_svg), error = function(e) e)
    if (inherits(nt, "error")) {
      add(vrow("texto_vivo", "0", "", "fallo", tol = 0, nota = paste("count_live_text:", first_line(conditionMessage(nt)))))
    } else {
      live <- isTRUE(suppressWarnings(as.numeric(nt)) > 0)
      add(vrow("texto_vivo", "0", fmt_val(nt), if (isTRUE(suppressWarnings(as.numeric(nt)) == 0)) "ok" else "fallo", tol = 0,
               nota = if (live) "<text> sin contornear en el SVG" else ""))
    }
  } else {
    add(vrow("ancho", fmt_val(spec$w_in), "", "fallo", tol = 0.001, nota = paste0("no existe ", basename(f_svg))))
    add(vrow("alto", fmt_val(spec$h_in), "", "fallo", tol = size_tol, nota = paste0("no existe ", basename(f_svg))))
  }
  px_exp <- c(round(spec$w_in * dpi), round(spec$h_in * dpi))
  px_tol <- c(1, max(1, round(size_tol * dpi)))     # ±1 px de ancho; de alto, lo que admite size_tol_in (0.01 in = 3 px)
  px_txt <- function(p) sprintf("%dx%d", as.integer(p[1]), as.integer(p[2]))
  if (file.exists(f_png)) {
    pd <- tryCatch(png_dims(f_png), error = function(e) e)
    if (inherits(pd, "error")) {
      add(vrow("png", px_txt(px_exp), "", "fallo", tol = px_tol[2], nota = paste("png::readPNG:", first_line(conditionMessage(pd)))))
    } else {
      add(vrow("png", px_txt(px_exp), px_txt(pd$px), if (isTRUE(all(abs(pd$px - px_exp) <= px_tol))) "ok" else "fallo", tol = px_tol[2],
               nota = paste0("round(w_in·dpi) × round(h_in·dpi) a ", dpi, " dpi (±", px_tol[1], " px ancho, ±", px_tol[2], " px alto)", if (!is.na(pd$dpi)) paste0("; el PNG declara ", pd$dpi, " dpi") else "")))
    }
  } else {
    # corregido (revisión 4): obtenido "ausente" (antes "", y la consola decía «png: 1821x1296→» sin decir que falta)
    png_failed <- identical(sfield(status, "estado"), "png")
    add(vrow("png", px_txt(px_exp), "ausente", if (png_failed) "fallo" else "suprimido", tol = px_tol[2],
             nota = if (png_failed) "Chrome no generó el PNG (estado png en figures_status)" else "PNG ausente (--no-png o sin Chrome): no se verifica"))
  }
  dm <- deps_mismatch(cfg)
  if (!is.null(dm) && nrow(dm) > 0) {
    add(vrow("entorno", paste(dm$paquete, dm$version, collapse = ", "), paste(dm$paquete, dm$instalada, collapse = ", "), "ok",
             nota = "aviso: versiones 'exacta' de deps.csv distintas de las instaladas (no es fallo)"))
  }

  # corregido (revisión 4): una figura que figures dejó en texto, dibujo, tamaño o png no sale "ok": la fila
  # "figures" (primera del detalle) falla con el motivo de figures_status y la tabla la muestra "fallo · figures:
  # <motivo>", como el error; antes solo se propagaba "error" (14 textos dudosos por una fuente equivocada daban "ok")
  est_fig <- sfield(status, "estado")
  fig_bad <- isTRUE(est_fig %in% FIGURES_NOT_OK)
  mot_fig <- if (fig_bad) coalesce(sfield(status, "motivo"), paste0(est_fig, " (ver log)")) else NA_character_
  if (fig_bad) det <- c(list(vrow("figures", "ok", est_fig, "fallo", nota = paste0("estado de --stage figures: ", mot_fig))), det)

  # (7) conocida: fila fallida cuyo label y text obtenido figuran en spec$nota como [conocida: <label>=<text>]
  detalle <- bind_rows(verify_rows_empty(), det)
  kp <- known_pairs(spec$nota)
  if (nrow(kp) > 0 && nrow(detalle) > 0) {
    got <- out %>% arrange(order) %>% distinct(kind, grouping, label, .keep_all = TRUE) %>% select(kind, grouping, label, text_out = text)
    txt <- detalle %>% select(kind, grouping, label) %>% left_join(got, by = c("kind", "grouping", "label")) %>%
      pull(text_out) %>% coalesce("")
    known <- detalle$resultado %in% c("fallo", "faltante", "extra") &
      paste(detalle$label, txt, sep = "\r") %in% paste(kp$label, kp$text, sep = "\r")
    if (any(known)) {
      detalle$nota[known] <- paste0("conocida (era ", detalle$resultado[known], "): declarada en la nota del manifiesto",
                                    ifelse(nzchar(detalle$nota[known]), paste0("; ", detalle$nota[known]), ""))
      detalle$resultado[known] <- "conocida"
    }
  }

  n_total <- nrow(detalle); n_ok <- sum(detalle$resultado %in% c("ok", "suprimido"))
  fails <- which(detalle$resultado %in% c("fallo", "faltante", "extra")); known <- which(detalle$resultado == "conocida")
  # corregido (V3-03): metodo wjpr sin parecido medido (sin PNG nuevo o publicado) → "sin dibujo", no "ok": el dibujo
  # no se comprobó (lo por fila de drawn.csv no ve la maqueta) y --strict sale con 1
  estado <- if (length(fails) > 0) "fallo" else if (!is.na(sin_dibujo)) "sin dibujo" else "ok"
  verificacion <- if (fig_bad) paste0("figures: ", first_line(mot_fig, 60))
                  else if (length(fails) > 0) sprintf("%d/%d · %s", n_ok, n_total, motivo(detalle[fails[1], ]))
                  else if (!is.na(sin_dibujo)) sprintf("%d/%d · sin dibujo: %s", n_ok, n_total, sin_dibujo)
                  else if (length(known) > 0) sprintf("%d/%d · conocida %s", n_ok, n_total, motivo(detalle[known[1], ]))
                  else sprintf("%d/%d ok%s%s", n_ok, n_total, if (is.na(f1)) "" else sprintf(" · parecido f1 %.3f", f1),
                               if (is.na(loc)) "" else sprintf(" · local %.3f", loc))
  detalle <- bind_cols(tibble(fig = rep(fig, n_total), slug = rep(spec$slug, n_total)), detalle)
  done(estado, verificacion, detalle)
}


## Todas las figuras -------------------------------------------------------------------------------------------------

# 00-verificacion.csv anterior con el esquema de verify_empty(), o vacío si no existe o no se puede leer
read_verificacion <- function(f) {
  if (!file.exists(f)) return(verify_empty())
  x <- tryCatch(read_csv(f, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE, progress = FALSE),
                error = function(e) { log_msg("verify: no se pudo leer ", basename(f), " (", first_line(conditionMessage(e)), "); se reescribe"); NULL })
  if (is.null(x) || !"fig" %in% names(x)) return(verify_empty())
  if ("tol" %in% names(x)) x$tol <- suppressWarnings(as.numeric(x$tol))
  bind_rows(verify_empty(), x)[, names(verify_empty())]
}

run_verify <- function(specs, cfg) {
  t0 <- Sys.time()
  lp <- verify_local_params(cfg)         # un umbral inválido detiene la etapa (no cada figura en "error")
  log_msg("verify: parecido f1 ≥ ", fmt_val(cfg$verify$min_f1 %||% MIN_F1), " · parecido_local ≤ ", fmt_val(lp$max_local),
          " (ventanas de ", lp$local_block_px, " px, |Δ| > ", fmt_val(lp$local_delta), " o > ", fmt_val(lp$local_delta_flat),
          " en zonas planas) · frac ± ", fmt_val(cfg$verify$tol_frac %||% 0.006))
  status_tbl <- read_figures_status(cfg)
  if (is.null(status_tbl)) log_msg("verify: sin log/figures_status.csv (no se comprueban md5 ni ctx_key)")
  rows <- vector("list", length(specs)); dets <- vector("list", length(specs)); done_figs <- character()
  for (i in seq_along(specs)) {
    spec <- specs[[i]]; fig <- sprintf("%02d", spec$fig); metodo <- spec$metodo %||% ""
    if (!is.null(spec$selected) && !isTRUE(spec$selected)) {
      rows[[i]] <- tibble(fig = fig, metodo = metodo, estado = "omitida", verificacion = "", archivo = "")
      next
    }
    done_figs <- c(done_figs, fig)
    res <- tryCatch(
      withCallingHandlers(
        verify_figure(spec, cfg, last_status(status_tbl, spec$fig)),
        warning = function(w) { log_msg("fig ", fig, " aviso: ", conditionMessage(w)); invokeRestart("muffleWarning") }),
      error = function(e) list(estado = "error", verificacion = first_line(conditionMessage(e)), archivo = "", detalle = verify_empty()))
    rows[[i]] <- tibble(fig = fig, metodo = metodo, estado = res$estado, verificacion = res$verificacion, archivo = res$archivo)
    dets[[i]] <- res$detalle
    log_msg("verify fig ", fig, " ", spec$slug, ": ", res$estado, if (nzchar(res$verificacion)) paste0(" · ", res$verificacion) else "")
  }
  detalle <- bind_rows(verify_empty(), dets)
  # upsert por fig: las figuras verificadas reemplazan sus filas y las demás conservan las de su última verificación
  # (antes el archivo se reescribía solo con las figuras de --figs y perdía el detalle del resto)
  f_det <- file.path(cfg$paths$output, "tables", "00-verificacion.csv")
  kept <- read_verificacion(f_det) %>% filter(!fig %in% done_figs)
  todo <- bind_rows(kept, detalle) %>% arrange(suppressWarnings(as.integer(fig)))
  write_csv(todo, f_det)
  out <- bind_rows(rows)
  log_msg("verify: ", sum(out$estado == "ok"), "/", sum(out$estado != "omitida"), " ok · ", nrow(detalle),
          " filas de detalle de esta corrida + ", nrow(kept), " conservadas de ", n_distinct(kept$fig),
          " figuras no verificadas ahora en ", rel_path(f_det, cfg$paths$root), " · ", round(secs(t0), 1), " s")
  out
}


## Prueba ------------------------------------------------------------------------------------------------------------
## Etapa 1: cd /tmp && Rscript --vanilla SCRATCH/agente_F/prueba.R (carga 00_setup.R, 01_load.R y este archivo;
## svg_size() y count_live_text() como stubs; referencia sintética desde finales/datos/Figure_09_access_information.csv):
## sin salida, estatica, sin referencia; tabla = referencia y drawn = frac → "ok", "98/98 ok" (30 text, 36 value [10
## suprimido], 26 frac, 2 orden, ancho, alto, png, texto_vivo); texto, valor fuera de tol, orden, faltante, extra,
## conocida, frac, alto/ancho, <text> vivo, PNG, md5 y ctx_key → los estados y motivos esperados. round(6.069·300) =
## 1821; round(8.375·300) = 2512. Etapa 2 (sankey 11): referencia sin frac → las 10 de drawn.csv no se comparan, "26/26 ok".
## Revisión (2026-10-23): cd /tmp && Rscript --vanilla SCRATCH/fix/G4/tests/t_07.R antes|despues (carga R/00-05 de la
## copia y 06-08 de orig/ o de la copia; referencias publicadas y sintéticas en una carpeta de prueba; salidas en
## SCRATCH/fix/G4/tests/out_07_{antes,despues}.txt). Antes → después (sin cambios: 53 → 77 comparaciones en la fig 03
## por n, 96 → 120 en la 05 por x, y, 51 → 75 en la 41 por lo, hi y 43 → 58 en la 29 por n; todas ok):
##   fig 03 value2 + 0.07 con tol2 = 0.06: ok → fallo "value2 Accidental: 5.99374→6.06374" (+ 0.05 → ok; sin tol2,
##   + 0.55 sigue ok con la tol 0.6 de la fila); fig 03 n + 1: ok → fallo; fig 05 red espejada (x → 1 − x, y → 1 − y):
##   ok → fallo "x Consumer: 1.01891→-0.0189145"; x + 0.005 → ok; x + 0.02: ok → fallo (sin tol_xy, 0.01); con tol_xy =
##   0.05, + 0.02 → ok; fig 41 lo/hi 29/55 → 2/62: ok → fallo "lo Judges: 29→2"; fig 29 n + 1: ok → fallo;
##   fig 09 drawn.csv con "99%" en National Average: "ok · 98/98 ok" → "fallo · 97/98 · text National Average:
##   61%→99%" (nota drawn.csv); figures_status con estado "error" y sin tabla: "sin salida" → "error · figures:
##   pendiente: fig_09 sin implementar" (sin columna motivo: "error · figures: error (ver log)"; con tabla y motivo
##   largo: el motivo recortado a 60 caracteres); estado "tamaño" sin referencia: "sin referencia" en ambas
##   (decisión 2); claves repetidas (A, A, B) en otro orden físico con el mismo order: "fallo · 6/12 · orden A: 1→2"
##   → "ok · 12/12 ok"; run_verify --figs 09 y luego --figs 10: 00-verificacion.csv con solo la 10 (90 filas) → la 09
##   (98) y la 10 (90), y otra corrida de la 09 no duplica filas. main.R --stage all --figs 09,10,11 → 3/3 (98/98,
##   90/90, 26/26); --stage verify y --stage compare sin error. main.R --stage all --figs 12 --no-png (stub): la consola
##   mostraba "sin salida" y "0 errores" → "12 svg_r error figures: pendiente: fig_12 sin implementar" y "0/1
##   verificadas · 0 conocidas · 1 errores".
## Adopción de WJPr (2026-10-23): main.R --stage all --figs 09,10,11 → "73/73 ok · parecido f1 1.000", "66/66 ok · …" y
##   "27/27 ok · …" (sin filas frac ni de drawn.csv; parecido 1.0000 y textos iguales); con SRC_REPORTS_LIB de la rama
##   main (a46c831) → 09 "fallo · 73/74 · parecido: ≥ 0.995→0.9069", 10 "fallo · 66/67 · parecido: ≥ 0.995→0.4189", 11
##   "28/28 ok · parecido f1 1.000" (+ fila entorno "WJPr 1.1.0@382aee9" → "WJPr 1.1.0@local"). --stage verify de las 42:
##   sin error de ejecución (3 ok, 1 error: la 12, de figures; 38 sin salida). t_adopcion.R: status sin wjpr_build →
##   "desactualizada · salida dibujada sin WJPr"; sin figures_status → textos suprimido; wjpr_textos ≠ iguales → "fallo ·
##   72/73 · textos: …"; sin PNG nuevo (también --no-png de punta a punta) o sin publicada → parecido suprimido y ok;
##   min_f1 1.5 → fallo; yml sin min_f1 → MIN_F1.
## Revisión 3 (2026-10-23, F1: V3-01, V3-02, V3-03, V3-07): main.R --stage all --figs 01,02,03,04,05,06,07,09,10,11 → 10/10
##   con 1.1.0@382aee9 y con main ("55/55 ok · parecido f1 1.000 · local 0.000" en la 01; local 0.444 la 06, 0.153 la
##   03, 0.118 la 07, 0.035 la 02, 0 el resto). SCRATCH/fix3/F1/pruebas/run_exp.sh, errores provocados de V3 (solo
##   plot()), --strict: E1-E5 (01 barra National Average +10 pp, 02 Public Services −5 pp, 04 Male +1.0, 06 colores de
##   banda 25-50 y ≥ 50 invertidos, 07 UE severidad 5 −1 pp): 5 × ok y exit 0 → 5 × fallo ("frac National Average:
##   0.681573→0.782037", "parecido_local: ≤ 0.5→0.833", "frac Male: 0.494344→0.593947", "parecido_local: ≤ 0.5→1.000"
##   en la 06 y en la 07) y exit 1; E7 (textos intercambiados en 01 y 04): ok → fallo "text 65-100: 61%→72%" / "3.5→6.4"
##   (drawn.csv); E6 (06 sin colores de banda) con --no-png: "ok 416/416", "1/1 verificadas", exit 0 → "sin dibujo ·
##   418/418 · sin dibujo: sin PNG nuevo (--no-png o sin Chrome)", "0/1 verificadas", exit 1; con PNG: fallo
##   "parecido: ≥ 0.995→0.2282"; --no-png de las diez: 10 × "sin dibujo", "0/10 verificadas", exit 1 (otra vez con PNG:
##   10/10, exit 0); svg_r (fig 09 con chart_grouped y metodo svg_r): "98/98 ok", con las mismas 98 filas de detalle y el
##   mismo drawn.csv y SVG (md5) que el código de antes. p11_unit.R:
##   verify_local_params (hungary.yml; max_local 2 y local_block_px 1.5 → error; sin claves → VERIFY_LOCAL),
##   textos_row (iguales → ok, "sin textos que comparar" → suprimido, faltan → fallo).
