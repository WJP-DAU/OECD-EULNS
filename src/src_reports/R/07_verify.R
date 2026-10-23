## 07_verify.R — verificación de la tabla, el dibujo y los archivos de cada figura contra reference/<slug> ---------
## Origen: nuevo para src_reports. Criterio de tolerancias tomado de src/hungary/finales/figure_specs.R:166-198
## (check_spec: texto impreso exacto; barra dentro de tol puntos de la escala; "suprimido" = ni dato ni texto).
## Usa de 00_setup.R: figure_stem(), log_msg(); de 01_load.R: rel_path(), read_key(), pkg_ver(), secs();
## de 04_svg.R (otro agente): svg_size(path) → list(width_in, height_in) y count_live_text(path) → n de <text>.
##
##   run_verify(specs, cfg)           → tibble(fig, metodo, estado, verificacion, archivo) (§4.7), una fila por spec en
##                                      el orden del manifiesto; escribe output/<slug>/tables/00-verificacion.csv (fig,
##                                      slug, kind, grouping, label, campo, esperado, obtenido, tol, resultado, nota) y
##                                      registra una línea por figura con log_msg().
##   verify_figure(spec, cfg, status) → list(estado, verificacion, archivo, detalle); status = última fila de
##                                      log/figures_status.csv para la figura (lista) o NULL.
##
## Estados de la figura: ok | fallo | error | sin salida | sin referencia | desactualizada | estatica | omitida.
## Resultado por fila del detalle: ok | fallo | conocida | suprimido | faltante | extra.
## Campos: text | text2 | value | value2 | frac | frac2 | orden | ancho | alto | png | texto_vivo | entorno.
## Pasos de verify_figure: (1) estados previos; (2) unión por (kind, grouping, label): faltante / extra; (3) orden de
## las filas comunes; (4) por fila: text/text2 exactos si la referencia los trae, |value − ref| ≤ tol (tol de la fila
## o cfg$verify$tol_pp; ambos NA → suprimido), value2 igual; (5) drawn.csv: frac/frac2 con cfg$verify$tol_frac y mismo
## orden; (6) archivo: ancho exacto (±0.001) y alto ± size_tol_in, PNG = round(w_in·dpi) × round(h_in·dpi), 0 <text>,
## versiones 'exacta' de deps.csv (aviso, campo entorno); (7) conocida: una fila fallida cuyo label y text obtenido
## aparezcan en spec$nota como [conocida: <label>=<text>] pasa a "conocida" (ni ok ni fallo; una figura solo con
## conocidas queda "ok" y su verificacion empieza por "conocida").
## Decisiones: value2 y frac/frac2 solo se comparan cuando alguna de las dos tablas trae el dato (si no, no hay fila);
## sin PNG → "suprimido" (omitido con --no-png o sin Chrome) salvo que figures_status diga estado "png" → fallo; el PNG
## se acepta con ±1 px de ancho y ±round(size_tol_in·dpi) px de alto (= 3): el alto real del SVG puede diferir del h_in
## del manifiesto dentro de size_tol_in, y round(8.375·300) = 2512 en R (redondeo al par) mientras el publicado mide 2513.

VERIFY_FIELDS  <- c("text", "text2", "value", "value2", "frac", "frac2", "orden", "ancho", "alto", "png", "texto_vivo", "entorno")
VERIFY_RESULTS <- c("ok", "fallo", "conocida", "suprimido", "faltante", "extra")
VERIFY_STATES  <- c("ok", "fallo", "error", "sin salida", "sin referencia", "desactualizada", "estatica", "omitida")
KEY_COLS       <- c("kind", "grouping", "label", ".dup")   # clave de unión (+ índice de repetición)


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


# Lee una tabla (salida, drawn o referencia) como texto y asegura las columnas del contrato: faltantes → NA / ""
read_table_csv <- function(f) {
  num <- c("value", "value2", "frac", "frac2", "tol"); chr <- c("kind", "grouping", "label", "text", "text2")
  x <- read_csv(f, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE, progress = FALSE)
  for (c in c("order", num)) if (!c %in% names(x)) x[[c]] <- NA_character_
  for (c in chr) if (!c %in% names(x)) x[[c]] <- ""
  x %>% mutate(order = suppressWarnings(as.integer(order)),
               across(all_of(num), ~ suppressWarnings(as.numeric(.x))),
               across(all_of(chr), ~ coalesce(.x, "")))
}

# Clave (kind, grouping, label) + índice de repetición: las claves duplicadas se emparejan por posición
add_key <- function(x) x %>% group_by(kind, grouping, label) %>% mutate(.dup = row_number()) %>% ungroup()

# Compara un número de la fila: ambos NA → suprimido; uno solo NA → fallo; |esperado − obtenido| ≤ tol → ok
cmp_num <- function(campo, esperado, obtenido, tol, meta, nota = "") {
  res <- if (is.na(esperado) && is.na(obtenido)) "suprimido"
         else if (is.na(esperado) || is.na(obtenido)) "fallo"
         else if (within_tol(esperado, obtenido, tol)) "ok" else "fallo"
  vrow(campo, fmt_val(esperado), fmt_val(obtenido), res, tol = tol, kind = meta$kind, grouping = meta$grouping,
       label = meta$label, nota = nota)
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

# "desactualizada": md5 de la tabla o del SVG ≠ figures_status, o ctx_key ≠ cache/ctx_<slug>.key (si existe).
# figures_status guarda ctx$meta$key (clave del master) y ctx_<slug>.key la extiende con |eu|…|slug|…|country|…,
# así que se acepta la igualdad con ctx_<slug>.key, con master.key o como prefijo de ctx_<slug>.key.
is_stale <- function(status, f_table, f_svg, cfg) {
  no <- list(stale = FALSE, why = "")
  if (is.null(status)) return(no)
  md5 <- function(f) unname(tools::md5sum(f))
  m_t <- sfield(status, "md5_table")
  if (!is.na(m_t) && !identical(md5(f_table), m_t)) return(list(stale = TRUE, why = "md5 de la tabla ≠ figures_status"))
  m_s <- sfield(status, "md5_svg")
  if (!is.na(m_s) && file.exists(f_svg) && !identical(md5(f_svg), m_s)) return(list(stale = TRUE, why = "md5 del SVG ≠ figures_status"))
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

# Paquetes 'exacta' de deps.csv con versión instalada distinta (tibble paquete, version, instalada) o NULL
deps_mismatch <- function(cfg) {
  f <- cfg$paths$deps
  if (is.null(f) || is.na(f) || !file.exists(f)) return(NULL)
  deps <- read_csv(f, col_types = cols(.default = col_character()), show_col_types = FALSE, progress = FALSE) %>%
    filter(modo == "exacta")
  if (nrow(deps) == 0) return(NULL)
  deps$instalada <- unname(vapply(deps$paquete, pkg_ver, ""))
  deps %>% filter(instalada != version)
}

# Dimensiones (ancho, alto) en px de un PNG y dpi declarados, con png::readPNG(native = TRUE, info = TRUE)
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

  # (1) estados previos, en este orden
  if (!file.exists(f_table)) return(done("sin salida"))
  if (identical(spec$metodo, "static")) return(done("estatica", "SVG estático: no se verifica"))
  if (!file.exists(f_ref)) return(done("sin referencia", "corra tools/build_reference.R"))
  st <- is_stale(status, f_table, f_svg, cfg)
  if (st$stale) return(done("desactualizada", st$why))
  if (identical(sfield(status, "estado"), "error")) return(done("error", "figures: error (ver log)"))

  tol_pp <- cfg$verify$tol_pp %||% 0.6; tol_frac <- cfg$verify$tol_frac %||% 0.006
  size_tol <- cfg$verify$size_tol_in %||% 0.01; dpi <- cfg$layout$dpi %||% 300
  out <- read_table_csv(f_table); ref <- read_table_csv(f_ref)
  det <- list()
  add <- function(row) det[[length(det) + 1L]] <<- row

  # (2) unión por (kind, grouping, label): fila de la referencia sin tabla → faltante; de la tabla sin referencia → extra
  ref_k <- add_key(ref); out_k <- add_key(out)
  if (any(ref_k$.dup > 1) || any(out_k$.dup > 1)) log_msg("fig ", fig, ": claves (kind, grouping, label) repetidas; se emparejan por posición")
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

  # (4) por fila común: text/text2 exactos si la referencia los trae; value con la tol de la fila; value2 igual
  uses_value2 <- any(!is.na(both$value2_ref)) || any(!is.na(both$value2_out))
  for (i in seq_len(nrow(both))) {
    b <- both[i, ]; meta <- list(kind = b$kind, grouping = b$grouping, label = b$label)
    tol_i <- if (is.na(b$tol_ref)) tol_pp else b$tol_ref
    if (nzchar(b$text_ref)) {
      add(vrow("text", b$text_ref, b$text_out, if (identical(b$text_ref, b$text_out)) "ok" else "fallo", tol = 0,
               kind = b$kind, grouping = b$grouping, label = b$label))
    }
    if (nzchar(b$text2_ref)) {
      add(vrow("text2", b$text2_ref, b$text2_out, if (identical(b$text2_ref, b$text2_out)) "ok" else "fallo", tol = 0,
               kind = b$kind, grouping = b$grouping, label = b$label))
    }
    add(cmp_num("value", b$value_ref, b$value_out, tol_i, meta))
    if (uses_value2) add(cmp_num("value2", b$value2_ref, b$value2_out, tol_i, meta))
  }

  # (5) dibujo: drawn.csv vs frac/frac2 de la referencia (solo donde alguna trae barra) y mismo orden; ggplot no dibuja
  if (!identical(spec$metodo, "ggplot")) {
    has_frac <- any(!is.na(ref$frac)) || any(!is.na(ref$frac2))
    if (file.exists(f_drawn)) {
      d_k <- add_key(read_table_csv(f_drawn))
      bd <- inner_join(ref_k, d_k, by = KEY_COLS, suffix = c("_ref", "_drawn")) %>% arrange(order_ref)
      if (nrow(bd) > 1) add(order_row(bd$order_ref, bd$order_drawn, bd, nota = "drawn.csv"))
      for (i in seq_len(nrow(bd))) {
        b <- bd[i, ]; meta <- list(kind = b$kind, grouping = b$grouping, label = b$label)
        if (!is.na(b$frac_ref) || !is.na(b$frac_drawn)) add(cmp_num("frac", b$frac_ref, b$frac_drawn, tol_frac, meta, nota = "drawn.csv"))
        if (!is.na(b$frac2_ref) || !is.na(b$frac2_drawn)) add(cmp_num("frac2", b$frac2_ref, b$frac2_drawn, tol_frac, meta, nota = "drawn.csv"))
      }
      md <- anti_join(filter(ref_k, !is.na(frac) | !is.na(frac2)), d_k, by = KEY_COLS) %>% arrange(order)
      for (i in seq_len(nrow(md))) {
        add(vrow("frac", fmt_val(coalesce(md$frac[i], md$frac2[i])), "", "fallo", tol = tol_frac, kind = md$kind[i],
                 grouping = md$grouping[i], label = md$label[i], nota = "barra de la referencia sin fila en drawn.csv"))
      }
    } else if (has_frac) {
      add(vrow("frac", sum(!is.na(ref$frac) | !is.na(ref$frac2)), "", "fallo", tol = tol_frac, nota = paste0("no existe ", basename(f_drawn))))
    }
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
    png_failed <- identical(sfield(status, "estado"), "png")
    add(vrow("png", px_txt(px_exp), "", if (png_failed) "fallo" else "suprimido", tol = px_tol[2],
             nota = if (png_failed) "Chrome no generó el PNG (estado png en figures_status)" else "PNG ausente (--no-png o sin Chrome): no se verifica"))
  }
  dm <- deps_mismatch(cfg)
  if (!is.null(dm) && nrow(dm) > 0) {
    add(vrow("entorno", paste(dm$paquete, dm$version, collapse = ", "), paste(dm$paquete, dm$instalada, collapse = ", "), "ok",
             nota = "aviso: versiones 'exacta' de deps.csv distintas de las instaladas (no es fallo)"))
  }

  # (7) conocida: fila fallida cuyo label y text obtenido figuran en spec$nota como [conocida: <label>=<text>]
  detalle <- bind_rows(verify_rows_empty(), det)
  kp <- known_pairs(spec$nota)
  if (nrow(kp) > 0 && nrow(detalle) > 0) {
    got <- out %>% distinct(kind, grouping, label, .keep_all = TRUE) %>% select(kind, grouping, label, text_out = text)
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
  estado <- if (length(fails) > 0) "fallo" else "ok"
  verificacion <- if (length(fails) > 0) sprintf("%d/%d · %s", n_ok, n_total, motivo(detalle[fails[1], ]))
                  else if (length(known) > 0) sprintf("%d/%d · conocida %s", n_ok, n_total, motivo(detalle[known[1], ]))
                  else sprintf("%d/%d ok", n_ok, n_total)
  detalle <- bind_cols(tibble(fig = rep(fig, n_total), slug = rep(spec$slug, n_total)), detalle)
  done(estado, verificacion, detalle)
}


## Todas las figuras -------------------------------------------------------------------------------------------------

run_verify <- function(specs, cfg) {
  t0 <- Sys.time()
  status_tbl <- read_figures_status(cfg)
  if (is.null(status_tbl)) log_msg("verify: sin log/figures_status.csv (no se comprueban md5 ni ctx_key)")
  rows <- vector("list", length(specs)); dets <- vector("list", length(specs))
  for (i in seq_along(specs)) {
    spec <- specs[[i]]; fig <- sprintf("%02d", spec$fig); metodo <- spec$metodo %||% ""
    if (!is.null(spec$selected) && !isTRUE(spec$selected)) {
      rows[[i]] <- tibble(fig = fig, metodo = metodo, estado = "omitida", verificacion = "", archivo = "")
      next
    }
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
  f_det <- file.path(cfg$paths$output, "tables", "00-verificacion.csv")
  write_csv(detalle, f_det)
  out <- bind_rows(rows)
  log_msg("verify: ", sum(out$estado == "ok"), "/", sum(out$estado != "omitida"), " ok · ", nrow(detalle),
          " filas de detalle en ", rel_path(f_det, cfg$paths$root), " · ", round(secs(t0), 1), " s")
  out
}


## Prueba ------------------------------------------------------------------------------------------------------------
## cd /tmp && Rscript --vanilla SCRATCH/agente_F/prueba.R (carga 00_setup.R, 01_load.R y este archivo; svg_size() y
## count_live_text() como stubs locales; cfg$paths$output/reference/cache redirigidos a SCRATCH/agente_F/).
## Referencia sintética: finales/datos/Figure_09_access_information.csv (36 filas: 2 avg, 6 header, 28 row; 4 N/A).
## verify_figure(): sin salida → "sin salida"; metodo static → "estatica"; sin referencia → "sin referencia";
##   tabla = referencia y drawn = frac → "ok", "98/98 ok" (30 text, 36 value [10 suprimido], 26 frac, 2 orden, ancho,
##   alto, png, texto_vivo); texto 60%→61% → "fallo", "97/98 · text Male: 60%→61%"; valor +1.0 pp → "fallo" (value) y
##   +0.4 pp → "ok"; orden intercambiado → "orden Male: 4→5"; fila borrada → "faltante Female"; fila añadida →
##   "extra Other"; nota "[conocida: Male=61%]" → "ok", "97/98 · conocida text Male: 60%→61%" (y "[conocida: Male=62%]"
##   sigue "fallo"); frac +0.02 → fallo y +0.004 → ok; sin drawn.csv → "frac: 26→"; alto +0.05 → fallo, +0.009 → ok;
##   ancho +0.01 → fallo; 3 <text> → "texto_vivo: 0→3"; PNG 10x10 → fallo, +2 px de alto → ok, +2 px de ancho → fallo;
##   sin PNG → ok (suprimido) y con status estado "png" → fallo; md5 de tabla o SVG ≠ status → "desactualizada";
##   ctx_key ≠ cache/ctx_<slug>.key → "desactualizada" (prefijo de la clave → ok); status estado "error" → "error".
## run_verify(): 42 specs sin salida (fig 1 no seleccionada) → 41 × "sin salida" + 1 "omitida", 00-verificacion.csv
##   con solo encabezado (11 columnas); figura 9 con figures_status coherente → "ok · 98/98 ok", 98 filas de detalle
##   (88 ok, 10 suprimido). round(6.069·300) = 1821; round(8.375·300) = 2512.
