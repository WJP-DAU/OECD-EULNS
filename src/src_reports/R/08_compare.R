## 08_compare.R — hojas publicada | nueva: compuesto PNG a 96 dpi e index.html en output/<slug>/compare/ ----------
## Origen: nuevo para src_reports (CONTRATOS §5.G). Propiedades del PNG (dim, dpi) como png_props() de
## src/hungary/finales/build_finales.R:154-157; lectura/escritura con png::readPNG / png::writePNG como en
## build_finales.R:146-149. Sin efectos al cargar: solo funciones y constantes.
##
## Por spec seleccionada: publicada = reference/<slug>/published/Figure_NN_slug.png (96 dpi; la escribe
## tools/build_reference.R) y nueva = output/<slug>/figures/Figure_NN_slug.png (cfg$layout$dpi; la escribe
## run_figures()). Sin nueva → "sin salida"; sin publicada → "sin publicada"; fallo de lectura/escritura → "error".
## Compuesto output/<slug>/compare/Figure_NN_slug.png: la nueva remuestreada a 96 dpi (promedio de bloques enteros y
## vecino más cercano por índices), ambas sobre fondo blanco a la misma altura (relleno blanco abajo), separador gris
## de 20 px, publicada a la izquierda; una comparación que no es ok borra el compuesto anterior. compare/index.csv
## guarda la última comparación de cada figura (fig, slug, estado, verificacion, archivo, fecha; upsert por fig, así
## una corrida con --figs parciales no pisa las demás). index.html: una fila por figura de figures/ (fig, título,
## tamaño de la spec, última comparación con su fecha) y las dos imágenes lado a lado (<img width="50%">), con
## rutas relativas a compare/.
## Devuelve (§4.7) tibble(fig, metodo, estado, verificacion, archivo): ok | sin salida | sin publicada | error | omitida.

COMPARE_DPI      <- 96     # resolución de las hojas (la publicada ya está a 96 dpi)
COMPARE_SEP_PX   <- 20     # ancho del separador entre publicada y nueva
COMPARE_SEP_GRAY <- 0.6    # gris del separador (0 = negro, 1 = blanco)
COMPARE_LOG_COLS <- c("fig", "slug", "estado", "verificacion", "archivo", "fecha")   # compare/index.csv


## PNG: lectura, remuestreo y compuesto ---------------------------------------------------------------------------

# Cualquier PNG leído con png::readPNG (gris, gris+alfa, RGB, RGBA) → arreglo alto × ancho × 3 en [0, 1],
# compuesto sobre fondo blanco (el alfa desaparece)
to_rgb <- function(img) {
  if (length(dim(img)) == 2) dim(img) <- c(dim(img), 1)
  k <- dim(img)[3]
  if (k %in% c(2, 4)) {                                   # último canal = alfa
    a <- img[, , k]
    img <- img[, , -k, drop = FALSE]
    for (ch in seq_len(dim(img)[3])) img[, , ch] <- img[, , ch] * a + (1 - a)
  }
  if (dim(img)[3] == 1) img <- array(img, c(dim(img)[1:2], 3))
  img
}

# origen: build_finales.R:154-157 (png_props): dimensiones en px y dpi (NA si el PNG no trae pHYs); devuelve
# además los píxeles ya en RGB
read_png_rgb <- function(path) {
  raw <- png::readPNG(path, info = TRUE)
  info <- attr(raw, "info")
  img <- to_rgb(raw)
  dpi <- if (is.null(info$dpi) || !is.finite(info$dpi[[1]])) NA_real_ else round(info$dpi[[1]])
  list(img = img, w = dim(img)[2], h = dim(img)[1], dpi = dpi)
}

# Vecino más cercano por índices (centros de píxel): índice de origen de cada píxel de destino
nn_index <- function(n_out, n_in) pmin(pmax(floor((seq_len(n_out) - 0.5) * n_in / n_out) + 1, 1), n_in)

# Remuestrea alto × ancho × 3 a w_out × h_out px sin interpolar
resample_nn <- function(img, w_out, h_out) img[nn_index(h_out, dim(img)[1]), nn_index(w_out, dim(img)[2]), , drop = FALSE]

# Promedio de bloques k × k (k entero): suaviza el texto antes del vecino más cercano, como la reducción de la
# publicada (sips); pierde como mucho k − 1 px de borde
box_average <- function(img, k) {
  h <- (dim(img)[1] %/% k) * k; w <- (dim(img)[2] %/% k) * k
  out <- array(0, c(h / k, w / k, dim(img)[3]))
  for (i in seq_len(k)) for (j in seq_len(k)) out <- out + img[seq(i, h, by = k), seq(j, w, by = k), , drop = FALSE]
  out / k^2
}

# Nueva (dpi_in) → dpi_out: tamaño destino = round(px · dpi_out / dpi_in); 300 → 96 dpi = bloques de 3 × 3 y vecino
# más cercano hasta el tamaño exacto
resample_to_dpi <- function(img, dpi_in, dpi_out = COMPARE_DPI) {
  f <- dpi_out / dpi_in
  w <- max(1L, round(dim(img)[2] * f)); h <- max(1L, round(dim(img)[1] * f))
  k <- floor(dpi_in / dpi_out)
  if (k >= 2) img <- box_average(img, k)
  resample_nn(img, w, h)
}

# Publicada a la izquierda, separador gris de sep_px, nueva a la derecha; fondo blanco; misma altura (la más
# baja se rellena de blanco por abajo)
compose_pair <- function(left, right, sep_px = COMPARE_SEP_PX, sep_gray = COMPARE_SEP_GRAY) {
  hl <- dim(left)[1]; wl <- dim(left)[2]; hr <- dim(right)[1]; wr <- dim(right)[2]
  out <- array(1, c(max(hl, hr), wl + sep_px + wr, 3))
  out[seq_len(hl), seq_len(wl), ] <- left
  out[, wl + seq_len(sep_px), ] <- sep_gray
  out[seq_len(hr), wl + sep_px + seq_len(wr), ] <- right
  out
}


## Una figura -----------------------------------------------------------------------------------------------------

# Rutas de la figura: publicada (96 dpi), nueva (cfg$layout$dpi) y compuesto
compare_paths <- function(spec, cfg) {
  stem <- figure_stem(spec)
  list(published = file.path(cfg$paths$reference, "published", paste0(stem, ".png")),
       new       = file.path(cfg$paths$output, "figures", paste0(stem, ".png")),
       out       = file.path(cfg$paths$output, "compare", paste0(stem, ".png")))
}

# Escribe el compuesto y devuelve list(estado, verificacion, archivo, detalle). Los errores los captura run_compare()
compare_figure <- function(spec, cfg) {
  p <- compare_paths(spec, cfg)
  if (!file.exists(p$new)) {
    return(list(estado = "sin salida", verificacion = "sin PNG nuevo", archivo = "", detalle = paste("falta", rel_path(p$new, cfg$paths$root))))
  }
  if (!file.exists(p$published)) {
    return(list(estado = "sin publicada", verificacion = "sin PNG publicado", archivo = "", detalle = paste("falta", rel_path(p$published, cfg$paths$root))))
  }
  pub <- read_png_rgb(p$published)
  new <- read_png_rgb(p$new)
  dpi_new <- if (is.na(new$dpi)) cfg$layout$dpi else new$dpi        # sin pHYs: la resolución de la configuración
  small <- resample_to_dpi(new$img, dpi_new)
  comp <- compose_pair(pub$img, small)
  png::writePNG(comp, p$out, dpi = COMPARE_DPI)
  list(estado = "ok",
       verificacion = sprintf("%d×%d | %d×%d px", pub$w, pub$h, dim(small)[2], dim(small)[1]),
       archivo = rel_path(p$out, cfg$paths$root),
       detalle = sprintf("publicada %d×%d px (%s dpi) · nueva %d×%d px (%s dpi) → %d×%d a %d dpi · compuesto %d×%d px",
                         pub$w, pub$h, if (is.na(pub$dpi)) "?" else pub$dpi, new$w, new$h, dpi_new,
                         dim(small)[2], dim(small)[1], COMPARE_DPI, dim(comp)[2], dim(comp)[1]))
}


## index.html -----------------------------------------------------------------------------------------------------

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE); gsub("\"", "&quot;", x, fixed = TRUE)
}

# Ruta relativa (href) de target desde from_dir, ambas absolutas y existentes; file:// si no comparten raíz
relative_href <- function(target, from_dir) {
  parts <- function(p) { s <- strsplit(normalizePath(p, mustWork = FALSE), "/", fixed = TRUE)[[1]]; s[nzchar(s)] }
  t <- parts(target); f <- parts(from_dir)
  k <- 0
  while (k < min(length(t), length(f)) && t[k + 1] == f[k + 1]) k <- k + 1
  if (k == 0) return(paste0("file://", normalizePath(target, mustWork = FALSE)))
  utils::URLencode(paste(c(rep("..", length(f) - k), t[seq.int(k + 1, length.out = length(t) - k)]), collapse = "/"), reserved = FALSE)
}

# compare/index.csv vacío (columnas COMPARE_LOG_COLS, todas texto)
compare_log_empty <- function() as_tibble(setNames(rep(list(character()), length(COMPARE_LOG_COLS)), COMPARE_LOG_COLS))

# compare/index.csv (tibble de texto con COMPARE_LOG_COLS) o vacío si no existe o no se puede leer
read_compare_log <- function(f) {
  empty <- compare_log_empty()
  if (!file.exists(f)) return(empty)
  x <- tryCatch(read_csv(f, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE, progress = FALSE),
                error = function(e) NULL)
  if (is.null(x) || !"fig" %in% names(x)) return(empty)
  bind_rows(empty, x)[, COMPARE_LOG_COLS] %>% mutate(across(everything(), ~ coalesce(.x, "")))
}

# Upsert por fig en compare/index.csv: las figuras comparadas en esta corrida (log_rows, sin las omitidas) reemplazan
# su fila y las demás conservan la de su última comparación. Devuelve la tabla completa
update_compare_log <- function(log_rows, cfg) {
  f <- file.path(cfg$paths$output, "compare", "index.csv")
  todo <- bind_rows(filter(read_compare_log(f), !fig %in% log_rows$fig), log_rows[, COMPARE_LOG_COLS]) %>%
    arrange(suppressWarnings(as.integer(fig)))
  write_csv(todo, f, na = "")
  todo
}

# Una fila por figura de figures/: fig, título (tipo · método), tamaño de la spec, última
# comparación registrada (estado, verificacion, fecha y enlace al compuesto si fue ok) y las dos imágenes lado a lado
# (<img width="50%">). Antes salía el estado de la corrida actual ("omitida" en las no seleccionadas) junto a
# compuestos de corridas anteriores, sin fecha
write_compare_index <- function(specs, hist, cfg) {
  dir <- file.path(cfg$paths$output, "compare")
  img_or <- function(path, alt, falta) {
    if (!file.exists(path)) return(paste0('<span class="falta">', falta, '</span>'))
    paste0('<img src="', relative_href(path, dir), '" width="50%" alt="', html_escape(alt), '" loading="lazy">')
  }
  rows <- bind_rows(lapply(specs, function(s) {
    h <- hist[hist$fig == sprintf("%02d", s$fig), ]
    if (nrow(h) == 0) return(tibble(fig = sprintf("%02d", s$fig), estado = "omitida", verificacion = "sin comparación registrada", fecha = ""))
    tibble(fig = h$fig[1], estado = h$estado[1], verificacion = h$verificacion[1], fecha = h$fecha[1])
  }))
  tr <- vapply(seq_along(specs), function(i) {
    s <- specs[[i]]; p <- compare_paths(s, cfg); r <- rows[i, ]
    paste0(
      '<tr id="fig', r$fig, '">',
      '<td class="num">', r$fig, '</td>',
      '<td>', html_escape(s$titulo), '<br><span class="meta">',
      html_escape(paste(s$tipo, s$metodo, sep = " · ")), '</span></td>',
      '<td class="num">', sprintf("%.3f × %.3f in", s$w_in, s$h_in), '<br><span class="meta">',
      sprintf("%d × %d px a %d dpi", round(s$w_in * COMPARE_DPI), round(s$h_in * COMPARE_DPI), COMPARE_DPI), '</span></td>',
      '<td><span class="estado ', gsub(" ", "-", r$estado, fixed = TRUE), '">', html_escape(r$estado), '</span>',
      if (nzchar(r$verificacion)) paste0('<br><span class="meta">', html_escape(r$verificacion), '</span>') else "",
      if (nzchar(r$fecha)) paste0('<br><span class="meta">comparada ', html_escape(substr(r$fecha, 1, 16)), '</span>') else "",
      if (identical(r$estado, "ok") && file.exists(p$out)) paste0('<br><a href="', relative_href(p$out, dir), '">compuesto</a>') else "",
      '</td>',
      '<td class="imgs"><div class="pair">',
      img_or(p$published, paste("publicada", r$fig), "sin PNG publicado"),
      img_or(p$new, paste("nueva", r$fig), "sin PNG nuevo"),
      '</div></td>',
      '</tr>')
  }, "")
  cnt <- count(rows, estado)
  html <- c(
    '<!doctype html>', '<html lang="es">', '<head>', '<meta charset="utf-8">',
    paste0('<title>', html_escape(paste0("Comparación publicada | nueva · ", cfg$country)), '</title>'),
    '<style>',
    'body{font-family:system-ui,-apple-system,sans-serif;margin:16px;color:#222}',
    'table{border-collapse:collapse;width:100%}',
    'th,td{border-top:1px solid #ddd;padding:6px 8px;vertical-align:top;text-align:left;font-size:13px}',
    'td.num{white-space:nowrap;font-variant-numeric:tabular-nums}',
    'td.imgs{width:60%}',
    '.pair img{width:50%;vertical-align:top;box-sizing:border-box;border:1px solid #eee}',
    '.falta{display:inline-block;width:50%;color:#999;font-style:italic}',
    '.meta{color:#666;font-size:12px}',
    '.estado{font-weight:600}.estado.ok{color:#1a7f37}.estado.error{color:#b42318}',
    '.estado.sin-salida,.estado.sin-publicada,.estado.omitida{color:#8a6d00}',
    '</style>', '</head>', '<body>',
    paste0('<h1>Comparación publicada | nueva · ', html_escape(cfg$country), '</h1>'),
    paste0('<p class="meta">', html_escape(cfg$report$title %||% ""), ' · generado ', format(Sys.time(), "%Y-%m-%d %H:%M"),
           ' · última comparación de cada figura: ', html_escape(paste(cnt$n, cnt$estado, collapse = ", ")), '</p>'),
    paste0('<p class="meta">Izquierda: PNG publicado (reference/', html_escape(cfg$slug), '/published, ', COMPARE_DPI, ' dpi). ',
           'Derecha: PNG nuevo (output/', html_escape(cfg$slug), '/figures, ', cfg$layout$dpi, ' dpi; el navegador lo escala al mismo ancho). ',
           'Compuesto: ambas a ', COMPARE_DPI, ' dpi, misma altura, separador gris de ', COMPARE_SEP_PX, ' px.</p>'),
    '<table>',
    '<thead><tr><th>fig</th><th>figura</th><th>tamaño (spec)</th><th>estado</th><th>publicada | nueva</th></tr></thead>',
    '<tbody>', tr, '</tbody>', '</table>', '</body>', '</html>')
  path <- file.path(dir, "index.html")
  writeLines(enc2utf8(html), path, useBytes = TRUE)
  invisible(path)
}


## Etapa ----------------------------------------------------------------------------------------------------------

# run_compare(specs, cfg) → tibble(fig, metodo, estado, verificacion, archivo) en orden de fig (§4.7).
# Escribe output/<slug>/compare/Figure_NN_slug.png por figura seleccionada con publicada y nueva, actualiza
# compare/index.csv y después index.html con todas las figuras. Nada va a consola: el detalle queda en el log.
run_compare <- function(specs, cfg) {
  t0 <- Sys.time(); fecha <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  dir.create(file.path(cfg$paths$output, "compare"), recursive = TRUE, showWarnings = FALSE)
  rows <- vector("list", length(specs)); log_rows <- vector("list", length(specs))
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    id <- sprintf("%02d", spec$fig)
    r <- if (isFALSE(spec$selected)) {
      list(estado = "omitida", verificacion = "", archivo = "", detalle = "no seleccionada (--figs)")
    } else {
      tryCatch(
        withCallingHandlers(compare_figure(spec, cfg),
                            warning = function(w) { log_msg("compare fig ", id, " aviso: ", conditionMessage(w)); invokeRestart("muffleWarning") }),
        error = function(e) list(estado = "error", verificacion = first_line(conditionMessage(e)), archivo = "",
                                 detalle = conditionMessage(e)))
    }
    if (!isFALSE(spec$selected)) {
      # un compuesto anterior no sobrevive a una comparación que no es ok (index.html lo seguía enlazando)
      if (!identical(r$estado, "ok")) unlink(compare_paths(spec, cfg)$out)
      log_rows[[i]] <- tibble(fig = id, slug = spec$slug, estado = r$estado, verificacion = r$verificacion,
                              archivo = r$archivo, fecha = fecha)
    }
    log_msg("compare fig ", id, " ", spec$slug, ": ", r$estado, if (nzchar(r$detalle)) paste0(" · ", r$detalle) else "",
            if (nzchar(r$archivo)) paste0(" → ", r$archivo) else "")
    rows[[i]] <- tibble(fig = id, metodo = spec$metodo, estado = r$estado, verificacion = r$verificacion, archivo = r$archivo)
  }
  rows <- bind_rows(rows)
  hist <- update_compare_log(bind_rows(compare_log_empty(), log_rows), cfg)
  index <- write_compare_index(specs, hist, cfg)
  cnt <- count(rows, estado)
  log_msg("compare: ", paste(cnt$n, cnt$estado, collapse = ", "), " · ", rel_path(index, cfg$paths$root), " · ", round(secs(t0), 1), " s")
  rows
}


## Prueba --------------------------------------------------------------------------------------------------------
## cd /tmp && Rscript --vanilla SCRATCH/agente_G/prueba.R  (carga 00_setup.R, 01_load.R y este archivo; cfg con
## paths$output y paths$reference redirigidos a SCRATCH/agente_G; PNG sintéticos escritos con png::writePNG):
##   fig 09: publicada 300×200 px RGB a 96 dpi + nueva 1000×600 px RGBA a 300 dpi → ok; compuesto 640×200 px a
##           96 dpi (izquierda = publicada tal cual; columnas 301-320 gris 0.6; derecha = nueva NN 320×192; filas
##           193-200 blancas; alfa 0.5 compuesto sobre blanco); verificacion "300×200 | 320×192 px".
##   fig 04: publicada RGBA + nueva gris 2D sin pHYs (toma cfg$layout$dpi) → ok; compuesto 158×60 px;
##           verificacion "90×60 | 48×29 px".
##   fig 01 solo publicada → sin salida; fig 02 solo nueva → sin publicada; fig 03 nueva corrupta → error
##   ("file is not in PNG format"); fig 10 selected = FALSE → omitida; las 37 restantes → sin salida.
##   42 filas en el orden del manifiesto con las columnas de §4.7; nada en consola; 42 + 1 líneas en el log; 0.19 s.
##   index.html: 42 <tr>; fila 09 con dos <img width="50%"> y rutas relativas ../figures/… y
##   ../../../reference/<slug>/published/…; marcadores "sin PNG nuevo" / "sin PNG publicado"; sin "{country}".
##   46 comprobaciones, 0 fallos. Parse: Rscript -e 'invisible(parse("R/08_compare.R"))' → OK.
## Integración (SCRATCH/agente_G/integracion.R): run_compare(specs, cfg) con el cfg real y sin figuras → 42 ×
## "sin salida" e index.html en output/<slug>/compare/.
## Revisión (2026-10-23): cd /tmp && Rscript --vanilla SCRATCH/fix/G4/tests/t_08.R antes|despues (PNG sintéticos de las
## figs 09 y 10; salidas en SCRATCH/fix/G4/tests/out_08_{antes,despues}.txt). Antes → después: tras --figs 09,10 y
## luego --figs 10, la fila 09 de index.html decía "omitida" con enlace al compuesto → "ok", su fecha y el enlace;
## sin PNG nuevo de la 09 y --figs 09: "sin salida" con el compuesto viejo y su enlace → "sin salida", compuesto
## borrado y sin enlace; la fila 10 conserva "ok" con fecha y enlace; compare/index.csv no existía → 2 filas (09 sin
## salida, 10 ok) con fig, slug, estado, verificacion, archivo, fecha.
