## 08_compare.R — hojas publicada | nueva: compuesto PNG a 96 dpi e index.html en output/<slug>/compare/ ----------
## Origen: nuevo para src_reports (CONTRATOS §5.G). Propiedades del PNG (dim, dpi) como png_props() de
## src/hungary/finales/build_finales.R:154-157; lectura/escritura con png::readPNG / png::writePNG como en
## build_finales.R:146-149. Sin efectos al cargar: solo funciones y constantes.
##
## Por spec seleccionada: publicada = reference/<slug>/published/Figure_NN_slug.png (96 dpi; la escribe
## tools/build_reference.R) y nueva = output/<slug>/figures/Figure_NN_slug.png (cfg$layout$dpi; la escribe
## run_figures()). Sin nueva → "sin salida"; sin publicada → "sin publicada"; fallo de lectura/escritura → "error".
## Compuesto output/<slug>/compare/Figure_NN_slug.png: la nueva remuestreada a 96 dpi (vecino más cercano por
## índices), ambas sobre fondo blanco a la misma altura (relleno blanco abajo), separador gris de 20 px, publicada
## a la izquierda. index.html: una fila por figura del manifiesto (fig, título, tamaño del manifiesto, estado) y
## las dos imágenes lado a lado (<img width="50%">), con rutas relativas a compare/.
## Devuelve (§4.7) tibble(fig, metodo, estado, verificacion, archivo): ok | sin salida | sin publicada | error | omitida.

COMPARE_DPI      <- 96     # resolución de las hojas (la publicada ya está a 96 dpi)
COMPARE_SEP_PX   <- 20     # ancho del separador entre publicada y nueva
COMPARE_SEP_GRAY <- 0.6    # gris del separador (0 = negro, 1 = blanco)


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

# Nueva (dpi_in) → dpi_out: tamaño destino = round(px · dpi_out / dpi_in)
resample_to_dpi <- function(img, dpi_in, dpi_out = COMPARE_DPI) {
  f <- dpi_out / dpi_in
  resample_nn(img, max(1L, round(dim(img)[2] * f)), max(1L, round(dim(img)[1] * f)))
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

# Una fila por figura del manifiesto: fig, título (sección · página · tipo · método), tamaño del manifiesto,
# estado (+ verificacion y enlace al compuesto) y las dos imágenes lado a lado (<img width="50%">)
write_compare_index <- function(specs, rows, cfg) {
  dir <- file.path(cfg$paths$output, "compare")
  img_or <- function(path, alt, falta) {
    if (!file.exists(path)) return(paste0('<span class="falta">', falta, '</span>'))
    paste0('<img src="', relative_href(path, dir), '" width="50%" alt="', html_escape(alt), '" loading="lazy">')
  }
  tr <- vapply(seq_along(specs), function(i) {
    s <- specs[[i]]; p <- compare_paths(s, cfg); r <- rows[i, ]
    paste0(
      '<tr id="fig', r$fig, '">',
      '<td class="num">', r$fig, '</td>',
      '<td>', html_escape(s$titulo), '<br><span class="meta">',
      html_escape(paste(s$seccion, paste0("p. ", s$pagina), s$tipo, s$metodo, sep = " · ")), '</span></td>',
      '<td class="num">', sprintf("%.3f × %.3f in", s$w_in, s$h_in), '<br><span class="meta">',
      sprintf("%d × %d px a %d dpi", round(s$w_in * COMPARE_DPI), round(s$h_in * COMPARE_DPI), COMPARE_DPI), '</span></td>',
      '<td><span class="estado ', gsub(" ", "-", r$estado, fixed = TRUE), '">', html_escape(r$estado), '</span>',
      if (nzchar(r$verificacion)) paste0('<br><span class="meta">', html_escape(r$verificacion), '</span>') else "",
      if (file.exists(p$out)) paste0('<br><a href="', relative_href(p$out, dir), '">compuesto</a>') else "",
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
           ' · ', html_escape(paste(cnt$n, cnt$estado, collapse = ", ")), '</p>'),
    paste0('<p class="meta">Izquierda: PNG publicado (reference/', html_escape(cfg$slug), '/published, ', COMPARE_DPI, ' dpi). ',
           'Derecha: PNG nuevo (output/', html_escape(cfg$slug), '/figures, ', cfg$layout$dpi, ' dpi; el navegador lo escala al mismo ancho). ',
           'Compuesto: ambas a ', COMPARE_DPI, ' dpi, misma altura, separador gris de ', COMPARE_SEP_PX, ' px.</p>'),
    '<table>',
    '<thead><tr><th>fig</th><th>figura</th><th>tamaño (manifiesto)</th><th>estado</th><th>publicada | nueva</th></tr></thead>',
    '<tbody>', tr, '</tbody>', '</table>', '</body>', '</html>')
  path <- file.path(dir, "index.html")
  writeLines(enc2utf8(html), path, useBytes = TRUE)
  invisible(path)
}


## Etapa ----------------------------------------------------------------------------------------------------------

# Primera línea de un mensaje, recortada a n caracteres (verificacion de las filas "error", como en figures)

# run_compare(specs, cfg) → tibble(fig, metodo, estado, verificacion, archivo) en el orden del manifiesto (§4.7).
# Escribe output/<slug>/compare/Figure_NN_slug.png por figura seleccionada con publicada y nueva, y después
# index.html con todas las figuras. Nada va a consola: el detalle queda en el log (log_msg).
run_compare <- function(specs, cfg) {
  t0 <- Sys.time()
  dir.create(file.path(cfg$paths$output, "compare"), recursive = TRUE, showWarnings = FALSE)
  rows <- vector("list", length(specs))
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
    log_msg("compare fig ", id, " ", spec$slug, ": ", r$estado, if (nzchar(r$detalle)) paste0(" · ", r$detalle) else "",
            if (nzchar(r$archivo)) paste0(" → ", r$archivo) else "")
    rows[[i]] <- tibble(fig = id, metodo = spec$metodo, estado = r$estado, verificacion = r$verificacion, archivo = r$archivo)
  }
  rows <- bind_rows(rows)
  index <- write_compare_index(specs, rows, cfg)
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
