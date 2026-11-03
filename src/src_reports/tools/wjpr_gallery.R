## tools/wjpr_gallery.R — galería de las figuras dibujadas con WJPr + complementos, y su fidelidad -------------------
## Para cada figura de figures/ cuyo tipo tiene wjpr_<tipo>() en R/09_wjpr.R: toma el df de su data() si la figura
## ya está implementada (su data() corre) o de la referencia publicada si no, dibuja con wjpr_<tipo>(df,
## spec), exporta como las figuras ggplot (svglite → trazos → tamaño de la spec → PNG) y mide: textos dibujados
## frente a df$text/text2 (drawn_text_diff_wjpr, la regla de figures), fuente (familias de svglite y textos dudosos),
## parecido visual con la publicada (f1 de tinta y diferencia local a 96 dpi, wjpr_similarity), lo dibujado por fila
## frente a la referencia si el adaptador lo expone (frac ± verify$tol_frac y texto exacto, como verify) y, si existe,
## el parecido de la figura propia (svg_r) de output/<slug>/figures/. Columna dibujo: ok o lo que no pasaría verify
## con los umbrales de config/<pais>.yml (min_f1, max_local, tol_frac).
## Salidas en output/<slug>/wjpr/: Figure_NN_slug.svg|png, compare/Figure_NN_slug.png (publicada | WJPr), 00-wjpr.csv,
## index.html y wjpr_gallery.log. No toca el resto de output/ ni figures/.
## Uso: Rscript src/src_reports/tools/wjpr_gallery.R [--figs 09,13] [--country hungary] [--out <carpeta>] [--sin-complementos]
## --sin-complementos: options(wjpr.complementos = FALSE): cada figura con WJPr sola (lo que el paquete logra sin ayuda).
## Otra build de WJPr: en una copia de la carpeta, renv::install("worldjusticeproject-org/WJPr@<commit>") (sin snapshot);
## queda en la columna wjpr_build.

ROOT <- local({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  r <- if (length(f) > 0) dirname(dirname(gsub("~\\+~", " ", sub("^--file=", "", f[1])))) else ""
  if (!file.exists(file.path(r, "R", "00_setup.R"))) r <- Sys.getenv("SRC_REPORTS_ROOT", unset = getwd())
  normalizePath(r, mustWork = TRUE)
})
for (f in sort(list.files(file.path(ROOT, "R"), pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))) source(f)

a <- commandArgs(trailingOnly = TRUE)
opt <- function(k) { i <- match(k, a); if (is.na(i) || i == length(a)) NULL else a[i + 1] }
cfg <- load_config(ROOT, opt("--country")); check_deps(cfg); setup_fonts(cfg); wjpr_require()
if ("--sin-complementos" %in% a) options(wjpr.complementos = FALSE)
specs <- read_specs(cfg)
figs <- if (is.null(opt("--figs"))) NULL else as.integer(strsplit(opt("--figs"), ",")[[1]])
if (!is.null(figs)) specs <- Filter(function(s) s$fig %in% figs, specs)
out_dir <- opt("--out") %||% file.path(cfg$paths$output, "wjpr")       # --out: otra carpeta (p. ej. otra versión de WJPr)
if (!grepl("^/", out_dir)) out_dir <- file.path(getwd(), out_dir)
dir.create(file.path(out_dir, "compare"), recursive = TRUE, showWarnings = FALSE)
log_open(file.path(out_dir, "wjpr_gallery.log"))
ctx <- tryCatch(load_ctx(cfg, build = FALSE)$ctx, error = function(e) { log_msg("sin ctx: ", conditionMessage(e)); NULL })
log_msg("WJPr ", wjpr_build(), " · complementos ", wjpr_complementos(), " · ", length(specs), " figuras")

lp <- verify_local_params(cfg); min_f1 <- cfg$verify$min_f1 %||% MIN_F1; tol_frac <- cfg$verify$tol_frac %||% 0.006
`%|NA|%` <- function(x, y) if (is.null(x) || length(x) == 0 || is.na(x[1])) y else x

# Lo dibujado por fila (wjpr_drawn) frente a la referencia, con las reglas de verify (07_verify.R): por (kind, grouping,
# label), drawn_ref_rows() (orden visual, frac/frac2 ± tol_frac donde la referencia trae barra y, con el contrato
# extendido, label_drawn/grouping_drawn exactos y val/val2 ± la tol de la fila) y texto exacto. "" si el adaptador no
# lo expone; "n filas ok" o el número de diferencias y la primera.
# corregido (revisión 4): drawn_ref_rows es la misma función de verify (antes una copia de su regla, sin el nombre
# impreso ni el valor); lo dibujado pasa por un drawn.csv temporal leído con read_table_csv, como en verify
por_fila <- function(p, spec) {
  dr <- wjpr_drawn(p)
  if (is.null(dr)) return(NA_character_)
  f_ref <- file.path(cfg$paths$reference, paste0(figure_stem(spec), ".csv"))
  if (!file.exists(f_ref)) return(paste(nrow(dr), "filas (sin referencia)"))
  tf <- tempfile(fileext = ".drawn.csv"); write_csv(dr, tf, na = "NA"); on.exit(unlink(tf), add = TRUE)
  ref <- add_key(read_table_csv(f_ref)); dk <- add_key(read_table_csv(tf))
  bd <- inner_join(ref, dk, by = KEY_COLS, suffix = c("_ref", "_drawn")) %>% arrange(order_ref)
  has_frac <- any(!is.na(ref$frac)) || any(!is.na(ref$frac2))    # sankey: sin barra medible
  rr <- bind_rows(verify_rows_empty(), drawn_ref_rows(ref, dk, tol_frac, cfg$verify$tol_pp %||% 0.6, has_frac))
  rr <- rr[rr$resultado %in% c("fallo", "faltante", "extra"), , drop = FALSE]
  bad <- if (nrow(rr)) vapply(seq_len(nrow(rr)), function(i) motivo(rr[i, ]), "") else character()
  for (i in seq_len(nrow(bd))) {
    b <- bd[i, ]
    if (nzchar(b$text_ref) && !identical(b$text_ref, b$text_drawn)) bad <- c(bad, sprintf("text %s: %s→%s", b$label, b$text_ref, b$text_drawn))
    if (nzchar(b$text2_ref) && !identical(b$text2_ref, b$text2_drawn)) bad <- c(bad, sprintf("text2 %s: %s→%s", b$label, b$text2_ref, b$text2_drawn))
  }
  miss <- nrow(ref) - nrow(bd)
  if (miss > 0) bad <- c(bad, paste(miss, "filas de la referencia sin dibujar"))
  if (length(bad)) paste0(length(bad), " ≠ (", bad[1], ")") else paste(nrow(dr), "filas ok")
}

row_for <- function(spec) {
  t0 <- Sys.time(); fn <- paste0("wjpr_", spec$tipo)
  base <- tibble(fig = sprintf("%02d", spec$fig), slug = spec$slug, tipo = spec$tipo, adaptador = fn, wjpr_build = wjpr_build(),
                 complementos_activos = wjpr_complementos())
  if (!exists(fn, mode = "function")) return(base %>% mutate(estado = "sin adaptador"))
  res <- tryCatch({
    use_data <- identical(spec$status, "ok") && !is.null(ctx)
    df <- if (use_data) load_figure(spec, cfg)$data(ctx) else wjpr_ref_df(spec, cfg)
    p <- get(fn, mode = "function")(df, spec)
    meta <- attr(p, "wjpr") %||% list(base = "?", complementos = character())
    # corregido: la regla de textos es la de figures (drawn_text_diff_wjpr, 06_export.R: compara, rótulos del
    # adaptador, "sin textos que comparar"); antes era una copia que podía divergir
    td <- drawn_text_diff_wjpr(p, df)
    fila <- por_fila(p, spec)
    r <- wjpr_render(p, spec, cfg, out_dir)
    pub <- file.path(cfg$paths$reference, "published", paste0(figure_stem(spec), ".png"))
    own <- file.path(cfg$paths$output, "figures", paste0(figure_stem(spec), ".png"))
    sim <- function(f) wjpr_similarity(f, pub, block = lp$local_block_px, delta = lp$local_delta, delta_flat = lp$local_delta_flat)
    sp <- if (!is.na(r$png) && file.exists(pub)) sim(r$png) else list(f1 = NA_real_, mad = NA_real_, local = NA_real_)
    so <- if (!is.na(r$png) && file.exists(own) && file.exists(pub)) sim(own) else list(f1 = NA_real_)
    if (!is.na(r$png) && file.exists(pub)) {
      l <- read_png_rgb(pub); rr <- read_png_rgb(r$png)
      png::writePNG(compose_pair(l$img, resample_to_dpi(rr$img, if (is.na(rr$dpi)) cfg$layout$dpi else rr$dpi)),
                    file.path(out_dir, "compare", paste0(figure_stem(spec), ".png")), dpi = COMPARE_DPI)
    }
    # lo que no pasaría verify (umbrales de config/<pais>.yml); "sin dibujo" sin PNG nuevo o publicado
    dib <- c(if (is.na(sp$f1)) "sin dibujo" else c(if (sp$f1 < min_f1 - 1e-9) "parecido", if (sp$local > lp$max_local + 1e-9) "local"),
             if (!is.na(fila) && grepl(" ≠ ", fila, fixed = TRUE)) "por fila",
             if (length(td$faltan) + length(td$sobran) > 0) "textos")
    base %>% mutate(
      entrada = if (use_data) "data()" else "referencia", base_wjpr = meta$base,
      complementos = paste(meta$complementos, collapse = "; "), n_complementos = length(meta$complementos),
      textos = td$resumen, por_fila = fila,
      familias = r$families, dudosos = r$doubtful, texto_vivo = r$n_text, png_px = r$png_px,
      f1_publicada = round(sp$f1, 3), mad_publicada = round(sp$mad, 4),
      local_publicada = round(sp$local, 3),
      local_xy_in = if (is.na(sp$local) || sp$local_px == 0) NA_character_ else sprintf("%.2f, %.2f", sp$local_x_in, sp$local_y_in),
      f1_propia = round(so$f1, 3), dibujo = if (length(dib)) paste(dib, collapse = " + ") else "ok",
      estado = "ok", segundos = round(secs(t0), 1))
  }, error = function(e) base %>% mutate(estado = "error", motivo = first_line(conditionMessage(e), 120), segundos = round(secs(t0), 1)))
  # corregido: res$motivo en un tibble sin esa columna avisaba "Unknown or uninitialised column" en cada figura ok
  log_msg("fig ", sprintf("%02d", spec$fig), " ", res$estado, if ("motivo" %in% names(res)) paste0(": ", res$motivo) else "",
          if ("dibujo" %in% names(res)) paste0(" · dibujo ", res$dibujo, " · f1 ", res$f1_publicada, " · local ", res$local_publicada,
                                               if (!is.na(res$local_xy_in)) paste0(" (", res$local_xy_in, " in)") else "",
                                               " · por fila ", res$por_fila %|NA|% "no expuesto") else "")
  res
}

rows <- bind_rows(lapply(specs, row_for))
for (nm in c("entrada", "base_wjpr", "complementos", "textos", "por_fila", "f1_publicada", "local_publicada", "dibujo", "motivo")) {
  if (!nm %in% names(rows)) rows[[nm]] <- NA_character_
}
old <- file.path(out_dir, "00-wjpr.csv")
if (file.exists(old)) {                                   # acumula entre corridas parciales (una fila por figura)
  prev <- read_csv(old, col_types = cols(.default = col_character()), na = character())
  rows <- bind_rows(prev %>% filter(!fig %in% rows$fig), rows %>% mutate(across(everything(), as.character))) %>% arrange(fig)
}
write_csv(rows %>% mutate(across(everything(), as.character)), old, na = "")

html_row <- function(r) {
  img <- function(p) if (file.exists(file.path(out_dir, p))) sprintf("<img src='%s' width='100%%'>", p) else ""
  sprintf("<tr><td>%s</td><td>%s</td><td>%s<br>%s</td><td>%s<br>%s</td><td>%s<br>local %s</td><td>%s<br>%s</td><td>%s</td></tr>",
          r$fig, r$tipo, r$base_wjpr %||% "", r$complementos %||% "", r$textos %||% r$motivo %||% "", r$por_fila %|NA|% "",
          r$f1_publicada %||% "", r$local_publicada %||% "", r$estado, r$dibujo %|NA|% "",
          img(file.path("compare", paste0("Figure_", r$fig, "_", r$slug, ".png"))))
}
writeLines(c("<!doctype html><meta charset='utf-8'><title>Galería WJPr</title>",
             "<style>body{font-family:sans-serif;margin:16px}td{vertical-align:top;border-top:1px solid #ccc;padding:4px;font-size:12px}td:last-child{width:60%}</style>",
             sprintf(paste0("<h1>Figuras con WJPr + complementos</h1><p>WJPr %s · complementos %s · %s · f1 = parecido con la publicada",
                            " (1 = igual; verify: ≥ %s) · local = peor ventana de %s px (0 = igual; verify: ≤ %s) · por fila = lo",
                            " dibujado por fila frente a la referencia (frac ± %s, texto exacto)</p>"),
                     wjpr_build(), if (wjpr_complementos()) "sí" else "no", format(Sys.time(), "%Y-%m-%d %H:%M"),
                     fmt_val(min_f1), lp$local_block_px, fmt_val(lp$max_local), fmt_val(tol_frac)),
             "<table><tr><th>fig</th><th>tipo</th><th>base · complementos</th><th>textos · por fila</th><th>f1 · local</th><th>estado · dibujo</th><th>publicada | WJPr</th></tr>",
             vapply(split(rows, seq_len(nrow(rows))), html_row, ""), "</table>"),
           file.path(out_dir, "index.html"))
cat(sprintf("%-4s %-14s %-16s %-11s %-8s %-6s %-6s %-14s %s\n", "fig", "tipo", "base", "entrada", "textos", "f1", "local", "por fila", "estado · dibujo"))
nz <- function(x) ifelse(is.na(x), "", as.character(x))
for (i in seq_len(nrow(rows))) cat(sprintf("%-4s %-14s %-16s %-11s %-8s %-6s %-6s %-14s %s\n", rows$fig[i], rows$tipo[i],
  substr(nz(rows$base_wjpr[i]), 1, 16), nz(rows$entrada[i]), substr(nz(rows$textos[i]), 1, 8), nz(rows$f1_publicada[i]),
  nz(rows$local_publicada[i]), substr(nz(rows$por_fila[i]), 1, 14),
  paste0(rows$estado[i], if (nzchar(nz(rows$dibujo[i]))) paste0(" · ", rows$dibujo[i]) else "",
         if (nzchar(nz(rows$motivo[i]))) paste0(": ", substr(rows$motivo[i], 1, 70)) else "")))
cat("salidas:", out_dir, "\n")

## Prueba
## Revisión 3 (2026-10-23, F1): cd /tmp && Rscript <copia>/tools/wjpr_gallery.R --out <dir> con 1.1.0@382aee9 y con main
## (SRC_REPORTS_LIB) → 42 ok en las dos (2 min 18 s y 2 min 3 s), textos iguales en 40 y "sin textos que comparar" en
## la 3 y la 7, f1 0.996-1.000, local 0.000-0.444 en 41 y 0.715 en la 42 (dibujo "local": tarjeta 42 % con texto negro;
## publicada, blanco), por fila "n filas ok" en las 20 barras agrupadas; mismos valores en las dos builds. Antes de los
## cambios: el aviso "Unknown or uninitialised column: `motivo`" en cada figura y la regla de textos copiada (ahora
## drawn_text_diff_wjpr de 06_export.R).
