## tools/wjpr_gallery.R — galería de las figuras dibujadas con WJPr + complementos, y su fidelidad -------------------
## Para cada figura del manifiesto cuyo tipo tiene wjpr_<tipo>() en R/09_wjpr.R: toma el df de su data() si la figura
## ya está implementada (status ok en el manifiesto) o de la referencia publicada si no, dibuja con wjpr_<tipo>(df,
## spec), exporta como las figuras ggplot (svglite → trazos → tamaño del manifiesto → PNG) y mide: textos dibujados
## frente a df$text/text2, fuente (familias de svglite y textos dudosos), parecido visual con la publicada (f1 de
## tinta a 96 dpi, R/09_wjpr.R) y, si existe, con la figura propia (svg_r) de output/<slug>/figures/.
## Salidas en output/<slug>/wjpr/: Figure_NN_slug.svg|png, compare/Figure_NN_slug.png (publicada | WJPr), 00-wjpr.csv,
## index.html y wjpr_gallery.log. No toca el resto de output/ ni el manifiesto.
## Uso: Rscript src/src_reports/tools/wjpr_gallery.R [--figs 09,13] [--country hungary] [--no-png]

lib <- Sys.getenv("SRC_REPORTS_LIB"); if (nzchar(lib)) .libPaths(unique(c(lib, .libPaths())))
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
specs <- read_manifest(cfg)
figs <- if (is.null(opt("--figs"))) NULL else as.integer(strsplit(opt("--figs"), ",")[[1]])
if (!is.null(figs)) specs <- Filter(function(s) s$fig %in% figs, specs)
out_dir <- opt("--out") %||% file.path(cfg$paths$output, "wjpr")       # --out: otra carpeta (p. ej. otra versión de WJPr)
if (!grepl("^/", out_dir)) out_dir <- file.path(getwd(), out_dir)
dir.create(file.path(out_dir, "compare"), recursive = TRUE, showWarnings = FALSE)
log_open(file.path(out_dir, "wjpr_gallery.log"))
ctx <- tryCatch(load_ctx(cfg, build = FALSE)$ctx, error = function(e) { log_msg("sin ctx: ", conditionMessage(e)); NULL })
log_msg("WJPr ", as.character(packageVersion("WJPr")), " · ", length(specs), " figuras")

row_for <- function(spec) {
  t0 <- Sys.time(); fn <- paste0("wjpr_", spec$tipo)
  base <- tibble(fig = sprintf("%02d", spec$fig), slug = spec$slug, tipo = spec$tipo, adaptador = fn)
  if (!exists(fn, mode = "function")) return(base %>% mutate(estado = "sin adaptador"))
  res <- tryCatch({
    use_data <- identical(spec$status, "ok") && !is.null(ctx)
    df <- if (use_data) load_figure(spec, cfg)$data(ctx) else wjpr_ref_df(spec, cfg)
    p <- get(fn, mode = "function")(df, spec)
    meta <- attr(p, "wjpr") %||% list(base = "?", complementos = character())
    # faltan = textos del df (columnas meta$compara) que no se dibujan; sobran = valores dibujados (números, %,
    # "(40%)", "+16 points", "N/A", "–") que el df no trae. Los rótulos fijos del diseño no cuentan.
    drawn <- wjpr_texts(p)
    is_value <- grepl("^(\\(?[−+±-]?[0-9]+([.,][0-9]+)?%?\\)?( points)?|N/A|–)$", drawn)
    td <- wjpr_text_diff(drawn[is_value | drawn %in% unlist(df[intersect(meta$compara %||% c("text", "text2"), names(df))])],
                         unlist(df[intersect(meta$compara %||% c("text", "text2"), names(df))]))
    r <- wjpr_render(p, spec, cfg, out_dir)
    pub <- file.path(cfg$paths$reference, "published", paste0(figure_stem(spec), ".png"))
    own <- file.path(cfg$paths$output, "figures", paste0(figure_stem(spec), ".png"))
    sp <- if (!is.na(r$png) && file.exists(pub)) wjpr_similarity(r$png, pub) else list(f1 = NA_real_, mad = NA_real_)
    so <- if (!is.na(r$png) && file.exists(own) && file.exists(pub)) wjpr_similarity(own, pub) else list(f1 = NA_real_)
    if (!is.na(r$png) && file.exists(pub)) {
      l <- read_png_rgb(pub); rr <- read_png_rgb(r$png)
      png::writePNG(compose_pair(l$img, resample_to_dpi(rr$img, if (is.na(rr$dpi)) cfg$layout$dpi else rr$dpi)),
                    file.path(out_dir, "compare", paste0(figure_stem(spec), ".png")), dpi = COMPARE_DPI)
    }
    base %>% mutate(
      entrada = if (use_data) "data()" else "referencia", base_wjpr = meta$base,
      complementos = paste(meta$complementos, collapse = "; "), n_complementos = length(meta$complementos),
      textos = if (length(td$faltan) + length(td$sobran) == 0) "iguales" else
        paste0("faltan ", length(td$faltan), " (", paste(head(unique(td$faltan), 3), collapse = " "), ") · sobran ",
               length(td$sobran), " (", paste(head(unique(td$sobran), 3), collapse = " "), ")"),
      familias = r$families, dudosos = r$doubtful, texto_vivo = r$n_text, png_px = r$png_px,
      f1_publicada = round(sp$f1, 3), mad_publicada = round(sp$mad, 4), f1_propia = round(so$f1, 3),
      estado = "ok", segundos = round(secs(t0), 1))
  }, error = function(e) base %>% mutate(estado = "error", motivo = first_line(conditionMessage(e), 120), segundos = round(secs(t0), 1)))
  log_msg("fig ", sprintf("%02d", spec$fig), " ", res$estado, if (!is.null(res$motivo)) paste0(": ", res$motivo) else "")
  res
}

rows <- bind_rows(lapply(specs, row_for))
for (nm in c("entrada", "base_wjpr", "complementos", "textos", "f1_publicada", "motivo")) if (!nm %in% names(rows)) rows[[nm]] <- NA_character_
old <- file.path(out_dir, "00-wjpr.csv")
if (file.exists(old)) {                                   # acumula entre corridas parciales (una fila por figura)
  prev <- read_csv(old, col_types = cols(.default = col_character()), na = character())
  rows <- bind_rows(prev %>% filter(!fig %in% rows$fig), rows %>% mutate(across(everything(), as.character))) %>% arrange(fig)
}
write_csv(rows %>% mutate(across(everything(), as.character)), old, na = "")

html_row <- function(r) {
  img <- function(p) if (file.exists(file.path(out_dir, p))) sprintf("<img src='%s' width='100%%'>", p) else ""
  sprintf("<tr><td>%s</td><td>%s</td><td>%s<br>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
          r$fig, r$tipo, r$base_wjpr %||% "", r$complementos %||% "", r$textos %||% r$motivo %||% "",
          r$f1_publicada %||% "", r$estado, img(file.path("compare", paste0("Figure_", r$fig, "_", r$slug, ".png"))))
}
writeLines(c("<!doctype html><meta charset='utf-8'><title>Galería WJPr</title>",
             "<style>body{font-family:sans-serif;margin:16px}td{vertical-align:top;border-top:1px solid #ccc;padding:4px;font-size:12px}td:last-child{width:60%}</style>",
             sprintf("<h1>Figuras con WJPr + complementos</h1><p>WJPr %s · %s · f1 = parecido con la publicada (1 = igual)</p>",
                     as.character(packageVersion("WJPr")), format(Sys.time(), "%Y-%m-%d %H:%M")),
             "<table><tr><th>fig</th><th>tipo</th><th>base · complementos</th><th>textos</th><th>f1</th><th>estado</th><th>publicada | WJPr</th></tr>",
             vapply(split(rows, seq_len(nrow(rows))), html_row, ""), "</table>"),
           file.path(out_dir, "index.html"))
cat(sprintf("%-4s %-14s %-16s %-11s %-8s %-6s %s\n", "fig", "tipo", "base", "entrada", "textos", "f1", "estado"))
nz <- function(x) ifelse(is.na(x), "", as.character(x))
for (i in seq_len(nrow(rows))) cat(sprintf("%-4s %-14s %-16s %-11s %-8s %-6s %s\n", rows$fig[i], rows$tipo[i],
  substr(nz(rows$base_wjpr[i]), 1, 16), nz(rows$entrada[i]), substr(nz(rows$textos[i]), 1, 8), nz(rows$f1_publicada[i]),
  paste0(rows$estado[i], if (nzchar(nz(rows$motivo[i]))) paste0(": ", substr(rows$motivo[i], 1, 70)) else "")))
cat("salidas:", out_dir, "\n")

## Prueba
## (pendiente: se completa al terminar los bloques de R/09_wjpr.R)
