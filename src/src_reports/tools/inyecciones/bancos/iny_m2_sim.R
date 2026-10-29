# Inyecciones de hbars (13) y columns (30) sin data() (no pasan por main.R --stage all): df desde la referencia
# (wjpr_ref_df; para columns, la propuesta de pruebas/ref_df_propuesta.R), el adaptador con la inyección (iny_m2.R),
# save_figure() de figures (06_export.R: estado y motivo, PNG con Chrome) en una salida temporal y, como verify, lo
# dibujado por fila frente a la referencia (drawn_ref_rows de 07_verify.R) y el parecido con la publicada.
# Uso: INY_ROOT=<copia de src_reports> Rscript iny_m2_sim.R <fig> <iny> [<iny> …] ("ninguna" = sin inyección; lo llama
# tools/inyecciones/correr.sh)
ROOT <- Sys.getenv("INY_ROOT"); if (!nzchar(ROOT)) stop("INY_ROOT: la copia de src_reports donde correr (tools/inyecciones/correr.sh)", call. = FALSE)
PR <- file.path(ROOT, "tools", "inyecciones", "bancos")
for (f in sort(list.files(file.path(ROOT, "R"), pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))) source(f)
source(file.path(PR, "iny_m2.R"))
a <- commandArgs(trailingOnly = TRUE); fg <- as.integer(a[1]); inys <- a[-1]
cfg <- load_config(ROOT, "hungary"); setup_fonts(cfg); wjpr_require()
spec <- Filter(function(s) s$fig == fg, read_manifest(cfg))[[1]]
out <- tempfile("iny_sim_"); for (d in c("tables", "figures", "log")) dir.create(file.path(out, d), recursive = TRUE)
cfg$paths$output <- out
lp <- verify_local_params(cfg)
ref <- add_key(read_table_csv(file.path(cfg$paths$reference, paste0(figure_stem(spec), ".csv"))))
pub <- file.path(cfg$paths$reference, "published", paste0(figure_stem(spec), ".png"))
for (iny in inys) {
  .iny <- if (iny == "ninguna") "" else iny
  df <- wjpr_ref_df(spec, cfg)
  p <- get(paste0("wjpr_", spec$tipo))(df, spec)
  res <- suppressMessages(save_figure(p, df, spec, cfg, png = TRUE))
  dk <- add_key(read_table_csv(figure_paths(spec, cfg)$drawn))
  rr <- bind_rows(verify_rows_empty(), drawn_ref_rows(ref, dk, cfg$verify$tol_frac, cfg$verify$tol_pp, TRUE))
  bad <- rr[rr$resultado %in% c("fallo", "faltante", "extra"), , drop = FALSE]
  # texto de cada fila frente al de la referencia (como por_fila de tools/wjpr_gallery.R)
  bd <- inner_join(ref, dk, by = KEY_COLS, suffix = c("_ref", "_dib"))
  for (k in seq_len(nrow(bd))) for (cc in c("text", "text2")) {
    e <- bd[[paste0(cc, "_ref")]][k]; o <- bd[[paste0(cc, "_dib")]][k]
    if (nzchar(e) && !identical(e, o)) bad <- bind_rows(bad, vrow(cc, e, o, "fallo", label = bd$label[k]))
  }
  s <- if (!is.na(res$png) && file.exists(pub)) wjpr_similarity(res$png, pub, block = lp$local_block_px, delta = lp$local_delta,
                                                                 delta_flat = lp$local_delta_flat) else list(f1 = NA, local = NA)
  cat(sprintf("== %s · figures %s%s\n   verify por fila: %d fallos%s · f1 %.3f · local %.3f\n", iny, res$estado,
              if (nzchar(res$motivo)) paste0(" · ", substr(res$motivo, 1, 150)) else "", nrow(bad),
              if (nrow(bad)) paste0(" (", bad$campo[1], " ", bad$label[1], ": ", bad$esperado[1], "→", bad$obtenido[1], ")") else "",
              s$f1, s$local))
}
unlink(out, recursive = TRUE)
