# Estado de figures (save_figure, la regla de 06_export.R) y por fila con la regla de verify (drawn_ref_rows, como la
# galería) de una figura dibujada desde su data() o, si no la tiene, desde la referencia. Uso: iny_fig.R <fig>
ROOT <- Sys.getenv("INY_ROOT"); if (!nzchar(ROOT)) stop("INY_ROOT: la copia de src_reports donde correr (tools/inyecciones/correr.sh)", call. = FALSE)
for (f in sort(list.files(file.path(ROOT, "R"), pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))) source(f)
fig <- as.integer(commandArgs(TRUE)[1])
cfg <- load_config(ROOT, "hungary"); setup_fonts(cfg); wjpr_require()
log_open(file.path(tempdir(), "iny_fig.log"))   # el log de la simulación, fuera de la copia
spec <- Filter(function(s) s$fig == fig, read_manifest(cfg))[[1]]
use_data <- identical(spec$status, "ok")
df <- if (use_data) load_figure(spec, cfg)$data(load_ctx(cfg, build = FALSE)$ctx) else wjpr_ref_df(spec, cfg)
p <- get(paste0("wjpr_", spec$tipo))(df, spec)
res <- save_figure(p, df, spec, cfg, png = FALSE)
ref <- add_key(read_table_csv(file.path(cfg$paths$reference, paste0(figure_stem(spec), ".csv"))))
dk <- add_key(read_table_csv(figure_paths(spec, cfg)$drawn))
rr <- bind_rows(verify_rows_empty(), drawn_ref_rows(ref, dk, cfg$verify$tol_frac %||% 0.006, cfg$verify$tol_pp %||% 0.6, any(!is.na(ref$frac))))
bad <- rr[rr$resultado %in% c("fallo", "faltante", "extra"), , drop = FALSE]
# texto exacto por fila frente a la referencia (como por_fila de tools/wjpr_gallery.R)
bd <- inner_join(ref, dk, by = KEY_COLS, suffix = c("_ref", "_drawn"))
tx <- bd[nzchar(bd$text_ref) & bd$text_ref != bd$text_drawn, , drop = FALSE]
cat(sprintf("fig %02d (%s) · figures %s · %s\n   verify (drawn frente a la referencia): %d de %d fallan%s · texto ≠ referencia en %d%s\n", fig,
            if (use_data) "data()" else "referencia", res$estado, substr(res$motivo, 1, 200), nrow(bad), nrow(rr),
            if (nrow(bad)) paste0(" · ", motivo(bad[1, ])) else "", nrow(tx),
            if (nrow(tx)) sprintf(" (%s: %s→%s)", tx$label[1], tx$text_ref[1], tx$text_drawn[1]) else ""))
quit(status = if (identical(res$estado, "ok") && !nrow(bad) && !nrow(tx)) 0L else 1L)
