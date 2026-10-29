# Inyecciones en el dibujo de las figuras sin data() de mis tipos (funnel 8/34, cards 32/41/42, dumbbell 15), dibujadas
# desde la referencia (wjpr_ref_df, como la galería). Por inyección, lo que diría figures (06_export.R: textos =
# drawn_text_diff_wjpr; por fila = drawn_text_diff y drawn_value_diff sobre wjpr_drawn_by_key → estado texto/dibujo) y
# lo que diría verify frente a la referencia (drawn_ref_rows con las tolerancias de config: filas fallo/faltante).
# Uso: Rscript t_iny_ref.R
ROOT <- Sys.getenv("INY_ROOT"); if (!nzchar(ROOT)) stop("INY_ROOT: la copia de src_reports donde correr (tools/inyecciones/correr.sh)", call. = FALSE)
for (f in sort(list.files(file.path(ROOT, "R"), pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))) source(f)
cfg <- load_config(ROOT, "hungary"); setup_fonts(cfg); wjpr_require()
specs <- read_manifest(cfg)
tol_frac <- cfg$verify$tol_frac %||% 0.006; tol_pp <- cfg$verify$tol_pp %||% 0.6

edit_data <- function(p, pick, f) {           # rehace las capas pick(l) con data f(data) (sin p$layers[[k]] <- …)
  p$layers <- lapply(p$layers, function(l) {
    if (!is.data.frame(l$data) || !isTRUE(pick(l))) return(l)
    l2 <- ggplot2::ggproto(NULL, l); l2$data <- f(l$data); l2
  })
  p
}
geom_is <- function(cls) function(l) inherits(l$geom, cls)
sw <- function(v, i, j) { v[c(i, j)] <- v[c(j, i)]; v }

# Inyecciones: fig → lista de funciones(p) (mismo error semántico que los bancos del lote 1)
INY <- list(
  "8" = list(
    fn_textos = function(p) edit_data(p, geom_is("GeomText"), function(d) { d$text <- sw(d$text, 1, 2); d }),
    fn_etiquetas = function(p) edit_data(p, geom_is("GeomLabel"), function(d) {   # 1.ª línea de la 2.ª etapa ↔ la de la 3.ª
      i <- which(abs(d$y - 78) < 29)[1]; j <- which(abs(d$y - 136) < 29)[1]; d$text <- sw(d$text, i, j); d }),
    fn_barra = function(p) edit_data(p, geom_is("GeomRect"), function(d) { d$xmin[2] <- d$xmin[2] - 2.5; d$xmax[2] <- d$xmax[2] + 2.5; d }),
    fn_oculto = function(p) { p$layers <- lapply(p$layers, function(l) {           # valor de la 3.ª etapa del color de la barra
      if (!inherits(l$geom, "GeomText")) return(l)
      l2 <- ggplot2::ggproto(NULL, l); l2$data <- l$data; l2$aes_params$colour <- NULL
      l2$mapping <- ggplot2::aes(x = 50, y = mid, label = text, colour = I(ifelse(seq_along(text) == 3, "#575796", "#FFFFFF"))); l2 })
      p }),
  "32" = list(
    cd_textos = function(p) edit_data(p, function(l) inherits(l$geom, "GeomText") && identical(rlang::as_label(l$mapping$y), "y_value"),
                                      function(d) { d$text <- sw(d$text, 1, 3); d }),
    cd_etiquetas = function(p) edit_data(p, function(l) inherits(l$geom, "GeomText") && !is.null(l$data$x) && all(l$data$x < 0),
                                         function(d) { i <- which(d$text == "Health issues"); j <- which(d$text == "Economic issues"); d$text <- sw(d$text, i, j); d }),
    cd_encabezados = function(p) edit_data(p, function(l) inherits(l$geom, "GeomText") && !is.null(l$data$y) && all(l$data$y < 0),
                                           function(d) { d$text <- rev(d$text); d }),
    cd_relleno = function(p) edit_data(p, geom_is("GeomPolygon"), function(d) {   # tarjeta 4 con el color de la 2
      d$fill[d$id == 4] <- d$fill[d$id == 2][1]; d }),
    cd_oculto = function(p) edit_data(p, function(l) inherits(l$geom, "GeomText") && identical(rlang::as_label(l$mapping$label), "delta"),
                                      function(d) { d$delta_col[2] <- d$fill[2]; d })),
  "15" = list(
    db_textos = function(p) edit_data(p, function(l) inherits(l$geom, "GeomText") && "series" %in% names(l$data), function(d) {
      pm <- p$layers[[1]]$data$pos[match(c("Male", "Female"), p$layers[[1]]$data$group)]; s1 <- d$series[1]
      i <- which(d$pos == pm[1] & d$series == s1); j <- which(d$pos == pm[2] & d$series == s1); d$label <- sw(d$label, i, j); d }),
    db_etiquetas = function(p) edit_data(p, function(l) inherits(l$geom, "GeomText") && "kind" %in% names(l$data), function(d) {
      d$label <- sw(d$label, which(d$label == "Urban"), which(d$label == "Rural")); d }),
    db_punto = function(p) edit_data(p, function(l) inherits(l$geom, "GeomPoint") && identical(rlang::as_label(l$mapping$y), "start"),
                                     function(d) { k <- d$group == "Male"; d$start[k] <- d$start[k] + 5; d }))
)
INY[["34"]] <- INY[["8"]]
INY[["41"]] <- INY[["32"]]; INY[["41"]]$cd_etiquetas <- function(p) edit_data(p, function(l) inherits(l$geom, "GeomText") && !is.null(l$data$x) && all(l$data$x < 0),
  function(d) { i <- which(d$text == "Needed a DRM, had access"); j <- which(d$text == "Needed a DRM, no access"); d$text <- sw(d$text, i, j); d })
INY[["42"]] <- INY[["41"]]; INY[["42"]]$cd_etiquetas <- function(p) edit_data(p, function(l) inherits(l$geom, "GeomText") && !is.null(l$data$x) && all(l$data$x < 0),
  function(d) { i <- which(d$text == "Problem, no hardship"); j <- which(d$text == "Problem, with hardship"); d$text <- sw(d$text, i, j); d })

estado <- function(p, df, spec) {
  b <- suppressWarnings(ggplot2::ggplot_build(p))
  wt <- drawn_text_diff_wjpr(p, df, b)
  dr <- wjpr_drawn(p, b); bk <- wjpr_drawn_by_key(dr, df)
  dif <- drawn_text_diff(bk, df); vdif <- drawn_value_diff(bk, df)
  est <- c(if (nrow(dif) || length(wt$faltan) + length(wt$sobran)) "texto", if (nrow(vdif)) "dibujo")
  mot <- c(if (nrow(dif)) sprintf("%s %s: %s→%s", dif$campo[1], dif$label[1], dif$df[1], dif$drawn[1]),
           if (length(wt$faltan) + length(wt$sobran)) paste("textos", wt$resumen),
           if (nrow(vdif)) sprintf("%s %s: %s→%s", vdif$campo[1], vdif$label[1], vdif$df[1], vdif$drawn[1]))
  f_ref <- file.path(cfg$paths$reference, paste0(figure_stem(spec), ".csv"))
  tf <- tempfile(fileext = ".drawn.csv"); write_csv(dr, tf, na = "NA")
  ref <- add_key(read_table_csv(f_ref)); dk <- add_key(read_table_csv(tf))
  has_frac <- any(!is.na(ref$frac)) || any(!is.na(ref$frac2))
  rr <- bind_rows(verify_rows_empty(), drawn_ref_rows(ref, dk, tol_frac, tol_pp, has_frac))
  rr <- rr[rr$resultado %in% c("fallo", "faltante", "extra"), , drop = FALSE]
  bd <- inner_join(ref, dk, by = KEY_COLS, suffix = c("_ref", "_drawn"))     # texto exacto frente a la referencia
  nt <- sum(nzchar(bd$text_ref) & bd$text_ref != bd$text_drawn) + sum(nzchar(bd$text2_ref) & bd$text2_ref != bd$text2_drawn)
  if (nt) rr <- bind_rows(rr, tibble(resultado = "fallo"))
  list(figures = if (length(est)) paste(est, collapse = "+") else "ok", motivo = paste(mot, collapse = " · "),
       verify = if (nrow(rr)) paste0("fallo (", nrow(rr), if (nt) paste0(", ", nt, " textos") else "", ")") else "ok")
}

for (fig in names(INY)) {
  spec <- Filter(function(s) s$fig == as.integer(fig), specs)[[1]]
  df <- wjpr_ref_df(spec, cfg); p0 <- get(paste0("wjpr_", spec$tipo))(df, spec)
  e <- estado(p0, df, spec)
  cat(sprintf("fig %s · sin inyección · figures %s · verify %s %s\n", fig, e$figures, e$verify, e$motivo))
  for (nm in names(INY[[fig]])) {
    p <- INY[[fig]][[nm]](p0); attr(p, "wjpr") <- attr(p0, "wjpr")
    e <- tryCatch(estado(p, df, spec), error = function(err) list(figures = "error", verify = "", motivo = conditionMessage(err)))
    cat(sprintf("fig %s · %-14s · figures %-12s · verify %s · %s\n", fig, nm, e$figures, e$verify, substr(e$motivo, 1, 150)))
  }
}
