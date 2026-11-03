## tools/pilot_wjpr.R — prueba de WJPr por tipo de gráfica (regla 4 del encargo) -----------------------------------
## Para cada tipo de gráfica con posible equivalente en WJPr se dibuja la figura publicada con la función de WJPr
## usando solo argumentos documentados (args() y ayuda de la versión instalada), se exporta igual que las figuras
## ggplot (svglite → texto en trazos → tamaño de la spec → PNG) y se evalúan criterios objetivos sobre el
## objeto ggplot construido y el SVG. La entrada son los valores publicados de reference/<slug>/: se prueba el
## dibujo, no la estimación. Una función se adopta solo si cumple todos los criterios sin hacks (sin tocar p$layers,
## sin registrar alias de fuentes, sin capas añadidas); el tema solo se cambia por el argumento documentado ptheme.
## Salidas en output/<slug>/compare/: pilot_wjpr_figNN.svg|png, pilot_wjpr_figNN_comparacion.png (publicada | WJPr),
## pilot_wjpr_criterios.csv, pilot_wjpr_resumen.md (tabla del README) y pilot_wjpr.log.
## Uso: Rscript src/src_reports/tools/pilot_wjpr.R [--country hungary] [--out <carpeta>]
## No lo carga main.R. Nunca llama WJPr::wjp_fonts(): activa showtext y el texto ya no se podría contornear.

ROOT <- local({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  r <- if (length(f) > 0) dirname(dirname(gsub("~\\+~", " ", sub("^--file=", "", f[1])))) else ""
  if (!file.exists(file.path(r, "R", "00_setup.R"))) r <- Sys.getenv("SRC_REPORTS_ROOT", unset = getwd())
  normalizePath(r, mustWork = TRUE)
})
for (f in sort(list.files(file.path(ROOT, "R"), pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))) source(f)

a <- commandArgs(trailingOnly = TRUE)
opt <- function(k) { i <- match(k, a); if (is.na(i) || i == length(a)) NULL else a[i + 1] }
cfg <- load_config(ROOT, opt("--country")); check_deps(cfg); setup_fonts(cfg)
specs <- read_specs(cfg)
spec_of <- function(n) specs[[which(vapply(specs, function(s) s$fig, 0L) == n)]]
out_dir <- opt("--out") %||% file.path(cfg$paths$output, "compare")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
log_open(file.path(out_dir, "pilot_wjpr.log"))
if (!requireNamespace("WJPr", quietly = TRUE)) stop("WJPr no está instalado (renv.lock; renv::restore())", call. = FALSE)
WJPR_VER <- paste0(packageVersion("WJPr"), " (", packageDescription("WJPr")$RemoteUsername %||% "?", "/",
                   packageDescription("WJPr")$RemoteRepo %||% "WJPr", "@", substr(packageDescription("WJPr")$RemoteSha %||% "", 1, 7), ")")
log_msg("WJPr ", WJPR_VER, " · ROOT ", ROOT)


## Entrada y utilidades ---------------------------------------------------------------------------------------------

ref <- function(n) {
  f <- file.path(cfg$paths$reference, paste0(figure_stem(spec_of(n)), ".csv"))
  read_csv(f, col_types = cols(.default = col_character()), na = character()) %>%
    mutate(across(any_of(c("order", "value", "value2", "frac", "frac2", "tol")), ~ suppressWarnings(as.numeric(.x))))
}
pct <- function(frac, value) coalesce(frac * 100, value)          # barra publicada exacta; si no hay, el valor

# Tema por el único medio documentado (ptheme): Inter Tight en todos los elementos de texto de WJP_theme()
inter_theme <- function() {
  e <- ggplot2::element_text(family = "inter")
  WJPr::WJP_theme() + ggplot2::theme(text = e, axis.text = e, axis.text.x = e, axis.text.y = e, axis.title = e,
                                     axis.title.x = e, axis.title.y = e, strip.text = e, legend.text = e, plot.title = e)
}

# Familias tipográficas fijadas en el gráfico final (capas y tema); "" = la del dispositivo
plot_families <- function(p) {
  lay <- unlist(lapply(p$layers, function(l) c(l$aes_params$family, l$geom_params$family)))
  th <- unlist(lapply(p$theme, function(e) if (inherits(e, "element_text") || inherits(e, "ggplot2::element_text")) e$family))
  sort(unique(c(lay, th)))
}
# Textos de valor dibujados: columna label de las capas de texto (geom_text/label/richtext; las demás capas también
# heredan label del mapeo global y no dibujan nada), sin marcas de texto enriquecido
printed <- function(b) {
  is_text <- vapply(b$plot$layers, function(l) inherits(l$geom, c("GeomText", "GeomLabel", "GeomRichText", "GeomTextRepel")), logical(1))
  x <- unlist(lapply(b$data[is_text], function(d) {
    if (!"label" %in% names(d)) return(NULL)
    drawn <- (if ("x" %in% names(d)) is.finite(d$x) else TRUE) & (if ("y" %in% names(d)) is.finite(d$y) else TRUE)
    as.character(d$label[drawn])                    # una etiqueta con posición NA no se dibuja
  }))
  x <- gsub("<[^>]+>", "", x[!is.na(x)]); trimws(x[nzchar(trimws(x))])
}
# Diferencia de multiconjuntos de textos: faltan (publicados no dibujados) y sobran (dibujados no publicados)
text_diff <- function(drawn, expected) {
  t1 <- table(drawn); t2 <- table(expected); k <- union(names(t1), names(t2))
  n1 <- as.integer(t1[k]); n2 <- as.integer(t2[k]); n1[is.na(n1)] <- 0L; n2[is.na(n2)] <- 0L
  list(faltan = rep(k, pmax(n2 - n1, 0)), sobran = rep(k, pmax(n1 - n2, 0)))
}
# Fuera de la columna de etiquetas, ¿se apilan barras? (alguna barra empieza por encima de 0)
stacked <- function(b) any(vapply(b$data, function(d) all(c("ymin", "ymax") %in% names(d)) && any(d$ymin > 1e-9, na.rm = TRUE), logical(1)))
# Cortes del eje continuo (en la escala del dato)
breaks_of <- function(b) {
  pp <- b$layout$panel_params[[1]]
  br <- unlist(lapply(c("x", "y"), function(ax) { s <- pp[[ax]]; if (!is.null(s) && !s$is_discrete()) s$get_breaks() }))
  br[!is.na(br)]
}
legend_shown <- function(p) {
  g <- suppressWarnings(ggplot2::ggplotGrob(p)); i <- grepl("guide-box", g$layout$name)   # métricas del dispositivo pdf
  any(vapply(g$grobs[i], function(x) !inherits(x, "zeroGrob"), logical(1)))
}

crit <- list()
add <- function(fig, funcion, criterio, esperado, obtenido, cumple) {
  tipo <- spec_of(fig)$tipo                        # antes de tibble(): dentro, `fig` sería ya la columna de texto
  crit[[length(crit) + 1]] <<- tibble(fig = sprintf("%02d", fig), tipo = tipo, funcion = funcion, criterio = criterio,
                                      esperado = as.character(esperado), obtenido = as.character(obtenido), cumple = isTRUE(cumple))
}
common <- function(fig, funcion, p, b, expected_text, res) {
  fam <- plot_families(p)
  add(fig, funcion, "fuente Inter Tight solo con argumentos (ptheme)", "inter", paste(fam, collapse = ", "), all(fam %in% "inter"))
  d <- text_diff(printed(b), expected_text)
  add(fig, funcion, "textos de valor = publicados", paste(length(expected_text), "textos"),
      if (length(d$faltan) + length(d$sobran) == 0) "iguales" else paste0("faltan ", length(d$faltan), " (", paste(head(unique(d$faltan), 4), collapse = " "),
                                                                      ") · sobran ", length(d$sobran), " (", paste(head(unique(d$sobran), 4), collapse = " "), ")"),
      length(d$faltan) + length(d$sobran) == 0)
  add(fig, funcion, "texto convertible a trazos (0 <text>)", "0 <text>, sin error",
      if (nzchar(res$outline_error)) res$outline_error else paste(res$n_text, "<text>"), !nzchar(res$outline_error) && res$n_text == 0)
  add(fig, funcion, "métrica del texto = Inter Tight (0 dudosos)", "0 dudosos",
      paste0(res$doubtful, " dudosos; familia en el SVG: ", res$svg_family), identical(res$doubtful, 0L))
}

# Exporta como una figura ggplot: svglite al tamaño de la spec (texto a 10 pt) → trazos → PNG → hoja de comparación
export_pilot <- function(p, fig) {
  s <- spec_of(fig); stem <- file.path(out_dir, sprintf("pilot_wjpr_fig%02d", fig))
  svg <- paste0(stem, ".svg"); png <- paste0(stem, ".png"); tmp <- tempfile(fileext = ".svg")
  if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)
  ggplot2::ggsave(tmp, p, device = svglite::svglite, width = s$w_in, height = s$h_in, units = "in")
  ol <- tryCatch(outline_svg_text(tmp), error = function(e) e)
  res <- list(outline_error = if (inherits(ol, "error")) first_line(conditionMessage(ol), 80) else "",
              doubtful = if (is.data.frame(ol)) as.integer(sum(ol$doubtful)) else NA_integer_,
              svg_family = if (is.data.frame(ol) && nrow(ol)) paste(unique(ol$family), collapse = ", ") else "")   # lo que escribió svglite
  if (!nzchar(res$outline_error)) set_size(tmp, s$w_in, s$h_in)
  file.copy(tmp, svg, overwrite = TRUE); unlink(tmp)
  res$n_text <- count_live_text(svg)
  res$png_px <- tryCatch(paste(chrome_png(svg, png, s$w_in, s$h_in, dpi = cfg$layout$dpi, chrome = cfg$paths$chrome), collapse = "x"),
                         error = function(e) paste("sin PNG:", first_line(conditionMessage(e), 50)))
  pub <- file.path(cfg$paths$reference, "published", paste0(figure_stem(s), ".png"))
  if (file.exists(pub) && file.exists(png)) {
    l <- read_png_rgb(pub); r <- read_png_rgb(png)
    png::writePNG(compose_pair(l$img, resample_to_dpi(r$img, if (is.na(r$dpi)) cfg$layout$dpi else r$dpi)),
                  paste0(stem, "_comparacion.png"), dpi = COMPARE_DPI)
  }
  log_msg("fig ", fig, ": svg ", basename(svg), " · png ", res$png_px, " · familias svglite ", res$svg_family)
  res
}

run_case <- function(fig, funcion, build_plot, checks) {
  p <- tryCatch(build_plot(), error = function(e) e)
  b <- if (inherits(p, "error")) p else tryCatch(ggplot2::ggplot_build(p), error = function(e) e)
  ok <- !inherits(b, "error")
  add(fig, funcion, "se dibuja con argumentos documentados", "sin error", if (ok) "sin error" else first_line(conditionMessage(b), 80), ok)
  if (!ok) return(invisible(NULL))
  res <- export_pilot(p, fig)
  checks(p, b, res)
  invisible(res)
}


## Barras agrupadas (fig 9 → 1, 4, 9, 12, 17, 20-28, 31, 33, 35, 38-40) ------------------------------------------------

r9 <- ref(9)
nat9 <- r9 %>% filter(kind == "avg", label == "National Average")
# columnas con nombres propios: WJPr crea internamente grouping/levels/labels_var y choca con nombres iguales
d9 <- r9 %>% filter(kind == "row") %>% transmute(grupo = grouping, categoria = label, v = pct(frac, value), etiqueta = text, text)
g9 <- unique(d9$grupo)
lev9 <- lapply(split(d9$categoria, factor(d9$grupo, levels = g9)), rev)   # ayuda: el primer nivel se dibuja abajo
run_case(9, "wjp_groupbars",
  function() WJPr::wjp_groupbars(data = d9, target = "v", grouping = "grupo", levels = "categoria",
                                 colors = c(STD$col$main, STD$col$track), labels = "etiqueta", group_order = g9, level_order = lev9,
                                 show_national = TRUE, national_value = pct(nat9$frac, nat9$value), national_style = "bar",
                                 national_label = nat9$label, strip_position = "top", ptheme = inter_theme()),
  function(p, b, res) {
    add(9, "wjp_groupbars", "dos filas de promedio (National + EU Average)", "2",
        "1 (national_value es un escalar; ningún argumento para la media UE)", FALSE)
    # Lo publicado dibuja cada encabezado como una fila más de la columna de etiquetas; WJPr usa strips de faceta
    add(9, "wjp_groupbars", "encabezados como filas de la columna de etiquetas", paste(length(g9), "filas de encabezado"),
        paste0(length(g9), " strips de faceta (", class(p$facet)[1], ")"), inherits(p$facet, "FacetNull"))
    n_rows <- sum(vapply(seq_along(b$layout$panel_params), function(i) {
      pp <- b$layout$panel_params[[i]]; s <- if (pp$y$is_discrete()) pp$y else pp$x; length(s$get_limits())
    }, 0L))
    add(9, "wjp_groupbars", "filas de barra (promedios + categorías)", 2 + nrow(d9), n_rows, n_rows == 2 + nrow(d9))
    n_na <- sum(printed(b) == "N/A")
    add(9, "wjp_groupbars", "celdas suprimidas impresas 'N/A'", sum(d9$text == "N/A"), n_na, n_na == sum(d9$text == "N/A"))
    common(9, "wjp_groupbars", p, b, c(nat9$text, d9$text), res)
  })


## Dumbbell (fig 10 → 10, 15) ----------------------------------------------------------------------------------------

r10 <- ref(10)
d10 <- r10 %>% filter(kind != "header")
long10 <- bind_rows(
  d10 %>% transmute(cat = label, serie = "Perception", v = pct(frac, value), lab = text),
  d10 %>% transmute(cat = label, serie = "Experience", v = pct(frac2, value2), lab = text2)
)
supp10 <- d10$label[is.na(d10$value) | is.na(d10$value2)]
run_case(10, "wjp_dumbbells",
  function() WJPr::wjp_dumbbells(data = long10, target = "v", grouping = "cat", cgroups = c("Perception", "Experience"),
                                 colors = "serie", labels = "lab", cvec = c(Perception = STD$col$main, Experience = STD$col$exp),
                                 order = setNames(seq_along(d10$label), d10$label), ptheme = inter_theme()),
  function(p, b, res) {
    h10 <- sum(r10$kind == "header")
    add(10, "wjp_dumbbells", "encabezados de grupo", paste(h10, "encabezados"), "0 (la función no admite grupos)", h10 == 0)
    br <- breaks_of(b)
    add(10, "wjp_dumbbells", "eje 0-100 % cada 25", "0 25 50 75 100", paste(br, collapse = " "), identical(sort(br), c(0, 25, 50, 75, 100)))
    lg <- legend_shown(p)
    add(10, "wjp_dumbbells", "leyenda Perception / Experience", "visible", if (lg) "visible" else "sin leyenda", lg)
    pts <- unlist(lapply(b$data, function(d) if ("shape" %in% names(d) && "y" %in% names(d)) sum(!is.na(d$y)) else 0))
    add(10, "wjp_dumbbells", "celda suprimida (un punto, sin texto)", paste0(2 * nrow(d10) - length(supp10), " puntos"),
        paste0(sum(pts), " puntos"), sum(pts) == 2 * nrow(d10) - length(supp10))
    common(10, "wjp_dumbbells", p, b, c(d10$text, d10$text2)[nzchar(c(d10$text, d10$text2))], res)
  })


## Barras horizontales (fig 13 → 13, 14, 18, 19, 36) ------------------------------------------------------------------

d13 <- ref(13) %>% filter(kind == "row") %>% mutate(v = pct(frac, value), serie = "main")
run_case(13, "wjp_bars",
  function() WJPr::wjp_bars(data = d13, target = "v", grouping = "label", labels = "text", colors = "serie",
                            cvec = c(main = STD$col$main), direction = "horizontal", stacked = FALSE, order = "order",
                            ptheme = inter_theme()),
  function(p, b, res) {
    add(13, "wjp_bars", "una barra por categoría (sin apilar)", "sin apilar", if (stacked(b)) "apiladas" else "sin apilar", !stacked(b))
    br <- breaks_of(b)
    add(13, "wjp_bars", "eje 0-100 % cada 20", "0 20 40 60 80 100", paste(br, collapse = " "), identical(sort(br), seq(0, 100, 20)))
    tl <- b$data[vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))]
    hj <- unique(unlist(lapply(tl, function(d) d$hjust)))
    add(13, "wjp_bars", "valor a continuación del final de la barra", "hjust 0 tras el final",
        paste("hjust", paste(hj, collapse = ", ")), length(hj) > 0 && all(hj <= 0))
    ff <- unique(unlist(lapply(p$layers, function(l) l$aes_params$fontface)))
    add(13, "wjp_bars", "valores en semibold cursiva", "semibold cursiva", paste(ff, collapse = ", "), any(grepl("italic", ff)))
    common(13, "wjp_bars", p, b, d13$text, res)
  })


## Barras pareadas (fig 2), columnas (fig 30 → 30, 37) y severidad (fig 7) con wjp_bars --------------------------------

paired_case <- function(fig, direction) {
  r <- ref(fig) %>% filter(kind == "row")
  long <- bind_rows(r %>% transmute(label, order, serie = cfg$country, v = pct(frac, value), lab = text),
                    r %>% transmute(label, order, serie = "EU Average", v = pct(frac2, value2), lab = text2))
  run_case(fig, "wjp_bars",
    function() WJPr::wjp_bars(data = long, target = "v", grouping = "label", labels = "lab", colors = "serie",
                              cvec = setNames(c(STD$col$main, STD$col$eu), c(cfg$country, "EU Average")),
                              direction = direction, stacked = FALSE, order = "order", ptheme = inter_theme()),
    function(p, b, res) {
      add(fig, "wjp_bars", paste0("país y UE lado a lado (", direction, ")"), "lado a lado", if (stacked(b)) "apiladas (position = 'stack' fijo)" else "lado a lado", !stacked(b))
      common(fig, "wjp_bars", p, b, c(long$lab)[nzchar(long$lab)], res)
    })
}
paired_case(2, "horizontal")
paired_case(30, "vertical")

r7 <- ref(7)
run_case(7, "wjp_bars",
  function() WJPr::wjp_bars(data = r7 %>% mutate(v = value, sev = as.numeric(label)), target = "v", grouping = "label",
                            colors = "grouping", cvec = setNames(c(STD$col$main, STD$col$eu), unique(r7$grouping)),
                            direction = "vertical", stacked = FALSE, order = "sev", ptheme = inter_theme()),
  function(p, b, res) {
    add(7, "wjp_bars", "dos series por severidad lado a lado", "lado a lado", if (stacked(b)) "apiladas (position = 'stack' fijo)" else "lado a lado", !stacked(b))
    common(7, "wjp_bars", p, b, character(), res)
  })


## Resultado --------------------------------------------------------------------------------------------------------

crit <- bind_rows(crit)
write_csv(crit, file.path(out_dir, "pilot_wjpr_criterios.csv"))
covers <- c(grouped_bars = "1, 4, 9, 12, 17, 20-28, 31, 33, 35, 38-40", dumbbell = "10, 15", hbars = "13, 14, 18, 19, 36",
            paired_bars = "2", columns = "30, 37", severity_bars = "7")
summ <- crit %>% group_by(fig, tipo, funcion) %>%
  summarise(cumplidos = sum(cumple), total = n(), fallados = paste(criterio[!cumple], collapse = "; "), .groups = "drop") %>%
  mutate(decision = if_else(cumplidos == total, "adoptada", "descartada"), figuras = unname(covers[tipo]))
none <- tibble(tipo = c("funnel", "sankey", "cards", "matrix", "upset", "network", "scatter"),
               figuras = c("8, 34", "11, 16", "32, 41, 42", "6", "29", "5", "3"),
               motivo = "WJPr no exporta una función de ese tipo (wjp_bars, wjp_groupbars, wjp_dumbbells, wjp_edgebars, wjp_dots, wjp_divbars, wjp_lines, wjp_lollipops, wjp_gauge, wjp_radar, wjp_rose, wjp_slope)")
md <- c(
  paste0("Prueba con WJPr ", WJPR_VER, " (", format(Sys.Date()), "); entrada = valores publicados de reference/", cfg$slug, "/; tema solo por `ptheme`; criterios en `pilot_wjpr_criterios.csv`."),
  "", "| Tipo (figuras) | Figura probada | Función | Criterios cumplidos | Fallados | Decisión |", "|---|---|---|---|---|---|",
  sprintf("| %s (%s) | %s | `%s` | %d/%d | %s | **%s** |", summ$tipo, summ$figuras, summ$fig, summ$funcion, summ$cumplidos, summ$total,
          ifelse(nzchar(summ$fallados), summ$fallados, "—"), summ$decision),
  sprintf("| %s (%s) | — | — | — | %s | **sin_equivalente** |", none$tipo, none$figuras, "WJPr no tiene función de este tipo"))
writeLines(md, file.path(out_dir, "pilot_wjpr_resumen.md"))
log_msg("criterios: ", nrow(crit), " · adoptadas: ", sum(summ$decision == "adoptada"))
cat(sprintf("WJPr %s · %d criterios · salidas en %s\n", WJPR_VER, nrow(crit), out_dir))
cat(sprintf("%-4s %-14s %-15s %-6s %s\n", "fig", "tipo", "funcion", "ok", "decision · fallados"))
for (i in seq_len(nrow(summ))) cat(sprintf("%-4s %-14s %-15s %-6s %s · %s\n", summ$fig[i], summ$tipo[i], summ$funcion[i],
                                           paste0(summ$cumplidos[i], "/", summ$total[i]), summ$decision[i], substr(summ$fallados[i], 1, 120)))

## Prueba
## Rscript --vanilla tools/pilot_wjpr.R (2026-10-23): ver la salida en consola y output/hungary/compare/pilot_wjpr_*.
