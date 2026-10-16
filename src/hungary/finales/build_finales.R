## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Build the final figures of the Hungary report and the delivery folder
## Author(s):         Santiago Pardo (spardo@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     October 2026
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Run from the project root:   Rscript src/hungary/finales/build_finales.R
## (after Rscript src/hungary/worker-hungary.R, which produces the R figures 3, 6, 7 and the data of 27;
## see README.md). Rscript src/hungary/finales/build_finales.R --registrar-fuentes records the md5 of the
## sources outside the report (fuentes.csv) after a reviewed change of those sources, and exits.
##
## For every figure of the final report (manifest.csv, numbered as in the report; origin in provenance.csv)
## writes the standard version defined in standard_figures.R: same content as the report, all text as
## Inter Tight outlines, text at TEXT_PT (9 pt) when the figure is placed at 100 % of its size, bar figures
## redrawn with one common layout, widths that fit the report page. SVG + PNG (300 dpi, same size).
## Stops before writing anything if the report changed since extract_report_figures.R, if provenance.csv
## does not match the report captions, if an R output or another source changed (fuentes.csv), or, once
## the figures are built in a temporary folder, if a printed value or bar differs from the data, a source
## differs from the report figure, text is left live or doubtful, or a figure is out of standard.
## Writes output/viz/Hungary/finales/ and the delivery folder output/finales/Hungria/ (report, figures and
## figuras.zip with the same figures, ready to send), replacing the files a previous run generated, and
## verifies the result.
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("src/hungary/finales/paths.R")
suppressWarnings(suppressMessages(library(tidyverse)))
source("src/hungary/finales/standard_figures.R")

# Stops the build: error in interactive sessions, exit status 1 under Rscript
fail <- function(...) {
  msg <- paste0(...)
  if (interactive()) stop(msg, call. = FALSE)
  message("ERROR: ", msg)
  quit(save = "no", status = 1)
}


## 0. Preconditions (nothing is written if any of them fails) ---------------------------------------------

if ("--registrar-fuentes" %in% commandArgs(TRUE)) {
  s <- register_sources()
  cat("fuentes.csv:", nrow(s), "archivos registrados\n")
  quit(save = "no", status = 0)
}

if (is.na(CHROME)) fail("Se necesita Chrome/Chromium para rasterizar los SVG (defina CHROMOTE_CHROME).")

missing_files <- c(FINALES_MANIFEST, FINALES_PROV, REPORT_DOCX, REPORT_PDF)
missing_files <- missing_files[!file.exists(missing_files)]
if (length(missing_files) > 0) {
  fail("No existen: ", paste(missing_files, collapse = ", "), ". El reporte final vive en output/viz, que no está ",
       "versionado: en un clon limpio cópielo ahí o ajuste REPORT_DOCX / REPORT_PDF en src/hungary/finales/paths.R.")
}

manifest <- read_csv(FINALES_MANIFEST, show_col_types = FALSE) %>% mutate(fig = as.integer(fig))
provenance <- read_csv(FINALES_PROV, col_types = cols(fig = col_integer(), .default = col_character()), na = "")

# The report must be the one the assets were extracted from
if (!all(c("md5_report_docx", "md5_report_pdf") %in% names(manifest))) {
  fail("manifest.csv no guarda los hashes del reporte: corra Rscript src/hungary/finales/extract_report_figures.R.")
}
report_now <- c(docx = unname(tools::md5sum(REPORT_DOCX)), pdf = unname(tools::md5sum(REPORT_PDF)))
changed <- c(
  if (any(manifest$report_docx != basename(REPORT_DOCX) | manifest$md5_report_docx != report_now[["docx"]])) REPORT_DOCX,
  if (any(manifest$report_pdf != basename(REPORT_PDF) | manifest$md5_report_pdf != report_now[["pdf"]])) REPORT_PDF
)
if (length(changed) > 0) {
  fail("El reporte cambió desde la extracción de las figuras (md5 o nombre distinto): ", paste(changed, collapse = ", "),
       ". Corra Rscript src/hungary/finales/extract_report_figures.R y revise provenance.csv antes de construir.")
}

# provenance.csv must describe exactly the figures of the report, with the same caption titles
prov_cols <- c("fig", "titulo", "slug", "fuente", "r_stem", "metodo_ue", "origen")
if (!all(prov_cols %in% names(provenance))) {
  fail("provenance.csv debe tener las columnas: ", paste(prov_cols, collapse = ", "))
}
if (anyDuplicated(provenance$fig) || !setequal(provenance$fig, manifest$fig)) {
  fail("provenance.csv no tiene una fila por figura del reporte (faltan: ",
       paste(setdiff(manifest$fig, provenance$fig), collapse = ", "), "; sobran o se repiten: ",
       paste(union(setdiff(provenance$fig, manifest$fig), provenance$fig[duplicated(provenance$fig)]), collapse = ", "), ").")
}
manifest <- manifest %>%
  inner_join(provenance, by = "fig") %>%
  arrange(fig) %>%
  mutate(file_stem = sprintf("Figure_%02d_%s", fig, slug))

bad_title <- manifest %>% filter(trimws(titulo) != trimws(title))
if (nrow(bad_title) > 0) {
  fail("El título de provenance.csv no coincide con el pie del reporte:\n",
       paste0("  Figura ", bad_title$fig, ": reporte «", bad_title$title, "» / provenance «", bad_title$titulo, "»",
              collapse = "\n"))
}

# Versioned report assets unchanged since the extraction
bad_assets <- manifest %>%
  filter(!is.na(svg_asset)) %>%
  filter(!file.exists(svg_asset) | unname(tools::md5sum(svg_asset)) != md5_svg)
if (nrow(bad_assets) > 0) {
  fail("Los SVG versionados de las figuras ", paste(bad_assets$fig, collapse = ", "),
       " ya no son los del reporte. Corra extract_report_figures.R.")
}

# R figures embedded only as PNG: the PNG produced by worker-hungary.R must still be the one in the report
# (their SVG and data are then the published content)
r_figs <- manifest %>%
  filter(fuente == "codigo_r") %>%
  mutate(r_png = file.path(R_OUTPUTS, paste0(r_stem, ".png")))
r_missing <- r_figs$r_png[!file.exists(r_figs$r_png)]
if (length(r_missing) > 0) {
  fail("Faltan salidas de R: ", paste(r_missing, collapse = ", "), ". Corra Rscript src/hungary/worker-hungary.R.")
}
# Sources outside the report (pipeline SVGs, R outputs, tables): unchanged since they were checked
bad_sources <- changed_sources()
if (nrow(bad_sources) > 0) {
  fail("Fuentes distintas de las registradas en src/hungary/finales/fuentes.csv:\n",
       paste0("  Figura ", bad_sources$fig, ": ", bad_sources$archivo, " (", bad_sources$problema, ")", collapse = "\n"),
       "\nRevise el cambio; si es correcto, corra build_finales.R --registrar-fuentes y vuelva a construir.")
}
r_changed <- r_figs %>% filter(unname(tools::md5sum(r_png)) != md5_png)
if (nrow(r_changed) > 0) {
  fail("La salida de worker-hungary.R cambió y ya no coincide con la figura del reporte: figuras ",
       paste(r_changed$fig, collapse = ", "),
       ". No se entrega una figura distinta a la del reporte: revise el cambio en el código o en los datos.")
}


## 1. Helpers ----------------------------------------------------------------------------------------------

# Headless Chrome screenshot of an HTML page holding one image at an exact CSS size
chrome_png <- function(img_path, width_in, height_in, out_png) {
  css_w <- width_in * 96; css_h <- height_in * 96
  html <- tempfile(fileext = ".html", tmpdir = dirname(normalizePath(img_path)))
  writeLines(sprintf(
    "<html><body style='margin:0;background:#ffffff'><img src='%s' style='display:block;width:%.3fpx;height:%.3fpx'></body></html>",
    basename(img_path), css_w, css_h), html)
  shot <- tempfile(fileext = ".png")
  system2(CHROME, c("--headless=new", "--disable-gpu", "--hide-scrollbars", "--force-color-profile=srgb",
                    sprintf("--force-device-scale-factor=%.6f", DPI / 96),
                    sprintf("--window-size=%d,%d", ceiling(css_w) + 2, ceiling(css_h) + 2),
                    paste0("--screenshot=", shot), paste0("file://", normalizePath(html))),
          stdout = FALSE, stderr = FALSE)
  unlink(html)
  if (!file.exists(shot)) fail("Chrome no pudo rasterizar ", img_path)
  # Crop to the exact pixel size of the figure and store the resolution in the PNG
  img <- png::readPNG(shot)
  px_w <- round(width_in * DPI); px_h <- round(height_in * DPI)
  img <- img[seq_len(min(px_h, dim(img)[1])), seq_len(min(px_w, dim(img)[2])), 1:3, drop = FALSE]
  png::writePNG(img, out_png, dpi = DPI)
  unlink(shot)
  invisible(out_png)
}

png_props <- function(path) {
  info <- attr(png::readPNG(path, info = TRUE), "info")
  list(w = info$dim[1], h = info$dim[2], dpi = if (is.null(info$dpi)) c(NA, NA) else round(info$dpi))
}

svg_live_text <- function(path) length(xml2::xml_find_all(xml2::read_xml(path), "//*[local-name()='text']"))


## 2. Standard figures in a staging folder -------------------------------------------------------------------

std_dir <- file.path(tempdir(), "finales_std")
build_dir <- file.path(tempdir(), "finales_build")
unlink(c(std_dir, build_dir), recursive = TRUE)
dir.create(build_dir)
dir.create(file.path(build_dir, "datos"))

std <- standardize_figures(std_dir)
if (!setequal(std$fig, manifest$fig)) fail("standard_figures.R no cubre las figuras ", paste(setdiff(manifest$fig, std$fig), collapse = ", "))

checks <- map_dfr(seq_len(nrow(manifest)), function(i) {
  f <- manifest[i, ]
  s <- std %>% filter(fig == f$fig)
  svg_out <- file.path(build_dir, paste0(f$file_stem, ".svg"))
  png_out <- file.path(build_dir, paste0(f$file_stem, ".png"))
  if (!file.copy(file.path(std_dir, sprintf("Figure_%02d.svg", f$fig)), svg_out, overwrite = TRUE)) fail("No se pudo copiar la figura ", f$fig)
  datos <- file.path(std_dir, sprintf("Figure_%02d_datos.csv", f$fig))
  if (file.exists(datos)) file.copy(datos, file.path(build_dir, "datos", paste0(f$file_stem, ".csv")), overwrite = TRUE)
  chrome_png(svg_out, s$width_in, s$height_in, png_out)
  pp <- png_props(png_out)
  tibble(fig = f$fig, live_text = svg_live_text(svg_out), png_px = paste0(pp$w, "x", pp$h),
         png_ok = pp$w == round(s$width_in * DPI) && pp$h == round(s$height_in * DPI) && all(pp$dpi == DPI))
})

value_checks <- std %>% select(fig, check_detail) %>% unnest(check_detail)
write_csv(value_checks, file.path(build_dir, "datos", "00-verificacion_valores.csv"))

index <- manifest %>%
  left_join(std %>% select(-check_detail) %>% rename(fuente_figura = fuente, w_std = width_in, h_std = height_in), by = "fig") %>%
  left_join(checks, by = "fig") %>%
  transmute(
    figura = fig, titulo = title, pagina_pdf = page,
    ancho_in = w_std, alto_in = h_std, texto_pt = text_pt, junto_a_recuadro = callout,
    metodo, fuente_figura, valores_impresos = n_printed, valores_verificados = n_checked,
    valores_distintos = n_check_fail, valores_sin_verificar = n_unverified,
    verificacion_adicional = extra_note, verificacion_adicional_ok = extra_ok,
    svg = paste0(file_stem, ".svg"), png = paste0(file_stem, ".png"), png_px, dpi = DPI,
    metodo_ue, origen_en_el_reporte = origen, texto_vivo = live_text, textos_dudosos = outline_doubtful,
    texto_mayor_pedido = larger_text_ok,
    png_tamano_ok = png_ok
  )
write_csv(index, file.path(build_dir, "00-INDICE.csv"))

verified_txt <- function(i) {
  r <- index[i, ]
  parts <- c(
    if (r$valores_impresos > 0) paste0(r$valores_verificados - r$valores_distintos, " de ", r$valores_impresos, " contra los datos",
                                       if (r$valores_sin_verificar > 0) paste0(" (", r$valores_sin_verificar, " sin dato)")),
    if (r$verificacion_adicional != "") r$verificacion_adicional
  )
  if (length(parts) == 0) "—" else paste(parts, collapse = "; ")
}
md_index <- c(
  "# Figuras finales · Paths to Justice in Hungary",
  "",
  paste0("Reporte: `", basename(REPORT_PDF), "` / `", basename(REPORT_DOCX), "`. Generado con ",
         "`Rscript src/hungary/finales/build_finales.R` el ", format(Sys.Date(), "%Y-%m-%d"),
         " (proceso y procedencia: `src/hungary/finales/README.md`)."),
  "",
  "Cada figura lleva el número y el título que tiene en el reporte y muestra los mismos valores que el reporte. Se entrega en SVG y en PNG (mismo tamaño, 300 dpi).",
  "",
  paste0("**Cómo insertarlas.** Al 100 % de su tamaño (el ancho y alto que declara cada SVG; el PNG mide lo mismo a 300 dpi). ",
         "Así el texto base de las figuras (etiquetas y títulos de grupo) mide ", TEXT_PT, " pt en todas; en el ensamblaje del 16-oct, que reduce toda la página al 82 %, ",
         "unos ", sprintf("%.1f", TEXT_PT * 0.822), " pt, un 11 % más que la Figura 31 de esa versión. Las figuras con recuadro de texto a la derecha miden como máximo ",
         MAX_W_CALLOUT_IN, " in y las demás ", MAX_W_FULL_IN, " in, lo que cabe en ese ensamblaje (prueba: `Rscript src/hungary/finales/proof_assembly.R`)."),
  "",
  paste0("**Estándar.** Todo el texto va en trazos con Inter Tight: se ve igual aunque la máquina que arma el reporte no tenga la fuente (antes salía «NationalAverage»). ",
         "Las figuras de barras se redibujan con las mismas medidas: etiquetas a ", STD$size_label, " pt, valores a ", STD$size_value,
         " pt en negrita cursiva y leyendas a ", STD$size_legend, " pt. Las barras agrupadas y los pares Hungría/UE comparten columna de etiquetas (",
         attr(std, "label_column_pt")[["std"]], " pt, alineadas a la derecha), barra de ", STD$track, " pt sobre fondo gris y filas de ", STD$pitch,
         " pt; los dumbbells (10, 15) y la Figura 27 llenan el ancho junto al recuadro; las barras horizontales 13, 14, 18, 19 y 36 siguen el diseño del pipeline ",
         "(etiquetas a la izquierda, eje 0–100 % arriba, valores al final de la barra) con columna común de ", attr(std, "label_column_pt")[["hbars"]], " pt. ",
         "Embudos, sankeys y tarjetas se redibujan con el diseño original de charts5.py, más largos y grandes; la red solo se reescala; ",
         "en las figuras de ggplot (3, 6, 7, 29) el texto base mide ", TEXT_PT, " pt y los títulos de eje o de panel algo más.",
         if (any(index$texto_pt < TEXT_PT - 0.05)) paste0(" Queda algo por debajo, porque su ancho llega al límite de la página: ",
                                                           paste0("Figura ", index$figura[index$texto_pt < TEXT_PT - 0.05], " (", sprintf("%.1f", index$texto_pt[index$texto_pt < TEXT_PT - 0.05]), " pt)", collapse = ", "), ".")),
  "",
  "**Cambios respecto al PDF del 15-oct** (ningún valor cambia):",
  "",
  "- Figura 20: «5 or more problems» → «6 or more problems» (la categoría es 6 o más problemas, como en las demás figuras; era la única sin corregir).",
  "- Figuras 30 y 37: columnas verticales, Hungría y UE lado a lado (antes barras horizontales en pares).",
  "- Figura 36: diseño de la Figura 13 (eje 0–100 % arriba, barras gruesas), en lugar de barras finas.",
  "- Figuras 13, 14, 18, 19 y 36 con medidas comunes entre sí. En 13 y 18 los valores salen de las barras publicadas y las etiquetas se transcriben de la figura; 14, 19 y 36 salen de sus tablas y se comprueban contra las barras publicadas.",
  "- Figuras 8 y 34 (embudos): la primera etapa ocupa todo el ancho y las demás conservan su proporción; barras más altas.",
  "- Figuras 11 y 16 (sankeys): a lo ancho de la columna, centrados y más largos; mismos valores, colores y diseño.",
  "- Figuras 10 y 15 (dumbbells): eje 0–100 % más largo y filas más altas; valores en negrita cursiva.",
  "- Figuras 32, 41 y 42 (tarjetas): más grandes, etiquetas a 11 pt; 41 y 42 con las mismas medidas y la misma escala de color.",
  "- Figura 27: más grande (barras más gruesas y largas). Figura 6: a lo ancho de la columna.",
  "- Figura 29: más grande y más alta, porcentajes sobre las barras en vertical.",
  "- Figura 4: el eje va de 0 a 10 problemas; las longitudes de barra salen de las barras originales.",
  "- Figuras 27 y 36 se redibujan desde sus tablas (`A01.csv` y `4_2_perceived_causes.csv`), verificadas contra la figura del reporte.",
  "",
  "**Pendiente de decisión (no se tocó):** la Figura 16 muestra 56 % en «Did not need or unknown reason» (47 % + 8 % en la columna anterior; el código da 55 %).",
  "",
  "| Figura | Título | Página | Tamaño (in) | Texto (pt) | Método | Verificación |",
  "|---|---|---|---|---|---|---|",
  sprintf("| %d | %s | %d | %.2f × %.2f | %.1f | %s | %s |", index$figura, index$titulo, index$pagina_pdf,
          index$ancho_in, index$alto_in, index$texto_pt, index$metodo, map_chr(seq_len(nrow(index)), verified_txt))
)
writeLines(md_index, file.path(build_dir, "00-INDICE.md"))

# Checks of the figures themselves, before anything is published
n <- nrow(index)
max_w <- if_else(index$junto_a_recuadro, MAX_W_CALLOUT_IN, MAX_W_FULL_IN)
off_text <- (index$texto_pt > TEXT_PT + 0.05 & !index$texto_mayor_pedido) | (index$texto_pt < TEXT_PT - 0.05 & index$ancho_in < max_w - 1e-3)
figs_where <- function(cond) paste(index$figura[cond], collapse = ", ")
problems <- c(
  if (!isTRUE(all(index$figura == seq_len(n)))) "La numeración de figuras no es 1..N",
  if (any(index$valores_distintos > 0))
    paste("Valores o barras distintos de los datos en las figuras:", figs_where(index$valores_distintos > 0),
          "(ver datos/00-verificacion_valores.csv en la carpeta temporal)"),
  if (any(!index$verificacion_adicional_ok)) paste("Fuente distinta de la figura del reporte:", figs_where(!index$verificacion_adicional_ok)),
  if (any(index$texto_vivo > 0)) paste("SVG con texto vivo (debe ir en trazos):", figs_where(index$texto_vivo > 0)),
  if (any(index$textos_dudosos > 0)) paste("Textos medidos con otra fuente o con otra familia que Inter:", figs_where(index$textos_dudosos > 0)),
  if (any(index$ancho_in > max_w + 1e-6)) paste("Figuras más anchas que el espacio de la página:", figs_where(index$ancho_in > max_w + 1e-6)),
  if (any(off_text)) paste("Texto fuera del estándar sin que lo limite el ancho de la página:", figs_where(off_text)),
  if (any(!index$png_tamano_ok)) paste("PNG con tamaño o dpi distinto al final:", figs_where(!index$png_tamano_ok))
)
if (length(problems) > 0) {
  cat(paste0("- ", problems), sep = "\n")
  fail("La verificación de las figuras encontró ", length(problems), " problema(s); no se publicó nada.")
}


## 3. Publish: output/viz/Hungary/finales and the delivery folder output/finales/Hungria ------------------

fig_pattern <- "^Figure_\\d{2}_.+\\.(svg|png)$"
idx_pattern <- "^00-INDICE\\.(csv|md)$"
dir.create(FINALES_VIZ, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(FINALES_OUT, "figuras"), recursive = TRUE, showWarnings = FALSE)

# Remove only the files a previous run of this script generated, so no stale figure remains
unlink(c(
  list.files(FINALES_VIZ, pattern = fig_pattern, full.names = TRUE),
  list.files(FINALES_VIZ, pattern = idx_pattern, full.names = TRUE),
  list.files(file.path(FINALES_OUT, "figuras"), pattern = fig_pattern, full.names = TRUE),
  list.files(FINALES_OUT, pattern = idx_pattern, full.names = TRUE)
), recursive = TRUE)

built_figs <- list.files(build_dir, pattern = fig_pattern, full.names = TRUE)
built_idx <- file.path(build_dir, c("00-INDICE.csv", "00-INDICE.md"))
copy_or_fail <- function(from, to, recursive = FALSE) {
  ok <- file.copy(from, to, overwrite = TRUE, recursive = recursive)
  if (!all(ok)) fail("No se pudo copiar a ", to, ": ", paste(basename(from[!ok]), collapse = ", "))
}
copy_or_fail(c(built_figs, built_idx), FINALES_VIZ)
# datos/ is updated in place (deleting and recreating a folder at once makes OneDrive leave a "datos 2" copy)
viz_datos <- file.path(FINALES_VIZ, "datos")
dir.create(viz_datos, showWarnings = FALSE)
new_datos <- list.files(file.path(build_dir, "datos"))
unlink(file.path(viz_datos, setdiff(list.files(viz_datos), new_datos)))
copy_or_fail(file.path(build_dir, "datos", new_datos), viz_datos)
copy_or_fail(built_figs, file.path(FINALES_OUT, "figuras"))
copy_or_fail(built_idx, FINALES_OUT)
copy_or_fail(c(REPORT_PDF, REPORT_DOCX), FINALES_OUT)
unlink(c(build_dir, std_dir), recursive = TRUE)

# figuras.zip: the delivered figures, in a folder figuras/ (replaces any previous zip)
zip_file <- file.path(normalizePath(FINALES_OUT), "figuras.zip")   # absolute: zip::zip() works from root
unlink(zip_file)
# the list is built before the call: zip::zip() evaluates its arguments after moving to root
zip_files <- file.path("figuras", sort(list.files(file.path(FINALES_OUT, "figuras"))))
zip::zip(zip_file, files = zip_files, root = FINALES_OUT, mode = "mirror")


## 4. Verification of the published copies -------------------------------------------------------------------

expected_figs <- c(index$svg, index$png)
same_set <- function(found, expected) setequal(found, expected) && length(found) == length(expected)
viz_files <- setdiff(list.files(FINALES_VIZ), "datos")
out_figs <- list.files(file.path(FINALES_OUT, "figuras"))
out_top <- list.files(FINALES_OUT)
expected_top <- c(basename(REPORT_PDF), basename(REPORT_DOCX), "00-INDICE.csv", "00-INDICE.md", "figuras", "figuras.zip")
md5_of <- function(dir, files) unname(tools::md5sum(file.path(dir, files)))

# figuras.zip holds exactly the delivered figures
zip_dir <- file.path(tempdir(), "zip_check")
unlink(zip_dir, recursive = TRUE)
zip_ok <- file.exists(file.path(FINALES_OUT, "figuras.zip")) && {
  utils::unzip(file.path(FINALES_OUT, "figuras.zip"), exdir = zip_dir)
  zipped <- list.files(file.path(zip_dir, "figuras"))
  same_set(zipped, expected_figs) && setequal(list.files(zip_dir), "figuras") &&
    identical(md5_of(file.path(zip_dir, "figuras"), expected_figs), md5_of(file.path(FINALES_OUT, "figuras"), expected_figs))
}
unlink(zip_dir, recursive = TRUE)

copies <- c(
  if (!same_set(viz_files, c(expected_figs, "00-INDICE.csv", "00-INDICE.md")))
    paste("output/viz/Hungary/finales no tiene exactamente SVG + PNG por figura + índice. Sobran:",
          paste(setdiff(viz_files, c(expected_figs, "00-INDICE.csv", "00-INDICE.md")), collapse = ", "),
          "| Faltan:", paste(setdiff(c(expected_figs, "00-INDICE.csv", "00-INDICE.md"), viz_files), collapse = ", ")),
  if (!same_set(out_figs, expected_figs))
    paste("La carpeta de entrega (figuras/) no tiene exactamente SVG + PNG por figura. Sobran:",
          paste(setdiff(out_figs, expected_figs), collapse = ", "), "| Faltan:", paste(setdiff(expected_figs, out_figs), collapse = ", ")),
  if (!all(expected_top %in% out_top)) paste("Falta en la carpeta de entrega:", paste(setdiff(expected_top, out_top), collapse = ", ")),
  if (all(expected_figs %in% out_figs) && all(expected_figs %in% viz_files) &&
      !identical(md5_of(FINALES_VIZ, expected_figs), md5_of(file.path(FINALES_OUT, "figuras"), expected_figs)))
    "Las figuras de la carpeta de entrega no son idénticas a las de output/viz/Hungary/finales",
  if (!identical(unname(tools::md5sum(c(REPORT_PDF, REPORT_DOCX))),
                 md5_of(FINALES_OUT, c(basename(REPORT_PDF), basename(REPORT_DOCX)))))
    "El PDF o el DOCX de la carpeta de entrega no son idénticos al reporte final",
  if (!zip_ok) "figuras.zip no contiene exactamente las figuras de la carpeta de entrega"
)

cat("Figuras:", n, "| archivos en output/viz/Hungary/finales:", length(viz_files),
    "| en la carpeta de entrega:", length(out_figs), "figuras +", sum(expected_top %in% out_top) - 1, "archivos (figuras.zip con las mismas",
    length(out_figs), "figuras)\n")
cat("Valores impresos:", sum(index$valores_impresos), "| verificados contra los datos:", sum(index$valores_verificados),
    "| distintos:", sum(index$valores_distintos), "| sin dato:", sum(index$valores_sin_verificar), "\n")
cat("Texto base:", paste0(sort(unique(index$texto_pt)), " pt", collapse = ", "), "| columnas de etiquetas de las barras:",
    paste0(names(attr(std, "label_column_pt")), " ", attr(std, "label_column_pt"), " pt", collapse = ", "), "\n")
# Files someone else put in the delivery folder are not removed: only reported
extra_top <- setdiff(out_top, expected_top)
if (length(extra_top) > 0) {
  cat("Aviso: la carpeta de entrega tiene archivos que este build no genera ni toca (revíselos a mano):",
      paste(extra_top, collapse = ", "), "\n")
}
if (sum(index$valores_sin_verificar) > 0) {
  cat("Aviso: valores impresos sin dato para verificarlos (¿data4web no disponible en path2EU?):",
      figs_where(index$valores_sin_verificar > 0), "\n")
}
if (length(copies) == 0) {
  cat("Verificación OK\n")
} else {
  cat(paste0("- ", copies), sep = "\n")
  fail("La verificación de las copias encontró ", length(copies), " problema(s).")
}
