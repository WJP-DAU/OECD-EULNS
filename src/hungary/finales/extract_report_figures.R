## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Extract the figures of the final Hungary report (DOCX) into versioned SVG assets
## Author(s):         Santiago Pardo (spardo@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     October 2026
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Run from the project root:   Rscript src/hungary/finales/extract_report_figures.R
##
## Reads the final report (REPORT_DOCX), maps every "Figure N." caption to the image placed right after
## it, and writes:
##   - src/hungary/finales/svg/Figure_NN.svg   SVG exactly as embedded in the report (figures that have
##                                             an SVG; the PNG-only figures come from R code, see
##                                             provenance.csv)
##   - src/hungary/finales/svg/Figure_NN_report.png  PNG embedded for the PNG-only figures (6, 7, 27)
##   - src/hungary/finales/manifest.csv        number, title, PDF page, size on the page (inches),
##                                             embedded media, hashes of every asset and of the report
##                                             DOCX/PDF (build_finales.R stops if the report changed)
## Re-run it only when a new final version of the report is issued.
## Requires poppler (pdftotext, pdfinfo) on the PATH.
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("src/hungary/finales/paths.R")

suppressWarnings(suppressMessages(library(tidyverse)))

missing_tools <- c("pdftotext", "pdfinfo")[!nzchar(Sys.which(c("pdftotext", "pdfinfo")))]
if (length(missing_tools) > 0) {
  stop("No se encuentra ", paste(missing_tools, collapse = " ni "), " en el PATH. Instale poppler ",
       "(macOS: brew install poppler) y vuelva a correr el script.", call. = FALSE)
}
missing_report <- c(REPORT_DOCX, REPORT_PDF)[!file.exists(c(REPORT_DOCX, REPORT_PDF))]
if (length(missing_report) > 0) {
  stop("No existe el reporte final: ", paste(missing_report, collapse = ", "),
       ". Revise REPORT_DOCX / REPORT_PDF en src/hungary/finales/paths.R.", call. = FALSE)
}

tmp <- file.path(tempdir(), "report_docx")
unlink(tmp, recursive = TRUE)
utils::unzip(REPORT_DOCX, exdir = tmp)

ns <- c(
  w   = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  a   = "http://schemas.openxmlformats.org/drawingml/2006/main",
  r   = "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
  wp  = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing",
  asvg = "http://schemas.microsoft.com/office/drawing/2016/SVG/main"
)
rels <- xml2::read_xml(file.path(tmp, "word", "_rels", "document.xml.rels"))
rel_map <- setNames(
  xml2::xml_attr(xml2::xml_children(rels), "Target"),
  xml2::xml_attr(xml2::xml_children(rels), "Id")
)
doc <- xml2::read_xml(file.path(tmp, "word", "document.xml"))
paragraphs <- xml2::xml_find_all(doc, "//w:body//w:p", ns)

emu_to_in <- function(x) round(as.numeric(x) / 914400, 3)

figures <- list()
pending <- NULL
for (p in paragraphs) {
  txt <- paste(xml2::xml_text(xml2::xml_find_all(p, ".//w:t", ns)), collapse = "")
  for (d in xml2::xml_find_all(p, ".//w:drawing", ns)) {
    if (is.null(pending)) next                       # cover and decorative images
    ext  <- xml2::xml_find_first(d, ".//wp:extent", ns)
    blip <- xml2::xml_find_first(d, ".//a:blip", ns)
    svgb <- xml2::xml_find_first(d, ".//asvg:svgBlip", ns)
    figures[[length(figures) + 1]] <- tibble(
      fig = pending$fig,
      title = pending$title,
      width_in = emu_to_in(xml2::xml_attr(ext, "cx")),
      height_in = emu_to_in(xml2::xml_attr(ext, "cy")),
      media_png = unname(rel_map[xml2::xml_attr(blip, "embed")]),
      media_svg = if (inherits(svgb, "xml_missing")) NA_character_ else unname(rel_map[xml2::xml_attr(svgb, "embed")])
    )
    pending <- NULL
  }
  m <- regmatches(txt, regexec("^\\s*Figure (\\d+)\\.\\s*(.*)$", txt))[[1]]
  if (length(m) == 3) pending <- list(fig = as.integer(m[2]), title = trimws(m[3]))
}
manifest <- bind_rows(figures) %>% arrange(fig)

stopifnot(
  "Figure numbers must be 1..N without gaps or duplicates" = identical(manifest$fig, seq_len(nrow(manifest)))
)

# PDF page of each caption (poppler's pdftotext)
pages <- map_dfr(seq_len(as.integer(sub(".*Pages:\\s+(\\d+).*", "\\1", paste(system2("pdfinfo", shQuote(REPORT_PDF), stdout = TRUE), collapse = " ")))), function(pg) {
  txt <- system2("pdftotext", c("-f", pg, "-l", pg, "-layout", shQuote(REPORT_PDF), "-"), stdout = TRUE)
  n <- as.integer(sub("^\\s*Figure (\\d+)\\..*$", "\\1", grep("^\\s*Figure \\d+\\.", txt, value = TRUE)))
  tibble(fig = n, page = rep(pg, length(n)))
})
manifest <- manifest %>% left_join(pages, by = "fig") %>% relocate(page, .after = title)
if (anyNA(manifest$page) || anyDuplicated(pages$fig)) {
  stop("No se pudo ubicar en el PDF una única página para las figuras: ",
       paste(union(manifest$fig[is.na(manifest$page)], pages$fig[duplicated(pages$fig)]), collapse = ", "),
       ". ¿El PDF corresponde al mismo reporte que el DOCX?", call. = FALSE)
}

# Versioned SVG assets
dir.create(FINALES_SVG, recursive = TRUE, showWarnings = FALSE)
manifest <- manifest %>%
  mutate(
    svg_asset = if_else(is.na(media_svg), NA_character_, file.path(FINALES_SVG, sprintf("Figure_%02d.svg", fig))),
    md5_png = unname(tools::md5sum(file.path(tmp, "word", media_png))),
    md5_svg = if_else(is.na(media_svg), NA_character_, unname(tools::md5sum(file.path(tmp, "word", coalesce(media_svg, media_png)))))
  )
for (i in which(!is.na(manifest$media_svg))) {
  ok <- file.copy(file.path(tmp, "word", manifest$media_svg[i]), manifest$svg_asset[i], overwrite = TRUE)
  if (!ok) stop("No se pudo copiar el SVG de la figura ", manifest$fig[i], call. = FALSE)
}
# Embedded PNG of the PNG-only figures, kept to verify that the R output still matches the report
for (i in which(is.na(manifest$media_svg))) {
  ok <- file.copy(file.path(tmp, "word", manifest$media_png[i]),
                  file.path(FINALES_SVG, sprintf("Figure_%02d_report.png", manifest$fig[i])), overwrite = TRUE)
  if (!ok) stop("No se pudo copiar el PNG de la figura ", manifest$fig[i], call. = FALSE)
}

# Hashes of the report itself: build_finales.R refuses to run if the DOCX/PDF changed after this extraction
write_csv(
  manifest %>% mutate(
    report_docx = basename(REPORT_DOCX), report_pdf = basename(REPORT_PDF),
    md5_report_docx = unname(tools::md5sum(REPORT_DOCX)), md5_report_pdf = unname(tools::md5sum(REPORT_PDF))
  ),
  FINALES_MANIFEST
)
stale_assets <- setdiff(list.files(FINALES_SVG, pattern = "^Figure_\\d{2}(_report\\.png|\\.svg)$"),
                        c(basename(na.omit(manifest$svg_asset)),
                          sprintf("Figure_%02d_report.png", manifest$fig[is.na(manifest$media_svg)])))
if (length(stale_assets) > 0) {
  warning("Activos en ", FINALES_SVG, " que ya no corresponden a ninguna figura del reporte (se ignoran): ",
          paste(stale_assets, collapse = ", "), call. = FALSE)
}
cat("Figures extracted:", nrow(manifest), "| SVG assets:", sum(!is.na(manifest$media_svg)),
    "| PNG-only:", sum(is.na(manifest$media_svg)), "\n")

# Self-check: every asset on disk is the one recorded in the manifest
asset_ok <- c(
  with(manifest[!is.na(manifest$media_svg), ], unname(tools::md5sum(svg_asset)) == md5_svg),
  with(manifest[is.na(manifest$media_svg), ],
       unname(tools::md5sum(file.path(FINALES_SVG, sprintf("Figure_%02d_report.png", fig)))) == md5_png)
)
if (!all(asset_ok)) stop("Los activos escritos no coinciden con los hashes del manifiesto.", call. = FALSE)
cat("Verificación OK\n")
