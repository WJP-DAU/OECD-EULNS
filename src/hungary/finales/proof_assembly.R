## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Assembly proof of the final Hungary figures
## Author(s):         Santiago Pardo (spardo@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     October 2026
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Run from the project root (after build_finales.R):
##   Rscript src/hungary/finales/proof_assembly.R [assembled_report.pdf]
##
## Lays out every final figure as the assembled report of 16 Oct 2026 does (measured on that PDF): US Letter,
## the whole page at 82.2 %, body text 16 px, text column 8.43 in, figure area 6.5 in with a 1.72 in callout
## box at its right. Each figure goes at 100 % of its declared size. When the assembled PDF is given, the
## text of each callout box and of each note is taken from it, so the proof shows the real proportions.
## Writes output/viz/Hungary/prueba_ensamblaje/prueba_ensamblaje.{html,pdf} and one PNG per page.
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

source("src/hungary/finales/paths.R")
suppressWarnings(suppressMessages(library(tidyverse)))
source("src/hungary/finales/standard_figures.R")   # CALLOUT_FIGS, MAX_W_*

args <- commandArgs(TRUE)
assembled <- if (length(args) > 0) args[1] else NA_character_
PROOF_DIR <- file.path("output", "viz", "Hungary", "prueba_ensamblaje")
SCALE <- 0.822          # page scale of the assembled report (figure placed at 100 % -> 82.2 % on paper)

index <- read_csv(file.path(FINALES_VIZ, "00-INDICE.csv"), show_col_types = FALSE)

# Callout text of each figure in the assembled report: words right of the figure area, between the
# figure caption and the note / source of the figure
callouts <- tibble(figura = integer(), callout = character(), note = character())
if (!is.na(assembled) && file.exists(assembled)) {
  n_pages <- as.integer(sub(".*Pages:\\s+(\\d+).*", "\\1", paste(system2("pdfinfo", shQuote(assembled), stdout = TRUE), collapse = " ")))
  words <- map_dfr(seq_len(n_pages), function(pg) {
    h <- system2("pdftotext", c("-f", pg, "-l", pg, "-bbox", shQuote(assembled), "-"), stdout = TRUE)
    m <- str_match(h, 'xMin="([0-9.]+)" yMin="([0-9.]+)" xMax="([0-9.]+)" yMax="([0-9.]+)">([^<]+)<')
    m <- m[!is.na(m[, 1]), , drop = FALSE]
    if (nrow(m) == 0) return(NULL)
    tibble(page = pg, x = as.numeric(m[, 2]), y = as.numeric(m[, 3]), t = m[, 6])
  })
  # caption = "Figure" followed by its number on the same line
  # (alone on its line: body text can also start a line with "Figure N")
  caps <- words %>% arrange(page, y, x) %>% group_by(page) %>%
    mutate(nxt = lead(t), nxt_y = lead(y), n_line = map_int(y, ~ sum(abs(y - .x) < 1))) %>% ungroup() %>%
    filter(t == "Figure", str_detect(nxt, "^\\d+$"), abs(nxt_y - y) < 1, x < 120, n_line == 2) %>%
    transmute(page, y, figura = as.integer(nxt))
  ends <- words %>% filter(t %in% c("Note:", "Source:"), x < 120)
  callouts <- map_dfr(seq_len(nrow(caps)), function(i) {
    c0 <- caps[i, ]
    y_end <- ends %>% filter(page == c0$page, y > c0$y) %>% pull(y) %>% min(c(Inf, .))
    # callout lines: words right of 460 pt on lines with nothing between 380 and 458 pt (body text that
    # runs to the right margin always has words there; the figures never reach 455 pt)
    pg <- words %>% filter(page == c0$page, y > c0$y, y < y_end) %>% mutate(line = round(y))
    busy <- pg %>% filter(x > 380, x < 458) %>% pull(line)
    w <- pg %>% filter(x > 460, !line %in% c(busy, busy - 1, busy + 1)) %>% arrange(line, x)
    # note: from the first Note:/Source: below the caption to the end of the Source: line
    src_y <- ends %>% filter(page == c0$page, y >= y_end, t == "Source:") %>% pull(y) %>% min(c(Inf, .))
    nt <- words %>% filter(page == c0$page, y >= y_end - 0.5, y <= src_y + 0.5) %>% arrange(round(y), x)
    tibble(figura = c0$figura, pagina = c0$page, callout = paste(w$t, collapse = " "), note = paste(nt$t, collapse = " "))
  }) %>%
    mutate(across(c(callout, note), ~ str_replace_all(.x, c("&amp;" = "&", "&apos;" = "'", "&quot;" = "\"", "&lt;" = "<", "&gt;" = ">"))))
}
index <- index %>% left_join(callouts, by = "figura") %>%
  mutate(callout = if_else(junto_a_recuadro & (is.na(callout) | callout == ""), "Callout text.", callout),
         note = if_else(is.na(note) | note == "", "Source: 2024 WJP GLNS", note))

esc <- function(x) x %>% str_replace_all("&", "&amp;") %>% str_replace_all("<", "&lt;") %>% str_replace_all(">", "&gt;")
blocks <- map_chr(seq_len(nrow(index)), function(i) {
  f <- index[i, ]
  img <- sprintf("<img src='../finales/%s' style='width:%.3fin;height:%.3fin;display:block'>", f$svg, f$ancho_in, f$alto_in)
  side <- if (f$junto_a_recuadro) sprintf("<div class='callout'>%s</div>", esc(f$callout)) else ""
  sprintf(paste0(
    "<section class='fig'><div class='head'><div class='num'>Figure %d</div><div class='title'>%s</div>",
    "<div class='meta'>%.2f × %.2f in · texto %.1f pt al 100 %%</div></div>",
    "<div class='row'><div class='area%s'>%s</div>%s</div>",
    "<p class='note'>%s</p></section>"),
    f$figura, esc(f$titulo), f$ancho_in, f$alto_in, f$texto_pt, if (f$junto_a_recuadro) "" else " full", img, side,
    esc(f$note) %>% str_replace(" Source:", "<br>Source:"))
})

html <- c(
  "<!doctype html><html><head><meta charset='utf-8'><title>Prueba de ensamblaje · Hungary</title><style>",
  "@page { size: Letter; margin: 0; }",
  "html, body { margin: 0; background: #fff; }",
  sprintf("body { zoom: %.3f; font-family: 'Inter Tight', 'Inter', Arial, sans-serif; color: #1a1a1a; font-size: 16px; }", SCALE),
  sprintf(".fig { width: %.3fin; padding: %.3fin %.3fin 0 %.3fin; box-sizing: content-box; break-inside: avoid; page-break-inside: avoid; }",
          8.43, 0.6 / SCALE, 0.4 / SCALE, 0.845 / SCALE),
  ".head { border-left: 3px solid #4b2fb0; padding-left: 14px; margin-bottom: 18px; }",
  ".num { color: #4b2fb0; font-size: 17px; } .title { font-weight: 700; font-size: 19px; margin-top: 4px; }",
  ".meta { color: #888; font-size: 12px; font-style: italic; margin-top: 2px; }",
  ".row { display: flex; align-items: flex-start; } .area { width: 6.5in; min-width: 6.5in; } .area.full { width: 8.43in; }",
  ".callout { width: 1.72in; margin-left: 0.21in; background: #e9e8fb; color: #4b2fb0; font-style: italic; font-size: 15px; line-height: 1.5; padding: 18px 20px; box-sizing: border-box; }",
  ".note { font-style: italic; font-size: 16px; line-height: 1.7; margin: 14px 0 0 0; }",
  "</style></head><body>",
  blocks,
  "</body></html>"
)
dir.create(PROOF_DIR, recursive = TRUE, showWarnings = FALSE)
write_csv(index %>% select(figura, junto_a_recuadro, callout, note), file.path(PROOF_DIR, "recuadros.csv"))
html_file <- file.path(PROOF_DIR, "prueba_ensamblaje.html")
pdf_file <- file.path(PROOF_DIR, "prueba_ensamblaje.pdf")
writeLines(html, html_file, useBytes = TRUE)
unlink(pdf_file)
system2(CHROME, c("--headless=new", "--disable-gpu", "--no-pdf-header-footer",
                  shQuote(paste0("--print-to-pdf=", normalizePath(PROOF_DIR), "/prueba_ensamblaje.pdf")),
                  shQuote(paste0("file://", utils::URLencode(normalizePath(html_file))))), stdout = FALSE, stderr = FALSE)
if (!file.exists(pdf_file)) stop("Chrome no generó ", pdf_file, call. = FALSE)
unlink(list.files(PROOF_DIR, pattern = "^pagina-\\d+\\.png$", full.names = TRUE))
system2("pdftoppm", c("-r", 80, "-png", shQuote(pdf_file), shQuote(file.path(PROOF_DIR, "pagina"))))
cat("Prueba de ensamblaje:", pdf_file, "|", length(list.files(PROOF_DIR, pattern = "^pagina-")), "páginas\n")
