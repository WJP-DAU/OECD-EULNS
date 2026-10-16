## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Hungary additional analyses - setup
## Author(s):         Santiago Pardo (spardo@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     September 2026
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Extends the existing pipeline WITHOUT modifying it: sources its utilities, data functions and viz
## functions, and writes every output to output/viz/Hungary/analisis adicionales/.
##
## Weighting (same as the report, written in every note):
##   - Every estimate uses the pipeline method: mean within each NUTS region, regions with n < 30
##     suppressed, regional means weighted by regionpoppct (get_nuts_avg() + get_wgt_national_avg()).
##   - Problem-level quantities (severity) use the same scheme with nuts_mean_long().
##   - EU averages are the simple average of the weighted country averages (same method as
##     eu_main_indicators.R), computed from the EU-GPP Stata file with the same quality check;
##     countries without the LNS module (Ireland) are NA.
##   - The lifts and the co-occurrence network are unweighted, as in the report (module_1.R, network_graph.R).
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

project_library <- file.path(
  getwd(), "renv", "library", "macos",
  paste0("R-", R.version$major, ".", strsplit(R.version$minor, "[.]")[[1]][1]),
  R.version$platform
)
if (dir.exists(project_library)) .libPaths(unique(c(project_library, .libPaths())))

suppressWarnings(suppressMessages(library("tidyverse", quietly = TRUE)))

if (!exists("opt")) opt <- list(verbose = TRUE)
source("src/utils/config.R")
source("src/utils/utils.R")
source("src/data/data-loading.R")
source("src/data/special-vars.R")
source("src/analysis/main_indicators.R")
source("src/analysis/eu_main_indicators.R")

HU      <- "Hungary"
OUT     <- file.path("output", "viz", "Hungary", "analisis adicionales")
CACHE   <- file.path("output", "tabs", "cache_hungary")
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)
OUT_ABS <- normalizePath(OUT)

COL_MAIN   <- "#575796"
COL_LIGHT  <- "#a8a8c8"
COL_BG     <- "#e5e8e8"
COL_ACCENT <- "#b200aa"
COL_TEXT   <- "#1a1a1a"
COL_GREY   <- "#9aa0a6"

problem_map <- c(
  A1 = "Consumer", A2 = "Consumer", A3 = "Consumer",
  B1 = "Land", B2 = "Land", B3 = "Land", B4 = "Land",
  C1 = "Housing", C2 = "Housing", C4 = "Housing", C3 = "Community", E3 = "Community",
  D1 = "Family", D2 = "Family", D3 = "Family", D4 = "Family", D5 = "Family", D6 = "Family",
  E1 = "Education", E2 = "Education",
  F1 = "Accidental", F2 = "Accidental",
  G1 = "Employment", G2 = "Employment", G3 = "Employment",
  H1 = "Public Services", H2 = "Public Services", H3 = "Public Services", J4 = "Public Services",
  I1 = "Law Enforcement",
  J1 = "Citizenship & ID", J2 = "Citizenship & ID", J3 = "Citizenship & ID",
  K1 = "Money & Debt", K2 = "Money & Debt", K3 = "Money & Debt", L1 = "Money & Debt", L2 = "Money & Debt"
)
categories <- sort(unique(problem_map))

binary_agree <- function(x) case_when(x %in% c(1, 2) ~ 1, x %in% c(3, 4, 98) ~ 0)


## 1. Data ----------------------------------------------------------------------------------------------

regions <- load_data(path2EU, "regions")

hu <- load_data(path2EU, "subset") %>%
  filter(country_name_ltn == HU)

hu_jg <- load_data(path2EU, "jg") %>%
  filter(str_starts(country_year_id, HU))

eu_cache <- file.path(CACHE, "eu_master.rds")
if (!file.exists(eu_cache)) {
  verbose_message("--- Building EU master from the EU-GPP Stata file (cached afterwards)...")
  # build_eu_master() -> add_a2j_vars() rewrites data/jg_data_*.csv: back them up and restore them
  jg_files <- c("data/jg_data_nodk.csv", "data/jg_data_keepdk.csv")
  backups  <- file.path(CACHE, basename(jg_files))
  file.copy(jg_files, backups, overwrite = TRUE)
  eu <- build_eu_master(path2EU, regions) %>% haven::zap_labels()
  file.copy(backups, jg_files, overwrite = TRUE)
  saveRDS(eu, eu_cache)
} else {
  eu <- readRDS(eu_cache)
}
eu <- eu %>% mutate(across(where(is.factor), as.character))



## 2. Person-level problem indicators --------------------------------------------------------------------

# Non-trivial (severity 4-10) and any-severity indicators per category, quality check already applied
# upstream (flagged respondents have every AJP_*_bin set to 2 and severities set to NA).
problem_flags <- function(df, min_sev = 4, any_severity = FALSE) {
  out <- lapply(categories, function(cat) {
    codes <- names(problem_map)[problem_map == cat]
    hits <- sapply(codes, function(code) {
      b <- df[[paste0("AJP_", code, "_bin")]]
      s <- df[[paste0("AJP_", code, "_sev")]]
      if (any_severity) !is.na(b) & b == 1
      else !is.na(b) & b == 1 & !is.na(s) & s >= min_sev & s < 98
    })
    rowSums(matrix(hits, nrow = nrow(df))) > 0
  })
  names(out) <- categories
  as_tibble(out)
}

has_lns <- function(df) {
  bins <- df %>% select(matches("^AJP_[A-L][0-9]_bin$"))
  rowSums(!is.na(bins)) > 0
}


## 3. Estimators ------------------------------------------------------------------------------------------

# Pipeline national estimate (NUTS average, n < 30 suppression) for one country or many.
nat_est <- function(df, target, transform = NULL, by = NULL) {
  df <- df %>% mutate(.t = .data[[target]])
  if (!is.null(transform)) df <- df %>% mutate(.t = transform(.t))
  n_tbl <- df %>%
    filter(!is.na(.t), if (is.null(by)) TRUE else !is.na(.data[[by]])) %>%
    count(country_name_ltn, across(any_of(by)), name = "n")
  reg <- get_nuts_avg(df, ".t", disaggregate_by = by)
  res <- get_wgt_national_avg(reg, regions, target_col = "proportion", group_col = if (is.null(by)) NULL else "group") %>%
    mutate(pct = proportion * 100) %>%
    select(-proportion)
  if (!is.null(by)) {
    res <- res %>% rename(!!by := category)
  }
  left_join(res, n_tbl, by = c("country_name_ltn", by))
}

# EU average = mean of the country estimates (countries with data only).
eu_est <- function(df, target, transform = NULL, by = NULL) {
  nat_est(df, target, transform, by) %>%
    group_by(across(any_of(by))) %>%
    summarise(
      eu_pct = mean(pct, na.rm = TRUE),
      n_countries = sum(!is.na(pct)),
      .groups = "drop"
    )
}

# Pipeline scheme for problem-level (long) data: mean of `value` within each country x NUTS region x
# group, regions with n < 30 suppressed, then weighted by regional population (renormalised).
nuts_mean_long <- function(long, value, groups = NULL) {
  long %>%
    filter(!is.na(.data[[value]])) %>%
    group_by(country_name_ltn, nuts_id, across(all_of(groups))) %>%
    summarise(m = mean(.data[[value]]), n = n(), .groups = "drop") %>%
    mutate(m = if_else(n < 30, NA_real_, m)) %>%
    left_join(regions %>% select(nuts_id, wgt = regionpoppct), by = "nuts_id") %>%
    group_by(country_name_ltn, across(all_of(groups))) %>%
    summarise(
      est = if (all(is.na(m))) NA_real_ else sum(m * wgt, na.rm = TRUE) / sum(wgt[!is.na(m)]),
      n = sum(n), .groups = "drop"
    )
}


## 4. Output helpers --------------------------------------------------------------------------------------

save_table <- function(df, id, suffix = NULL) {
  file <- file.path(OUT, paste0(id, if (!is.null(suffix)) paste0("_", suffix), ".csv"))
  write_csv(df, file, na = "")
  invisible(file)
}

save_note <- function(id, ...) {
  writeLines(c(...), file.path(OUT, paste0(id, ".md")))
}

md_table <- function(df, digits = 1) {
  df <- df %>% mutate(across(where(is.numeric), \(x) round(x, digits)))
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep    <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  rows   <- apply(df, 1, function(r) paste0("| ", paste(ifelse(is.na(r), "", r), collapse = " | "), " |"))
  c(header, sep, rows)
}


## 5. Fonts and plotting helpers --------------------------------------------------------------------------

path2fonts <- file.path(path2DA, "6. Country Reports", "0. Fonts")
sysfonts::font_add(
  "inter",
  regular    = file.path(path2fonts, "InterTight-Regular.ttf"),
  bold       = file.path(path2fonts, "InterTight-Bold.ttf"),
  italic     = file.path(path2fonts, "InterTight-Italic.ttf"),
  bolditalic = file.path(path2fonts, "InterTight-SemiBoldItalic.ttf")
)
systemfonts::register_font(
  name       = "inter",
  plain      = file.path(path2fonts, "InterTight-Regular.ttf"),
  bold       = file.path(path2fonts, "InterTight-Bold.ttf"),
  italic     = file.path(path2fonts, "InterTight-Italic.ttf"),
  bolditalic = file.path(path2fonts, "InterTight-SemiBoldItalic.ttf")
)
showtext::showtext_auto(FALSE)

theme_hu <- function(base = 13) {
  theme_minimal(base_size = base, base_family = "inter") +
    theme(
      text               = element_text(color = COL_TEXT),
      plot.title         = element_text(face = "bold", size = base + 3, color = COL_TEXT),
      plot.subtitle      = element_text(face = "italic", size = base, color = COL_TEXT),
      plot.caption       = element_text(hjust = 0, size = base - 3, color = "#555555", lineheight = 1.1),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      strip.text         = element_text(face = "bold.italic", color = COL_MAIN, size = base),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position    = "top",
      legend.title       = element_blank()
    )
}

wrap_caption <- function(x, width) {
  paste(map_chr(str_split(x, "\n")[[1]], \(l) str_wrap(l, width)), collapse = "\n")
}

save_fig <- function(p, file_stem, w, h) {
  # gen_grouped_bars() re-enables showtext when it finishes; with showtext on, ragg/svglite text is
  # drawn at the wrong scale, so make sure it is off before saving
  showtext::showtext_auto(FALSE)
  # Wrap long captions to the figure width so they never run off the canvas
  if (!is.null(p$labels$caption)) p$labels$caption <- wrap_caption(p$labels$caption, floor(w * 0.42))
  ggsave(file.path(OUT, paste0(file_stem, ".svg")), p, device = svglite::svglite,
         width = w, height = h, units = "mm", scale = 0.75)
  ggsave(file.path(OUT, paste0(file_stem, ".png")), p, device = ragg::agg_png,
         width = w, height = h, units = "mm", scale = 0.75, dpi = 300, bg = "white")
  invisible(file_stem)
}

# Runs an existing viz function (which saves to output/viz/{country}/...) inside a scratch root, then
# copies the produced file into the additional-analyses folder under the requested name. The original
# figures in output/viz/Hungary are never touched.
render_existing <- function(render_fn, produced, final) {
  dir.create(file.path(CACHE, "render", "output", "viz", HU), recursive = TRUE, showWarnings = FALSE)
  root <- normalizePath(file.path(CACHE, "render"))
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  render_fn()
  ok <- file.copy(file.path(root, "output", "viz", HU, produced), file.path(OUT_ABS, final), overwrite = TRUE)
  if (!ok) stop("render_existing(): could not produce ", final)
  invisible(final)
}

verbose_message("--- Hungary setup ready")
