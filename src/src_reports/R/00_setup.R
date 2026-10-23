## 00_setup.R — paquetes, configuración, dependencias, fuentes, registro y tabla de consola --------------------
## Origen: nuevo para src_reports (sustituye src/utils/config.R, que elegía rutas por Sys.info()["user"]).
## Carga: main.R hace source() de R/00-08 en orden; aquí no hay código que se ejecute al cargar salvo las
## librerías y el entorno .sr (estado de sesión).

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr); library(stringr); library(tibble); library(ggplot2)
})
options(dplyr.summarise.inform = FALSE, readr.show_col_types = FALSE, warn = 1)
invisible(Sys.setlocale("LC_COLLATE", "C"))          # orden alfabético determinista (rows_* usan .locale = "C")
if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)   # svglite debe emitir <text>
options(wjpr.family = "inter")

.sr <- new.env(parent = emptyenv())                   # estado de sesión: log, memo de estimadores, fuentes


## Configuración ----------------------------------------------------------------------------------------------

# cfg = config/<country>.yml + config/local.yml (rutas de la máquina) + variables de entorno; cfg$paths absolutas.
# country NULL = el único yml de config/ que no sea local*.
load_config <- function(root, country = NULL) {
  ymls <- setdiff(list.files(file.path(root, "config"), pattern = "\\.yml$"), c("local.yml", "local.yml.example"))
  if (is.null(country)) {
    if (length(ymls) != 1) stop("Indique --country: hay ", length(ymls), " configuraciones en config/", call. = FALSE)
    country <- sub("\\.yml$", "", ymls)
  }
  file <- file.path(root, "config", paste0(country, ".yml"))
  if (!file.exists(file)) stop("No existe ", file, call. = FALSE)
  cfg <- yaml::read_yaml(file)
  local_file <- file.path(root, "config", "local.yml")
  loc <- if (file.exists(local_file)) yaml::read_yaml(local_file) else list()
  nz <- function(x) !is.null(x) && nzchar(x)
  path2EU <- if (nz(loc$path2EU)) loc$path2EU else NA_character_
  cache <- Sys.getenv("SRC_REPORTS_CACHE")
  if (!nzchar(cache)) cache <- if (nz(loc$cache_dir)) loc$cache_dir else file.path(root, "cache")
  cfg$paths <- list(
    root      = root,
    config    = file,
    dta       = if (is.na(path2EU)) NA_character_ else file.path(path2EU, cfg$data$dta),
    data4web  = if (is.na(path2EU) || is.null(cfg$data$data4web)) NA_character_ else file.path(path2EU, cfg$data$data4web),
    regions   = file.path(root, cfg$data$regions),
    eu_values = file.path(root, cfg$data$eu_values),
    fonts     = file.path(root, "fonts"),
    chrome    = find_chrome(if (nz(loc$chrome)) loc$chrome else NULL),
    cache     = cache,
    output    = file.path(root, "output", cfg$slug),
    reference = file.path(root, "reference", cfg$slug),
    manifest  = file.path(root, "manifest", "figures.csv"),
    deps      = file.path(root, "deps.csv"),
    published = if (nz(loc$published_dir)) loc$published_dir else NA_character_,          # solo tools/build_reference.R
    reference_src = if (nz(loc$reference_src_dir)) loc$reference_src_dir else NA_character_  # idem (CSV publicados)
  )
  for (d in c("figures", "tables", "compare", "log")) dir.create(file.path(cfg$paths$output, d), recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$paths$cache, recursive = TRUE, showWarnings = FALSE)
  cfg
}

# Chrome: CHROMOTE_CHROME > local.yml > candidatos fijos > PATH. NA si no hay (los PNG se omiten con aviso).
find_chrome <- function(local = NULL) {
  cand <- c(Sys.getenv("CHROMOTE_CHROME"), local,
            "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
            "/Applications/Google Chrome .app/Contents/MacOS/Google Chrome",
            "/opt/homebrew/bin/chromium", "/usr/bin/google-chrome", "/usr/bin/chromium", "/usr/bin/chromium-browser",
            unname(Sys.which(c("google-chrome", "chromium", "chromium-browser", "chrome-headless-shell"))))
  cand <- cand[!is.na(cand) & nzchar(cand) & file.exists(cand)]
  if (length(cand) == 0) NA_character_ else cand[1]
}


## Dependencias y fuentes ---------------------------------------------------------------------------------------

# deps.csv: paquete, version, origen, modo (exacta | minima). Falta de 'minima' detiene; desajuste de 'exacta' avisa.
check_deps <- function(cfg) {
  deps <- read_csv(cfg$paths$deps, show_col_types = FALSE)
  installed <- vapply(deps$paquete, function(p) if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else NA_character_, "")
  deps$instalada <- unname(installed)
  missing <- deps %>% filter(modo == "minima" & (is.na(instalada) | package_version(instalada) < package_version(version)))
  if (nrow(missing) > 0) {
    stop("Faltan paquetes (o versión menor a la mínima): ",
         paste0(missing$paquete, " >= ", missing$version, collapse = ", "),
         "\nInstale con: install.packages(c(", paste0("'", missing$paquete, "'", collapse = ", "), "))", call. = FALSE)
  }
  exact <- deps %>% filter(modo == "exacta" & (is.na(instalada) | instalada != version))
  .sr$deps_warning <- if (nrow(exact) > 0) paste0(exact$paquete, " ", coalesce(exact$instalada, "ausente"), " (esperada ", exact$version, ")", collapse = ", ") else NULL
  if (!is.null(.sr$deps_warning)) log_msg("deps.csv: versiones distintas de las exactas: ", .sr$deps_warning)
  invisible(deps)
}

# Las 8 TTF estáticas de Inter Tight viven en fonts/ (OFL). Mismo registro que src/hungary/setup.R:200-206.
setup_fonts <- function(cfg) {
  FONT_DIR <<- cfg$paths$fonts
  needed <- c("Regular", "Italic", "Medium", "MediumItalic", "SemiBold", "SemiBoldItalic", "Bold", "BoldItalic")
  files <- file.path(FONT_DIR, paste0("InterTight-", needed, ".ttf"))
  bad <- files[!file.exists(files) | file.size(files) == 0]
  if (length(bad) > 0) stop("Faltan o están vacías (OneDrive sin descargar) las fuentes: ", paste(basename(bad), collapse = ", "), call. = FALSE)
  md5_file <- file.path(FONT_DIR, "md5.txt")
  if (file.exists(md5_file)) {
    rec <- read.table(md5_file, col.names = c("md5", "archivo"), stringsAsFactors = FALSE)
    now <- unname(tools::md5sum(file.path(FONT_DIR, rec$archivo)))
    if (any(now != rec$md5)) log_msg("fonts/: md5 distinto de fonts/md5.txt en ", paste(rec$archivo[now != rec$md5], collapse = ", "))
  }
  systemfonts::register_font(
    name = "inter",
    plain = file.path(FONT_DIR, "InterTight-Regular.ttf"), bold = file.path(FONT_DIR, "InterTight-Bold.ttf"),
    italic = file.path(FONT_DIR, "InterTight-Italic.ttf"), bolditalic = file.path(FONT_DIR, "InterTight-SemiBoldItalic.ttf")
  )
  invisible(FONT_DIR)
}


## Manifiesto y figuras -------------------------------------------------------------------------------------------

# Lista de specs (una fila del manifiesto cada una, con titulo resuelto). spec es la única acepción de 'spec'.
read_manifest <- function(cfg) {
  m <- read_csv(cfg$paths$manifest, col_types = cols(.default = col_character()), na = character())
  need <- c("fig", "slug", "titulo", "seccion", "pagina", "tipo", "metodo", "funcion", "w_in", "h_in", "callout",
            "label_col", "base_pt", "fuente_datos", "metodo_ue", "codigo_origen", "wjpr", "status", "verificada", "nota")
  miss <- setdiff(need, names(m))
  if (length(miss) > 0) stop("manifest/figures.csv sin columnas: ", paste(miss, collapse = ", "), call. = FALSE)
  m <- m %>% mutate(fig = as.integer(fig), pagina = as.integer(pagina), w_in = as.numeric(w_in), h_in = as.numeric(h_in),
                    callout = tolower(callout) == "true", base_pt = suppressWarnings(as.numeric(base_pt)),
                    titulo = str_replace_all(titulo, fixed("{country}"), cfg$country))
  if (anyDuplicated(m$fig)) stop("manifest/figures.csv: fig repetido", call. = FALSE)
  lapply(seq_len(nrow(m)), function(i) as.list(m[i, ]))
}

figure_file <- function(spec, cfg) file.path(cfg$paths$root, "figures", sprintf("fig_%02d_%s.R", spec$fig, spec$slug))
figure_stem <- function(spec) sprintf("Figure_%02d_%s", spec$fig, spec$slug)

# Carga figures/fig_NN_slug.R en un entorno propio y devuelve fig_NN (list(data, plot))
load_figure <- function(spec, cfg) {
  f <- figure_file(spec, cfg)
  if (!file.exists(f)) stop("No existe ", basename(f), call. = FALSE)
  env <- new.env(parent = globalenv())
  sys.source(f, envir = env)
  obj <- sprintf("fig_%02d", spec$fig)
  fig <- get0(obj, envir = env, inherits = FALSE)
  if (!is.list(fig) || !all(c("data", "plot") %in% names(fig))) stop(basename(f), " debe terminar con ", obj, " <- list(data, plot)", call. = FALSE)
  fig
}


## Registro (log) y consola ---------------------------------------------------------------------------------------

# El log se escribe mensaje a mensaje (abrir-anexar-cerrar): una conexión abierta durante toda la corrida sobre
# OneDrive pierde escrituras y falla al cerrar ("Operation timed out"). with_log() desvía la salida y los mensajes
# a un archivo temporal local y los anexa al log al terminar.
log_open <- function(path) { .sr$log <- path; cat("", file = path); invisible(path) }
log_path <- function() .sr$log %||% ""
log_msg <- function(...) {
  if (!is.null(.sr$log)) {
    try(cat(format(Sys.time(), "%H:%M:%S"), " ", paste0(..., collapse = ""), "\n", sep = "", file = .sr$log, append = TRUE),
        silent = TRUE)
  }
  invisible(NULL)
}
log_close <- function() invisible(NULL)               # sin conexión persistente; .sr$log se conserva para log_path()

# Primera línea de un mensaje de error, recortada, para la columna verificacion (la usan 06, 07 y 08)
first_line <- function(msg, n = 60) substr(trimws(strsplit(paste0(msg, "\n"), "\n", fixed = TRUE)[[1]][1]), 1, n)

# Evalúa expr con la salida y los mensajes desviados a un temporal local, que se anexa al log con el prefijo " | "
with_log <- function(expr) {
  if (is.null(.sr$log)) return(expr)
  tmp <- tempfile("salida_", fileext = ".log"); con <- file(tmp, open = "wt")
  sink(con, type = "output"); sink(con, type = "message")
  on.exit({
    sink(type = "message"); sink(type = "output"); close(con)
    out <- tryCatch(readLines(tmp, warn = FALSE), error = function(e) character())
    if (length(out) > 0) try(cat(paste0("         | ", out), file = .sr$log, sep = "\n", append = TRUE), silent = TRUE)
    unlink(tmp)
  }, add = TRUE)
  expr
}

print_header <- function(cfg, stage, run_id, notes = NULL) {
  cat(sprintf("%s | stage %s | %s | lib %s | renv %s | Chrome %s | locale %s%s\n",
              cfg$country, stage, run_id, .libPaths()[1], if (nzchar(Sys.getenv("RENV_PROJECT"))) "sí" else "no",
              if (is.na(cfg$paths$chrome)) "no" else "sí", Sys.getlocale("LC_COLLATE"),
              if (length(notes)) paste0(" | ", paste(notes, collapse = " | ")) else ""))
  if (!is.null(.sr$deps_warning)) cat("aviso deps.csv:", .sr$deps_warning, "\n")
}

# Filas de la tabla: tibble(fig, metodo, estado, verificacion, archivo). Se imprime la tabla más completa
# disponible (verify > figures > compare), precedida por las filas de la etapa data.
print_rows <- function(rows) {
  tbl <- rows$verify %||% rows$figures %||% rows$compare
  all <- bind_rows(rows$data, tbl)
  if (is.null(all) || nrow(all) == 0) return(invisible(NULL))
  cat(sprintf("%-4s %-7s %-15s %-44s %s\n", "fig", "metodo", "estado", "verificacion", "archivo"))
  for (i in seq_len(nrow(all))) {
    cat(sprintf("%-4s %-7s %-15s %-44s %s\n", all$fig[i], substr(all$metodo[i], 1, 7), substr(all$estado[i], 1, 15),
                substr(all$verificacion[i], 1, 44), all$archivo[i]))
  }
  invisible(all)
}
