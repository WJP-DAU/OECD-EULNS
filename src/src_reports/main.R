## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## src_reports · main.R — único punto de entrada
## Produce las figuras del reporte Paths to Justice desde los microdatos: carga → variables → estimación →
## figura → verificación. Autocontenido: no hace source() de nada fuera de esta carpeta.
##
##   Rscript main.R [--stage data|figures|verify|compare|all] [--figs 09,12] [--country hungary]
##                  [--force] [--no-png] [--strict] [--help]
##
## Variables de entorno (opcionales): SRC_REPORTS_LIB (librería R que se antepone a .libPaths()),
## SRC_REPORTS_CACHE (carpeta de caché), SRC_REPORTS_ROOT (raíz si no se puede deducir de --file),
## CHROMOTE_CHROME (binario de Chrome). Rutas de la máquina: config/local.yml (ver local.yml.example).
## Consola: solo la tabla resumen (fig | metodo | estado | verificacion | archivo). Detalle en output/<slug>/log/.
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lib <- Sys.getenv("SRC_REPORTS_LIB")
if (nzchar(lib)) .libPaths(unique(c(lib, .libPaths())))

ROOT <- local({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  # Rscript sustituye los espacios de la ruta por "~+~" en --file=; se deshace antes de usarla
  r <- if (length(f) > 0) dirname(gsub("~\\+~", " ", sub("^--file=", "", f[1]))) else Sys.getenv("SRC_REPORTS_ROOT", unset = getwd())
  normalizePath(r, mustWork = FALSE)
})
if (!file.exists(file.path(ROOT, "R", "00_setup.R"))) {
  stop("No se encuentra R/00_setup.R en ", ROOT, ". Ejecute 'Rscript <carpeta>/main.R' o defina SRC_REPORTS_ROOT.",
       call. = FALSE)
}
here <- function(...) file.path(ROOT, ...)
for (f in sort(list.files(here("R"), pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))) source(f)

## Argumentos ------------------------------------------------------------------------------------------------

parse_args <- function(a) {
  out <- list(stage = "all", figs = NULL, country = NULL, force = FALSE, no_png = FALSE, strict = FALSE, help = FALSE)
  i <- 1
  while (i <= length(a)) {
    key <- a[i]
    take <- function() { if (i + 1 > length(a)) stop("Falta el valor de ", key, call. = FALSE); i <<- i + 1; a[i] }
    switch(key,
      "--stage"   = out$stage <- take(),
      "--figs"    = out$figs <- as.integer(strsplit(take(), ",")[[1]]),
      "--country" = out$country <- take(),
      "--force"   = out$force <- TRUE,
      "--no-png"  = out$no_png <- TRUE,
      "--strict"  = out$strict <- TRUE,
      "--help"    = out$help <- TRUE,
      stop("Argumento desconocido: ", key, " (use --help)", call. = FALSE))
    i <- i + 1
  }
  if (!out$stage %in% c("data", "figures", "verify", "compare", "all")) {
    stop("--stage debe ser data, figures, verify, compare o all", call. = FALSE)
  }
  if (!is.null(out$figs) && anyNA(out$figs)) stop("--figs espera números separados por coma, p. ej. 09,12", call. = FALSE)
  out
}

usage <- function() cat(
  "Uso: Rscript main.R [--stage data|figures|verify|compare|all] [--figs 09,12] [--country hungary]\n",
  "                    [--force] [--no-png] [--strict] [--help]\n",
  "  data     lee el .dta (path2EU en config/local.yml) y construye cache/ (master y ctx del país)\n",
  "  figures  escribe output/<slug>/tables/*.csv y figures/*.svg|png de las figuras seleccionadas\n",
  "  verify   compara output/<slug>/tables con reference/<slug> y escribe tables/00-verificacion.csv\n",
  "  compare  hojas publicada | nueva en output/<slug>/compare/\n",
  "  all      data -> figures -> verify -> compare (defecto)\n",
  "  --force  reconstruye el caché; --no-png omite los PNG; --strict sale con 1 si alguna figura no es ok\n", sep = "")

args <- parse_args(commandArgs(trailingOnly = TRUE))
if (args$help) { usage(); quit(save = "no", status = 0) }

## Configuración y corrida -------------------------------------------------------------------------------------

cfg <- load_config(ROOT, args$country)
run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
dir.create(file.path(cfg$paths$output, "log"), recursive = TRUE, showWarnings = FALSE)
log_open(file.path(cfg$paths$output, "log", paste0("run_", run_id, ".log")))
log_msg("run ", run_id, " | stage ", args$stage, " | country ", cfg$country, " | root ", ROOT)
check_deps(cfg)
setup_fonts(cfg)
specs <- read_manifest(cfg)
specs <- lapply(specs, function(s) { s$selected <- is.null(args$figs) || s$fig %in% args$figs; s })   # no seleccionada → "omitida"
if (!any(vapply(specs, `[[`, logical(1), "selected"))) stop("Ninguna figura del manifiesto coincide con --figs", call. = FALSE)
stages <- if (args$stage == "all") c("data", "figures", "verify", "compare") else args$stage

rows <- list(); notes <- character()
ctx <- NULL
if ("data" %in% stages) {
  res <- with_log(load_ctx(cfg, force = args$force, build = TRUE))
  ctx <- res$ctx
  rows$data <- res$table
  notes <- c(notes, res$note)
}
if ("figures" %in% stages) {
  if (is.null(ctx)) ctx <- with_log(load_ctx(cfg, build = TRUE))$ctx
  rows$figures <- with_log(run_figures(specs, ctx, cfg, run_id, png = !args$no_png))
  notes <- c(notes, if (args$no_png) "PNG omitidos (--no-png)" else if (is.na(cfg$paths$chrome)) "Chrome no encontrado: PNG omitidos" else NULL)
}
if ("verify" %in% stages) rows$verify <- with_log(run_verify(specs, cfg))
if ("compare" %in% stages) rows$compare <- with_log(run_compare(specs, cfg))
log_close()

## Tabla resumen (única salida a consola) --------------------------------------------------------------------

print_header(cfg, args$stage, run_id, notes)
print_rows(rows)
status <- 0L
if ("verify" %in% stages) {
  v <- rows$verify[rows$verify$estado != "omitida", ]
  n_ok <- sum(v$estado == "ok"); n_err <- sum(v$estado == "error"); n_known <- sum(grepl("conocida", v$verificacion))
  cat(sprintf("%d/%d verificadas · %d conocidas · %d errores · log: %s\n", n_ok, nrow(v), n_known, n_err, log_path()))
  if (args$strict && n_ok < nrow(v)) status <- 1L
}
quit(save = "no", status = status)
