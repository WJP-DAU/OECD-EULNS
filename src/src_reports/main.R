## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## src_reports · main.R — único punto de entrada
## Produce las figuras del reporte Paths to Justice desde los microdatos: carga → variables → estimación →
## figura → verificación. Autocontenido: no hace source() de nada fuera de esta carpeta.
##
##   Rscript main.R [--stage data|figures|verify|compare|all] [--figs 09,12] [--country hungary]
##                  [--force] [--no-png] [--strict] [--help]
##
## Variables de entorno (opcionales): SRC_REPORTS_LIB (librería R que se antepone a .libPaths()),
## SRC_REPORTS_CACHE (carpeta de caché), SRC_REPORTS_ROOT (raíz si junto a main.R no está R/00_setup.R),
## SRC_REPORTS_ARGS (argumentos cuando no llegan por la línea de comandos, p. ej. con source() desde RStudio),
## CHROMOTE_CHROME (binario de Chrome). Rutas de la máquina: config/local.yml (ver local.yml.example).
## Consola: cabecera, tabla resumen (fig | metodo | estado | verificacion | archivo) y línea de cierre con la ruta del
## log; el detalle va a output/<slug>/log/run_<id>.log. Un error que detiene una etapa se muestra como
## "Error en la etapa <x>: <primera línea>" + "Detalle: <log>" y la salida es 1.
## Con source() (sesión interactiva u otro script) no se llama quit(): source("main.R")$value es el código de salida.
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

lib <- Sys.getenv("SRC_REPORTS_LIB")
if (nzchar(lib)) .libPaths(unique(c(lib, .libPaths())))

# Raíz = primera carpeta con R/00_setup.R entre: la del archivo que recibió source() (RStudio, otro script), la del
# --file= de Rscript (Rscript cambia los espacios por "~+~"), SRC_REPORTS_ROOT y el directorio de trabajo. Las rutas
# se normalizan antes de dirname() para resolver un enlace simbólico a main.R (antes se tomaba la carpeta del
# enlace, y SRC_REPORTS_ROOT, que sugería el error, no se consultaba si había --file; revisión R5-15).
ROOT <- local({
  src <- NULL
  for (i in rev(seq_len(sys.nframe()))) {
    if (identical(sys.function(i), base::source)) { src <- get0("ofile", envir = sys.frame(i), inherits = FALSE); break }
  }
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  dir_of <- function(p) {
    if (!is.character(p) || length(p) != 1 || is.na(p) || !nzchar(p)) return(NA_character_)
    dirname(normalizePath(p, mustWork = FALSE))
  }
  cand <- c(dir_of(src), dir_of(gsub("~\\+~", " ", f[1])), Sys.getenv("SRC_REPORTS_ROOT"), getwd())
  cand <- unique(normalizePath(cand[!is.na(cand) & nzchar(cand)], mustWork = FALSE))
  ok <- cand[file.exists(file.path(cand, "R", "00_setup.R"))]
  if (length(ok) == 0) {
    stop("No se encuentra R/00_setup.R en ", paste(cand, collapse = " ni en "),
         ". Ejecute 'Rscript <carpeta>/main.R' o defina SRC_REPORTS_ROOT.", call. = FALSE)
  }
  ok[1]
})
for (f in sort(list.files(file.path(ROOT, "R"), pattern = "^[0-9]{2}_.*\\.R$", full.names = TRUE))) source(f)

## Argumentos ------------------------------------------------------------------------------------------------

# Estricto (revisión R4-14): --help gana a cualquier otro argumento, aunque sea inválido; una opción repetida, un
# valor ausente (o que es otra opción) y un --figs que no sea una lista de enteros dan error (antes '9.7' y '0x9'
# daban 9 sin aviso y con '--stage verify --stage compare' ganaba la última).
parse_args <- function(a) {
  out <- list(stage = "all", figs = NULL, country = NULL, force = FALSE, no_png = FALSE, strict = FALSE,
              help = "--help" %in% a)
  if (out$help) return(out)
  seen <- character()
  i <- 1
  while (i <= length(a)) {
    key <- a[i]
    if (key %in% seen) stop("Opción repetida: ", key, " (indíquela una sola vez)", call. = FALSE)
    seen <- c(seen, key)
    take <- function() {
      if (i + 1 > length(a) || startsWith(a[i + 1], "--")) stop("Falta el valor de ", key, call. = FALSE)
      i <<- i + 1; a[i]
    }
    switch(key,
      "--stage"   = out$stage <- take(),
      "--figs"    = out$figs <- parse_figs(take()),
      "--country" = out$country <- take(),
      "--force"   = out$force <- TRUE,
      "--no-png"  = out$no_png <- TRUE,
      "--strict"  = out$strict <- TRUE,
      stop("Argumento desconocido: ", key, " (use --help)", call. = FALSE))
    i <- i + 1
  }
  if (!out$stage %in% c("data", "figures", "verify", "compare", "all")) {
    stop("--stage debe ser data, figures, verify, compare o all (recibido: ", out$stage, ")", call. = FALSE)
  }
  out
}

# "09,12" → c(9L, 12L): cada elemento solo dígitos (sin la coerción silenciosa de as.integer)
parse_figs <- function(x) {
  v <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  if (length(v) == 0 || !all(grepl("^[0-9]+$", v))) {
    stop("--figs espera números separados por coma, p. ej. 09,12 (recibido: ", x, ")", call. = FALSE)
  }
  unique(as.integer(v))
}

usage <- function() cat(
  "Uso: Rscript main.R [--stage data|figures|verify|compare|all] [--figs 09,12] [--country hungary]\n",
  "                    [--force] [--no-png] [--strict] [--help]\n",
  "  data     lee el .dta (path2EU en config/local.yml) y construye cache/ (master y ctx del país)\n",
  "  figures  escribe output/<slug>/tables/*.csv y figures/*.svg|png de las figuras seleccionadas\n",
  "  verify   compara output/<slug>/tables con reference/<slug> y escribe tables/00-verificacion.csv\n",
  "  compare  hojas publicada | nueva en output/<slug>/compare/\n",
  "  all      data -> figures -> verify -> compare (defecto)\n",
  "  --figs   figuras de figures, verify y compare (data no lo usa); --force reconstruye el caché;\n",
  "  --no-png omite los PNG; --strict sale con 1 si alguna figura seleccionada no es ok en figures o verify\n",
  "           (o da error en compare)\n",
  "  Con source() (RStudio): Sys.setenv(SRC_REPORTS_ARGS = \"--stage figures --figs 09\") y luego source(\"main.R\")\n",
  sep = "")

## Corrida -----------------------------------------------------------------------------------------------------

# Una etapa (o el inicio) con su salida desviada al log. Un error la detiene (decisión 8 de la revisión): se registra
# en el log si with_log no lo hizo ya y se relanza como condición sr_fatal con el nombre de la etapa, que main()
# convierte en "Error en la etapa <x>: …" + "Detalle: <log>" (antes en consola solo quedaba 'Ejecución interrumpida').
# Un error de R con llamada conserva su prefijo, como en R ("readRDS(f_rds): error al leer desde conexión").
run_stage <- function(stage, expr) {
  tryCatch(with_log(expr), error = function(e) {
    cl <- conditionCall(e)
    msg <- if (is.null(cl)) conditionMessage(e) else paste0(deparse(cl, nlines = 1L)[1], ": ", conditionMessage(e))
    if (isTRUE(e$en_log)) log_msg("etapa ", stage, " interrumpida") else log_msg("error en la etapa ", stage, ": ", msg)
    stop(structure(class = c("sr_fatal", "error", "condition"), list(message = msg, call = NULL, stage = stage)))
  })
}

# Devuelve el código de salida: 0; 1 si una etapa se detuvo, si los argumentos no son válidos o, con --strict, si
# alguna figura seleccionada no es ok (figures y verify) o da error en compare (antes --strict solo miraba verify).
main <- function(argv) {
  args <- tryCatch(parse_args(argv), error = function(e) e)
  if (inherits(args, "error")) { cat("Error en los argumentos: ", conditionMessage(args), "\n", sep = ""); usage(); return(1L) }
  if (args$help) { usage(); return(0L) }
  if (interactive() && length(argv) == 0) {         # source() sin argumentos no corre 'all' por defecto (R5-10)
    cat("Sesión interactiva sin argumentos: defina SRC_REPORTS_ARGS y vuelva a hacer source(\"main.R\").\n")
    usage(); return(1L)
  }
  stages <- if (args$stage == "all") c("data", "figures", "verify", "compare") else args$stage
  run_id <- new_run_id()                            # fecha-hora, ms y PID: único aunque coincidan en el segundo
  cfg <- NULL; rows <- list(); notes <- .sr$locale_note
  fail <- tryCatch({
    cfg <- run_stage("inicio", {
      cfg <- load_config(ROOT, args$country)
      log_open(file.path(cfg$paths$output, "log", paste0("run_", run_id, ".log")))
      cfg
    })
    specs <- run_stage("inicio", {
      log_msg("run ", run_id, " | stage ", args$stage, " | country ", cfg$country, " | root ", ROOT,
              " | args ", if (length(argv)) paste(argv, collapse = " ") else "(ninguno)")
      if (!is.null(.sr$locale_note)) log_msg("locale: ", .sr$locale_note)
      for (a in .sr$chrome_avisos) log_msg("aviso Chrome: ", a)
      log_msg("Chrome: ", if (is.na(cfg$paths$chrome)) "no encontrado" else cfg$paths$chrome)
      check_deps(cfg)
      setup_fonts(cfg)
      specs <- read_manifest(cfg)
      if (length(attr(specs, "avisos"))) notes <- c(notes, sprintf("avisos del manifiesto: %d (ver log)", length(attr(specs, "avisos"))))
      # --figs solo se aplica si hay etapas de figuras; las que no existen se avisan y ninguna coincidencia detiene
      fig_stages <- intersect(stages, c("figures", "verify", "compare"))
      figs <- if (length(fig_stages)) args$figs else NULL
      if (!is.null(args$figs) && is.null(figs)) notes <- c(notes, "--figs no aplica a --stage data")
      unknown <- setdiff(figs, vapply(specs, function(s) as.integer(s$fig), 1L))
      if (length(unknown)) {
        notes <- c(notes, paste0("--figs sin figura en el manifiesto: ", paste(sprintf("%02d", unknown), collapse = ", ")))
        log_msg("--figs sin figura en el manifiesto: ", paste(sprintf("%02d", unknown), collapse = ", "))
      }
      specs <- lapply(specs, function(s) { s$selected <- is.null(figs) || s$fig %in% figs; s })   # no seleccionada → "omitida"
      if (length(fig_stages) && !any(vapply(specs, `[[`, logical(1), "selected"))) {
        stop("Ninguna figura del manifiesto coincide con --figs ", paste(sprintf("%02d", figs), collapse = ","), call. = FALSE)
      }
      adopt <- vapply(specs, function(s) isTRUE(s$selected) && identical(s$wjpr, "adoptada"), logical(1))
      if (length(.sr$deps_optional_missing) && any(adopt)) {
        notes <- c(notes, paste0("falta el paquete opcional ", paste(.sr$deps_optional_missing, collapse = ", "),
                                 " (figuras con wjpr = adoptada)"))
      }
      specs
    })
    ctx <- NULL
    if ("data" %in% stages) {
      res <- run_stage("data", load_ctx(cfg, force = args$force, build = TRUE))
      ctx <- res$ctx; rows$data <- res$table; notes <- c(notes, res$note)
    }
    if ("figures" %in% stages) {
      if (is.null(ctx)) {     # decisión 7: notas ('sin comprobar', comprobaciones) y filas data también en figures
        res <- run_stage("figures", load_ctx(cfg, build = TRUE))
        ctx <- res$ctx; rows$data <- res$table; notes <- c(notes, res$note)
      }
      if (!args$no_png && !is.na(cfg$paths$chrome)) {   # qué binario rasteriza los PNG (R5-11)
        v <- chrome_version(cfg$paths$chrome)
        log_msg("Chrome --version: ", if (is.na(v)) "sin respuesta" else v)
      }
      rows$figures <- run_stage("figures", run_figures(specs, ctx, cfg, run_id, png = !args$no_png))
      notes <- c(notes, if (args$no_png) "PNG omitidos (--no-png)" else if (is.na(cfg$paths$chrome)) "Chrome no encontrado: PNG omitidos" else NULL)
    }
    if ("verify" %in% stages) rows$verify <- run_stage("verify", run_verify(specs, cfg))
    if ("compare" %in% stages) rows$compare <- run_stage("compare", run_compare(specs, cfg))
    NULL
  }, error = function(e) e)

  ## Consola: cabecera, tabla y cierre (o el error que detuvo la corrida)
  if (!is.null(cfg)) print_header(cfg, args$stage, run_id, notes)
  print_rows(rows)
  if (!is.null(fail)) {
    stage <- if (inherits(fail, "sr_fatal")) fail$stage else "inicio"
    if (!inherits(fail, "sr_fatal")) log_msg("error: ", conditionMessage(fail))
    cat("Error en la etapa ", stage, ": ", first_line(conditionMessage(fail), 400), "\n",
        "Detalle: ", if (nzchar(log_path())) log_path() else "sin log (el error ocurrió antes de abrirlo)", "\n", sep = "")
    log_close()
    return(1L)
  }
  cat(closing_line(rows), "\n", sep = "")
  log_msg("cierre: ", closing_line(rows, log = NULL))
  log_close()
  bad <- function(t, ok) if (is.null(t)) 0L else sum(!t$estado %in% ok)
  n_bad <- bad(rows$figures, c("ok", "omitida")) + bad(rows$verify, c("ok", "omitida")) +
           sum(rows$compare$estado %in% "error")
  if (args$strict && n_bad > 0) 1L else 0L
}

argv <- commandArgs(trailingOnly = TRUE)
if (length(argv) == 0 && nzchar(trimws(Sys.getenv("SRC_REPORTS_ARGS")))) {
  argv <- strsplit(trimws(Sys.getenv("SRC_REPORTS_ARGS")), "[[:space:]]+")[[1]]
}
status <- main(argv)
# quit() solo con Rscript main.R (nivel superior y no interactivo): con source() la sesión sigue (R4-21, R5-10)
if (sys.nframe() == 0L && !interactive()) quit(save = "no", status = status) else invisible(status)

## Prueba ----------------------------------------------------------------------------------------------------------
## Regresión: cd /tmp && Rscript <carpeta>/main.R --stage all --figs 09,10,11 → 3/3 verificadas, salidas idénticas
## (md5) a las de ROOT; --stage verify y --stage compare sin error. Casos de SCRATCH/fix/G1/pruebas/t/ (antes → después):
## - Sin config/local.yml ni caché, --stage data y --stage figures --figs 09: 'Ejecución interrumpida' → "Error en la
##   etapa data|figures: No se encuentra el .dta (NA). Defina path2EU …" + "Detalle: <log>", exit 1; master.rds
##   truncado → "Error en la etapa figures: readRDS(f_rds): error al leer desde conexión" (traza en el log).
## - --figs 9,99 → corre la 09 y avisa de la 99; 9.7, 0x9, abc (antes 9, 9 y aviso de coerción), --figs sin valor y
##   --country --force → "Error en los argumentos: …"; --stage repetido → 'Opción repetida' (antes ganaba la última);
##   --help --stage foo → uso, exit 0 (antes error); --stage data --figs 99 → corre data (antes 'Ninguna figura').
## - --stage figures sin .dta: cabecera con 'caché de master sin comprobar' y filas data (antes nada).
## - --stage all --figs 09,12: '12 sin salida' y '0 errores' → '12 error figures: pendiente: fig_12 sin implementar' y
##   '1/2 verificadas · 0 conocidas · 1 errores'; --stage figures --figs 12 --strict: exit 0 → 1.
## - Cierre también en figures ('1/1 figuras ok · 0 errores · log: …'), compare ('… comparadas …') y data ('log: …').
## - Rscript -e 'source("main.R")' + otra línea: la línea no corría (quit) → corre y source()$value = 0; R --interactive
##   con SRC_REPORTS_ARGS → corre esa etapa (antes 'all') y la sesión sigue; sin argumentos → uso y la sesión sigue.
## - Enlace simbólico a main.R, y copia de main.R fuera de la carpeta con SRC_REPORTS_ROOT: 'No se encuentra
##   R/00_setup.R' → corren. Dos corridas a la vez → dos logs (run_id con ms y PID).
