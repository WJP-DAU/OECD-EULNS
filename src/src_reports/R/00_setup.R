## 00_setup.R — locale, paquetes, configuración, dependencias, fuentes, manifiesto, registro y tabla de consola ------
## Origen: nuevo para src_reports (sustituye src/utils/config.R, que elegía rutas por Sys.info()["user"]).
## Carga: main.R hace source() de R/00-09 en orden. Al cargar solo se ejecutan la guarda de locale UTF-8
## (ensure_utf8, decisión 10 de la revisión), las librerías, las opciones y el entorno .sr (estado de sesión).

.sr <- new.env(parent = emptyenv())                   # estado de sesión: log, memo de estimadores, avisos de arranque

# LC_CTYPE UTF-8 obligatorio: en locale C/POSIX (cron, launchd, Docker sin LANG) yaml::read_yaml() leía los yml
# vacíos y textshaping/write_csv convertían '−', '–' y '·' en '<U+2212>'. Si no es UTF-8 se prueban los candidatos;
# si ninguno existe se detiene (mensaje en ASCII: el locale aún no es UTF-8). Devuelve la nota del cambio o NULL.
ensure_utf8 <- function(cands = c("C.UTF-8", "en_US.UTF-8")) {
  if (isTRUE(l10n_info()[["UTF-8"]])) return(invisible(NULL))
  prev <- Sys.getlocale("LC_CTYPE")
  for (l in cands) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l))) && isTRUE(l10n_info()[["UTF-8"]])) break
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    stop("src_reports necesita un locale UTF-8: LC_CTYPE es '", prev, "' y no se pudo fijar ",
         paste(cands, collapse = " ni "), ". Defina LANG=en_US.UTF-8 (p. ej. LANG=en_US.UTF-8 Rscript main.R ...).",
         call. = FALSE)
  }
  invisible(paste0("LC_CTYPE ", prev, " → ", Sys.getlocale("LC_CTYPE")))
}
.sr$locale_note <- ensure_utf8()

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr); library(stringr); library(tibble); library(ggplot2)
})
options(dplyr.summarise.inform = FALSE, readr.show_col_types = FALSE, warn = 1)
invisible(Sys.setlocale("LC_COLLATE", "C"))          # orden alfabético determinista (rows_* usan .locale = "C")
if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)   # svglite debe emitir <text>
options(wjpr.family = "inter")


## Configuración ----------------------------------------------------------------------------------------------

# cfg = config/<country>.yml + config/local.yml (rutas de la máquina) + variables de entorno; cfg$paths absolutas.
# country NULL = el único yml de config/ que no sea local*. Rutas de local.yml y SRC_REPORTS_CACHE (decisión 11):
# "~" se expande y las relativas se resuelven contra la carpeta (root), no contra el directorio de trabajo (un
# cache_dir relativo dejaba master.rds, con microdatos, fuera de la carpeta y de .gitignore, uno por cada cwd).
load_config <- function(root, country = NULL) {
  ymls <- setdiff(list.files(file.path(root, "config"), pattern = "\\.yml$"), c("local.yml", "local.yml.example"))
  hay <- paste(sub("\\.yml$", "", ymls), collapse = ", ")
  if (is.null(country)) {
    if (length(ymls) != 1) stop("Indique --country: hay ", length(ymls), " configuraciones en config/ (", hay, ")", call. = FALSE)
    country <- sub("\\.yml$", "", ymls)
  }
  file <- file.path(root, "config", paste0(country, ".yml"))
  if (!file.exists(file)) stop("No existe ", file, " (configuraciones en config/: ", hay, ")", call. = FALSE)
  cfg <- check_cfg(read_yml(file), file)
  local_file <- file.path(root, "config", "local.yml")
  loc <- if (file.exists(local_file)) read_yml(local_file) else list()
  if (!is.list(loc)) stop(local_file, " debe ser un mapa YAML (clave: valor; ver local.yml.example)", call. = FALSE)
  path2EU <- local_path(loc$path2EU, root)
  cache <- local_path(Sys.getenv("SRC_REPORTS_CACHE"), root)
  if (is.na(cache)) cache <- local_path(loc$cache_dir, root)
  if (is.na(cache)) cache <- file.path(root, "cache")
  chrome <- find_chrome(local_path(loc$chrome, root))
  .sr$chrome_avisos <- attr(chrome, "avisos")          # ruta configurada que no existe: cabecera y log (main.R)
  attr(chrome, "avisos") <- NULL
  cfg$paths <- list(
    root      = root,
    config    = file,
    dta       = local_path(cfg$data$dta, path2EU),          # relativas a path2EU: NA si no hay path2EU
    data4web  = local_path(cfg$data$data4web, path2EU),
    regions   = local_path(cfg$data$regions, root),
    eu_values = local_path(cfg$data$eu_values, root),
    fonts     = file.path(root, "fonts"),
    chrome    = chrome,
    cache     = cache,
    output    = file.path(root, "output", cfg$slug),
    reference = file.path(root, "reference", cfg$slug),
    manifest  = file.path(root, "manifest", "figures.csv"),
    manifest_country = file.path(root, "manifest", paste0("figures_", cfg$slug, ".csv")),   # opcional (decisión 5)
    deps      = file.path(root, "deps.csv"),
    published = local_path(loc$published_dir, root),          # solo tools/build_reference.R
    reference_src = local_path(loc$reference_src_dir, root)   # idem (CSV publicados)
  )
  for (d in c("figures", "tables", "compare", "log")) dir.create(file.path(cfg$paths$output, d), recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$paths$cache, recursive = TRUE, showWarnings = FALSE)
  cfg
}

# yml → lista (archivo vacío → list()). Un yml mal formado da un error con el archivo, no uno críptico más adelante.
read_yml <- function(f) {
  y <- tryCatch(yaml::read_yaml(f), error = function(e) stop("YAML inválido en ", basename(f), ": ", conditionMessage(e), call. = FALSE))
  if (is.null(y)) list() else y
}

# Claves de config/<pais>.yml que el código usa sin valor por defecto: sin ellas el fallo llegaba tarde y críptico
# (dir.create(character(0)), if (logical(0))). Las numéricas opcionales (tolerancias, anchos máximos) se validan si están,
# y cada valor esperado de checks debe ser un número ('access2info:' vacío fallaba en data_checks con 'valor ausente').
check_cfg <- function(cfg, file) {
  if (!is.list(cfg)) stop(basename(file), " debe ser un mapa YAML (clave: valor)", call. = FALSE)
  get <- function(a, b = NULL) { x <- cfg[[a]]; if (is.null(b)) x else if (is.list(x)) x[[b]] else NULL }
  txt <- function(x) is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
  num <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x > 0
  bad <- c(
    if (!txt(get("country"))) "country",
    if (!txt(get("slug")) || !grepl("^[A-Za-z0-9_-]+$", get("slug"))) "slug (letras, números, _ o -)",
    if (!txt(get("data", "dta"))) "data$dta", if (!txt(get("data", "regions"))) "data$regions",
    if (!txt(get("data", "eu_values"))) "data$eu_values",
    if (!is.null(get("data", "data4web")) && !txt(get("data", "data4web"))) "data$data4web",
    if (!num(get("layout", "dpi"))) "layout$dpi", if (!num(get("layout", "text_pt"))) "layout$text_pt",
    if (!num(get("verify", "size_tol_in"))) "verify$size_tol_in",
    if (!is.null(get("verify", "min_f1")) && !(num(get("verify", "min_f1")) && get("verify", "min_f1") <= 1)) {
      "verify$min_f1 (entre 0 y 1)"
    },
    unlist(lapply(list(c("verify", "tol_pp"), c("verify", "tol_frac"), c("layout", "max_w_callout_in"), c("layout", "max_w_full_in")),
                  function(k) if (!is.null(get(k[1], k[2])) && !num(get(k[1], k[2]))) paste0(k[1], "$", k[2]))),
    if (!is.null(cfg$checks)) check_leaves(cfg$checks, "checks")
  )
  if (length(bad) > 0) stop(basename(file), ": faltan o no son válidas las claves ", paste(bad, collapse = ", "), call. = FALSE)
  cfg
}

# Rutas (a$b$c) de las hojas de una lista del yml que no son un único número
check_leaves <- function(x, path) {
  if (is.list(x)) return(unlist(lapply(names(x), function(n) check_leaves(x[[n]], paste0(path, "$", n)))))
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) NULL else path
}

# Ruta de configuración → absoluta: NULL/""/NA → NA; "~" expandida; absoluta tal cual; relativa → bajo base (NA si
# base es NA, p. ej. data$dta sin path2EU)
local_path <- function(x, base) {
  if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) return(NA_character_)
  x <- path.expand(as.character(x))
  if (grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", x)) x else if (is.na(base)) NA_character_ else file.path(base, x)
}

# Chrome: CHROMOTE_CHROME > local.yml > candidatos fijos > PATH. NA si no hay (los PNG se omiten con aviso). Una ruta
# configurada (entorno o local.yml) que no existe o no es un archivo se descarta con aviso en attr(, "avisos") (antes
# se ignoraba en silencio) y "~" se expande (system2 no la expande). main.R registra en el log la ruta y su --version.
find_chrome <- function(local = NULL) {
  ok <- function(p) !is.na(p) && nzchar(p) && file.exists(p) && !dir.exists(p)
  conf <- c("CHROMOTE_CHROME" = path.expand(Sys.getenv("CHROMOTE_CHROME")),
            "config/local.yml" = if (length(local) == 1 && !is.na(local)) path.expand(local) else "")
  conf <- conf[nzchar(conf)]
  avisos <- unname(vapply(names(conf), function(k) if (ok(conf[[k]])) "" else paste0(k, " no existe o no es un archivo: ", conf[[k]]), ""))
  cand <- c(unname(conf),
            "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
            "/Applications/Google Chrome .app/Contents/MacOS/Google Chrome",
            "/opt/homebrew/bin/chromium", "/usr/bin/google-chrome", "/usr/bin/chromium", "/usr/bin/chromium-browser",
            unname(Sys.which(c("google-chrome", "chromium", "chromium-browser", "chrome-headless-shell"))))
  cand <- cand[vapply(cand, ok, logical(1))]
  out <- if (length(cand) == 0) NA_character_ else unname(cand[1])
  if (any(nzchar(avisos))) attr(out, "avisos") <- avisos[nzchar(avisos)]
  out
}

# Primera línea de '<chrome> --version' (qué binario rasteriza los PNG; main.R la registra en el log), NA si no
# responde en 15 s. Con exec no queda un sh intermedio que escriba en la consola si Chrome muere por una señal.
chrome_version <- function(chrome) {
  if (is.na(chrome)) return(NA_character_)
  v <- tryCatch(suppressWarnings(system2("exec", c(shQuote(chrome), "--version"), stdout = TRUE, stderr = FALSE, timeout = 15)),
                error = function(e) character())
  v <- trimws(v[nzchar(trimws(v))])
  if (length(v) == 0) NA_character_ else v[1]
}


## Dependencias y fuentes ---------------------------------------------------------------------------------------

# deps.csv: paquete, version, origen, modo (exacta | minima | opcional), uso. Falta de 'minima' detiene; desajuste de
# 'exacta' avisa (cabecera y log); 'opcional' se registra en el log con su versión o "ausente", sin cargarlo (los
# ausentes quedan en .sr$deps_optional_missing y en el log). En 'exacta', si origen fija un commit
# ("github worldjusticeproject-org/WJPr@382aee9") se compara también la build instalada (deps_exact_mismatch): dos
# builds de WJPr con la misma versión 1.1.0 dibujan distinto (la del 13-jul y la rama main). Que una figura con
# metodo wjpr no se pueda dibujar sin WJPr lo comprueba check_method_deps() en la etapa figures.
check_deps <- function(cfg) {
  deps <- read_csv(cfg$paths$deps, col_types = cols(.default = col_character()), na = character())
  ver <- function(p, modo) {
    if (modo == "opcional") return(tryCatch(as.character(packageVersion(p)), error = function(e) NA_character_))
    if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else NA_character_
  }
  deps$instalada <- unname(mapply(ver, deps$paquete, deps$modo))
  missing <- deps %>% filter(modo == "minima" & (is.na(instalada) | package_version(instalada) < package_version(version)))
  if (nrow(missing) > 0) {
    stop("Faltan paquetes (o versión menor a la mínima): ",
         paste0(missing$paquete, " >= ", missing$version, collapse = ", "),
         "\nInstale con: install.packages(c(", paste0("'", missing$paquete, "'", collapse = ", "), "))", call. = FALSE)
  }
  exact <- deps_exact_mismatch(deps)
  same_v <- !is.na(exact$instalada) & sub("@.*$", "", exact$instalada) == sub("@.*$", "", exact$esperada)
  .sr$deps_warning <- if (nrow(exact) > 0) {
    paste0(exact$paquete, " ", coalesce(exact$instalada, "ausente"), " (esperada ", exact$esperada,
           ifelse(same_v, ": otra build con la misma versión", ""), ")", collapse = ", ")
  } else NULL
  if (!is.null(.sr$deps_warning)) log_msg("deps.csv: versiones distintas de las exactas: ", .sr$deps_warning)
  opt <- deps %>% filter(modo == "opcional")
  for (i in seq_len(nrow(opt))) {
    log_msg("deps.csv opcional: ", opt$paquete[i], " ", coalesce(opt$instalada[i], "ausente"), " (esperada ", opt$version[i], ")",
            if (nzchar(opt$uso[i] %||% "")) paste0(" · ", opt$uso[i]) else "")
  }
  .sr$deps_optional_missing <- opt$paquete[is.na(opt$instalada)]
  invisible(deps)
}

# Commit que fija la columna origen ("github worldjusticeproject-org/WJPr@382aee9" → "382aee9"); NA si no trae "@sha"
dep_sha_expected <- function(origen) {
  m <- regmatches(origen, regexec("@([0-9a-fA-F]{7,40})([^0-9a-fA-F]|$)", origen))
  vapply(m, function(x) if (length(x) >= 2) tolower(x[2]) else NA_character_, "")
}

# Commit de la build cargada o instalada: RemoteSha (remotes, pak) o GithubSHA1 (devtools antiguo); "local" si se
# instaló desde un clon o un tarball (R CMD INSTALL no lo registra); NA si el paquete no está
dep_sha_installed <- function(p) {
  d <- suppressWarnings(utils::packageDescription(p))
  if (!inherits(d, "packageDescription")) return(NA_character_)
  tolower(d$RemoteSha %||% d$GithubSHA1 %||% "local")
}

# Paquetes 'exacta' cuya build instalada difiere de la de deps.csv: tibble(paquete, esperada, instalada), con
# "versión@commit" cuando origen fija el commit (instalada NA si falta). La usan check_deps() y verify (campo entorno).
deps_exact_mismatch <- function(deps) {
  ex <- deps[deps$modo == "exacta", , drop = FALSE]
  sha <- dep_sha_expected(ex$origen)
  v <- unname(vapply(ex$paquete, function(p) {
    if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else NA_character_
  }, ""))
  got_sha <- unname(vapply(ex$paquete, dep_sha_installed, ""))
  esperada <- ifelse(is.na(sha), ex$version, paste0(ex$version, "@", sha))
  instalada <- ifelse(is.na(v), NA_character_, ifelse(is.na(sha), v, paste0(v, "@", substr(got_sha, 1, nchar(sha)))))
  tibble(paquete = ex$paquete, esperada = esperada, instalada = instalada) %>% filter(is.na(instalada) | instalada != esperada)
}

# Paquetes sin los que un metodo de dibujo no funciona, aunque deps.csv los declare 'exacta' (que solo avisa): las
# figuras con metodo wjpr se dibujan con WJPr, y wjp_groupbars y wjp_edgebars usan ggtext (geom_richtext).
METHOD_DEPS <- list(wjpr = c("WJPr", "ggtext"))

# Si alguna figura seleccionada tiene un metodo de METHOD_DEPS y falta uno de sus paquetes, error legible (la etapa
# figures se detiene antes de dibujar, en vez de un "there is no package called 'WJPr'" por figura) con cómo instalarlo
# según origen de deps.csv. La llama run_figures() (06_export.R), que conoce la selección de --figs.
check_method_deps <- function(specs, cfg) {
  sel <- Filter(function(s) isTRUE(s$selected %||% TRUE), specs)
  for (m in names(METHOD_DEPS)) {
    figs <- vapply(Filter(function(s) identical(s$metodo, m), sel), function(s) sprintf("%02d", as.integer(s$fig)), "")
    miss <- METHOD_DEPS[[m]][!vapply(METHOD_DEPS[[m]], requireNamespace, logical(1), quietly = TRUE)]
    if (length(figs) == 0 || length(miss) == 0) next
    deps <- read_csv(cfg$paths$deps, col_types = cols(.default = col_character()), na = character())
    hint <- vapply(miss, function(p) {
      o <- deps$origen[match(p, deps$paquete)]
      if (!is.na(o) && grepl("^github ", o)) sprintf("remotes::install_github(\"%s\")", sub("^github ", "", o))
      else sprintf("install.packages(\"%s\")", p)
    }, "")
    stop("Las figuras ", paste(figs, collapse = ", "), " (metodo ", m, ") necesitan ", paste(miss, collapse = " y "), ", que no ",
         if (length(miss) > 1) "están instalados" else "está instalado", " en ", paste(.libPaths(), collapse = " ni en "),
         ". Instale con ", paste(hint, collapse = " y "), ", defina SRC_REPORTS_LIB con una librería que ",
         if (length(miss) > 1) "los" else "lo", " tenga o use el respaldo (metodo svg_r, chart_<tipo>) en el manifiesto",
         call. = FALSE)
  }
  invisible(TRUE)
}

# Las 8 TTF estáticas de Inter Tight viven en fonts/ (OFL). Mismo registro que src/hungary/setup.R:200-206.
# Cada archivo se lee (md5): un .ttf de OneDrive sin descargar (dataless) informa su tamaño lógico, así que el tamaño
# no basta, y si la lectura falla (sin conexión, sin permisos) md5sum da NA: error legible en vez de 'valor ausente
# donde TRUE/FALSE es necesario'.
setup_fonts <- function(cfg) {
  FONT_DIR <<- cfg$paths$fonts
  needed <- c("Regular", "Italic", "Medium", "MediumItalic", "SemiBold", "SemiBoldItalic", "Bold", "BoldItalic")
  files <- file.path(FONT_DIR, paste0("InterTight-", needed, ".ttf"))
  md5 <- setNames(unname(tools::md5sum(files)), basename(files))
  size <- file.size(files)
  bad <- basename(files)[is.na(md5) | is.na(size) | size == 0]
  if (length(bad) > 0) {
    stop("Fuentes de fonts/ que faltan, pesan 0 bytes o no se pudieron leer (¿OneDrive sin descargar o sin conexión?): ",
         paste(bad, collapse = ", "), call. = FALSE)
  }
  md5_file <- file.path(FONT_DIR, "md5.txt")
  if (file.exists(md5_file)) {
    rec <- tryCatch(read.table(md5_file, col.names = c("md5", "archivo"), stringsAsFactors = FALSE),
                    error = function(e) { log_msg("fonts/md5.txt ilegible: ", conditionMessage(e)); NULL })
    if (!is.null(rec)) {
      now <- unname(md5[match(rec$archivo, names(md5))])
      other <- is.na(match(rec$archivo, names(md5)))
      if (any(other)) now[other] <- unname(tools::md5sum(file.path(FONT_DIR, rec$archivo[other])))
      dif <- rec$archivo[is.na(now) | now != rec$md5]
      if (length(dif) > 0) log_msg("fonts/: md5 distinto de fonts/md5.txt (o ilegible) en ", paste(dif, collapse = ", "))
    }
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
# spec$country = cfg$country (decisión 3): plot(df, spec) no recibe ctx y las leyendas {country} | EU Average de
# chart_paired/chart_columns lo leen de spec. Se aceptan columnas extra (p. ej. eu_id, decisión 4). Manifiesto por país
# opcional (decisión 5): manifest/figures_<slug>.csv, ver manifest_override(). w_in se compara con layout$max_w_* (avisos
# en el log y en attr(, "avisos"); revisión R7-15).
read_manifest <- function(cfg) {
  m <- read_csv(cfg$paths$manifest, col_types = cols(.default = col_character()), na = character())
  need <- c("fig", "slug", "titulo", "seccion", "pagina", "tipo", "metodo", "funcion", "w_in", "h_in", "callout",
            "label_col", "base_pt", "fuente_datos", "metodo_ue", "codigo_origen", "wjpr", "status", "verificada", "nota")
  miss <- setdiff(need, names(m))
  if (length(miss) > 0) stop("manifest/figures.csv sin columnas: ", paste(miss, collapse = ", "), call. = FALSE)
  f_pais <- cfg$paths$manifest_country %||% file.path(dirname(cfg$paths$manifest), paste0("figures_", cfg$slug, ".csv"))
  if (file.exists(f_pais)) m <- manifest_override(m, f_pais)
  m <- m %>% mutate(fig = suppressWarnings(as.integer(fig)), pagina = as.integer(pagina), w_in = as.numeric(w_in), h_in = as.numeric(h_in),
                    callout = tolower(callout) == "true", base_pt = suppressWarnings(as.numeric(base_pt)),
                    titulo = str_replace_all(titulo, fixed("{country}"), cfg$country), country = cfg$country)
  if (anyNA(m$fig)) stop("manifest/figures.csv: fig no numérico en ", sum(is.na(m$fig)), " fila(s)", call. = FALSE)
  if (anyDuplicated(m$fig)) stop("manifest/figures.csv: fig repetido", call. = FALSE)
  avisos <- manifest_width_check(m, cfg$layout)
  for (a in avisos) log_msg("aviso manifiesto: ", a)
  specs <- lapply(seq_len(nrow(m)), function(i) as.list(m[i, ]))
  attr(specs, "avisos") <- avisos
  specs
}

# manifest/figures_<slug>.csv (decisión 5): columna fig y cualquier columna del manifiesto a sobrescribir (h_in, nota,
# ...). Una celda vacía conserva el valor común; columnas o figuras que no están en el manifiesto → error (erratas).
manifest_override <- function(m, f) {
  o <- read_csv(f, col_types = cols(.default = col_character()), na = character())
  if (!"fig" %in% names(o)) stop(basename(f), " necesita la columna fig", call. = FALSE)
  extra <- setdiff(names(o), names(m))
  if (length(extra) > 0) stop(basename(f), ": columnas que no están en manifest/figures.csv: ", paste(extra, collapse = ", "), call. = FALSE)
  key <- suppressWarnings(as.integer(o$fig))
  if (anyNA(key) || anyDuplicated(key)) stop(basename(f), ": fig vacío, no numérico o repetido", call. = FALSE)
  idx <- match(key, suppressWarnings(as.integer(m$fig)))
  if (anyNA(idx)) stop(basename(f), ": figuras que no están en el manifiesto: ", paste(o$fig[is.na(idx)], collapse = ", "), call. = FALSE)
  cols <- setdiff(names(o), "fig")
  for (col in cols) {
    use <- nzchar(trimws(o[[col]]))
    m[[col]][idx[use]] <- o[[col]][use]
  }
  log_msg("manifiesto del país: ", basename(f), " (", nrow(o), " figuras; columnas ", paste(cols, collapse = ", "), ")")
  m
}

# w_in contra los anchos máximos del yml: con recuadro (callout) ≤ max_w_callout_in, a página completa ≤
# max_w_full_in; w_in ausente o no positivo también se informa. Devuelve los avisos (texto), vacío si todo cabe.
manifest_width_check <- function(m, layout) {
  over <- function(rows, max, nombre) {
    if (!is.numeric(max) || length(max) != 1) return(character())
    b <- rows & !is.na(m$w_in) & m$w_in > max + 1e-9
    sprintf("fig %02d: w_in %s in > %s %s in", m$fig[b], format(m$w_in[b]), nombre, format(max))
  }
  bad <- is.na(m$w_in) | m$w_in <= 0
  c(sprintf("fig %02d: w_in ausente o no positivo", m$fig[bad]),
    over(m$callout, layout$max_w_callout_in, "max_w_callout_in (con recuadro)"),
    over(!m$callout, layout$max_w_full_in, "max_w_full_in (página completa)"))
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

# Identificador de corrida: fecha-hora, milisegundos y PID. Con resolución de 1 s, dos corridas en el mismo segundo
# compartían run_<id>.log (log_open lo trunca: se perdía la cabecera de la primera) y run_id en figures_status.csv.
new_run_id <- function(t = Sys.time()) {
  sprintf("%s-%03d-%d", format(t, "%Y%m%d-%H%M%S"), as.integer(floor(as.numeric(t) %% 1 * 1000)), Sys.getpid())
}

# Primera línea de un mensaje de error, recortada, para la columna verificacion (la usan 06, 07 y 08)
first_line <- function(msg, n = 60) substr(trimws(strsplit(paste0(msg, "\n"), "\n", fixed = TRUE)[[1]][1]), 1, n)

# Traza compacta (funciones de sys.calls(), sin la maquinaria de tryCatch/with_log) para el log de un error fatal
call_trace <- function(calls) {
  skip <- c("tryCatch", "tryCatchList", "tryCatchOne", "doTryCatch", "withCallingHandlers", "with_log", ".handleSimpleError",
            "h", "stop", "eval", "withVisible", "source", "sys.source")
  fn <- vapply(calls, function(cl) { f <- cl[[1]]; if (is.name(f)) as.character(f) else paste(deparse(f, nlines = 1L), collapse = "") }, "")
  substr(paste(fn[!fn %in% skip], collapse = " → "), 1, 400)
}

# Evalúa expr con la salida y los mensajes desviados a un temporal local, que se anexa al log con el prefijo " | ".
# Reentrante: dentro de otra captura evalúa expr sin sinks nuevos (sink(type = "message") no es una pila: el interno
# devolvía los mensajes del externo a la consola) y al salir restaura la conexión de mensajes previa. Ante un error
# (decisión 8) restaura los sinks, registra el mensaje y la traza en el log y lo relanza con e$en_log = TRUE: antes
# el 'Error: …' de R se escribía en el temporal y en consola solo quedaba 'Ejecución interrumpida'.
with_log <- function(expr) {
  if (is.null(.sr$log) || isTRUE(.sr$capturing)) return(expr)
  tmp <- tempfile("salida_", fileext = ".log"); con <- file(tmp, open = "wt")
  n_out <- sink.number(); msg_prev <- sink.number(type = "message")
  sink(con, type = "output"); sink(con, type = "message"); .sr$capturing <- TRUE
  restore <- function() {
    if (!isTRUE(.sr$capturing)) return(invisible(NULL))
    .sr$capturing <- FALSE
    if (msg_prev == 2L) sink(type = "message") else sink(getConnection(msg_prev), type = "message")
    while (sink.number() > n_out) sink()
    close(con)
    out <- tryCatch(readLines(tmp, warn = FALSE), error = function(e) character())
    if (length(out) > 0) try(cat(paste0("         | ", out), file = .sr$log, sep = "\n", append = TRUE), silent = TRUE)
    unlink(tmp)
    invisible(NULL)
  }
  on.exit(restore(), add = TRUE)
  calls <- NULL
  tryCatch(
    withCallingHandlers(expr, error = function(e) calls <<- sys.calls()),
    error = function(e) {
      restore()
      log_msg("error: ", conditionMessage(e))
      tr <- if (length(calls) > 0) call_trace(calls) else ""
      if (nzchar(tr)) log_msg("traza: ", tr)
      e$en_log <- TRUE
      stop(e)
    })
}

print_header <- function(cfg, stage, run_id, notes = NULL) {
  cat(sprintf("%s | stage %s | %s | lib %s | renv %s | Chrome %s | locale %s/%s%s\n",
              cfg$country, stage, run_id, .libPaths()[1], if (nzchar(Sys.getenv("RENV_PROJECT"))) "sí" else "no",
              if (is.na(cfg$paths$chrome)) "no" else "sí", Sys.getlocale("LC_COLLATE"), Sys.getlocale("LC_CTYPE"),
              if (length(notes)) paste0(" | ", paste(notes, collapse = " | ")) else ""))
  if (!is.null(.sr$deps_warning)) cat("aviso deps.csv:", .sr$deps_warning, "\n")
  if (length(.sr$chrome_avisos)) cat("aviso Chrome:", paste(.sr$chrome_avisos, collapse = "; "), "\n")
}

# Filas de la tabla: tibble(fig, metodo, estado, verificacion, archivo). Se imprime la tabla más completa
# disponible (verify > figures > compare), precedida por las filas de la etapa data. Columnas alineadas por
# caracteres (formatC; sprintf rellenaba por bytes y 'caché', 'tamaño', '·', '×' las desplazaban) y verificacion tan
# ancha como su texto más largo, hasta 70 (antes se cortaba a 44: la 'ok' de figures perdía '<text>').
# Una figura con estado error en figures de esta corrida se muestra con ese error ("figures: <motivo>", decisión 2)
# aunque verify diga otra cosa: su salida se borró al empezar y verify la daba 'sin salida' sin motivo (R5-07).
# corregido (revisión 4): una figura que figures dejó en texto, dibujo, tamaño o png no sale "ok" (ni "sin dibujo"):
# se muestra "fallo" con "figures: <motivo>" (verify_figure ya lo hace con figures_status.csv; esto lo asegura en la
# consola para la corrida actual) y --strict la cuenta (main.R: verify no ok)
print_rows <- function(rows) {
  tbl <- rows$verify %||% rows$figures %||% rows$compare
  if (!is.null(rows$verify) && !is.null(rows$figures)) {
    f <- rows$figures[rows$figures$estado == "error", , drop = FALSE]
    i <- which(tbl$fig %in% f$fig & !tbl$estado %in% c("error", "omitida"))
    j <- match(tbl$fig[i], f$fig)
    tbl$estado[i] <- "error"; tbl$verificacion[i] <- paste0("figures: ", f$verificacion[j]); tbl$archivo[i] <- f$archivo[j]
    f <- rows$figures[rows$figures$estado %in% c("texto", "dibujo", "tamaño", "png"), , drop = FALSE]
    i <- which(tbl$fig %in% f$fig & tbl$estado %in% c("ok", "sin dibujo"))
    j <- match(tbl$fig[i], f$fig)
    tbl$estado[i] <- "fallo"; tbl$verificacion[i] <- paste0("figures: ", f$verificacion[j])
  }
  all <- bind_rows(rows$data, tbl)
  if (is.null(all) || nrow(all) == 0) return(invisible(NULL))
  cell <- function(x, w) {
    x <- coalesce(as.character(x), "")
    formatC(ifelse(nchar(x) > w, paste0(substr(x, 1, w - 1), "…"), x), width = -w)
  }
  w_v <- min(70L, max(nchar("verificacion"), nchar(coalesce(all$verificacion, ""))))
  cat(paste(cell("fig", 4), cell("metodo", 7), cell("estado", 15), cell("verificacion", w_v), "archivo"), sep = "\n")
  cat(paste(cell(all$fig, 4), cell(all$metodo, 7), cell(all$estado, 15), cell(all$verificacion, w_v), coalesce(all$archivo, "")),
      sep = "\n")
  invisible(all)
}

# Línea de cierre (decisión 9): con verify, "N/M verificadas · k conocidas · e errores [· s sin dibujo] · log: <ruta>", donde e = figuras
# con estado error en figures o en verify (cada figura una vez); sin verify, el recuento de figures o de compare; la
# ruta del log siempre (antes solo se imprimía si la corrida incluía verify). log = NULL: sin la ruta (para el log).
closing_line <- function(rows, log = log_path()) {
  sel <- function(t) if (is.null(t)) NULL else t[t$estado != "omitida", , drop = FALSE]
  f <- sel(rows$figures); v <- sel(rows$verify); cmp <- sel(rows$compare)
  n_err <- function(...) length(unique(unlist(lapply(list(...), function(t) t$fig[t$estado == "error"]))))
  head <- if (!is.null(v)) {
    # corregido (V3-03): nombra las figuras wjpr sin PNG que comparar («sin dibujo»); antes solo restaban de N/M
    n_sd <- sum(v$estado == "sin dibujo")
    paste0(sprintf("%d/%d verificadas · %d conocidas · %d errores", sum(v$estado == "ok"), nrow(v), sum(grepl("conocida", v$verificacion)), n_err(f, v)),
           if (n_sd > 0) sprintf(" · %d sin dibujo", n_sd) else "")
  } else if (!is.null(f)) {
    sprintf("%d/%d figuras ok · %d errores", sum(f$estado == "ok"), nrow(f), n_err(f))
  } else if (!is.null(cmp)) {
    sprintf("%d/%d comparadas · %d errores", sum(cmp$estado == "ok"), nrow(cmp), n_err(cmp))
  }
  paste(c(head, if (!is.null(log)) paste0("log: ", log)), collapse = " · ")
}


## Prueba ----------------------------------------------------------------------------------------------------------
## bash SCRATCH/fix/G1/pruebas/t/run_all.sh (mkroot.sh arma raíces ligeras: before = main.R y 00_setup.R originales,
## after = estos; resumen en pruebas/out/00_resumen.txt). Antes → después:
## - Locale C (env -u LANG -u LC_ALL -u LC_CTYPE): 'entrada inválida … hungary.yml' + 'argumento path inválido', exit 1 →
##   nota 'LC_CTYPE C → C.UTF-8' y --stage all --figs 09,10,11 3/3 con tablas, drawn, SVG, PNG y 00-verificacion.csv
##   idénticos (md5) a los de UTF-8; ensure_utf8("xx_XX.UTF-8") en C → error legible.
## - local_path: '~/x' expandida, 'rel/a' → <raíz>/rel/a, '/abs' tal cual, ''/NULL → NA; cache_dir 'cache_local' y
##   SRC_REPORTS_CACHE relativos → bajo la raíz (con dos cwd: dos carpetas cache_local, una por cwd → una).
## - check_cfg/read_yml: yml sin slug, vacío o mal formado, local.yml escalar y 'access2info:' vacío en checks → "hungary.yml:
##   faltan o no son válidas las claves …", "YAML inválido en hungary.yml: …" (antes dir.create, yaml.load, '$ operator
##   is invalid' o 'valor ausente' en data_checks).
## - find_chrome: CHROMOTE_CHROME y chrome de local.yml inexistentes → dos avisos (cabecera y log; antes ninguno); el
##   log registra la ruta elegida y 'Chrome --version: Google Chrome 155.0.8059.86'.
## - check_deps: 'deps.csv opcional: WJPr 1.1.0 (esperada 1.1.0)' y, con una fila WJPrFalso, '… ausente' en el log
##   (antes 0 líneas); nota en cabecera si una figura seleccionada tiene wjpr = adoptada.
## - setup_fonts: .ttf con chmod 000 → 'Fuentes de fonts/ que … no se pudieron leer …: InterTight-Medium.ttf' (antes
##   'valor ausente donde TRUE/FALSE es necesario'); de 0 bytes → el mismo mensaje; un byte añadido → aviso md5 en el log.
## - read_manifest: spec$country = "Hungary" (antes NULL); figures_hungary.csv sobrescribe h_in, nota y w_in (celda
##   vacía conserva el valor); columna o figura ajenas, sin fig o fig repetido → error con el archivo; w_in 7.0 con
##   recuadro → 'fig 09: w_in 7 in > max_w_callout_in (con recuadro) 6.5 in' (log; "avisos del manifiesto: 1" en cabecera).
## - with_log anidado: el mensaje del externo tras el interno va al log (antes a consola); error → sinks restaurados,
##   'error: …' (y la traza si la hay) en el log y relanzado con en_log = TRUE.
## - new_run_id: verify y compare lanzados a la vez → dos logs con su cabecera (antes uno, con la cabecera truncada).
## - print_rows: ancho antes de 'archivo' 82 en todas las líneas con 'caché', 'tamaño', '·', '×' (antes 74/70/74/68) y
##   la 'ok' de figures entera ('98 <text>'); figura en error en figures → "error · figures: pendiente: fig_12 sin
##   implementar" en vez de 'sin salida'. closing_line: ver la prueba de main.R.
## Adopción de WJPr (2026-10-23): cd /tmp && Rscript SCRATCH/adopt_M3/pruebas/t_adopcion.R <copia> → check_method_deps:
##   sin error con WJPr y ggtext, con la figura no seleccionada o con metodo svg_r; con un paquete ausente "Las figuras
##   09 (metodo wjpr) necesitan …, que no está instalado en <librerías>. Instale con remotes::install_github("…@382aee9")
##   …" (install.packages si origen no es github; plural con dos). check_cfg: verify$min_f1 2, "a" o 0 → "faltan o no son
##   válidas las claves verify$min_f1 (entre 0 y 1)"; ausente → sin error. deps_exact_mismatch: deps.csv tal cual → sin
##   diferencias; origen @deadbee → "WJPr 1.1.0@382aee9 ≠ 1.1.0@deadbee"; exacta ausente → instalada NA; origen sin
##   commit → solo la versión. main.R con SRC_REPORTS_LIB de la rama main (instalada desde un clon) → cabecera "aviso
##   deps.csv: WJPr 1.1.0@local (esperada 1.1.0@382aee9: otra build con la misma versión)".
