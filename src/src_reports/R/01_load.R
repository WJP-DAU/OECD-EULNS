## 01_load.R — lectura del .dta, master de 27 países, contexto del país (ctx) y caché ---------------------------
## Origen: src/data/data-loading.R:7-18 (lectura del .dta); src/data/worker-data.R:22-82 (study_vars y flujo
## quality check → add_special_demographics → add_a2j_vars → select); src/analysis/eu_main_indicators.R:8-60
## (los 27 países de regions.csv y non_trivial_problem_broader). El caché, ctx y las comprobaciones son nuevos.
## Caché (data/, en .gitignore): master.key = componentes de master_key_parts() (.dta, regions.csv, 01/02, paquetes, R,
## renv.lock); ctx_<slug>.key = ctx_key() (+ eu_values.csv, slug, país). Sin .dta solo se reutiliza un caché cuya clave difiera
## únicamente en la parte del .dta (nota "sin comprobar"); si difiere en otra cosa, load_ctx() se detiene. Las claves se
## escriben solo después de guardar bien el .rds (--force no borra nada antes).
## Usa de 02_variables.R: apply_a2j_quality_check(), add_special_demographics(), add_a2j_vars() (que además
## devuelve attr(, "jg") = list(noDK, keepDK) en vez de escribir data/jg_*.csv) y add_report_vars().
## Usa de 03_estimators.R solo en data_checks(): nat_est(), eu_est(), journey().

# Columnas equivalentes a data/master.csv: study_vars (worker-data.R:22-51) + AJP_/AJD_/AJR_/AJE_ (= 200: 53 + 147 AJ*)
MASTER_VARS <- c(
  "country_year_id", "country_name_ltn", "nuts_id",
  "gender", "age", "financial", "edu", "residence", "age_group", "edu_level", "marital_status", "nationality", "emp_status",
  "JSE_access2info", "JSE_access2assis", "JSE_mediation", "JSE_affordcosts", "JSE_fairoutcomes",
  "TRT_judges", "TRT_prosecutors", "TRT_pda",
  "prevalence1", "prevalence2", "vulnerability1", "vulnerability2", "vulnerability3",
  "access2info", "access2rep", "access2drm",
  "rp_time", "rp_cost", "rp_fair", "rp_outcome", "rp_quick", "rp_expensive",
  "non_trivial_problem", "selected_problem_category", "problem_status", "resolution_favor", "rp_satisfaction",
  "nproblems", "cooccurence_group",
  "hardship", "hardships_health", "hardships_emotional", "hardships_income", "hardships_drugs",
  "inside_justice_gap_nodk", "inside_justice_gap_keepdk",
  "sankey_advice_stage_1", "sankey_advice_stage_2", "sankey_drm_stage_1", "sankey_drm_stage_2"
)
MASTER_PREFIXES <- c("AJP_", "AJD_", "AJR_", "AJE_")

secs <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))
# Ruta relativa a root si p está dentro (con separador: '<root>_cache/…' es una carpeta hermana, no '<root>/…')
rel_path <- function(p, root) {
  r <- paste0(sub("/+$", "", root), "/")
  ifelse(startsWith(p, r), substring(p, nchar(r) + 1), p)
}


## Lectura ------------------------------------------------------------------------------------------------------

# origen: data-loading.R:7-18 (lectura completa; la ruta viene de config, no de Sys.info()["user"])
load_raw <- function(cfg) {
  p <- cfg$paths$dta
  if (is.na(p) || !file.exists(p)) {
    stop("No se encuentra el .dta (", p, "). Defina path2EU en config/local.yml (ver local.yml.example).", call. = FALSE)
  }
  log_msg("leyendo ", p, " (", round(file.size(p) / 1e6), " MB; si OneDrive no lo ha descargado puede tardar)")
  if (nzchar(cfg$data$dta_md5 %||% "")) {
    m <- unname(tools::md5sum(p))
    if (m != cfg$data$dta_md5) log_msg("aviso: md5 del .dta ", m, " distinto del de config (", cfg$data$dta_md5, ")")
  }
  haven::read_stata(p)
}

# regions.csv: BOM UTF-8 al inicio, nombres con espacios finales; regionpoppct no se recalcula
load_regions <- function(cfg) {
  r <- read_csv(cfg$paths$regions, show_col_types = FALSE, locale = locale(encoding = "UTF-8"))
  names(r) <- str_remove(names(r), "^﻿")
  r %>% mutate(across(where(is.character), str_trim), regionpoppct = as.numeric(regionpoppct))
}

# medias UE cacheadas de data4web (tools/build_reference.R). Sin archivo: tibble vacío y aviso en el log.
load_eu_values <- function(cfg) {
  f <- cfg$paths$eu_values
  if (!file.exists(f)) {
    log_msg("aviso: no existe ", f, " (eu_value() fallará; corra tools/build_reference.R)")
    return(tibble(id = character(), value = numeric()))
  }
  v <- read_csv(f, show_col_types = FALSE)
  if (!all(c("id", "value") %in% names(v))) stop(f, " debe tener columnas id y value", call. = FALSE)
  v %>% mutate(id = as.character(id), value = as.numeric(value))
}


## Master (27 países) --------------------------------------------------------------------------------------------

# origen: worker-data.R:57-82 y eu_main_indicators.R:31-59. Una sola lectura del .dta para todos los países de
# regions.csv (la media UE y el país salen del mismo objeto). Devuelve list(master, jg).
build_master <- function(raw, regions) {
  countries <- sort(unique(regions$country))
  base <- raw %>% filter(country_name_ltn %in% countries) %>% apply_a2j_quality_check()
  log_msg("quality check aplicado: ", nrow(base), " encuestados en ", length(countries), " países")
  a2j <- add_a2j_vars(base)
  jg <- attr(a2j, "jg")
  if (!is.list(jg) || !all(c("noDK", "keepDK") %in% names(jg))) {
    stop("add_a2j_vars() debe devolver attr(, 'jg') = list(noDK, keepDK)", call. = FALSE)
  }
  jg_cols <- jg$keepDK %>%
    select(country_year_id, starts_with("jg_inverse_"), any_of(c("a2j_score", "a2j_inverse_score"))) %>%
    rename_with(~ paste0(.x, "_keepdk"), -country_year_id)
  master <- base %>%
    left_join(add_special_demographics(base), by = "country_year_id") %>%
    left_join(a2j, by = "country_year_id") %>%
    mutate(
      non_trivial_problem_broader = case_when(          # origen: eu_main_indicators.R:49-54
        non_trivial_problem == 1 ~ "Yes",
        non_trivial_problem == 0 ~ "No",
        is.na(non_trivial_problem) ~ "No"
      )
    ) %>%
    select(any_of(MASTER_VARS), non_trivial_problem_broader, starts_with(MASTER_PREFIXES)) %>%
    left_join(jg_cols, by = "country_year_id") %>%
    haven::zap_labels() %>% haven::zap_formats() %>% haven::zap_label() %>%
    mutate(across(where(is.factor), as.character)) %>%
    add_report_vars()
  list(master = master, jg = jg)
}


## ctx ----------------------------------------------------------------------------------------------------------

# ctx = todo lo que necesita data(ctx) de una figura. master (27 países) no se guarda en ctx_<slug>.rds.
# meta$key = clave del master; meta$ctx_key = la de cache/ctx_<slug>.key (memo() y figures_status la usan).
build_ctx <- function(cfg, master, jg, regions, key) {
  national <- master %>% filter(country_name_ltn == cfg$country)
  if (nrow(national) == 0) stop("Ningún encuestado con country_name_ltn == '", cfg$country, "'", call. = FALSE)
  # corregido: guarda de módulo. Un país sin módulo LNS (prevalence2 vacío: Irlanda) no tiene figuras de problemas;
  # antes el ctx se construía y cada figura fallaba con un error críptico de WJPr, ggplot o R base, o salía «ok» llena
  # de N/A (revisión V4-06)
  if (all(is.na(national$prevalence2))) {
    stop(cfg$country, " sin módulo LNS (prevalence2 vacío en sus ", nrow(national), " encuestados): no hay figuras de ",
         "problemas", call. = FALSE)
  }
  jg_nat <- jg$keepDK %>% semi_join(national, by = "country_year_id")
  add <- setdiff(c("country_name_ltn", "nuts_id"), names(jg_nat))
  if (length(add) > 0) jg_nat <- left_join(jg_nat, select(national, country_year_id, all_of(add)), by = "country_year_id")
  deps <- lock_deps(cfg$paths$lock)
  list(
    cfg = cfg[setdiff(names(cfg), "paths")],
    country = cfg$country,
    slug = cfg$slug,
    master = master,
    national = national,
    regions = regions,
    jg = jg_nat,
    eu_values = load_eu_values(cfg),
    meta = list(
      key = key, ctx_key = ctx_key(cfg, key), built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), r = R.version.string,
      packages = setNames(coalesce(vapply(deps$paquete, pkg_installed, ""), "-"), deps$paquete),
      dta = list(path = cfg$paths$dta, sig = file_sig(cfg$paths$dta)),
      n_master = nrow(master), countries = sort(unique(master$country_name_ltn)), n_national = nrow(national),
      reference_version = cfg$report$reference_version
    )
  )
}


## Caché ---------------------------------------------------------------------------------------------------------

file_sig <- function(p) if (!is.na(p) && file.exists(p)) paste0(file.size(p), "|", as.integer(file.mtime(p))) else "ausente"
pkg_ver <- function(p) if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else "-"

# Componentes de la clave del master, con nombre: el .dta (tamaño|fecha|ruta), regions.csv, el código de carga/variables
# (md5 de 01_load.R,02_variables.R), los paquetes de lectura, R y renv.lock. Por componentes para poder decir, sin el
# .dta, qué cambió (load_ctx).
master_key_parts <- function(cfg) {
  root <- cfg$paths$root
  pk <- c("haven", "dplyr", "tidyr", "readr")
  c(
    dta = paste(file_sig(cfg$paths$dta), cfg$paths$dta, sep = "|"),
    regions = unname(tools::md5sum(cfg$paths$regions)),
    code = paste(unname(tools::md5sum(file.path(root, "R", c("01_load.R", "02_variables.R")))), collapse = ","),
    pkgs = paste(pk, vapply(pk, pkg_ver, ""), sep = "=", collapse = ","),
    R = paste(R.version$major, R.version$minor, sep = "."),
    deps = unname(tools::md5sum(cfg$paths$lock))
  )
}

# Cambia si cambian el .dta, regions.csv, el código de carga/variables, los paquetes de lectura o R. Mismo texto de
# siempre: "dta|<tamaño>|<fecha>|<ruta>|regions|<md5>|code|<md5>,<md5>|pkgs|…|R|…|deps|<md5>"
master_key <- function(cfg) { p <- master_key_parts(cfg); paste(rbind(names(p), p), collapse = "|") }

# Inversa de master_key(): componentes con nombre de una clave guardada (NULL si no tiene ese formato)
key_parts <- function(key) {
  m <- regmatches(key, regexec("^dta\\|(.*)\\|regions\\|(.*)\\|code\\|(.*)\\|pkgs\\|(.*)\\|R\\|(.*)\\|deps\\|(.*)$", key))[[1]]
  if (length(m) != 7) NULL else setNames(m[-1], c("dta", "regions", "code", "pkgs", "R", "deps"))
}

# Componentes, salvo el .dta, en que la clave guardada difiere de la actual (parts = master_key_parts(cfg)), dichos para
# el mensaje de load_ctx(); character(0) si solo cambia el .dta
key_mismatch <- function(stored, parts, cfg) {
  old <- key_parts(stored)
  if (is.null(old)) return("formato de clave en master.key")
  split <- function(x) strsplit(x, ",", fixed = TRUE)[[1]]
  dif <- setdiff(names(parts)[!mapply(identical, unname(old[names(parts)]), unname(parts))], "dta")
  unname(vapply(dif, function(k) switch(k,
    regions = cfg$data$regions %||% "regions.csv",
    code = {
      a <- split(old[["code"]]); b <- split(parts[["code"]]); f <- c("R/01_load.R", "R/02_variables.R")
      paste(if (length(a) == length(b)) f[a != b] else f, collapse = " y ")
    },
    pkgs = paste0("paquete ", paste(setdiff(split(old[["pkgs"]]), split(parts[["pkgs"]])), collapse = ", "),
                  " (ahora ", paste(setdiff(split(parts[["pkgs"]]), split(old[["pkgs"]])), collapse = ", "), ")"),
    R = paste0("R ", old[["R"]], " (ahora ", parts[["R"]], ")"),
    deps = "renv.lock"
  ), ""))
}

ctx_key <- function(cfg, mkey) {
  eu <- if (file.exists(cfg$paths$eu_values)) unname(tools::md5sum(cfg$paths$eu_values)) else "ausente"
  paste(mkey, "eu", eu, "slug", cfg$slug, "country", cfg$country, sep = "|")
}

# Primera línea del archivo de clave; "" si no existe o está vacío
read_key <- function(f) { k <- if (file.exists(f)) readLines(f, warn = FALSE)[1] else ""; if (is.na(k)) "" else k }

# saveRDS a un temporal de la misma carpeta y rename: si algo falla, el .rds anterior (y su clave) quedan como estaban
save_rds <- function(obj, f) {
  tmp <- paste0(f, ".tmp"); on.exit(unlink(tmp))
  saveRDS(obj, tmp)
  if (!file.rename(tmp, f)) stop("No se pudo escribir ", f, call. = FALSE)
  invisible(f)
}

# Devuelve list(ctx, table, note). force = reconstruir (necesita el .dta; no borra nada: las claves se reescriben tras
# guardar el .rds nuevo); build = FALSE no lee el .dta (error si no hay caché). Las notas van también al log.
load_ctx <- function(cfg, force = FALSE, build = TRUE) {
  cache <- cfg$paths$cache
  f_rds <- file.path(cache, "master.rds"); f_key <- file.path(cache, "master.key")
  f_ctx <- file.path(cache, paste0("ctx_", cfg$slug, ".rds")); f_ckey <- file.path(cache, paste0("ctx_", cfg$slug, ".key"))
  notes <- character()
  note <- function(x) { notes <<- c(notes, x); log_msg("aviso: ", x) }   # main.R las muestra en la consola

  regions <- load_regions(cfg)
  dta_ok <- !is.na(cfg$paths$dta) && file.exists(cfg$paths$dta)
  sin_dta <- if (is.na(cfg$paths$dta)) "path2EU sin definir en config/local.yml" else paste("no se encuentra", cfg$paths$dta)
  # antes --force borraba las claves y, sin .dta, dejaba inservible un caché intacto: ahora se detiene sin tocar nada
  if (force && !dta_ok) stop("--force necesita el .dta (", sin_dta, "); el caché no se ha tocado", call. = FALSE)
  parts <- master_key_parts(cfg)
  key <- master_key(cfg)
  stored <- read_key(f_key)
  reuse <- !force && file.exists(f_rds) && identical(stored, key)
  if (!force && !reuse && !dta_ok && file.exists(f_rds)) {
    # sin .dta solo se acepta una clave que difiera en la parte del .dta: el resto (código, regions, deps, paquetes, R)
    # sí se puede comprobar, y reutilizarlo daría figuras de un master desactualizado sin aviso
    if (!nzchar(stored)) {
      stop("Falta ", f_key, ": sin el .dta no se puede comprobar el caché; ejecute --stage data con acceso al .dta", call. = FALSE)
    }
    dif <- key_mismatch(stored, parts, cfg)
    if (length(dif) > 0) {
      stop("El caché se construyó con otro ", paste(dif, collapse = ", otro "), "; ejecute --stage data con acceso al .dta (",
           sin_dta, ")", call. = FALSE)
    }
    reuse <- TRUE; key <- stored
    note("caché de master sin comprobar (.dta no disponible)")
  }
  t0 <- Sys.time()
  if (reuse) {
    obj <- readRDS(f_rds); master <- obj$master; jg <- obj$jg
    estado_m <- "caché"
    log_msg("master desde caché ", f_rds, " (", nrow(master), " x ", ncol(master), ")")
  } else {
    if (!build) stop("No hay caché válido de master en ", cache, ". Ejecute --stage data.", call. = FALSE)
    raw <- load_raw(cfg)
    log_msg("leído .dta: ", nrow(raw), " filas x ", ncol(raw), " columnas en ", round(secs(t0)), " s")
    built <- build_master(raw, regions); rm(raw)
    master <- built$master; jg <- built$jg
    save_rds(list(master = master, jg = jg), f_rds)
    writeLines(key, f_key)                              # la clave, solo después de guardar bien el .rds
    estado_m <- "construido"
    log_msg("master construido: ", nrow(master), " x ", ncol(master), " en ", round(secs(t0)), " s → ", f_rds)
  }
  row_m <- tibble(
    fig = "data", metodo = "master", estado = estado_m,
    verificacion = sprintf("%d x %d · %d países · %.0f s", nrow(master), ncol(master), n_distinct(master$country_name_ltn), secs(t0)),
    archivo = rel_path(f_rds, cfg$paths$root)
  )

  t1 <- Sys.time()
  ckey <- ctx_key(cfg, key)
  if (!force && file.exists(f_ctx) && identical(read_key(f_ckey), ckey)) {
    ctx <- readRDS(f_ctx); ctx$master <- master
    # el yml no entra en la clave: cfg y lo que sale de él se toman del cfg vivo, no del de la construcción
    ctx$cfg <- cfg[setdiff(names(cfg), "paths")]
    ctx$meta$reference_version <- cfg$report$reference_version
    ctx$meta$ctx_key <- ckey                            # también en cachés anteriores a meta$ctx_key
    estado_c <- "caché"
  } else {
    ctx <- build_ctx(cfg, master, jg, regions, key)
    save_rds(ctx[setdiff(names(ctx), "master")], f_ctx)
    writeLines(ckey, f_ckey)
    estado_c <- "construido"
  }
  .sr$memo <- list(); .sr$fn_envs <- list()            # los estimadores memorizados dependen de ctx (03_estimators.R)
  checks <- data_checks(ctx, cfg)
  row_c <- tibble(
    fig = "data", metodo = "ctx", estado = estado_c,
    verificacion = sprintf("%d filas · comprobaciones %d/%d · %.0f s", nrow(ctx$national), checks$ok, checks$n, secs(t1)),
    archivo = rel_path(f_ctx, cfg$paths$root)
  )
  if (checks$n > checks$ok) note(sprintf("comprobaciones de datos: %d/%d (ver log)", checks$ok, checks$n))
  write_inputs_md5(cfg)
  list(ctx = ctx, table = bind_rows(row_m, row_c), note = notes)
}


## Comprobaciones de la etapa data ------------------------------------------------------------------------------

# cfg$checks (config/<pais>.yml): ncol_master_csv, national{var: valor}, eu{var: valor}, journey{campo: valor}.
# Se registran en el log; no detienen: un valor esperado vacío o no numérico cuenta como DIFIERE ("esperado inválido")
# y una sección que no es var: valor se ignora con aviso (antes abortaban la etapa). Devuelve list(ok, n).
data_checks <- function(ctx, cfg) {
  ch <- if (is.list(cfg$checks)) cfg$checks else list(); ok <- 0L; n <- 0L
  if (!is.null(cfg$checks) && !is.list(cfg$checks)) log_msg("aviso: checks no es una lista (ver config/<pais>.yml); se ignora")
  scalar <- function(x) if (is.data.frame(x)) as.numeric(x$value[[1]]) else as.numeric(x)[[1]]
  test <- function(label, got, want, tol = 0.01) {
    n <<- n + 1L
    w <- if (length(want) == 1 && is.atomic(want)) suppressWarnings(as.numeric(want)) else NA_real_
    pass <- isTRUE(!is.na(got) && !is.na(w) && abs(got - w) <= tol)
    if (pass) ok <<- ok + 1L
    shown <- if (!is.na(w)) w else if (length(unlist(want)) == 0) "inválido: vacío" else
      paste0("inválido: '", paste(unlist(want), collapse = ", "), "'")
    log_msg("check ", label, " = ", formatC(got, format = "f", digits = 2), " (esperado ", shown, ") ", if (pass) "ok" else "DIFIERE")
  }
  safe <- function(expr) tryCatch(scalar(expr), error = function(e) { log_msg("check error: ", conditionMessage(e)); NA_real_ })
  sec <- function(s) {
    x <- ch[[s]]
    if (is.null(x) || (is.list(x) && !is.null(names(x)))) return(x)
    log_msg("aviso: checks$", s, " no es una lista var: valor; se ignora"); NULL
  }
  if ("ncol_master_csv" %in% names(ch)) {
    nm <- names(ctx$master)
    got <- sum(nm %in% MASTER_VARS) + sum(Reduce(`|`, lapply(MASTER_PREFIXES, function(p) startsWith(nm, p))))
    test("ncol_master_csv", got, ch[["ncol_master_csv"]], tol = 0)
  }
  nat <- sec("national"); eu <- sec("eu"); jo <- sec("journey")
  for (v in names(nat)) test(paste0("nat_est(", v, ")"), safe(nat_est(ctx, v)), nat[[v]])
  for (v in names(eu)) test(paste0("eu_est(", v, ")"), safe(eu_est(ctx, v)), eu[[v]])
  for (v in names(jo)) test(paste0("journey()$", v), safe(journey(ctx)[[v]]), jo[[v]])
  list(ok = ok, n = n)
}

# output/<slug>/log/inputs_md5.csv: huella de los insumos de la corrida
write_inputs_md5 <- function(cfg) {
  p <- cfg$paths
  fonts <- list.files(p$fonts, pattern = "\\.ttf$", full.names = TRUE)
  files <- c(regions = p$regions, eu_values = p$eu_values, renv_lock = p$lock, setNames(fonts, basename(fonts)))
  tbl <- tibble(
    insumo = c("dta", names(files)),
    ruta = c(p$dta, unname(files)),
    huella = c(file_sig(p$dta), vapply(unname(files), function(f) if (file.exists(f)) unname(tools::md5sum(f)) else "ausente", ""))
  )
  write_csv(tbl, file.path(p$output, "log", "inputs_md5.csv"))
  invisible(tbl)
}


## Prueba ---------------------------------------------------------------------------------------------------------
## Comandos (G2 = SCRATCH/fix/G2; SCRATCH = scratchpad de la sesión): cd /tmp && Rscript --vanilla G2/pruebas/t01_claves.R
## (y t04_cfg_meta.R, t05_data_checks.R, t09_comentarios.R); bash G2/pruebas/t02_sin_dta.sh y t08_yml_vacio.sh (copias con
## path2EU inexistente). Resultado (2026-10-23), antes → ahora:
##  - Claves (t01): master_key() da el mismo texto que antes y key_parts() es su inversa; key_mismatch() vacío si solo cambia
##    el .dta y nombra R/02_variables.R, regions, paquete, R o deps.csv si cambian. rel_path() con '<root>_cache/…' daba
##    'cache/…' (ahora la ruta absoluta); read_key() de un .key vacío daba NA (ahora "").
##  - Sin .dta (t02): --stage data y --stage figures usan el caché con "aviso: caché de master sin comprobar" en el log.
##    --stage data --force borraba master.key y ctx_<slug>.key y después nada corría; ahora se detiene ("--force necesita
##    el .dta …; el caché no se ha tocado"), cache/ queda igual (md5) y --stage all --figs 09 verifica 1/1. Con una línea
##    añadida a R/02_variables.R, data (solo con la nota genérica) y figures (sin aviso) usaban el master viejo; ahora se
##    detienen: "El caché se construyó con otro R/02_variables.R; ejecute --stage data con acceso al .dta (…)".
##  - yml (t04): con text_pt, dpi, title y reference_version cambiados, el ctx del caché traía los de su construcción; ahora
##    ctx$cfg y meta$reference_version salen del cfg vivo y meta$ctx_key = cache/ctx_<slug>.key.
##  - data_checks (t05, t08): un esperado vacío, no numérico o lista abortaba la etapa; ahora cuenta como DIFIERE
##    ("esperado inválido") y --stage data termina con comprobaciones 4/5 y el aviso en el log; una sección que no es
##    var: valor se ignora con aviso.
##  - Comentarios (t09): 53 MASTER_VARS + 147 AJ* = 200 = checks$ncol_master_csv (log: "check ncol_master_csv = 200.00
##    (esperado 200) ok").
##  - Regresión: cd /tmp && Rscript <copia>/main.R --stage all --figs 09,10,11 → comprobaciones 5/5 y 3/3 verificadas;
##    --stage verify y --stage compare sin error.
