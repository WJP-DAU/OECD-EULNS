## 01_load.R — lectura del .dta, master de 27 países, contexto del país (ctx) y caché ---------------------------
## Origen: src/data/data-loading.R:7-18 (lectura del .dta); src/data/worker-data.R:22-82 (study_vars y flujo
## quality check → add_special_demographics → add_a2j_vars → select); src/analysis/eu_main_indicators.R:8-60
## (los 27 países de regions.csv y non_trivial_problem_broader). El caché, ctx y las comprobaciones son nuevos.
## Usa de 02_variables.R: apply_a2j_quality_check(), add_special_demographics(), add_a2j_vars() (que además
## devuelve attr(, "jg") = list(noDK, keepDK) en vez de escribir data/jg_*.csv) y add_report_vars().
## Usa de 03_estimators.R solo en data_checks(): nat_est(), eu_est(), journey().

# Columnas equivalentes a data/master.csv: study_vars (worker-data.R:22-51) + AJP_/AJD_/AJR_/AJE_ (= 199)
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
rel_path <- function(p, root) if (startsWith(p, root)) substring(p, nchar(root) + 2) else p


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
build_ctx <- function(cfg, master, jg, regions, key) {
  national <- master %>% filter(country_name_ltn == cfg$country)
  if (nrow(national) == 0) stop("Ningún encuestado con country_name_ltn == '", cfg$country, "'", call. = FALSE)
  jg_nat <- jg$keepDK %>% semi_join(national, by = "country_year_id")
  add <- setdiff(c("country_name_ltn", "nuts_id"), names(jg_nat))
  if (length(add) > 0) jg_nat <- left_join(jg_nat, select(national, country_year_id, all_of(add)), by = "country_year_id")
  deps <- read_csv(cfg$paths$deps, show_col_types = FALSE)
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
      key = key, built = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), r = R.version.string,
      packages = setNames(vapply(deps$paquete, pkg_ver, ""), deps$paquete),
      dta = list(path = cfg$paths$dta, sig = file_sig(cfg$paths$dta)),
      n_master = nrow(master), countries = sort(unique(master$country_name_ltn)), n_national = nrow(national),
      reference_version = cfg$report$reference_version
    )
  )
}


## Caché ---------------------------------------------------------------------------------------------------------

file_sig <- function(p) if (!is.na(p) && file.exists(p)) paste0(file.size(p), "|", as.integer(file.mtime(p))) else "ausente"
pkg_ver <- function(p) if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else "-"

# Cambia si cambian el .dta, regions.csv, el código de carga/variables, los paquetes de lectura o R
master_key <- function(cfg) {
  root <- cfg$paths$root
  pk <- c("haven", "dplyr", "tidyr", "readr")
  paste(
    "dta", file_sig(cfg$paths$dta), cfg$paths$dta,
    "regions", unname(tools::md5sum(cfg$paths$regions)),
    "code", paste(unname(tools::md5sum(file.path(root, "R", c("01_load.R", "02_variables.R")))), collapse = ","),
    "pkgs", paste(pk, vapply(pk, pkg_ver, ""), sep = "=", collapse = ","),
    "R", paste(R.version$major, R.version$minor, sep = "."),
    "deps", unname(tools::md5sum(cfg$paths$deps)),
    sep = "|"
  )
}

ctx_key <- function(cfg, mkey) {
  eu <- if (file.exists(cfg$paths$eu_values)) unname(tools::md5sum(cfg$paths$eu_values)) else "ausente"
  paste(mkey, "eu", eu, "slug", cfg$slug, "country", cfg$country, sep = "|")
}

read_key <- function(f) if (file.exists(f)) readLines(f, warn = FALSE)[1] else ""

# Devuelve list(ctx, table, note). force = reconstruir; build = FALSE no lee el .dta (error si no hay caché).
load_ctx <- function(cfg, force = FALSE, build = TRUE) {
  cache <- cfg$paths$cache
  f_rds <- file.path(cache, "master.rds"); f_key <- file.path(cache, "master.key")
  f_ctx <- file.path(cache, paste0("ctx_", cfg$slug, ".rds")); f_ckey <- file.path(cache, paste0("ctx_", cfg$slug, ".key"))
  if (force) unlink(c(f_key, f_ckey))
  notes <- character()

  regions <- load_regions(cfg)
  dta_ok <- !is.na(cfg$paths$dta) && file.exists(cfg$paths$dta)
  key <- master_key(cfg)
  stored <- read_key(f_key)
  reuse <- file.exists(f_rds) && (identical(stored, key) || (!dta_ok && nzchar(stored)))
  t0 <- Sys.time()
  if (reuse) {
    obj <- readRDS(f_rds); master <- obj$master; jg <- obj$jg
    estado_m <- "caché"
    if (!identical(stored, key)) { notes <- c(notes, "caché de master sin comprobar (.dta no disponible)"); key <- stored }
    log_msg("master desde caché ", f_rds, " (", nrow(master), " x ", ncol(master), ")")
  } else {
    if (!build) stop("No hay caché válido de master en ", cache, ". Ejecute --stage data.", call. = FALSE)
    raw <- load_raw(cfg)
    log_msg("leído .dta: ", nrow(raw), " filas x ", ncol(raw), " columnas en ", round(secs(t0)), " s")
    built <- build_master(raw, regions); rm(raw)
    master <- built$master; jg <- built$jg
    saveRDS(list(master = master, jg = jg), f_rds)
    writeLines(key, f_key)
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
  if (file.exists(f_ctx) && identical(read_key(f_ckey), ckey)) {
    ctx <- readRDS(f_ctx); ctx$master <- master
    estado_c <- "caché"
  } else {
    ctx <- build_ctx(cfg, master, jg, regions, key)
    saveRDS(ctx[setdiff(names(ctx), "master")], f_ctx)
    writeLines(ckey, f_ckey)
    estado_c <- "construido"
  }
  .sr$memo <- list()                                    # los estimadores memorizados dependen de ctx
  checks <- data_checks(ctx, cfg)
  row_c <- tibble(
    fig = "data", metodo = "ctx", estado = estado_c,
    verificacion = sprintf("%d filas · comprobaciones %d/%d · %.0f s", nrow(ctx$national), checks$ok, checks$n, secs(t1)),
    archivo = rel_path(f_ctx, cfg$paths$root)
  )
  if (checks$n > checks$ok) notes <- c(notes, sprintf("comprobaciones de datos: %d/%d (ver log)", checks$ok, checks$n))
  write_inputs_md5(cfg)
  list(ctx = ctx, table = bind_rows(row_m, row_c), note = notes)
}


## Comprobaciones de la etapa data ------------------------------------------------------------------------------

# cfg$checks (config/<pais>.yml): ncol_master_csv, national{var: valor}, eu{var: valor}, journey{campo: valor}.
# Se registran en el log; no detienen. Devuelve list(ok, n).
data_checks <- function(ctx, cfg) {
  ch <- cfg$checks; ok <- 0L; n <- 0L
  scalar <- function(x) if (is.data.frame(x)) as.numeric(x$value[[1]]) else as.numeric(x)[[1]]
  test <- function(label, got, want, tol = 0.01) {
    n <<- n + 1L
    pass <- !is.na(got) && abs(got - want) <= tol
    if (pass) ok <<- ok + 1L
    log_msg("check ", label, " = ", formatC(got, format = "f", digits = 2), " (esperado ", want, ") ", if (pass) "ok" else "DIFIERE")
  }
  safe <- function(expr) tryCatch(scalar(expr), error = function(e) { log_msg("check error: ", conditionMessage(e)); NA_real_ })
  if (!is.null(ch$ncol_master_csv)) {
    nm <- names(ctx$master)
    got <- sum(nm %in% MASTER_VARS) + sum(Reduce(`|`, lapply(MASTER_PREFIXES, function(p) startsWith(nm, p))))
    test("ncol_master_csv", got, ch$ncol_master_csv, tol = 0)
  }
  for (v in names(ch$national)) test(paste0("nat_est(", v, ")"), safe(nat_est(ctx, v)), ch$national[[v]])
  for (v in names(ch$eu)) test(paste0("eu_est(", v, ")"), safe(eu_est(ctx, v)), ch$eu[[v]])
  for (v in names(ch$journey)) test(paste0("journey()$", v), safe(journey(ctx)[[v]]), ch$journey[[v]])
  list(ok = ok, n = n)
}

# output/<slug>/log/inputs_md5.csv: huella de los insumos de la corrida
write_inputs_md5 <- function(cfg) {
  p <- cfg$paths
  fonts <- list.files(p$fonts, pattern = "\\.ttf$", full.names = TRUE)
  files <- c(regions = p$regions, eu_values = p$eu_values, manifest = p$manifest, deps = p$deps, setNames(fonts, basename(fonts)))
  tbl <- tibble(
    insumo = c("dta", names(files)),
    ruta = c(p$dta, unname(files)),
    huella = c(file_sig(p$dta), vapply(unname(files), function(f) if (file.exists(f)) unname(tools::md5sum(f)) else "ausente", ""))
  )
  write_csv(tbl, file.path(p$output, "log", "inputs_md5.csv"))
  invisible(tbl)
}
