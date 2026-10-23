## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## tools/build_reference.R — construye reference/<slug>/ (la verdad que usa --stage verify) desde lo publicado
##
## Produce, para el país de config/<pais>.yml:
##   reference/<slug>/Figure_NN_slug.csv   una por figura del manifiesto (contratos §4.4/§4.6): order, kind, grouping,
##                                         label, value, value2, text, text2, frac, frac2, tol (+ tol2 en la 3;
##                                         tol_xy, x, y en la red; lo, hi en las tarjetas; n en el scatter y el upset)
##   reference/<slug>/eu_values.csv        medias UE de data4web: id, value (0-100), fuente, archivo, md5, fecha, nota;
##                                         ids = columna eu_id del manifiesto en las figuras con metodo_ue data4web
##   reference/<slug>/published/*.png      figuras publicadas remuestreadas a 96 dpi con resample_to_dpi() de
##                                         R/08_compare.R (el mismo remuestreo que --stage compare; igual en macOS y Linux)
##   reference/<slug>/README.md            nivel de cada figura, fuentes, md5 y fecha de los insumos, cómo regenerar
##
## Insumos (rutas de la máquina en config/local.yml). Solo escribe en reference/<slug>/; load_config() crea además,
## vacías y si faltan, output/<slug>/{figures,tables,compare,log} y la carpeta de caché.
##   reference_src_dir = …/output/viz/<pais>/finales/datos   37 CSV publicados + 00-verificacion_valores.csv
##   published_dir     = …/output/finales/<Pais>/figuras      Figure_NN_slug.png (300 dpi); ../00-INDICE.csv (títulos)
##   <viz> = dirname(dirname(reference_src_dir)): "<viz>/analisis adicionales/" (A03, A04, A05, F01, F02, F03, F08,
##           R04_fig24_conjuntos, cooccurrence_network.svg) y "<viz>/<country>_jg_upset.svg" (solo se registra)
##   path2EU + data.data4web = data4web_gpp.csv (medias UE; se lee con col_select, 1-2 s)
## Guarda de país: published_dir y reference_src_dir son de la máquina, no del país; antes de escribir en reference/
## se comprueba que <viz> es output/viz/<country>, que existen <country>_jg_upset.svg y la columna <country> de F03.csv
## y que los títulos de 00-INDICE.csv nombran el país. Si no, se detiene sin tocar reference/<slug>/.
##
## Uso:  Rscript src/src_reports/tools/build_reference.R [--country <slug>]
##
## Origen de las convenciones (solo lectura, nada se ejecuta del repo):
##   figure_specs.R:166-198  check_spec: 00-verificacion_valores.csv (data = valor de la tabla redondeado como se imprime,
##                           bar = fracción·escala a 2 decimales); grupo "National" para las filas avg
##   standard_figures.R:108-119 funnel_rows; 128-177 sankey_def + PUBLISHED_SANKEY; 184-204 scorecard_def
##   legacy_python/charts5.py:151-184 scorecards: "NN%" = round(v); delta = round(v) − round(ref) con signo "+", "−"
##                           (U+2212) o "±" y sufijo " points"; lo/hi = min/max de round(v) de la figura
##   network_graph.R:160-199 layout (pad 0.16, min_dist 0.28, hstretch 1.15), 212 anchos de arista, 273-297 lienzo
##   F01.R:12-19 matriz P(fila | columna) y bandas; F02.R:14-25 scatter; F03.R:3-18 severidad; F08.R:3-23 upset
##   render_figure.R:44-57   upset publicado: réplicas round(pct·10) en ComplexUpset (min_size 1, sort_sets FALSE)
##   report_charts.R:259-266 columnas (30, 37): altura = col_h · frac / vmax (vmax = máximo de las dos series)
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

t_ini <- Sys.time()
ROOT <- local({
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  # Rscript codifica los espacios de la ruta como "~+~": se decodifican antes de usarla
  r <- if (length(f) > 0) dirname(dirname(gsub("~\\+~", " ", sub("^--file=", "", f[1])))) else Sys.getenv("SRC_REPORTS_ROOT", unset = "")
  if (!nzchar(r)) stop("Ejecute 'Rscript <carpeta>/tools/build_reference.R' o defina SRC_REPORTS_ROOT", call. = FALSE)
  normalizePath(r, mustWork = TRUE)
})
source(file.path(ROOT, "R", "00_setup.R"))
source(file.path(ROOT, "R", "08_compare.R"))   # read_png_rgb(), resample_to_dpi(), COMPARE_DPI: el remuestreo de compare

args <- commandArgs(TRUE)
country <- if (length(args) >= 2 && args[1] == "--country") args[2] else NULL
cfg <- load_config(ROOT, country)
P <- cfg$paths
if (is.na(P$reference_src) || !dir.exists(P$reference_src))
  stop("config/local.yml: reference_src_dir no existe: ", P$reference_src, call. = FALSE)
if (is.na(P$published) || !dir.exists(P$published))
  stop("config/local.yml: published_dir no existe: ", P$published, call. = FALSE)
if (is.na(P$data4web) || !file.exists(P$data4web))
  stop("No existe data4web (path2EU de config/local.yml + data.data4web de config/<pais>.yml): ", P$data4web, call. = FALSE)
SRC <- P$reference_src
PUB <- P$published
VIZ <- dirname(dirname(SRC))
ADD <- file.path(VIZ, "analisis adicionales")
UPSET_SVG <- file.path(VIZ, paste0(cfg$country, "_jg_upset.svg"))
REF <- P$reference
specs <- read_manifest(cfg)


## Utilidades ---------------------------------------------------------------------------------------------------

# Igual que fmt_num() de R/05_charts.R (contratos §5.C): formatC(round(x, d), "f", d) + "%"; NA → "N/A"
fmt_num <- function(x, decimals = 0, pct = TRUE) {
  out <- paste0(formatC(round(x, decimals), format = "f", digits = decimals), if (pct) "%" else "")
  out[is.na(x)] <- "N/A"
  out
}
num <- function(x) suppressWarnings(as.numeric(x))          # "" y "NA" → NA sin aviso
read_chr <- function(f) read_csv(f, col_types = cols(.default = col_character()), na = character(), show_col_types = FALSE)
read_add <- function(name) {
  f <- file.path(ADD, name)
  if (!file.exists(f)) stop("No existe analisis adicionales/", name, call. = FALSE)
  read_csv(f, show_col_types = FALSE)
}
src_csv <- function(spec) file.path(SRC, paste0(figure_stem(spec), ".csv"))
secs <- function(t0) round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)

# Tabla de referencia (§4.6): columnas fijas en orden y tipos; extras al final, solo si la figura las usa: tol2
# (tolerancia de value2; verify toma tol si falta), tol_xy (de x e y; 0.01 si falta), x, y, n, lo, hi; order = 1..n
REF_COLS <- c("order", "kind", "grouping", "label", "value", "value2", "text", "text2", "frac", "frac2", "tol")
REF_EXTRAS <- c("tol2", "tol_xy", "x", "y", "n", "lo", "hi")
ref_tbl <- function(d, tol = 0.6) {
  if (!"order" %in% names(d)) d$order <- seq_len(nrow(d))
  for (k in c("grouping", "text", "text2")) if (!k %in% names(d)) d[[k]] <- ""
  for (k in c("value2", "frac", "frac2")) if (!k %in% names(d)) d[[k]] <- NA_real_
  if (!"tol" %in% names(d)) d$tol <- tol
  d <- d %>% mutate(order = as.integer(order),
                    across(c(kind, grouping, label, text, text2), ~ coalesce(as.character(.x), "")),
                    across(c(value, value2, frac, frac2, tol), as.numeric),
                    across(any_of(c("tol2", "tol_xy", "x", "y", "lo", "hi")), as.numeric))
  extras <- intersect(REF_EXTRAS, names(d))
  d <- d[, c(REF_COLS, extras)]
  if (nrow(d) == 0 || !identical(d$order, seq_len(nrow(d)))) stop("order debe ser 1..n", call. = FALSE)
  d
}
res <- function(df, nivel, fuentes, obs = "") list(df = df, nivel = nivel, fuentes = fuentes, obs = obs)

# Guarda de país (ver encabezado): las fuentes deben ser de cfg$country; si no, se detiene antes de escribir en reference/
check_sources_country <- function() {
  f3 <- file.path(ADD, "F03.csv")
  idx <- file.path(dirname(PUB), "00-INDICE.csv")
  bad <- c(
    if (basename(VIZ) != cfg$country) sprintf("reference_src_dir está bajo output/viz/%s, no bajo output/viz/%s", basename(VIZ), cfg$country),
    if (!file.exists(UPSET_SVG)) sprintf("no existe %s/%s", basename(VIZ), basename(UPSET_SVG)),
    if (!file.exists(f3)) "no existe analisis adicionales/F03.csv"
    else if (!cfg$country %in% names(read_csv(f3, n_max = 0, show_col_types = FALSE))) sprintf("F03.csv no tiene la columna %s", cfg$country),
    if (!file.exists(idx)) sprintf("no existe %s/00-INDICE.csv (títulos de las figuras de published_dir)", basename(dirname(PUB)))
    else if (!any(str_detect(read_chr(idx)$titulo %||% "", fixed(cfg$country)))) sprintf("ningún título de %s/00-INDICE.csv nombra %s", basename(dirname(PUB)), cfg$country)
  )
  if (length(bad) > 0) {
    stop("Las fuentes de config/local.yml no son de ", cfg$country, ": ", paste(bad, collapse = "; "),
         ". Apunte reference_src_dir y published_dir a las figuras publicadas de ese país.", call. = FALSE)
  }
  invisible(TRUE)
}

# 00-verificacion_valores.csv (figure_specs.R:166-198): fig, label, grupo, serie, printed, data, bar, estado, ok
VER_FILE <- file.path(SRC, "00-verificacion_valores.csv")
VER <- if (file.exists(VER_FILE)) {
  read_csv(VER_FILE, col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
    transmute(fig = as.integer(fig), label, grupo = coalesce(grupo, ""), serie, data = num(data))
} else tibble(fig = integer(), label = character(), grupo = character(), serie = character(), data = numeric())


## Constructores por tipo ----------------------------------------------------------------------------------------

# CSV uniforme de finales/datos (order, kind, label, v1_frac, v1_text, v2_frac, v2_text): barras agrupadas, pares,
# dumbbell, hbars y columnas. grouping: "" en avg y en filas sin header previo; en header su propio nombre; en row el
# header anterior. value/value2 = columna data de 00-verificacion (valor impreso como número; unida por fig, label y
# grupo/serie, con grupo "National" para avg) y, donde falte, v1_frac·escala (100, o 10 si no se imprime "%").
# text/text2 = v1_text/v2_text tal cual ("61%", "N/A", "6.0", ""); frac/frac2 = fracción dibujada (§4.5): v1_frac/v2_frac
# (la pista llena es 100 %), salvo en columns (30, 37), cuya altura es frac/vmax (report_charts.R:259-266).
ref_standard <- function(spec) {
  f <- src_csv(spec)
  if (!file.exists(f)) stop("No existe ", basename(f), call. = FALSE)
  d <- read_chr(f)
  need <- c("order", "kind", "label", "v1_frac", "v1_text", "v2_frac", "v2_text")
  if (!all(need %in% names(d))) stop(basename(f), ": formato no uniforme (", paste(names(d), collapse = ","), ")", call. = FALSE)
  pct <- any(str_detect(d$v1_text, "%"))
  scale <- if (pct) 100 else 10
  grp <- character(nrow(d)); cur <- ""
  for (i in seq_len(nrow(d))) {
    if (d$kind[i] == "header") cur <- d$label[i]
    grp[i] <- if (d$kind[i] == "avg") "" else cur
  }
  v <- VER %>% filter(fig == spec$fig)
  d <- d %>%
    mutate(order = as.integer(order), grouping = grp, key = if_else(kind == "avg", "National", grouping),
           v1_frac = num(v1_frac), v2_frac = num(v2_frac)) %>%
    left_join(v %>% filter(serie == "v1") %>% select(label, key = grupo, d1 = data), by = c("label", "key")) %>%
    left_join(v %>% filter(serie == "v2") %>% select(label, key = grupo, d2 = data), by = c("label", "key"))
  if (nrow(d) != length(grp)) stop(figure_stem(spec), ": la unión con 00-verificacion duplica filas", call. = FALSE)
  n_ver <- sum(!is.na(d$d1)) + sum(!is.na(d$d2))
  gap <- if (n_ver > 0) max(abs(d$d1 - d$v1_frac * scale), abs(d$d2 - d$v2_frac * scale), na.rm = TRUE) else NA_real_
  # corregido: en columns la fracción dibujada es v/vmax (vmax = máximo de las dos series), no v1_frac = value/100
  vmax <- if (spec$tipo == "columns") max(c(d$v1_frac, d$v2_frac), na.rm = TRUE) else 1
  out <- d %>%
    transmute(order, kind, grouping, label, value = coalesce(d1, v1_frac * scale), value2 = coalesce(d2, v2_frac * scale),
              text = v1_text, text2 = v2_text, frac = v1_frac / vmax, frac2 = v2_frac / vmax) %>%
    ref_tbl(tol = if (pct) 0.6 else 0.06)
  res(out, "texto_publicado",
      c(file.path("finales/datos", basename(f)), if (n_ver > 0) "finales/datos/00-verificacion_valores.csv"),
      paste0(if (n_ver > 0) sprintf("%d valores de 00-verificacion (|data − frac·%d| máx %.2f); resto v1_frac·%d", n_ver, scale, gap, scale)
             else sprintf("sin filas en 00-verificacion: value = v1_frac·%d", scale),
             if (spec$tipo == "columns") sprintf("; frac = v_frac/vmax (vmax %.4f, altura dibujada)", vmax) else ""))
}

# Embudos (8, 34): CSV label, v, value, frac de funnel_rows (standard_figures.R:108-119). kind "stage"; value = v;
# text = value ("61%"); frac = barra relativa a la más ancha. Las etiquetas conservan el salto de línea impreso.
ref_funnel <- function(spec) {
  f <- src_csv(spec)
  if (!file.exists(f)) stop("No existe ", basename(f), call. = FALSE)
  d <- read_chr(f)
  if (!all(c("label", "v", "value", "frac") %in% names(d))) stop(basename(f), ": se esperaban label, v, value, frac", call. = FALSE)
  out <- ref_tbl(tibble(kind = "stage", grouping = "", label = d$label, value = num(d$v), text = d$value, frac = num(d$frac)))
  res(out, "texto_publicado", file.path("finales/datos", basename(f)), "etiquetas con salto de línea como se imprimen")
}

# Sankeys (11, 16): el CSV publicado solo trae los textos (columna valor) en el orden de PUBLISHED_SANKEY
# (standard_figures.R:174-177): columna 1 (contacted, not_contacted), columna 2 (order2 de sankey_def), columna 3
# (access, no_access, excluded). Claves de sankey_def (standard_figures.R:128-170); la definición se elige por el
# número de nodos (10 asistencia, 9 DRM) y se contrasta con el slug. value = texto como número; sin fracciones.
SANKEY_NODES <- list(
  assistance = tibble(grouping = c("1", "1", "2", "2", "2", "2", "2", "3", "3", "3"),
                      label = c("contacted", "not_contacted", "appropriate", "non_appropriate", "needed_no", "no_need",
                                "unknown", "access", "no_access", "excluded")),
  drm = tibble(grouping = c("1", "1", "2", "2", "2", "2", "3", "3", "3"),
               label = c("contacted", "not_contacted", "turned", "needed_no", "no_need", "unknown", "access", "no_access", "excluded"))
)
ref_sankey <- function(spec) {
  f <- src_csv(spec)
  if (!file.exists(f)) stop("No existe ", basename(f), call. = FALSE)
  d <- read_chr(f)
  if (!"valor" %in% names(d)) stop(basename(f), ": se esperaba la columna valor", call. = FALSE)
  which <- if (nrow(d) == 10) "assistance" else if (nrow(d) == 9) "drm" else stop(basename(f), ": ", nrow(d), " valores (se esperaban 10 u 9)", call. = FALSE)
  if (xor(which == "drm", str_detect(spec$slug, "drm"))) stop(basename(f), ": número de nodos incoherente con el slug", call. = FALSE)
  def <- SANKEY_NODES[[which]]
  out <- ref_tbl(tibble(kind = "node", grouping = def$grouping, label = def$label,
                        value = num(sub("%$", "", d$valor)), text = d$valor))
  res(out, "texto_publicado", file.path("finales/datos", basename(f)),
      sprintf("definición %s (standard_figures.R:128-177); value = texto publicado como número", which))
}

# Tarjetas (32, 41, 42): valores de A03/A04/A05 con las definiciones de scorecard_def (filas, columnas, referencia y
# etiquetas publicadas) y los textos como los dibujó charts5.py:151-184: text = round(v) + "%"; text2 = delta
# round(v) − round(ref) con signo "+", "−" (U+2212) o "±" y sufijo " points" ("" en las tarjetas de referencia, cuyo
# value2 es NA); value2 = valor crudo de la tarjeta de referencia; lo/hi = min/max de round(v) de la figura (escala de
# color de charts5.py:160, por figura: 2/62, 29/55, 30/55). Cotejo: finales/datos/Figure_NN_slug.csv (fila × columnas
# a 2 decimales, filas con los nombres de A0x) debe coincidir con v (|Δ| ≤ 0.005) → nivel texto_publicado; sin ese CSV
# queda "transcrito" (revisar); si no coincide, error.
ACTORS <- c("Judges", "Prosecutors", "Public defense attorneys")
# origen: standard_figures.R:184-204 (scorecard_def), con add() → analisis adicionales/
scorecard_def <- function(which) {
  switch(which,
    hardship_outcome = {
      a3 <- read_add("A03.csv") %>% filter(corte == "Resolución")
      hs <- c("At least one hardship", "Health issues", "Economic issues", "Relationship breakdown", "Alcohol/Drug problems")
      list(rows = c("At least one hardship", "Health issues", "Economic issues", "Relationship breakdown", "Alcohol or drug problems"),
           keys = hs, cols = c("Fully resolved", "Not resolved"), ref = "col",          # keys: filas de A03 y de finales/datos
           vals = t(sapply(hs, function(h) sapply(c("Fully resolved", "Not resolved"), function(g) a3$pct[a3$desenlace == h & a3$grupo == g]))))
    },
    exposure = {
      a4 <- read_add("A04.csv")
      g <- c("No non-trivial problem", "Problem, did not need a DRM", "Needed a DRM, had access", "Needed a DRM, no access")
      list(rows = c("Without a non-trivial problem", g[-1]), keys = g, cols = ACTORS, ref = "row",
           vals = t(sapply(g, function(gr) sapply(ACTORS, function(a) a4$pct[a4$actor == a & a4$grupo == gr]))))
    },
    hardship = {
      a5 <- read_add("A05.csv") %>% filter(corte == "Escalera con hardship")
      g <- c("No non-trivial problem", "Problem, no hardship", "Problem, with hardship")
      list(rows = c("Without a non-trivial problem", g[-1]), keys = g, cols = ACTORS, ref = "row",
           vals = t(sapply(g, function(gr) sapply(ACTORS, function(a) a5$pct[a5$actor == a & a5$grupo == gr]))))
    },
    stop("scorecard_def: definición desconocida ", which, call. = FALSE))
}
CARDS_DEF <- c(hardships_by_outcome = "hardship_outcome", trust_by_exposure = "exposure", trust_by_hardship = "hardship")
CARDS_SRC <- c(hardship_outcome = "A03.csv", exposure = "A04.csv", hardship = "A05.csv")
ref_cards <- function(spec) {
  if (!spec$slug %in% names(CARDS_DEF)) stop("tarjetas sin definición para el slug ", spec$slug, call. = FALSE)
  which <- CARDS_DEF[[spec$slug]]
  s <- scorecard_def(which)
  v <- matrix(as.numeric(s$vals), nrow = length(s$rows), ncol = length(s$cols))
  if (any(is.na(v))) stop(figure_stem(spec), ": valores NA en ", CARDS_SRC[[which]], call. = FALSE)
  # corregido: cotejo automático con los datos publicados de la figura (antes solo a ojo con el PNG)
  f <- src_csv(spec); dmax <- NA_real_
  if (file.exists(f)) {
    p <- read_chr(f)
    if (!identical(names(p), c("fila", s$cols)) || !identical(p$fila, s$keys))
      stop(basename(f), ": filas o columnas distintas de scorecard_def('", which, "')", call. = FALSE)
    pv <- matrix(num(unlist(p[, s$cols])), nrow = nrow(p))
    dmax <- max(abs(v - pv))
    if (is.na(dmax) || dmax > 0.005) stop(basename(f), sprintf(": distinto de %s (|Δ| máx %.4f > 0.005)", CARDS_SRC[[which]], dmax), call. = FALSE)
  }
  r <- round(v); lo <- min(r); hi <- max(r)
  rows <- list()
  for (i in seq_along(s$rows)) for (j in seq_along(s$cols)) {
    is_ref <- (s$ref == "row" && i == 1) || (s$ref == "col" && j == 1)
    base <- if (s$ref == "row") v[1, j] else v[i, 1]
    d <- r[i, j] - round(base)
    ds <- paste0(if (d > 0) "+" else if (d < 0) "−" else "±", abs(d), " points")
    rows[[length(rows) + 1]] <- tibble(kind = "card", grouping = s$rows[i], label = s$cols[j], value = v[i, j],
                                       value2 = if (is_ref) NA_real_ else base, text = paste0(r[i, j], "%"),
                                       text2 = if (is_ref) "" else ds, lo = lo, hi = hi)
  }
  out <- ref_tbl(bind_rows(rows))
  if (is.na(dmax)) {
    return(res(out, "transcrito", c(file.path("analisis adicionales", CARDS_SRC[[which]]), paste0("published/", figure_stem(spec), ".png")),
               sprintf("revisar: %d tarjetas; sin %s: textos y deltas según charts5.py, cotejados a ojo con el PNG; lo/hi = %d/%d",
                       nrow(out), basename(f), lo, hi)))
  }
  res(out, "texto_publicado", c(file.path("analisis adicionales", CARDS_SRC[[which]]), file.path("finales/datos", basename(f))),
      sprintf("%d tarjetas = finales/datos (|Δ| máx %.4f); textos y deltas (de valores redondeados) según charts5.py; lo/hi = %d/%d",
              nrow(out), dmax, lo, hi))
}

# Dispersión (3): F02.csv (F02.R:14-25 category_profile) filas corte == "Area of Residence". grouping = faceta (Rural,
# Urban), label = categoría, value = prevalencia (x; tol 0.6), value2 = severidad media 0-10 (y; tol2 0.06),
# n = encuestados con problema no trivial (punto hueco si n < 30; sin severidad → el punto no se dibuja).
# Sin textos. Orden: faceta Rural y luego Urban; dentro, categorías en el orden de F02.csv (alfabético).
ref_scatter <- function(spec) {
  f <- read_add("F02.csv") %>% filter(corte == "Area of Residence")
  need <- c("grupo", "category", "prevalencia", "severidad_media", "n_con_problema_nt")
  if (!all(need %in% names(f)) || nrow(f) == 0) stop("F02.csv: faltan columnas o filas de Area of Residence", call. = FALSE)
  f <- f %>% mutate(.g = match(grupo, c("Rural", "Urban"))) %>% arrange(.g)
  out <- ref_tbl(tibble(kind = "point", grouping = f$grupo, label = f$category, value = f$prevalencia,
                        value2 = f$severidad_media, text = "", text2 = "", tol2 = 0.06, n = f$n_con_problema_nt))
  res(out, "tabla_analisis", "analisis adicionales/F02.csv",
      sprintf("%d puntos; %d sin severidad (value2 NA = suprimido); value2 con tol2 0.06", nrow(out), sum(is.na(out$value2))))
}

# Barras de severidad (7): F03.csv (F03.R:3-18; columnas sev, <país>, EU Average): % de problemas por severidad 0-10.
# kind "bar", grouping = serie (<país> | "EU Average"), label = severidad "0".."10"; sin textos. Orden: por severidad,
# el país antes que la UE (orden de dodge).
ref_severity <- function(spec) {
  f <- read_add("F03.csv")
  orden_series <- c(cfg$country, "EU Average")
  if (!all(c("sev", orden_series) %in% names(f))) stop("F03.csv: se esperaban columnas sev, ", cfg$country, ", EU Average", call. = FALSE)
  # corregido: dentro de mutate() 'series' es la columna; el orden país → UE sale del vector exterior (.env)
  long <- f %>% select(sev, all_of(orden_series)) %>% pivot_longer(-sev, names_to = "series", values_to = "value") %>%
    mutate(.s = match(series, .env$orden_series)) %>% arrange(sev, .s)
  out <- ref_tbl(tibble(kind = "bar", grouping = long$series, label = as.character(long$sev), value = long$value, text = "", text2 = ""))
  res(out, "tabla_analisis", "analisis adicionales/F03.csv", sprintf("%d barras sin texto impreso", nrow(out)))
}

# Matriz (6): F01.csv (F01.R:12-20; entre_quienes_tuvieron = columna de la figura, tambien_tuvieron = fila; pct =
# P(fila | columna), ponderación NUTS con n < 30 suprimido). Orden de filas y columnas = el del archivo (prevalencia no
# trivial descendente); order = filas de arriba abajo y, dentro, columnas de izquierda a derecha. grouping = fila,
# label = columna. text: "–" en la diagonal, "N/A" si pct NA, si no round(pct) + "%". text2 = banda
# cut(pct, c(-Inf, 25, 50, Inf), right = FALSE) sobre pct sin redondear ("" si NA).
ref_matrix <- function(spec) {
  f <- read_add("F01.csv")
  if (!all(c("entre_quienes_tuvieron", "tambien_tuvieron", "pct") %in% names(f))) stop("F01.csv: columnas inesperadas", call. = FALSE)
  ord <- unique(f$entre_quienes_tuvieron)
  g <- expand_grid(row = ord, col = ord) %>%
    left_join(f %>% select(col = entre_quienes_tuvieron, row = tambien_tuvieron, pct), by = c("row", "col"))
  if (nrow(g) != length(ord)^2) stop("F01.csv: pares repetidos", call. = FALSE)
  band <- cut(g$pct, c(-Inf, 25, 50, Inf), labels = c("Less than 25%", "25% to 50%", "50% or more"), right = FALSE)
  out <- ref_tbl(tibble(kind = "cell", grouping = g$row, label = g$col, value = g$pct,
                        text = case_when(g$row == g$col ~ "–", is.na(g$pct) ~ "N/A", TRUE ~ paste0(round(g$pct), "%")),
                        text2 = coalesce(as.character(band), "")))
  res(out, "tabla_analisis", "analisis adicionales/F01.csv",
      sprintf("%d celdas: %d con valor, %d N/A, %d diagonal", nrow(out), sum(!is.na(out$value)), sum(out$text == "N/A"), length(ord)))
}

# Upset (29): F08.csv (F08.R:3-23: combinaciones de necesidades insatisfechas dentro de la brecha, pct ponderado NUTS)
# y R04_fig24_conjuntos.csv (pct_en_figura = Σ round(pct·10)/10 por conjunto). kind "row" = barra de intersección:
# label = sus conjuntos en el orden fijo de UPSET_SETS unidos por " + " (= combinacion de F08.csv), de izquierda a
# derecha como las dibuja ComplexUpset. corregido: antes, el orden de F08.csv (pct descendente); la figura publicada
# (render_figure.R:44-57) pasa réplicas round(pct·10) a upset(min_size = 1, sort_sets = FALSE), que ordena por número
# de réplicas y desempata a su manera (fig 29: 59 y 59 → "Information + RP + PR" en la 8, "Assistance + RP + PR" en la
# 9); aquí el orden sale de ComplexUpset::upset_data() con los mismos argumentos (rev(sorted$intersections) = de
# izquierda a derecha). kind "set" = conjunto, de arriba abajo como la matriz (rev(sorted$groups): el orden fijo
# invertido, no el tamaño). text = fmt_num(value, 1); n = casos sin ponderar. <country>_jg_upset.svg no conserva
# <text> (glifos en trazos): textos y combinación de cada columna cotejados con el PNG publicado.
# origen: render_figure.R:44-48 (sets y columnas de F08 que los definen)
UPSET_SETS <- c("Access to Assistance" = "jg_inverse_access2rep", "Access to Information" = "jg_inverse_access2info",
                "Resolution Process" = "jg_inverse_rp", "Problem Resolved" = "jg_inverse_outcome")
ref_upset <- function(spec) {
  f8 <- read_add("F08.csv")
  r4 <- read_add("R04_fig24_conjuntos.csv")
  sets <- names(UPSET_SETS)
  if (!all(c(UPSET_SETS, "combinacion", "pct", "n") %in% names(f8)) || !all(c("necesidad", "pct_en_figura") %in% names(r4)))
    stop("F08.csv / R04_fig24_conjuntos.csv: columnas inesperadas", call. = FALSE)
  if (!requireNamespace("ComplexUpset", quietly = TRUE)) stop("Falta ComplexUpset (deps.csv): da el orden de las intersecciones", call. = FALSE)
  member <- as.matrix(f8[, unname(UPSET_SETS)]) == 1
  colnames(member) <- sets
  combo <- apply(member, 1, function(m) paste(sets[m], collapse = " + "))
  if (!identical(combo, f8$combinacion)) stop("F08.csv: combinacion no coincide con las columnas jg_inverse_* en el orden fijo", call. = FALSE)
  reps <- round(f8$pct * 10)                                             # réplicas de render_figure.R:45-46
  ud_in <- setNames(as.data.frame(member[rep(seq_len(nrow(f8)), reps), , drop = FALSE]), sets)
  ud <- ComplexUpset::upset_data(ud_in, sets, min_size = 1, sort_sets = FALSE)
  mf <- ud$matrix_frame %>% mutate(across(c(intersection, group), as.character))
  left_right <- rev(as.character(ud$sorted$intersections))
  lab_lr <- vapply(left_right, function(id) paste(sets[sets %in% mf$group[mf$intersection == id & mf$value]], collapse = " + "), "")
  pos <- match(combo, lab_lr)
  if (anyDuplicated(lab_lr) || !identical(!is.na(pos), reps > 0) || length(lab_lr) != sum(reps > 0))
    stop("ComplexUpset: intersecciones distintas de las combinaciones de F08.csv", call. = FALSE)
  keep <- order(pos)[seq_len(sum(!is.na(pos)))]                           # de izquierda a derecha; sin réplicas no se dibuja
  top_bottom <- rev(as.character(ud$sorted$groups))
  tot <- colSums(member * reps) / 10                                     # alto de cada conjunto en la figura
  r4 <- r4[match(top_bottom, r4$necesidad), ]
  if (anyNA(r4$necesidad) || max(abs(tot[top_bottom] - r4$pct_en_figura)) > 1e-6)
    stop("R04_fig24_conjuntos.csv: pct_en_figura distinto de Σ round(pct·10)/10 de F08.csv", call. = FALSE)
  out <- ref_tbl(bind_rows(
    tibble(kind = "row", grouping = "", label = combo[keep], value = f8$pct[keep], text = fmt_num(f8$pct[keep], 1), n = as.integer(f8$n[keep])),
    tibble(kind = "set", grouping = "", label = r4$necesidad, value = r4$pct_en_figura, text = fmt_num(r4$pct_en_figura, 1), n = NA_integer_)))
  moved <- sum(keep != seq_along(keep))
  res(out, "tabla_analisis", c("analisis adicionales/F08.csv", "analisis adicionales/R04_fig24_conjuntos.csv"),
      sprintf("%d intersecciones en el orden de ComplexUpset %s (%d fuera del orden de F08.csv) + %d conjuntos de arriba abajo; textos fmt_num(·, 1)",
              length(keep), packageVersion("ComplexUpset"), moved, nrow(r4)))
}

# Red (5): analisis adicionales/cooccurrence_network.svg (salida de network_graph.R por F01.R:46-50 con problemas no
# triviales): <circle> = nodos en el orden de cats, con <text> centrados ("Nombre" en 1-2 líneas y "(NN%)"); <line> =
# aristas en el orden de edges_all (i < j). Nodo: value = prevalencia impresa (entero), text "(NN%)", x, y =
# coordenadas de datos del layout de network_graph.R tras pad, min_dist y hstretch, sin renormalizar (x ≈ 0.5 ± 0.575,
# y 0-1; tol_xy 0.01; sin columna n): se invierte el lienzo de network_graph.R:273-297 (ggsave 320·1.15 ×
# 320 mm, scale 0.75 → pt) y coord_fixed(xlim = 0.5 ± 0.575, ylim = 0..1) con expansión 5 % y plot.margin 20 pt: el
# gráfico (rect de fondo, centrado) menos los márgenes es el panel; alto del panel = ancho · 1.1/1.265.
# Arista: value = lift invertido de stroke-width (linewidth = (lift − 0.8)·2.5, network_graph.R:212; svglite escribe
# linewidth · .pt · 0.75 pt, .pt = 72.27/25.4), tol 0.05; label "from|to". Comprobado contra una réplica sobre
# data/master.csv: |Δx|, |Δy| < 2e-4 y |Δlift| < 1e-3 (## Prueba).
ref_network <- function(spec) {
  f <- file.path(ADD, "cooccurrence_network.svg")
  if (!file.exists(f)) stop("No existe analisis adicionales/cooccurrence_network.svg", call. = FALSE)
  doc <- xml2::read_xml(f)
  find <- function(xp) xml2::xml_find_all(doc, xp)
  at <- function(nodes, a) num(xml2::xml_attr(nodes, a))
  svg_w <- num(sub("pt$", "", xml2::xml_attr(doc, "width")))
  circ <- find("//*[local-name()='circle']"); lin <- find("//*[local-name()='line']"); txt <- find("//*[local-name()='text']")
  rect <- find("//*[local-name()='rect'][@x]")
  rw <- at(rect, "width"); rx <- at(rect, "x")
  cand <- which(!is.na(rw) & rw < svg_w - 1)
  if (length(circ) == 0 || length(lin) == 0 || length(cand) == 0) stop("cooccurrence_network.svg: faltan circle, line o rect de fondo", call. = FALSE)
  k <- cand[which.max(rw[cand])]
  margin <- 20; x_half <- 0.5 * 1.15; expand <- 0.05                   # network_graph.R:196-198, 277-279; theme plot.margin
  xr <- 2 * x_half * (1 + 2 * expand); yr <- 1 + 2 * expand
  panel_x0 <- rx[k] + margin; panel_w <- rw[k] - 2 * margin; panel_y0 <- margin; panel_h <- panel_w * yr / xr
  x_min <- 0.5 - x_half - 2 * x_half * expand; y_max <- 1 + expand
  cx <- at(circ, "cx"); cy <- at(circ, "cy")
  tx <- at(txt, "x"); ty <- at(txt, "y"); tt <- xml2::xml_text(txt)
  nodes <- map_dfr(seq_along(cx), function(i) {
    sel <- which(abs(tx - cx[i]) < 0.01 & abs(ty - cy[i]) < 60)
    lab <- tt[sel][order(ty[sel])]
    pv <- lab[str_detect(lab, "^\\(\\d+%\\)$")]
    if (length(pv) != 1) stop("cooccurrence_network.svg: nodo ", i, " sin texto (NN%)", call. = FALSE)
    tibble(label = paste(lab[!str_detect(lab, "^\\(\\d+%\\)$")], collapse = " "), text = pv,
           value = num(str_extract(pv, "\\d+")),
           x = x_min + (cx[i] - panel_x0) / panel_w * xr, y = y_max - (cy[i] - panel_y0) / panel_h * yr)
  })
  if (anyDuplicated(nodes$label)) stop("cooccurrence_network.svg: etiquetas de nodo repetidas", call. = FALSE)
  x1 <- at(lin, "x1"); y1 <- at(lin, "y1"); x2 <- at(lin, "x2"); y2 <- at(lin, "y2")
  sw <- num(str_match(xml2::xml_attr(lin, "style"), "stroke-width: *([0-9.]+)")[, 2])
  near <- function(x, y) which.min((cx - x)^2 + (cy - y)^2)
  pt <- 72.27 / 25.4                                                     # ggplot2::.pt
  edges <- tibble(from = nodes$label[map2_int(x1, y1, near)], to = nodes$label[map2_int(x2, y2, near)],
                  value = sw / (pt * 0.75) / 2.5 + 0.8)
  out <- ref_tbl(bind_rows(
    tibble(kind = "node", grouping = "", label = nodes$label, value = nodes$value, text = nodes$text, text2 = "",
           x = nodes$x, y = nodes$y, tol = 0.6, tol_xy = 0.01),
    tibble(kind = "edge", grouping = "", label = paste(edges$from, edges$to, sep = "|"), value = edges$value, text = "",
           text2 = "", x = NA_real_, y = NA_real_, tol = 0.05, tol_xy = NA_real_)))
  obs <- sprintf("%d nodos + %d aristas (todas las parejas: %s); lift %.2f-%.2f; x, y del layout sin renormalizar (tol_xy 0.01)", nrow(nodes), nrow(edges),
                 if (nrow(edges) == choose(nrow(nodes), 2)) "sí" else "no", min(edges$value), max(edges$value))
  res(out, "svg", "analisis adicionales/cooccurrence_network.svg", obs)
}

BUILDERS <- list(grouped_bars = ref_standard, paired_bars = ref_standard, dumbbell = ref_standard, hbars = ref_standard,
                 columns = ref_standard, funnel = ref_funnel, sankey = ref_sankey, cards = ref_cards, scatter = ref_scatter,
                 network = ref_network, matrix = ref_matrix, severity_bars = ref_severity, upset = ref_upset)


## Medias UE (data4web) --------------------------------------------------------------------------------------------

# ids = columna eu_id del manifiesto en las figuras con metodo_ue == "data4web" (uno o varios separados por ";"), en el
# orden del manifiesto. corregido: antes se sacaban con una regex de la prosa de fuente_datos. Filas level == "eu"
# (country "European Union"), demographic == "Total Sample"; valor del CSV en fracción 0-1 → ×100; rp_time viene
# duplicado → distinct.
build_eu_values <- function() {
  if (!"eu_id" %in% names(specs[[1]])) stop("manifest/figures.csv sin columna eu_id (id de la media UE de cada figura)", call. = FALSE)
  d4w <- Filter(function(s) identical(s$metodo_ue, "data4web"), specs)
  ids_fig <- lapply(d4w, function(s) { x <- trimws(strsplit(coalesce(s$eu_id, ""), ";", fixed = TRUE)[[1]]); x[nzchar(x)] })
  sin_id <- vapply(ids_fig, length, 1L) == 0
  if (any(sin_id)) stop("manifest/figures.csv: metodo_ue data4web sin eu_id en las figuras ",
                        paste(vapply(d4w[sin_id], function(s) sprintf("%02d", s$fig), ""), collapse = ", "), call. = FALSE)
  ids <- unique(unlist(ids_fig))
  if (length(ids) == 0) stop("El manifiesto no declara ninguna media UE de data4web (metodo_ue data4web + eu_id)", call. = FALSE)
  d <- read_csv(P$data4web, col_select = c(country, level, id, demographic, value),
                col_types = cols(.default = col_character(), value = col_double()), show_col_types = FALSE, progress = FALSE)
  sel <- d %>% filter(level == "eu", demographic == "Total Sample", id %in% ids)
  dup_src <- sel %>% count(id) %>% filter(n > 1) %>% pull(id)
  eu <- sel %>% distinct(id, value)
  amb <- eu %>% count(id) %>% filter(n > 1) %>% pull(id)
  if (length(amb) > 0) stop("data4web: valores distintos para ", paste(amb, collapse = ", "), call. = FALSE)
  falta <- setdiff(ids, eu$id)
  if (length(falta) > 0) stop("data4web: no hay media UE para ", paste(falta, collapse = ", "), call. = FALSE)
  tibble(id = ids) %>% left_join(eu, by = "id") %>%
    mutate(value = value * 100, fuente = "data4web", archivo = cfg$data$data4web,
           md5 = unname(tools::md5sum(P$data4web)), fecha = format(file.mtime(P$data4web), "%Y-%m-%d"),
           nota = paste0("level = eu (European Union), demographic = Total Sample; valor del CSV (0-1) x 100",
                         if_else(id %in% dup_src, "; fila duplicada en el origen (distinct)", "")))
}


## PNG publicados a 96 dpi (para --stage compare) --------------------------------------------------------------------

# corregido: antes sips --resampleWidth (truncaba el alto: 1 px menos en 11 figuras) o, sin sips, un vecino más cercano
# en R que redondeaba distinto. Ahora read_png_rgb() + resample_to_dpi() de R/08_compare.R: fondo blanco, bloques 3 × 3
# y vecino más cercano hasta round(px · 96 / dpi), el mismo remuestreo que compare aplica a la figura nueva y el mismo
# resultado en macOS y Linux. dpi de origen: pHYs del PNG o cfg$layout$dpi. Devuelve c(ancho, alto) en px.
resample_png <- function(src, dst, dpi_out = COMPARE_DPI) {
  p <- read_png_rgb(src)
  out <- resample_to_dpi(p$img, if (is.na(p$dpi)) cfg$layout$dpi else p$dpi, dpi_out)
  png::writePNG(out, dst, dpi = dpi_out)
  c(dim(out)[2], dim(out)[1])
}


## Ejecución ---------------------------------------------------------------------------------------------------------

check_sources_country()                                                  # se detiene antes de escribir en reference/
dir.create(file.path(REF, "published"), recursive = TRUE, showWarnings = FALSE)
rows <- map_dfr(specs, function(spec) {
  t0 <- Sys.time()
  r <- tryCatch({
    b <- BUILDERS[[spec$tipo]]
    if (is.null(b)) stop("tipo sin constructor: ", spec$tipo, call. = FALSE)
    b(spec)
  }, error = function(e) res(NULL, "error", "", paste("revisar:", conditionMessage(e))))
  if (!is.null(r$df)) write_csv(r$df, file.path(REF, paste0(figure_stem(spec), ".csv")), na = "NA")
  tibble(fig = spec$fig, slug = spec$slug, tipo = spec$tipo, nivel = r$nivel, fuentes = paste(r$fuentes, collapse = "; "),
         n = if (is.null(r$df)) 0L else nrow(r$df),
         tol = if (is.null(r$df)) "" else paste(c(paste(format(sort(unique(r$df$tol))), collapse = " / "),
                                                  if ("tol2" %in% names(r$df)) paste("tol2", paste(unique(na.omit(r$df$tol2)), collapse = "/")),
                                                  if ("tol_xy" %in% names(r$df)) paste("tol_xy", paste(unique(na.omit(r$df$tol_xy)), collapse = "/"))),
                                                collapse = " · "),
         obs = r$obs, seg = secs(t0))
})

t_png <- Sys.time()
pngs <- map_dfr(specs, function(spec) {
  src <- file.path(PUB, paste0(figure_stem(spec), ".png"))
  dst <- file.path(REF, "published", paste0(figure_stem(spec), ".png"))
  if (!file.exists(src)) return(tibble(fig = spec$fig, archivo = basename(src), px300 = "", md5 = "", px96 = "sin publicada", w_ok = NA))
  d <- tryCatch(resample_png(src, dst), error = function(e) c(NA, NA))
  info <- attr(png::readPNG(src, info = TRUE), "info")
  tibble(fig = spec$fig, archivo = basename(src), px300 = paste(info$dim, collapse = "x"), md5 = unname(tools::md5sum(src)),
         px96 = if (anyNA(d)) "error" else paste(d, collapse = "x"), w_ok = !anyNA(d) && d[1] == round(spec$w_in * 96))
})
seg_png <- secs(t_png)

t_eu <- Sys.time()
eu <- build_eu_values()
write_csv(eu, file.path(REF, "eu_values.csv"), na = "NA")
seg_eu <- secs(t_eu)

## README ----------------------------------------------------------------------------------------------------------
md5_row <- function(f, etiqueta = basename(f)) {
  if (!file.exists(f)) return(sprintf("| %s | (no existe) | |", etiqueta))
  sprintf("| %s | %s | %s |", etiqueta, unname(tools::md5sum(f)), format(file.mtime(f), "%Y-%m-%d %H:%M"))
}
md_cell <- function(x) str_replace_all(coalesce(x, ""), "\\|", "\\\\|")
insumos <- c(
  "| Archivo | md5 | Fecha |", "|---|---|---|",
  md5_row(P$data4web, cfg$data$data4web),
  vapply(sort(list.files(SRC, pattern = "\\.csv$", full.names = TRUE)), function(f) md5_row(f, file.path("finales/datos", basename(f))), ""),
  vapply(c("A03.csv", "A04.csv", "A05.csv", "F01.csv", "F02.csv", "F03.csv", "F08.csv", "R04_fig24_conjuntos.csv", "cooccurrence_network.svg"),
         function(n) md5_row(file.path(ADD, n), file.path("analisis adicionales", n)), ""),
  md5_row(UPSET_SVG, paste0(basename(UPSET_SVG), " (consultado: sin <text>, glifos en trazos)")),
  md5_row(file.path(dirname(PUB), "00-INDICE.csv"), paste0(basename(dirname(PUB)), "/00-INDICE.csv (guarda de país: títulos)"))
)
tabla <- c(
  "| fig | slug | nivel | fuente(s) | filas | tol | observaciones |", "|---|---|---|---|---|---|---|",
  sprintf("| %02d | %s | %s | %s | %d | %s | %s |", rows$fig, rows$slug, rows$nivel, md_cell(rows$fuentes), rows$n, rows$tol, md_cell(rows$obs))
)
tabla_png <- c(
  "| fig | archivo (300 dpi) | px 300 dpi | px 96 dpi | md5 (300 dpi) |", "|---|---|---|---|---|",
  sprintf("| %02d | %s | %s | %s | %s |", pngs$fig, pngs$archivo, pngs$px300, pngs$px96, pngs$md5)
)
tabla_eu <- c(
  "| id | value | redondeado | nota |", "|---|---|---|---|",
  sprintf("| %s | %.4f | %d | %s |", eu$id, eu$value, round(eu$value), eu$nota)
)
revisar <- rows %>% filter(nivel %in% c("transcrito", "error"))
readme <- c(
  sprintf("# reference/%s — valores y fracciones publicados (verdad para `--stage verify`)", cfg$slug),
  "",
  sprintf("Generado por `tools/build_reference.R` el %s con %s (readr %s, dplyr %s, xml2 %s, png %s) para **%s** (%s).",
          format(Sys.time(), "%Y-%m-%d %H:%M"), R.version.string, packageVersion("readr"), packageVersion("dplyr"),
          packageVersion("xml2"), packageVersion("png"), cfg$country, cfg$report$reference_version %||% ""),
  "Nada de esta carpeta se edita a mano: se regenera con",
  "", "    Rscript src/src_reports/tools/build_reference.R", "",
  "con `reference_src_dir` (finales/datos), `published_dir` (figuras publicadas) y `path2EU` (data4web) en `config/local.yml`.",
  "",
  "## Convenciones (contratos §4.4 y §4.6)",
  "",
  "- `Figure_NN_slug.csv`: `order, kind, grouping, label, value, value2, text, text2, frac, frac2, tol` (+ `tol2` en la 3; `tol_xy, x, y` en la red; `lo, hi` en las tarjetas; `n` en el scatter y el upset). `text`/`text2` es exactamente lo impreso; `frac`/`frac2` la fracción efectivamente dibujada en la figura publicada (NA si no hay barra medible): barras = valor/escala de la pista (100, o 10 en la 4); embudos (8, 34) = barra/la más ancha; columnas (30, 37) = valor/vmax, con vmax el máximo de las dos series de la figura (`report_charts.R:259-266`).",
  "- Tolerancias: `tol` = de `value` por fila: 0.6 pp; 0.06 en escala 0-10 (fig 4); 0.05 en lifts (aristas de la 5). `tol2` = de `value2` (si falta o es NA, la `tol` de la fila): 0.06 en la severidad 0-10 de la 3. `tol_xy` = de `x` e `y` (si falta o es NA, 0.01): 0.01 en los nodos de la 5. `lo`, `hi` y `n`, si están, se comparan exactos.",
  "- Niveles: **texto_publicado** = CSV de `finales/datos` (valores y fracciones de la figura publicada; en las figuras con filas en `00-verificacion_valores.csv`, `value` es la columna `data` = valor impreso como número, y donde falta `v1_frac·escala`); en las tarjetas (32, 41, 42), valores de A03/A04/A05 que coinciden con `finales/datos` (2 decimales, |Δ| ≤ 0.005) y textos y deltas según `charts5.py`; **tabla_analisis** = tablas de `analisis adicionales` (F01, F02, F03, F08 + R04); **svg** = leído del SVG (`cooccurrence_network.svg`: coordenadas invertidas del lienzo de ggplot y lifts invertidos del ancho de trazo); **transcrito** = tarjetas sin `finales/datos` con que cotejarlas (textos según `charts5.py`, cotejados a ojo con el PNG) → **revisar**.",
  "- grouped_bars/dumbbell: `grouping` = \"\" en `avg`, nombre del grupo en `header` y en sus `row`. sankey: `grouping` = columna (\"1\", \"2\", \"3\"), `label` = clave del nodo de `sankey_def`. cards: `grouping` = fila impresa, `label` = columna impresa, `value2` = valor crudo de la tarjeta de referencia (NA en ella), `text2` = delta de valores redondeados con signo \"+\", \"−\" (U+2212) o \"±\" y sufijo \" points\" (\"\" en la tarjeta de referencia), `lo`, `hi` = min/max de round(value) de la figura. matrix: `grouping` = fila (\"People who also experienced\"), `label` = columna (\"Among people who experienced problems with\"), `text2` = banda. network: nodos con `x`, `y` = coordenadas de datos del layout de `network_graph.R` tras pad, min_dist y hstretch, sin renormalizar (x ≈ 0.5 ± 0.575, y 0-1; algunos nodos caen fuera de [0, 1]); `label` de arista = \"from|to\" en el orden de `edges_all` (i < j en `cats`); sin `n`. upset: `label` de intersección = sus conjuntos en el orden fijo (Access to Assistance, Access to Information, Resolution Process, Problem Resolved) unidos por \" + \"; intersecciones de izquierda a derecha como las dibuja ComplexUpset (`upset_data()` sobre réplicas round(pct·10): tamaño descendente con su desempate); conjuntos de arriba abajo como la matriz (orden fijo invertido). scatter: `grouping` = faceta, `value` = prevalencia, `value2` = severidad. severity_bars: `grouping` = serie (país | EU Average; el país primero), `label` = severidad.",
  "- `eu_values.csv`: medias UE de data4web (`level == \"eu\"`, `Total Sample`) de los ids de la columna `eu_id` del manifiesto en las figuras con `metodo_ue` data4web; `value` en 0-100 sin redondear; `rp_time` está duplicado en el origen (`distinct`).",
  "- `published/`: PNG publicados remuestreados a 96 dpi para `--stage compare` con `resample_to_dpi()` de `R/08_compare.R` (fondo blanco, bloques 3 × 3 y vecino más cercano hasta round(px · 96/300)), el mismo remuestreo que compare aplica a la figura nueva; mismo tamaño en macOS y Linux.",
  "- Guarda de país: antes de escribir en `reference/<slug>/`, la herramienta comprueba que las fuentes de `config/local.yml` son del país (carpeta `output/viz/<país>`, `<país>_jg_upset.svg`, columna del país en F03.csv, títulos de `00-INDICE.csv`) y si no se detiene.",
  "- Lectura de los CSV: `read_csv(f, na = \"NA\", col_types = cols(.default = col_character()))` y convertir a numérico `value, value2, frac, frac2, tol` (y `tol2, tol_xy, x, y, n, lo, hi`): con tipos automáticos las columnas vacías pasan a NA lógico y los `grouping` \"1\"/\"2\"/\"3\" a número. `\"\"` (vacío) y `NA` significan lo mismo en `text`/`value` (nada impreso / suprimido).",
  "",
  "## Figuras",
  "", tabla, "",
  if (nrow(revisar) > 0) c("### Revisar", "", sprintf("- Fig %02d (%s): %s", revisar$fig, revisar$slug, md_cell(sub("^revisar: ", "", revisar$obs))), "") else character(),
  "## Medias UE (`eu_values.csv`)",
  "", sprintf("Fuente: `%s` (md5 `%s`, fecha %s).", eu$archivo[1], eu$md5[1], eu$fecha[1]), "", tabla_eu, "",
  "## Figuras publicadas (`published/`)",
  "", sprintf("Ancho a 96 dpi = round(w_in·96) del manifiesto en %d de %d.", sum(pngs$w_ok, na.rm = TRUE), nrow(pngs)), "", tabla_png, "",
  "## Insumos",
  "", insumos, "",
  "## Tiempos",
  "",
  sprintf("- Tablas: %.1f s (%d figuras). PNG a 96 dpi: %.1f s. Medias UE: %.1f s. Total: %.1f s.", sum(rows$seg), nrow(rows), seg_png, seg_eu, secs(t_ini)),
  ""
)
writeLines(readme, file.path(REF, "README.md"), useBytes = FALSE)

## Consola: solo conteos y agregados -------------------------------------------------------------------------------
cat(sprintf("reference/%s · %s · %s\n", cfg$slug, cfg$country, R.version.string))
cat(sprintf("%-4s %-16s %-6s %-14s %s\n", "fig", "nivel", "filas", "tol", "fuente(s)"))
for (i in seq_len(nrow(rows))) cat(sprintf("%02d   %-16s %-6d %-14s %s\n", rows$fig[i], rows$nivel[i], rows$n[i], rows$tol[i], rows$fuentes[i]))
cat(sprintf("\nCSV escritos: %d de %d (%d filas en total); niveles: %s\n", sum(rows$n > 0), nrow(rows), sum(rows$n),
            paste(sprintf("%s %d", names(table(rows$nivel)), as.integer(table(rows$nivel))), collapse = ", ")))
cat(sprintf("PNG a 96 dpi: %d de %d (%.1f s)\n", sum(grepl("^[0-9]+x[0-9]+$", pngs$px96)), nrow(pngs), seg_png))
cat(sprintf("Medias UE (%d): %s (%.1f s)\n", nrow(eu), paste(sprintf("%s=%d", eu$id, round(eu$value)), collapse = ", "), seg_eu))
if (nrow(revisar) > 0) cat(sprintf("Revisar: %s\n", paste(sprintf("%02d", revisar$fig), collapse = ", ")))
cat(sprintf("README: %s · total %.1f s\n", file.path(REF, "README.md"), secs(t_ini)))


## Prueba ----------------------------------------------------------------------------------------------------------
## cd /tmp && Rscript <copia>/tools/build_reference.R (2026-10-23, revisión G5; scripts en SCRATCH/fix/G5/pruebas/)
## - Ejecución: ~10 s (PNG 96 dpi 5-7 s, medias UE 1.2 s); 42 CSV, 881 filas; niveles texto_publicado 37 (con 32, 41 y
##   42), tabla_analisis 4 (3, 6, 7, 29), svg 1 (5); sin "Revisar". Dos corridas y una copia limpia dan los 85 archivos
##   (43 CSV + 42 PNG) idénticos por md5.
## - Frente a ROOT/reference/hungary cambian 5 CSV (t_referencia.R): 3 (+ tol2 0.06 en sus 24 filas; valores iguales),
##   5 (+ tol_xy: 0.01 en los 12 nodos, NA en las 66 aristas; valores iguales), 29 (2 filas: posiciones 8 y 9
##   intercambiadas), 30 (4 filas) y 37 (3 filas) con frac/frac2 = v_frac/vmax; las otras 37 y eu_values.csv, iguales.
## - Fig 29: la combinación de cada columna leída del PNG publicado coincide en 15/15 (antes 13/15: 8 y 9); conjuntos de
##   arriba abajo = la matriz del PNG. Columnas 30/37: |altura medida en el PNG − frac| ≤ 0.0022 en 14/14 barras (antes
##   14/14 fuera de 0.006). published/: alto = round(h300·96/300) en 42/42 (antes 30/42) y ancho = round(w_in·96) en
##   42/42; --stage compare de 09-11 da tamaños iguales (fig 11: 806×496 | 806×496; antes la publicada medía 806×495).
## - Tarjetas (t_funciones.R): round(value, 2) = finales/datos en 10/12/9 tarjetas (|Δ| ≤ 0.0049) → texto_publicado; un
##   valor alterado en 0.01 → error de la figura; sin ese CSV → "transcrito" (revisar).
## - ref_severity con la UE antes que el país en la tabla larga: queda el país primero (antes, la UE). eu_values: ids de
##   eu_id = los 10 de ROOT (mismo orden y valores); data4web sin eu_id → error con la figura; una prosa con la forma
##   de la plantilla ya no pierde ids (antes: "El manifiesto no declara ningún eu_value(data4web, id)").
## - Guarda de país (t_guarda_pais.out): copia con config/malta.yml y las fuentes de Hungría → se detiene con las 4
##   causas y no crea reference/malta/ (antes escribía 44 archivos con los datos de Hungría). En una copia sin cache/ ni
##   output/ aparecen solo, vacías, output/hungary/{compare,figures,log,tables} y cache/, más reference/hungary/published/.
## - Red: x ∈ [−0.0145, 1.0189], y ∈ [0.0085, 0.9262] (2 nodos fuera de [0, 1]), sin columna n.
## - Sin cambios desde la prueba del 2026-10-22: value = data de 00-verificacion en 432 comparaciones (24 figuras); fig 9
##   = finales/datos; sankeys 11/16 = PUBLISHED_SANKEY; fig 6 con 12 "–" y 11 N/A; lifts de la red vs F01_lifts_texto
##   (desvío 0.0008) y coordenadas vs réplica de network_graph.R (1.4e-4); medias UE 49, 61, 40, 74, 54, 82, 72, 57, 57, 52.
