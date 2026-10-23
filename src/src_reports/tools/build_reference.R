## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## tools/build_reference.R — construye reference/<slug>/ (la verdad que usa --stage verify) desde lo publicado
##
## Produce, para el país de config/<pais>.yml:
##   reference/<slug>/Figure_NN_slug.csv   una por figura del manifiesto (contratos §4.4/§4.6): order, kind, grouping,
##                                         label, value, value2, text, text2, frac, frac2, tol (+ x, y en la red;
##                                         lo, hi en las tarjetas; n en el scatter y el upset)
##   reference/<slug>/eu_values.csv        medias UE de data4web: id, value (0-100), fuente, archivo, md5, fecha, nota
##   reference/<slug>/published/*.png      figuras publicadas remuestreadas a 96 dpi (ancho round(w_in·96) px)
##   reference/<slug>/README.md            nivel de cada figura, fuentes, md5 y fecha de los insumos, cómo regenerar
##
## Insumos (rutas de la máquina en config/local.yml; no se modifica nada fuera de reference/<slug>/):
##   reference_src_dir = …/output/viz/<pais>/finales/datos   37 CSV publicados + 00-verificacion_valores.csv
##   published_dir     = …/output/finales/<Pais>/figuras      Figure_NN_slug.png (300 dpi)
##   <viz> = dirname(dirname(reference_src_dir)): "<viz>/analisis adicionales/" (A03, A04, A05, F01, F02, F03, F08,
##           R04_fig24_conjuntos, cooccurrence_network.svg) y "<viz>/<country>_jg_upset.svg" (solo se registra)
##   path2EU + data.data4web = data4web_gpp.csv (medias UE; se lee con col_select, 1-2 s)
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
dir.create(file.path(REF, "published"), recursive = TRUE, showWarnings = FALSE)
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

# Tabla de referencia (§4.6): columnas fijas en orden y tipos; extras x, y, n, lo, hi al final; order = 1..n
REF_COLS <- c("order", "kind", "grouping", "label", "value", "value2", "text", "text2", "frac", "frac2", "tol")
ref_tbl <- function(d, tol = 0.6) {
  if (!"order" %in% names(d)) d$order <- seq_len(nrow(d))
  for (k in c("grouping", "text", "text2")) if (!k %in% names(d)) d[[k]] <- ""
  for (k in c("value2", "frac", "frac2")) if (!k %in% names(d)) d[[k]] <- NA_real_
  if (!"tol" %in% names(d)) d$tol <- tol
  d <- d %>% mutate(order = as.integer(order),
                    across(c(kind, grouping, label, text, text2), ~ coalesce(as.character(.x), "")),
                    across(c(value, value2, frac, frac2, tol), as.numeric))
  extras <- intersect(c("x", "y", "n", "lo", "hi"), names(d))
  d <- d[, c(REF_COLS, extras)]
  if (nrow(d) == 0 || !identical(d$order, seq_len(nrow(d)))) stop("order debe ser 1..n", call. = FALSE)
  d
}
res <- function(df, nivel, fuentes, obs = "") list(df = df, nivel = nivel, fuentes = fuentes, obs = obs)

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
# text/text2 = v1_text/v2_text tal cual ("61%", "N/A", "6.0", ""); frac/frac2 = v1_frac/v2_frac.
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
  out <- d %>%
    transmute(order, kind, grouping, label, value = coalesce(d1, v1_frac * scale), value2 = coalesce(d2, v2_frac * scale),
              text = v1_text, text2 = v2_text, frac = v1_frac, frac2 = v2_frac) %>%
    ref_tbl(tol = if (pct) 0.6 else 0.06)
  res(out, "texto_publicado",
      c(file.path("finales/datos", basename(f)), if (n_ver > 0) "finales/datos/00-verificacion_valores.csv"),
      if (n_ver > 0) sprintf("%d valores de 00-verificacion (|data − frac·%d| máx %.2f); resto v1_frac·%d", n_ver, scale, gap, scale)
      else sprintf("sin filas en 00-verificacion: value = v1_frac·%d", scale))
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
# color de charts5.py:160; el manifiesto prevé una escala común 41/42 en cards_scale(ctx), aquí va la publicada).
# Transcripción cotejada a ojo con los PNG publicados el 2026-10-22: marcada "revisar" en el README.
ACTORS <- c("Judges", "Prosecutors", "Public defense attorneys")
# origen: standard_figures.R:184-204 (scorecard_def), con add() → analisis adicionales/
scorecard_def <- function(which) {
  switch(which,
    hardship_outcome = {
      a3 <- read_add("A03.csv") %>% filter(corte == "Resolución")
      hs <- c("At least one hardship", "Health issues", "Economic issues", "Relationship breakdown", "Alcohol/Drug problems")
      list(rows = c("At least one hardship", "Health issues", "Economic issues", "Relationship breakdown", "Alcohol or drug problems"),
           cols = c("Fully resolved", "Not resolved"), ref = "col",
           vals = t(sapply(hs, function(h) sapply(c("Fully resolved", "Not resolved"), function(g) a3$pct[a3$desenlace == h & a3$grupo == g]))))
    },
    exposure = {
      a4 <- read_add("A04.csv")
      g <- c("No non-trivial problem", "Problem, did not need a DRM", "Needed a DRM, had access", "Needed a DRM, no access")
      list(rows = c("Without a non-trivial problem", g[-1]), cols = ACTORS, ref = "row",
           vals = t(sapply(g, function(gr) sapply(ACTORS, function(a) a4$pct[a4$actor == a & a4$grupo == gr]))))
    },
    hardship = {
      a5 <- read_add("A05.csv") %>% filter(corte == "Escalera con hardship")
      g <- c("No non-trivial problem", "Problem, no hardship", "Problem, with hardship")
      list(rows = c("Without a non-trivial problem", g[-1]), cols = ACTORS, ref = "row",
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
  res(out, "transcrito", c(file.path("analisis adicionales", CARDS_SRC[[which]]), paste0("published/", figure_stem(spec), ".png")),
      sprintf("revisar: %d tarjetas; textos y deltas según charts5.py (delta de valores redondeados), cotejados a ojo con el PNG; lo/hi = %d/%d", nrow(out), lo, hi))
}

# Dispersión (3): F02.csv (F02.R:14-25 category_profile) filas corte == "Area of Residence". grouping = faceta (Rural,
# Urban), label = categoría, value = prevalencia (x; tol 0.6), value2 = severidad media 0-10 (y; tol 0.06, no cabe en la
# columna tol), n = encuestados con problema no trivial (punto hueco si n < 30; sin severidad → el punto no se dibuja).
# Sin textos. Orden: faceta Rural y luego Urban; dentro, categorías en el orden de F02.csv (alfabético).
ref_scatter <- function(spec) {
  f <- read_add("F02.csv") %>% filter(corte == "Area of Residence")
  need <- c("grupo", "category", "prevalencia", "severidad_media", "n_con_problema_nt")
  if (!all(need %in% names(f)) || nrow(f) == 0) stop("F02.csv: faltan columnas o filas de Area of Residence", call. = FALSE)
  f <- f %>% mutate(.g = match(grupo, c("Rural", "Urban"))) %>% arrange(.g)
  out <- ref_tbl(tibble(kind = "point", grouping = f$grupo, label = f$category, value = f$prevalencia,
                        value2 = f$severidad_media, text = "", text2 = "", n = f$n_con_problema_nt))
  res(out, "tabla_analisis", "analisis adicionales/F02.csv",
      sprintf("%d puntos; %d sin severidad (value2 NA = suprimido); value2 requiere tol 0.06", nrow(out), sum(is.na(out$value2))))
}

# Barras de severidad (7): F03.csv (F03.R:3-18; columnas sev, <país>, EU Average): % de problemas por severidad 0-10.
# kind "bar", grouping = serie (<país> | "EU Average"), label = severidad "0".."10"; sin textos. Orden: por severidad,
# el país antes que la UE (orden de dodge).
ref_severity <- function(spec) {
  f <- read_add("F03.csv")
  series <- c(cfg$country, "EU Average")
  if (!all(c("sev", series) %in% names(f))) stop("F03.csv: se esperaban columnas sev, ", cfg$country, ", EU Average", call. = FALSE)
  long <- f %>% select(sev, all_of(series)) %>% pivot_longer(-sev, names_to = "series", values_to = "value") %>%
    mutate(.s = match(series, series)) %>% arrange(sev, .s)
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
# y R04_fig24_conjuntos.csv (pct_en_figura = Σ round(pct·10)/10 por conjunto). kind "row" = barra de intersección
# (label = combinacion de F08.csv, orden del archivo = pct descendente, de izquierda a derecha), kind "set" = conjunto
# (de arriba abajo como en la figura: tamaño ascendente). text = fmt_num(value, 1); n = casos sin ponderar.
# <country>_jg_upset.svg no conserva <text> (glifos en trazos): los textos se forman con fmt_num y se cotejaron con el PNG.
ref_upset <- function(spec) {
  f8 <- read_add("F08.csv")
  r4 <- read_add("R04_fig24_conjuntos.csv") %>% arrange(pct_en_figura)
  if (!all(c("combinacion", "pct", "n") %in% names(f8)) || !all(c("necesidad", "pct_en_figura") %in% names(r4)))
    stop("F08.csv / R04_fig24_conjuntos.csv: columnas inesperadas", call. = FALSE)
  out <- ref_tbl(bind_rows(
    tibble(kind = "row", grouping = "", label = f8$combinacion, value = f8$pct, text = fmt_num(f8$pct, 1), n = as.integer(f8$n)),
    tibble(kind = "set", grouping = "", label = r4$necesidad, value = r4$pct_en_figura, text = fmt_num(r4$pct_en_figura, 1), n = NA_integer_)))
  res(out, "tabla_analisis", c("analisis adicionales/F08.csv", "analisis adicionales/R04_fig24_conjuntos.csv"),
      sprintf("%d intersecciones + %d conjuntos; textos fmt_num(·, 1)", nrow(f8), nrow(r4)))
}

# Red (5): analisis adicionales/cooccurrence_network.svg (salida de network_graph.R por F01.R:46-50 con problemas no
# triviales): <circle> = nodos en el orden de cats, con <text> centrados ("Nombre" en 1-2 líneas y "(NN%)"); <line> =
# aristas en el orden de edges_all (i < j). Nodo: value = prevalencia impresa (entero), text "(NN%)", x, y =
# coordenadas de datos del layout (tol 0.01): se invierte el lienzo de network_graph.R:273-297 (ggsave 320·1.15 ×
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
           x = nodes$x, y = nodes$y, tol = 0.6),
    tibble(kind = "edge", grouping = "", label = paste(edges$from, edges$to, sep = "|"), value = edges$value, text = "",
           text2 = "", x = NA_real_, y = NA_real_, tol = 0.05)))
  obs <- sprintf("%d nodos + %d aristas (todas las parejas: %s); lift %.2f-%.2f; x, y con tol 0.01", nrow(nodes), nrow(edges),
                 if (nrow(edges) == choose(nrow(nodes), 2)) "sí" else "no", min(edges$value), max(edges$value))
  res(out, "svg", "analisis adicionales/cooccurrence_network.svg", obs)
}

BUILDERS <- list(grouped_bars = ref_standard, paired_bars = ref_standard, dumbbell = ref_standard, hbars = ref_standard,
                 columns = ref_standard, funnel = ref_funnel, sankey = ref_sankey, cards = ref_cards, scatter = ref_scatter,
                 network = ref_network, matrix = ref_matrix, severity_bars = ref_severity, upset = ref_upset)


## Medias UE (data4web) --------------------------------------------------------------------------------------------

# ids = los eu_value(data4web, <id>) declarados en fuente_datos del manifiesto. Filas level == "eu" (country "European
# Union"), demographic == "Total Sample"; valor del CSV en fracción 0-1 → ×100; rp_time viene duplicado → distinct.
build_eu_values <- function() {
  ids <- unique(unlist(lapply(specs, function(s) str_match_all(s$fuente_datos, "eu_value\\(data4web, *([A-Za-z0-9_]+)\\)")[[1]][, 2])))
  if (length(ids) == 0) stop("El manifiesto no declara ningún eu_value(data4web, id)", call. = FALSE)
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

# ancho round(w_in·96) px; sips (macOS) o, si no hay, vecino más cercano en R. Devuelve c(ancho, alto) en px.
resample_png <- function(src, dst, w_px) {
  ok <- FALSE
  if (nzchar(Sys.which("sips"))) {
    r <- suppressWarnings(system2("sips", c("--resampleWidth", w_px, "-s", "dpiWidth", "96", "-s", "dpiHeight", "96",
                                            shQuote(src), "--out", shQuote(dst)), stdout = FALSE, stderr = FALSE))
    ok <- identical(r, 0L) && file.exists(dst)
  }
  if (!ok) {
    img <- png::readPNG(src)
    h_px <- round(dim(img)[1] * w_px / dim(img)[2])
    ri <- round(seq(1, dim(img)[1], length.out = h_px)); ci <- round(seq(1, dim(img)[2], length.out = w_px))
    out <- if (length(dim(img)) == 3) img[ri, ci, , drop = FALSE] else img[ri, ci, drop = FALSE]
    png::writePNG(out, dst, dpi = 96)
  }
  rev(dim(png::readPNG(dst))[1:2])
}


## Ejecución ---------------------------------------------------------------------------------------------------------

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
         tol = if (is.null(r$df)) "" else paste(format(sort(unique(r$df$tol))), collapse = " / "),
         obs = r$obs, seg = secs(t0))
})

t_png <- Sys.time()
pngs <- map_dfr(specs, function(spec) {
  src <- file.path(PUB, paste0(figure_stem(spec), ".png"))
  dst <- file.path(REF, "published", paste0(figure_stem(spec), ".png"))
  if (!file.exists(src)) return(tibble(fig = spec$fig, archivo = basename(src), px300 = "", md5 = "", px96 = "sin publicada"))
  d <- tryCatch(resample_png(src, dst, round(spec$w_in * 96)), error = function(e) c(NA, NA))
  info <- attr(png::readPNG(src, info = TRUE), "info")
  tibble(fig = spec$fig, archivo = basename(src), px300 = paste(info$dim, collapse = "x"), md5 = unname(tools::md5sum(src)),
         px96 = if (anyNA(d)) "error" else paste(d, collapse = "x"))
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
  md5_row(UPSET_SVG, paste0(basename(UPSET_SVG), " (consultado: sin <text>, glifos en trazos)"))
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
  "- `Figure_NN_slug.csv`: `order, kind, grouping, label, value, value2, text, text2, frac, frac2, tol` (+ `x, y` en la red, `lo, hi` en las tarjetas, `n` en el scatter y el upset). `text`/`text2` es exactamente lo impreso; `frac`/`frac2` la fracción de barra publicada (NA si no hay barra medible).",
  "- `tol` = tolerancia de `value` por fila: 0.6 pp; 0.06 en escala 0-10 (fig 4); 0.05 en lifts (aristas de la 5); `x`, `y` de la red: 0.01 (no cabe en `tol`); `value2` de la 3 (severidad 0-10): 0.06.",
  "- Niveles: **texto_publicado** = CSV de `finales/datos` (valores y fracciones de la figura publicada; en las figuras con filas en `00-verificacion_valores.csv`, `value` es la columna `data` = valor impreso como número, y donde falta `v1_frac·escala`); **tabla_analisis** = tablas de `analisis adicionales` (F01, F02, F03, F08 + R04); **svg** = leído del SVG (`cooccurrence_network.svg`: coordenadas invertidas del lienzo de ggplot y lifts invertidos del ancho de trazo); **transcrito** = valores de A03/A04/A05 con los textos reconstruidos según `charts5.py` y cotejados a ojo con el PNG publicado → **revisar**.",
  "- grouped_bars/dumbbell: `grouping` = \"\" en `avg`, nombre del grupo en `header` y en sus `row`. sankey: `grouping` = columna (\"1\", \"2\", \"3\"), `label` = clave del nodo de `sankey_def`. cards: `grouping` = fila impresa, `label` = columna impresa, `value2` = valor crudo de la tarjeta de referencia (NA en ella), `text2` = delta de valores redondeados con signo U+2212 y sufijo \" points\". matrix: `grouping` = fila (\"People who also experienced\"), `label` = columna (\"Among people who experienced problems with\"), `text2` = banda. network: `label` de arista = \"from|to\" en el orden de `edges_all` (i < j en `cats`). upset: `label` = `combinacion` de F08.csv (\" + \"), conjuntos de arriba abajo (tamaño ascendente). scatter: `grouping` = faceta, `value` = prevalencia, `value2` = severidad. severity_bars: `grouping` = serie (país | EU Average), `label` = severidad.",
  "- `eu_values.csv`: medias UE de data4web (`level == \"eu\"`, `Total Sample`), `value` en 0-100 sin redondear; `rp_time` está duplicado en el origen (`distinct`).",
  "- `published/`: PNG publicados remuestreados a 96 dpi (ancho `round(w_in·96)` px) para `--stage compare`.",
  "- Lectura de los CSV: `read_csv(f, na = \"NA\", col_types = cols(.default = col_character()))` y convertir a numérico `value, value2, frac, frac2, tol` (y `x, y, n, lo, hi`): con tipos automáticos las columnas vacías pasan a NA lógico y los `grouping` \"1\"/\"2\"/\"3\" a número. `\"\"` (vacío) y `NA` significan lo mismo en `text`/`value` (nada impreso / suprimido).",
  "",
  "## Figuras",
  "", tabla, "",
  if (nrow(revisar) > 0) c("### Revisar", "", sprintf("- Fig %02d (%s): %s", revisar$fig, revisar$slug, md_cell(sub("^revisar: ", "", revisar$obs))), "") else character(),
  "## Medias UE (`eu_values.csv`)",
  "", sprintf("Fuente: `%s` (md5 `%s`, fecha %s).", eu$archivo[1], eu$md5[1], eu$fecha[1]), "", tabla_eu, "",
  "## Figuras publicadas (`published/`)",
  "", tabla_png, "",
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
## cd /tmp && Rscript --vanilla SCRATCH/agente_H/prueba.R  (ejecuta esta herramienta y comprueba reference/hungary/; 2026-10-22)
## - Ejecución: ~8 s (tablas 0.2 s, PNG 96 dpi 4.7 s, medias UE 1.3 s); 42 CSV, 881 filas; niveles texto_publicado 34,
##   tabla_analisis 4 (3, 6, 7, 29), svg 1 (5), transcrito 3 (32, 41, 42 → revisar); 42 PNG en published/ (583-806 px, 96 dpi).
## - Columnas y order 1..n en los 42 CSV; filas por figura = las esperadas (fig 9: 36; 5: 12 nodos + 66 aristas; 6: 144;
##   29: 15 + 4; 32/41/42: 10/12/9 tarjetas). Fig 9 reproduce etiquetas, textos, kind y orden alfabético inverso de
##   finales/datos (61% National, 60% Male …, 4 N/A) con grouping inferido de los headers.
## - value == columna data de 00-verificacion_valores.csv en las 432 comparaciones (24 figuras); sin filas allí,
##   value = v1_frac·100 (fig 13: 54.3166) o ·10 (fig 4: 4.9, tol 0.06).
## - Sankeys 11/16 = PUBLISHED_SANKEY (…, 33% / …, 56%); embudos 8/34 con frac 1 en la primera etapa; fig 3: 24 puntos,
##   3 sin severidad; fig 7: 22 barras; fig 6: 12 "–" en la diagonal y 11 N/A (columna Law Enforcement), 62% en
##   (Public Services, Consumer) con banda "50% or more"; fig 29: textos 16.9% … 0.2% y conjuntos 36.5/55.6/63.3/71.2%.
## - Tarjetas: textos y deltas iguales a la transcripción a ojo de los PNG publicados ("+16 points", "\u22123 points" …);
##   lo/hi 2/62, 29/55, 30/55.
## - Red: prevalencias de los nodos = textos de la fig 2; lifts vs F01_lifts_texto.csv (11 pares) desvío máx 0.0008
##   (tol 0.05); coordenadas vs réplica de network_graph.R sobre data/master.csv (explora_red.R) desvío máx 1.4e-4 (tol 0.01).
## - eu_values.csv: 10 ids, redondeados 49, 61, 40, 74, 54, 82, 72, 57, 57, 52; rp_time duplicado en el origen (distinct).
## - Determinismo: los 43 CSV son idénticos (md5) entre dos corridas. README con secciones Figuras, Revisar (32, 41, 42),
##   Medias UE, Figuras publicadas, Insumos (md5 y fecha) y Tiempos.
