# Inyecciones de la revisión en las barras agrupadas (fig 01), portadas a la estructura de capas de wjp_groupbars 1.2.0
# en modo fila (M1, 2026-10-27; origen: fix5/bancos/iny_gb.R): se añade al final de R/09_wjpr.R de la copia y se quita
# después (iny_run.sh, md5). Cambian solo el dibujo (df intacto). INY = nombre de la inyección.
# Capas: GeomRect pista (fill = pista), GeomRect barra (fill = principal), GeomText valores (datos con x) y GeomText
# etiquetas (datos con face). Mismo error semántico que las de 1.1.0: dos etiquetas impresas intercambiadas (la línea
# de WJPr no cambia: se cambia el texto que imprime la capa), un encabezado impreso cambiado, un valor con alfa 0, otra
# fuente en los valores, la barra de Male +5 pp; nuevas: sin la pista de Male, Male y Female cambiadas de línea.

.iny <- Sys.getenv("INY")
.gb_orig <- wjpr_grouped_bars
.gb_which <- function(p, role) {
  is <- function(l, g) inherits(l$geom, g)
  k <- which(vapply(p$layers, function(l) switch(role,
    labels = is(l, "GeomText") && "face" %in% names(l$data),
    values = is(l, "GeomText") && "x" %in% names(l$data),
    bars   = is(l, "GeomRect") && identical(toupper(l$aes_params$fill %||% ""), toupper(STD$col$main)),
    tracks = is(l, "GeomRect") && identical(toupper(l$aes_params$fill %||% ""), toupper(STD$col$track))), logical(1)))
  if (length(k) != 1) stop("inyección: capa ", role, " no encontrada (", length(k), ")")
  k
}
# copia de la capa con ggproto(NULL, old): el padre se toma de una variable que no se reasigna (ggproto evalúa
# `_inherit` tarde; con ggproto(NULL, p$layers[[k]]) y luego p$layers[[k]] <- la copia, la capa es su propio padre y
# ggplot_build desborda la pila)
.gb_edit <- function(p, role, f) {
  k <- .gb_which(p, role); ls <- p$layers; old <- ls[[k]]; force(old)
  ls[[k]] <- f(ggplot2::ggproto(NULL, old)); p$layers <- ls; p
}
.gb_map <- function(l, ...) {             # la capa con estéticas cambiadas o añadidas (quosures)
  m <- as.list(l$mapping); new <- list(...); m[names(new)] <- new
  l$mapping <- ggplot2::aes(!!!m); l
}
.gb_swap <- function(p, grp, a, b) .gb_edit(p, "labels", function(l) .gb_map(l, label = rlang::quo(
  ifelse(group == !!grp & label == !!a, !!b, ifelse(group == !!grp & label == !!b, !!a, label)))))

wjpr_grouped_bars <- function(df, spec, ...) {
  p <- .gb_orig(df, spec, ...)
  a <- attr(p, "wjpr")
  if (.iny == "male_female") p <- .gb_swap(p, "Gender", "Male", "Female")
  if (.iny == "edad") p <- .gb_swap(p, "Age Group", "65-100", "18-24")
  if (.iny == "area") p <- .gb_swap(p, "Area of Residence", "Urban", "Rural")
  if (.iny == "encabezado") p <- .gb_edit(p, "labels", function(l) .gb_map(l, label = rlang::quo(
    ifelse(kind == "header" & label == "Gender", "Sex", label))))
  if (.iny == "valor_oculto") p <- .gb_edit(p, "values", function(l) .gb_map(l, alpha = rlang::quo(
    I(ifelse(group == "Gender" & label == "Male", 0, 1)))))
  if (.iny == "fuente") p <- .gb_edit(p, "values", function(l) { l$aes_params$family <- "Helvetica"; l })
  # barra de Male +5 pp (solo la geometría: el texto sigue diciendo 65%) → estado "dibujo"
  if (.iny == "barra") p <- .gb_edit(p, "bars", function(l) {
    l$data <- transform(l$data, value = ifelse(group == "Gender" & label == "Male", value + 5, value)); l })
  # nuevas (M1): sin la pista de Male (marca fija) → "dibujo"; Male y Female cambiadas de línea en todas las capas
  if (.iny == "sin_pista") p <- .gb_edit(p, "tracks", function(l) {
    l$data <- l$data[!(l$data$group == "Gender" & l$data$label == "Male"), , drop = FALSE]; l })
  if (.iny == "orden") {
    ym <- function(d) { i <- d$group == "Gender" & d$label == "Male"; j <- d$group == "Gender" & d$label == "Female"; list(i = i, j = j) }
    for (role in c("tracks", "bars", "values", "labels")) p <- .gb_edit(p, role, function(l) {
      d <- l$data; s <- ym(d); dy <- d$y[s$j][1] - d$y[s$i][1]
      for (cc in intersect(c("y", "ymin", "ymax"), names(d))) { d[[cc]][s$i] <- d[[cc]][s$i] + dy; d[[cc]][s$j] <- d[[cc]][s$j] - dy }
      l$data <- d; l })
  }
  attr(p, "wjpr") <- a
  p
}
