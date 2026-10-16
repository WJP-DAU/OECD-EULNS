## Content of the bar figures of the final report ------------------------------------------------------------
## Sourced by build_finales.R. The published figure is the source of truth: its SVG (versioned in
## src/hungary/finales/svg, or the 9-Jul pipeline SVG when the report only has the text as outlines) gives
## the order, the groups, the labels, the printed values and the bar lengths. report_charts.R redraws
## exactly that content with the standard layout. check_spec() compares the printed values and the bar
## lengths with the pipeline tables (output/tabs/csv), the EU averages (data4web) or the CSVs of the
## additional analyses, and reports every printed value it could not match.

suppressWarnings(suppressMessages(library(tidyverse)))

# Texts, rects and circles of an SVG, with their style resolved (CSS style first, then attributes)
read_svg_items <- function(path) {
  doc <- xml2::read_xml(path)
  bad <- xml2::xml_find_all(doc, "//*[@transform][.//*[local-name()='text' or local-name()='rect' or local-name()='circle']]")
  if (length(bad) > 0) stop("read_svg_items: grupos con transform en ", path, call. = FALSE)
  prop <- function(nodes, p) {
    st <- xml2::xml_attr(nodes, "style")
    css <- str_match(st, paste0("(?:^|;)\\s*", p, "\\s*:\\s*([^;]+)"))[, 2]
    coalesce(str_trim(css), xml2::xml_attr(nodes, p))
  }
  num <- function(v) suppressWarnings(as.numeric(sub("px$", "", v)))
  tx <- xml2::xml_find_all(doc, "//*[local-name()='text']")
  rc <- xml2::xml_find_all(doc, "//*[local-name()='rect']")
  ci <- xml2::xml_find_all(doc, "//*[local-name()='circle']")
  list(
    texts = tibble(t = str_squish(xml2::xml_text(tx)), x = num(xml2::xml_attr(tx, "x")), y = num(xml2::xml_attr(tx, "y")),
                   size = num(prop(tx, "font-size")), fill = toupper(prop(tx, "fill")), weight = prop(tx, "font-weight"),
                   anchor = coalesce(prop(tx, "text-anchor"), "start")),
    rects = tibble(x = num(xml2::xml_attr(rc, "x")), y = num(xml2::xml_attr(rc, "y")), w = num(xml2::xml_attr(rc, "width")),
                   h = num(xml2::xml_attr(rc, "height")), fill = toupper(prop(rc, "fill"))),
    circles = tibble(cx = num(xml2::xml_attr(ci, "cx")), cy = num(xml2::xml_attr(ci, "cy")), fill = toupper(prop(ci, "fill")))
  )
}

MAIN <- "#575796"; TRACK <- "#E5E8E8"; TEXT <- "#1A1A1A"; EU <- "#A8A8C8"; EXP <- "#B200AA"
VALUE_RE <- "^(\\d+(\\.\\d+)?%?|N/A)$"

# Words of a multi-word label placed one by one (ggtext) -> one string, by line
join_words <- function(d) {
  d %>% mutate(line = round(y)) %>% group_by(line) %>% arrange(x, .by_group = TRUE) %>%
    summarise(label = paste(t[t != ""], collapse = " "), y = mean(y), .groups = "drop")
}

## gen_grouped_bars() -----------------------------------------------------------------------------------------
# axis_max: for the figures without grey track (perc = FALSE, Figure 4), the value of a full bar; the
# bar lengths then come from the source geometry (points per unit = median of bar width / printed value)
parse_grouped <- function(path, axis_max = NULL) {
  it <- read_svg_items(path)
  tx <- it$texts %>% filter(t != "")
  vals <- tx %>% filter(str_detect(t, VALUE_RE), fill == MAIN) %>% arrange(y)
  other <- tx %>% anti_join(vals, by = c("t", "x", "y"))
  bars <- it$rects %>% filter(fill %in% c(MAIN, TRACK))
  span <- c(min(bars$x), max(bars$x + bars$w))
  near <- function(y_v, d, tol = 3) d %>% filter(abs(y - y_v) < tol)
  rows <- map_dfr(seq_len(nrow(vals)), function(i) {
    yv <- vals$y[i]
    lab <- near(yv, other)
    black <- lab %>% filter(fill == TEXT)
    purple <- lab %>% filter(fill == MAIN)
    kind <- if (nrow(black) == 0 && nrow(purple) > 0) "avg" else "row"
    label <- if (kind == "avg") join_words(purple)$label else paste(black$t, collapse = " ")
    b <- bars %>% filter(fill == MAIN, y - 2 <= yv, yv <= y + h + 3)
    if (nrow(b) > 1) stop("parse_grouped: más de una barra en la fila «", label, "» de ", path, call. = FALSE)
    frac <- if (vals$t[i] == "N/A") NA_real_ else if (nrow(b) == 0) 0 else b$w / diff(span)
    tibble(y = yv, kind = kind, label = label, v1_frac = frac, v1_text = vals$t[i], bar_w = if (nrow(b)) b$w else NA_real_)
  })
  if (!is.null(axis_max)) {
    k <- median(rows$bar_w / as.numeric(sub("%$", "", rows$v1_text)), na.rm = TRUE)
    rows <- rows %>% mutate(v1_frac = bar_w / (k * axis_max))
  }
  rows <- rows %>% select(-bar_w)
  headers <- other %>% filter(fill == MAIN) %>% filter(map_lgl(y, ~ all(abs(.x - vals$y) >= 3)))
  headers <- if (nrow(headers) == 0) NULL else join_words(headers) %>% transmute(y, kind = "header", label)
  bind_rows(rows, headers) %>% arrange(y) %>% mutate(order = row_number(), v2_frac = NA_real_, v2_text = NA_character_) %>%
    select(order, kind, label, v1_frac, v1_text, v2_frac, v2_text)
}

## Paired bars (src/hungary/viz_helpers.R paired_bars) --------------------------------------------------------
parse_paired <- function(path) {
  it <- read_svg_items(path)
  tx <- it$texts %>% filter(t != "")
  vx <- max(tx$x[str_detect(tx$t, VALUE_RE)])
  vals <- tx %>% filter(str_detect(t, VALUE_RE), abs(x - vx) < 1) %>% arrange(y)
  labs <- tx %>% filter(anchor == "end", !str_detect(t, VALUE_RE)) %>% arrange(y)
  legend <- tx %>% filter(anchor != "end", !str_detect(t, VALUE_RE)) %>% arrange(x)
  # legend swatches: squares above the first label
  swatch <- function(d) abs(d$w - d$h) < 0.5 & d$y < min(labs$y) - 5
  hu <- it$rects %>% filter(fill == MAIN) %>% filter(!swatch(.)) %>% arrange(y)
  eu <- it$rects %>% filter(fill == EU) %>% filter(!swatch(.)) %>% arrange(y)
  track <- it$rects %>% filter(fill == TRACK)
  span <- diff(c(min(track$x), max(track$x + track$w)))
  stopifnot("paired: one HU and one EU bar per label" = nrow(hu) == nrow(labs) && nrow(eu) == nrow(labs),
            "paired: two values per label" = nrow(vals) == 2 * nrow(labs))
  spec <- tibble(order = seq_len(nrow(labs)), kind = "row", label = labs$t,
                 v1_frac = hu$w / span, v1_text = vals$t[seq(1, nrow(vals), 2)],
                 v2_frac = eu$w / span, v2_text = vals$t[seq(2, nrow(vals), 2)])
  attr(spec, "legend") <- legend$t
  spec
}

## Dumbbells (src/hungary/viz_helpers.R report_dumbbell) ------------------------------------------------------
# Rows are anchored on their labels (a row can have no dot when both values are suppressed)
parse_dumbbell <- function(path) {
  it <- read_svg_items(path)
  tx <- it$texts %>% filter(t != "")
  ticks <- tx %>% filter(anchor == "middle", str_detect(t, "^\\d+%$"))
  x_0 <- ticks$x[ticks$t == "0%"]; x_100 <- ticks$x[ticks$t == "100%"]
  ax_y <- max(ticks$y)
  legend <- tx %>% filter(y < min(ticks$y) - 1) %>% arrange(y) %>% pull(t)
  body <- tx %>% filter(y > ax_y + 1)
  dots <- it$circles %>% filter(fill %in% c(MAIN, EXP), cy > ax_y)
  frac_of <- function(cx) (cx - x_0) / (x_100 - x_0)
  pct <- body %>% filter(str_detect(t, "^\\d+%$"))
  lines <- bind_rows(
    body %>% filter(!str_detect(t, "^\\d+%$"), fill == TEXT) %>% transmute(y, label = t, black = TRUE),
    join_words(body %>% filter(!str_detect(t, "^\\d+%$"), fill == MAIN)) %>% transmute(y, label, black = FALSE)
  ) %>% arrange(y)
  # dots sit about 0.35 em above the label baseline
  spec <- map_dfr(seq_len(nrow(lines)), function(i) {
    yl <- lines$y[i]
    d <- dots %>% filter(abs(cy - (yl - 4.7)) < 4)
    v <- pct %>% filter(abs(y - yl) < 3)
    kind <- if (lines$black[i]) "row" else if (nrow(d) > 0) "avg" else "header"
    one <- function(col) {
      k <- d$cx[d$fill == col]; t <- v$t[v$fill == col]
      list(frac = if (length(k)) frac_of(k[1]) else NA_real_, text = if (length(t)) t[1] else NA_character_)
    }
    p <- one(MAIN); e <- one(EXP)
    tibble(kind = kind, label = lines$label[i], v1_frac = p$frac, v1_text = p$text, v2_frac = e$frac, v2_text = e$text)
  }) %>% mutate(order = row_number(), .before = 1)
  if (nrow(dots) != sum(!is.na(c(spec$v1_frac, spec$v2_frac)))) stop("parse_dumbbell: puntos sin fila en ", path, call. = FALSE)
  attr(spec, "legend") <- legend
  spec
}

## Horizontal bars of the pipeline (gen_horizontal_bars) -------------------------------------------------------
# Their text is already outlined in the report: the bars and the dashed 0-100 % grid give each value
hbar_fracs <- function(path) {
  doc <- xml2::read_xml(path)
  grid <- xml2::xml_find_all(doc, paste0("//*[(local-name()='path' or local-name()='polyline') and ",
                                         "(contains(@style, 'dasharray') or @stroke-dasharray)]"))
  xs <- as.numeric(str_match(coalesce(xml2::xml_attr(grid, "points"), xml2::xml_attr(grid, "d")), "^M?\\s*([0-9.]+)")[, 2])
  xs <- sort(unique(round(xs, 2)))
  bars <- read_svg_items(path)$rects %>% filter(fill == MAIN) %>% arrange(y)
  if (length(xs) != 6 || abs(min(xs) - min(bars$x)) > 0.5) stop("hbar_fracs: eje 0-100 % no reconocido en ", path, call. = FALSE)
  bars$w / (max(xs) - min(xs))
}
spec_rows <- function(labels, fracs) {
  tibble(order = seq_along(labels), kind = "row", label = labels, v1_frac = fracs,
         v1_text = paste0(round(fracs * 100), "%"), v2_frac = NA_real_, v2_text = NA_character_)
}
# Same bars (order, lengths and printed values) as a published horizontal-bar figure
same_hbars <- function(spec, published, tol = 0.006) {
  fr <- hbar_fracs(published)
  length(fr) == nrow(spec) && max(abs(fr - spec$v1_frac)) < tol && all(paste0(round(fr * 100), "%") == spec$v1_text)
}

## Checks against the data ------------------------------------------------------------------------------------
# One row per line of the figure and series: the printed value against the table (category, value and,
# when categories repeat across groups, grouping), and the bar length against the same value.
#   estado: "ok" | "distinto" (printed value differs) | "barra" (bar length differs by more than tol_bar
#           points of the scale) | "ambiguo" (the label matches several rows of the table) |
#           "sin dato" (printed value without a row in the table: not verified) | "suprimido" (N/A or nothing
#           printed and no data: suppressed value)
# scale: value of a full bar (100 for percentages); relabel: table category -> figure label.
check_spec <- function(spec, table, series = "v1", decimals = 0, relabel = c(), scale = 100, tol_bar = 0.6) {
  empty <- tibble(label = character(), grupo = character(), serie = character(), printed = character(),
                  data = character(), bar = double(), estado = character(), ok = logical())
  if (is.null(table)) return(empty)
  tab <- table %>% mutate(category = recode(category, "National Average" = "National", !!!relabel))
  if (!"grouping" %in% names(tab)) tab$grouping <- NA_character_
  group <- character(nrow(spec)); current <- NA_character_
  for (i in seq_len(nrow(spec))) {
    if (spec$kind[i] == "header") current <- spec$label[i]
    group[i] <- if (spec$kind[i] == "avg") "National" else current
  }
  rows <- spec %>% mutate(grupo = group) %>% filter(kind != "header")
  out <- map_dfr(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    printed <- r[[paste0(series, "_text")]]
    frac <- r[[paste0(series, "_frac")]]
    key <- recode(r$label, "National Average" = "National")
    cand <- tab %>% filter(category == key)
    if (nrow(cand) > 1) cand <- cand %>% filter(grouping == r$grupo | (grouping == "National" & key == "National"))
    value <- if (nrow(cand) == 1) cand$value else NA_real_
    data <- if (nrow(cand) == 1 && !is.na(value)) formatC(round(value, decimals), format = "f", digits = decimals) else "NA"
    shown <- !is.na(printed) && printed != "N/A"
    estado <- if (nrow(cand) == 0) { if (shown) "sin dato" else "suprimido" }
              else if (nrow(cand) > 1) "ambiguo"
              else if (!shown) { if (is.na(value)) "suprimido" else "distinto" }
              else if (sub("%$", "", printed) != data) "distinto"
              else if (!is.na(frac) && abs(frac * scale - value) > tol_bar * scale / 100) "barra"
              else "ok"
    tibble(label = r$label, grupo = r$grupo, serie = series, printed = coalesce(printed, ""), data = data,
           bar = round(frac * scale, 2), estado = estado, ok = estado %in% c("ok", "suprimido"))
  })
  bind_rows(empty, out)
}

# Same content as another spec (labels, kinds, printed values; bars within tol of the full bar)
same_spec <- function(spec, other, tol = 0.006) {
  cols <- c("kind", "label", "v1_text")
  nrow(spec) == nrow(other) && identical(as.data.frame(spec[cols]), as.data.frame(other[cols])) &&
    isTRUE(all(abs(spec$v1_frac - other$v1_frac) < tol | (is.na(spec$v1_frac) & is.na(other$v1_frac))))
}

# Bars of a published SVG whose text is outlined (Figure 36): same order and lengths as the spec
same_bar_fracs <- function(spec, published, tol = 0.006) {
  r <- read_svg_items(published)$rects %>% filter(fill %in% c(MAIN, TRACK))
  span <- max(r$x + r$w) - min(r$x)
  fr <- r %>% filter(fill == MAIN) %>% arrange(y) %>% mutate(frac = w / span) %>% pull(frac)
  rows <- spec %>% filter(kind != "header")
  length(fr) == nrow(rows) && max(abs(fr - rows$v1_frac)) < tol &&
    all(paste0(round(fr * 100), "%") == rows$v1_text)
}

