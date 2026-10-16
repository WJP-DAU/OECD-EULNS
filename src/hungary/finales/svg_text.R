## Text as outlines (Inter Tight) for the final Hungary figures -----------------------------------------------
## Sourced by build_finales.R (and report_charts.R). The report is assembled on machines without Inter Tight:
## live SVG text falls back to another font there (e.g. "NationalAverage" when ggtext places each word with
## Inter Tight metrics). Drawing every glyph as a path makes each figure look the same everywhere.
##
##   text_path(): one string -> <path> element (for the charts drawn by report_charts.R)
##   text_width(): advance width of a string (layout)
##   outline_svg_text(): replaces every <text> of an existing SVG (svglite / ggplot output) by its outline

source("src/utils/config.R")

FONT_DIR <- file.path(path2DA, "6. Country Reports", "0. Fonts")

# Static Inter Tight files by weight class. The pipeline registers bold.italic as SemiBoldItalic (setup.R
# and utils.R), and svglite writes that face as font-weight 600.
font_file <- function(weight = 400, italic = FALSE) {
  weight <- if (is.character(weight)) c(normal = 400, bold = 700, bolder = 700)[[weight]] else weight
  face <- if (weight >= 650) "Bold" else if (weight >= 550) "SemiBold" else if (weight >= 450) "Medium" else ""
  name <- paste0("InterTight-", if (face == "" && !italic) "Regular" else paste0(face, if (italic) "Italic"), ".ttf")
  path <- file.path(FONT_DIR, name)
  if (!file.exists(path)) stop("No existe la fuente ", path, " (revise path2DA en src/utils/config.R)", call. = FALSE)
  path
}

# Glyph outlines are computed once per font and glyph at size 100 and scaled afterwards
.glyph_cache <- new.env(parent = emptyenv())
.REF_SIZE <- 100
glyph_contours <- function(path, index) {
  key <- paste(basename(path), index)
  hit <- .glyph_cache[[key]]
  if (!is.null(hit)) return(hit)
  g <- systemfonts::glyph_outline(index, path = path, size = .REF_SIZE, tolerance = 1)
  out <- if (nrow(g) == 0) list() else lapply(split(g, g$contour), function(d) cbind(d$x, d$y))
  .glyph_cache[[key]] <- out
  out
}

shape_ref <- function(string, path) {
  s <- textshaping::shape_text(string, path = path, size = .REF_SIZE, res = 72)
  list(index = s$shape$index, x = s$shape$x_offset, width = s$metrics$width[1])
}

text_width <- function(string, size, weight = 400, italic = FALSE) {
  if (!nzchar(string)) return(0)
  shape_ref(string, font_file(weight, italic))$width * size / .REF_SIZE
}

fmt <- function(v) {
  out <- sprintf("%.2f", v)
  out <- sub("\\.?0+$", "", out)
  out[out == "-0"] <- "0"
  out
}

# Path data of a string with its baseline origin at (x, y). anchor: start | middle | end.
# stretch: horizontal factor applied around the anchor (svglite's textLength).
text_d <- function(string, x, y, size, weight = 400, italic = FALSE, anchor = "start", stretch = 1) {
  if (!nzchar(trimws(string))) return("")
  path <- font_file(weight, italic)
  sh <- shape_ref(string, path)
  k <- size / .REF_SIZE
  w <- sh$width * k * stretch
  x0 <- x - switch(anchor, start = 0, middle = w / 2, end = w)
  parts <- character()
  for (i in seq_along(sh$index)) {
    contours <- glyph_contours(path, sh$index[i])
    if (length(contours) == 0) next
    gx <- x0 + sh$x[i] * k * stretch
    for (m in contours) {
      # absolute coordinates rounded to 0.01, written as relative moves (exact, and half the size)
      px <- round(gx + m[, 1] * k * stretch, 2)
      py <- round(y - m[, 2] * k, 2)
      parts <- c(parts, paste0("M", fmt(px[1]), " ", fmt(py[1]),
                               if (length(px) > 1) paste0("l", paste(fmt(diff(px)), fmt(diff(py)), collapse = " ")), "z"))
    }
  }
  paste(parts, collapse = "")
}

text_path <- function(string, x, y, size, weight = 400, italic = FALSE, anchor = "start", fill = "#1A1A1A",
                      transform = NULL) {
  d <- text_d(string, x, y, size, weight, italic, anchor)
  if (!nzchar(d)) return("")
  # inline style: svglite and other generators style every path with CSS (stroke, fill: none)
  sprintf("<path d=\"%s\" style=\"fill:%s;stroke:none\"%s/>", d, fill,
          if (is.null(transform)) "" else sprintf(" transform=\"%s\"", transform))
}


## Outline every <text> of an existing SVG ---------------------------------------------------------------------

css_prop <- function(style, prop) {
  if (is.na(style)) return(NA_character_)
  m <- regmatches(style, regexec(paste0("(?:^|;)\\s*", prop, "\\s*:\\s*([^;]+)"), style, perl = TRUE))[[1]]
  if (length(m) == 2) trimws(m[2]) else NA_character_
}

# Text attribute, CSS first (svglite writes style=...), then presentation attribute, then inherited from
# the ancestors, nearest first (<g fill=...>)
text_prop <- function(node, prop) {
  for (nd in c(list(node), as.list(xml2::xml_parents(node)))) {
    v <- css_prop(xml2::xml_attr(nd, "style"), prop)
    if (is.na(v)) v <- xml2::xml_attr(nd, prop)
    if (!is.na(v)) return(v)
  }
  NA_character_
}

outline_svg_text <- function(svg_in, svg_out, max_stretch = 0.03) {
  doc <- xml2::read_xml(svg_in)
  texts <- xml2::xml_find_all(doc, "//*[local-name()='text']")
  log <- vector("list", length(texts))
  for (i in seq_along(texts)) {
    node <- texts[[i]]
    if (length(xml2::xml_find_all(node, "./*[local-name()='tspan']")) > 0) {
      stop("outline_svg_text: <tspan> no soportado en ", svg_in, call. = FALSE)
    }
    string <- xml2::xml_text(node)
    num <- function(v, default) if (is.na(v)) default else as.numeric(sub("px$", "", v))
    x <- num(xml2::xml_attr(node, "x"), 0)
    y <- num(xml2::xml_attr(node, "y"), 0)
    size <- num(text_prop(node, "font-size"), 16)
    weight <- text_prop(node, "font-weight")
    weight <- if (is.na(weight)) 400 else if (grepl("^[0-9]+$", weight)) as.numeric(weight) else weight
    italic <- identical(text_prop(node, "font-style"), "italic")
    anchor <- dplyr::coalesce(text_prop(node, "text-anchor"), "start")
    fill <- dplyr::coalesce(text_prop(node, "fill"), "#000000")
    opacity <- text_prop(node, "fill-opacity")
    tr <- xml2::xml_attr(node, "transform")
    # svglite gives the width it measured (textLength): it is honoured, as a browser would, when it agrees
    # with Inter Tight; a larger difference means the SVG was measured with another font: the text is
    # flagged as doubtful (column doubtful of the returned log), as is any font family other than Inter
    stretch <- 1
    w_inter <- text_width(string, size, weight, italic)
    tl <- xml2::xml_attr(node, "textLength")
    ratio <- if (!is.na(tl) && w_inter > 0) as.numeric(sub("px$", "", tl)) / w_inter else NA_real_
    if (!is.na(ratio) && abs(ratio - 1) <= max_stretch) stretch <- ratio
    d <- text_d(string, x, y, size, weight, italic, anchor, stretch)
    family <- text_prop(node, "font-family")
    log[[i]] <- tibble::tibble(text = string, size = size, weight = as.character(weight), italic = italic,
                               family = family, textlength_ratio = ratio,
                               doubtful = (!is.na(ratio) && abs(ratio - 1) > max_stretch) ||
                                 (!is.na(family) && !grepl("inter", family, ignore.case = TRUE)))
    if (!nzchar(d)) {
      xml2::xml_remove(node)
      next
    }
    p <- xml2::xml_add_sibling(node, "path", .where = "before")
    xml2::xml_set_attr(p, "d", d)
    xml2::xml_set_attr(p, "style", paste0("fill:", fill, ";stroke:none",
                                          if (!is.na(opacity)) paste0(";fill-opacity:", opacity)))
    if (!is.na(tr)) xml2::xml_set_attr(p, "transform", tr)
    xml2::xml_remove(node)
  }
  xml2::write_xml(doc, svg_out, options = c("format", "no_declaration"))
  invisible(dplyr::bind_rows(log))
}
