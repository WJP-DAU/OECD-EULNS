## Charts of the final Hungary report, drawn straight to SVG ---------------------------------------------------
## Sourced by build_finales.R. Draws the report figures with ONE set of measures (STD), so that text sizes,
## row heights and colours match across figures. All text is drawn as Inter Tight outlines (svg_text.R).
##   chart_grouped()     grouped bars, look of the pipeline's gen_grouped_bars() (src/viz/grouped_bars.R)
##   chart_paired()      Hungary vs EU average, horizontal pairs
##   chart_dumbbell()    perception vs experience
##   chart_hbars_axis()  horizontal bars with a 0-100 % axis, look of the pipeline's gen_horizontal_bars()
##   chart_columns()     Hungary vs EU average, vertical columns
##   chart_funnel(), chart_sankey(), chart_scorecards()  designs of the external charts5.py (legacy_python/)
##
## Units: points. The SVG is declared at width = W / 72 in, so the sizes below are the printed sizes when
## the figure is placed at 100 %. Every chart takes st (a copy of STD with the measures a figure changes).
## Input of the bar charts ("spec", one row per line of the figure, see figure_specs.R):
##   kind  = "avg" (National / EU Average) | "header" (group title) | "row"
##   label, v1_frac / v1_text (bar or first series), v2_frac / v2_text (second series, paired / dumbbell);
##   *_frac is the share of the full bar (0-1), NA when the value is suppressed (N/A).

source("src/hungary/finales/svg_text.R")

# Proportions of gen_grouped_bars() (labels 16, values 14.23, row 27, bar 24.4 units in Figure 31 of the
# report), at a base size 11 % larger than that figure
STD <- list(
  size_label  = 10,     # category labels, group headers, National / EU Average
  size_value  = 9,      # values at the end of the bars
  size_legend = 9.5,
  size_axis   = 9,
  pitch       = 15.5,   # one bar row
  bar         = 13.5,   # bar height (grouped bars)
  header      = 22,     # block of a group header
  header_base = 17,     # baseline of the header inside its block
  gap_label   = 6,      # label -> bar
  track       = 250,    # full bar (100 %)
  gap_value   = 5,      # bar end -> value
  value_w     = 27,     # room for the value ("100%", "N/A")
  pair_bar    = 9,      # paired bars: height of each bar (2 pt between the two bars)
  pair_pitch  = 26,
  dot_r       = 3.8,
  segment     = 7,
  col = list(main = "#575796", track = "#E5E8E8", text = "#1A1A1A", eu = "#A8A8C8", eu_text = "#7D7DA8",
             exp = "#B200AA", axis = "#1A1A1A", grid = "#D0D1D3", node = "#8E9393", excl = "#C4C8C8",
             box = "#F2F3F3", note = "#7D7DA8", delta = "#4B4B7A", card_low = "#ECECF4")
)
with_std <- function(...) utils::modifyList(STD, list(...))

# Baseline that centres digits / capitals on a given y (Inter Tight cap height = 0.727 em)
mid_base <- function(cy, size) cy + 0.36 * size

r2 <- function(v) fmt(round(v, 2))
rect_el <- function(x, y, w, h, fill, rx = 0, stroke = NULL, sw = 0.6, opacity = 1) {
  if (w <= 0 || h <= 0) return("")
  sprintf("<rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\"%s style=\"fill:%s;%s%s\"/>", r2(x), r2(y), r2(w), r2(h),
          if (rx > 0) sprintf(" rx=\"%s\"", r2(rx)) else "", fill,
          if (is.null(stroke)) "stroke:none" else sprintf("stroke:%s;stroke-width:%s", stroke, r2(sw)),
          if (opacity < 1) sprintf(";fill-opacity:%s", r2(opacity)) else "")
}
circle_el <- function(cx, cy, r, fill) {
  sprintf("<circle cx=\"%s\" cy=\"%s\" r=\"%s\" style=\"fill:%s;stroke:none\"/>", r2(cx), r2(cy), r2(r), fill)
}
line_el <- function(x1, y1, x2, y2, col, width, dash = NULL) {
  sprintf("<line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" style=\"stroke:%s;stroke-width:%s;fill:none%s\"/>",
          r2(x1), r2(y1), r2(x2), r2(y2), col, r2(width), if (is.null(dash)) "" else paste0(";stroke-dasharray:", dash))
}
path_el <- function(d, fill, opacity = 1) {
  sprintf("<path d=\"%s\" style=\"fill:%s;stroke:none%s\"/>", d, fill, if (opacity < 1) sprintf(";fill-opacity:%s", r2(opacity)) else "")
}
svg_doc <- function(W, H, body) {
  paste0(sprintf("<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%.3fin\" height=\"%.3fin\" viewBox=\"0 0 %s %s\">",
                 W / 72, H / 72, r2(W), r2(H)),
         rect_el(0, 0, W, H, "#FFFFFF"), paste(body, collapse = ""), "</svg>")
}
chart_out <- function(W, H, els, text_pt = STD$size_label) list(svg = svg_doc(W, H, els), width_pt = W, height_pt = H, text_pt = text_pt)

# Greedy word wrap at a maximum width (pt)
wrap_text <- function(s, size, max_w, weight = 400, italic = FALSE) {
  words <- strsplit(s, " ", fixed = TRUE)[[1]]
  lines <- character(); cur <- ""
  for (w in words) {
    cand <- if (cur == "") w else paste(cur, w)
    if (cur != "" && text_width(cand, size, weight, italic) > max_w) { lines <- c(lines, cur); cur <- w } else cur <- cand
  }
  c(lines, cur)
}
# Lines centred vertically on yc
lines_els <- function(lines, x, yc, size, anchor = "end", weight = 400, italic = FALSE, fill = STD$col$text, lh = 1.22) {
  step <- size * lh
  y0 <- yc - step * (length(lines) - 1) / 2 + 0.36 * size
  vapply(seq_along(lines), function(i) text_path(lines[i], x, y0 + (i - 1) * step, size, weight, italic, anchor, fill), "")
}

# Label column: right edge of the labels = widest label of the given specs
label_column <- function(specs, st = STD) {
  s <- dplyr::bind_rows(specs)
  if (nrow(s) == 0) return(NA_real_)
  w <- mapply(function(label, kind) {
    if (kind == "row") text_width(label, st$size_label) else text_width(label, st$size_label, 600, TRUE)
  }, s$label, s$kind)
  ceiling(max(w)) + 2
}

label_el <- function(r, L, base, st = STD) {
  if (r$kind == "row") text_path(r$label, L, base, st$size_label, anchor = "end", fill = st$col$text)
  else text_path(r$label, L, base, st$size_label, 600, TRUE, anchor = "end", fill = st$col$main)
}
value_el <- function(txt, x, base, fill, anchor = "start", st = STD) {
  text_path(txt, x, base, st$size_value, 600, TRUE, anchor = anchor, fill = fill)
}

# Rows and headers, shared by the bar charts. draw_row(r, y) returns the elements of one data line and its
# height.
layout_rows <- function(spec, L, y0, draw_row, st = STD) {
  y <- y0; els <- character()
  for (i in seq_len(nrow(spec))) {
    r <- spec[i, ]
    if (r$kind == "header") {
      els <- c(els, label_el(r, L, y + st$header_base, st))
      y <- y + st$header
    } else {
      out <- draw_row(r, y)
      els <- c(els, out$els)
      y <- y + out$h
    }
  }
  list(els = els, y = y)
}


## Grouped bars (gen_grouped_bars look) -------------------------------------------------------------------
# track = FALSE and value_at_end = TRUE reproduce gen_grouped_bars(perc = FALSE): no grey track and the
# value right after the bar (Figure 4)

chart_grouped <- function(spec, L, track = TRUE, value_at_end = FALSE, st = STD) {
  x0 <- L + st$gap_label; xv <- x0 + st$track + st$gap_value
  rows <- layout_rows(spec, L, 2, function(r, y) {
    cy <- y + st$pitch / 2
    els <- label_el(r, L, mid_base(cy, st$size_label), st)
    if (!is.na(r$v1_frac)) {
      if (track) els <- c(els, rect_el(x0, cy - st$bar / 2, st$track, st$bar, st$col$track))
      els <- c(els, rect_el(x0, cy - st$bar / 2, st$track * min(max(r$v1_frac, 0), 1), st$bar, st$col$main))
    }
    x_val <- if (value_at_end && !is.na(r$v1_frac)) x0 + st$track * r$v1_frac + st$gap_value else xv
    els <- c(els, value_el(r$v1_text, x_val, mid_base(cy, st$size_value), st$col$main, st = st))
    list(els = els, h = st$pitch)
  }, st)
  chart_out(xv + st$value_w, rows$y + 4, rows$els, st$size_label)
}


## Paired bars: Hungary vs EU average ------------------------------------------------------------------------

chart_paired <- function(spec, L, legend = c("Hungary", "EU Average"), st = STD) {
  x0 <- L + st$gap_label; xv <- x0 + st$track + st$gap_value
  sw <- 9; base_leg <- 10
  leg_x2 <- x0 + sw + 4 + text_width(legend[1], st$size_legend) + 14
  legend_els <- c(
    rect_el(x0, base_leg - sw + 1, sw, sw, st$col$main),
    text_path(legend[1], x0 + sw + 4, base_leg, st$size_legend, fill = st$col$text),
    rect_el(leg_x2, base_leg - sw + 1, sw, sw, st$col$eu),
    text_path(legend[2], leg_x2 + sw + 4, base_leg, st$size_legend, fill = st$col$text)
  )
  b <- st$pair_bar
  rows <- layout_rows(spec, L, 20, function(r, y) {
    top <- y + (st$pair_pitch - (2 * b + 2)) / 2
    cy1 <- top + b / 2; cy2 <- top + b + 2 + b / 2
    els <- label_el(r, L, mid_base((cy1 + cy2) / 2, st$size_label), st)
    for (k in 1:2) {
      frac <- r[[paste0("v", k, "_frac")]]; txt <- r[[paste0("v", k, "_text")]]
      cy <- if (k == 1) cy1 else cy2
      if (!is.na(frac)) {
        els <- c(els, rect_el(x0, cy - b / 2, st$track, b, st$col$track),
                 rect_el(x0, cy - b / 2, st$track * min(max(frac, 0), 1), b, if (k == 1) st$col$main else st$col$eu))
      }
      els <- c(els, value_el(txt, xv, mid_base(cy, st$size_value), if (k == 1) st$col$main else st$col$eu_text, st = st))
    }
    list(els = els, h = st$pair_pitch)
  }, st)
  chart_out(xv + st$value_w, rows$y + 4, c(legend_els, rows$els), st$size_label)
}


## Dumbbells: perception (general population) vs experience --------------------------------------------------

chart_dumbbell <- function(spec, L, legend, st = STD) {
  x0 <- L + st$gap_label; xT <- x0 + st$track
  xs <- function(frac) x0 + st$track * frac
  # legend (two lines) and axis on top
  leg <- c(
    circle_el(x0 + st$dot_r, 10 - 3.4, st$dot_r, st$col$main),
    text_path(legend[1], x0 + 2 * st$dot_r + 5, 10, st$size_legend, fill = st$col$text),
    circle_el(x0 + st$dot_r, 24 - 3.4, st$dot_r, st$col$exp),
    text_path(legend[2], x0 + 2 * st$dot_r + 5, 24, st$size_legend, fill = st$col$text)
  )
  ax_base <- 44; ax_line <- 49
  axis <- c(
    vapply(c(0, 0.25, 0.5, 0.75, 1), function(t) {
      text_path(paste0(round(100 * t), "%"), xs(t), ax_base, st$size_axis, anchor = "middle", fill = st$col$text)
    }, ""),
    line_el(x0, ax_line, xT, ax_line, st$col$axis, 0.75)
  )
  rows <- layout_rows(spec, L, ax_line + 3, function(r, y) {
    cy <- y + st$pitch / 2
    els <- label_el(r, L, mid_base(cy, st$size_label), st)
    pts <- tibble::tibble(frac = c(r$v1_frac, r$v2_frac), txt = c(r$v1_text, r$v2_text),
                          col = c(st$col$main, st$col$exp)) %>% dplyr::filter(!is.na(frac))
    if (nrow(pts) == 2) els <- c(els, line_el(xs(min(pts$frac)), cy, xs(max(pts$frac)), cy, st$col$track, st$segment))
    if (nrow(pts) > 0) {
      left <- if (nrow(pts) == 2) which.min(pts$frac) else 0
      for (k in seq_len(nrow(pts))) {
        els <- c(els, circle_el(xs(pts$frac[k]), cy, st$dot_r, pts$col[k]))
        on_left <- k == left
        els <- c(els, value_el(pts$txt[k], xs(pts$frac[k]) + if (on_left) -(st$dot_r + 3) else st$dot_r + 3,
                               mid_base(cy, st$size_value), pts$col[k], anchor = if (on_left) "end" else "start", st = st))
      }
    }
    list(els = els, h = st$pitch)
  }, st)
  chart_out(xT + st$gap_value + st$value_w, rows$y + 4, c(leg, axis, rows$els), st$size_label)
}


## Horizontal bars with a 0-100 % axis (gen_horizontal_bars look) ---------------------------------------------
# Labels left-aligned, axis labels on top with dashed grid lines every 20 %, values in black bold italic
# right after the bar. L = width of the label column (labels start at x = 2).

chart_hbars_axis <- function(spec, L, st = STD) {
  x0 <- L + 12; tr <- st$track
  xs <- function(f) x0 + tr * f
  y_axis <- 12; y_top <- 20
  rows <- spec %>% dplyr::filter(kind != "header")
  H <- y_top + nrow(rows) * st$pitch + 4
  grid <- unlist(lapply(seq(0, 1, by = 0.2), function(t) {
    c(line_el(xs(t), y_top - 2, xs(t), H, st$col$grid, 0.6, "4,4"),
      text_path(paste0(round(100 * t), "%"), xs(t), y_axis, st$size_label, anchor = "middle", fill = st$col$text))
  }))
  bars <- unlist(lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]; cy <- y_top + (i - 0.5) * st$pitch
    c(text_path(r$label, 2, mid_base(cy, st$size_label), st$size_label, fill = st$col$text),
      rect_el(x0, cy - st$bar / 2, tr * min(max(r$v1_frac, 0), 1), st$bar, st$col$main),
      text_path(r$v1_text, xs(r$v1_frac) + st$gap_value, mid_base(cy, st$size_label), st$size_label, 600, TRUE, fill = st$col$text))
  }))
  chart_out(x0 + tr + st$value_w + 6, H, c(grid, bars), st$size_label)
}


## Vertical columns: Hungary vs EU average -------------------------------------------------------------------
# spec rows: label, v1 (Hungary), v2 (EU). Heights relative to the largest value (no axis: values printed).

chart_columns <- function(spec, W, legend = c("Hungary", "EU Average"), st = STD) {
  n <- nrow(spec); m <- 6
  gw <- (W - 2 * m) / n; cw <- st$col_w; gap <- 4
  sw <- 9; base_leg <- 11
  leg_x2 <- m + sw + 4 + text_width(legend[1], st$size_legend) + 14
  els <- c(
    rect_el(m, base_leg - sw + 1, sw, sw, st$col$main),
    text_path(legend[1], m + sw + 4, base_leg, st$size_legend, fill = st$col$text),
    rect_el(leg_x2, base_leg - sw + 1, sw, sw, st$col$eu),
    text_path(legend[2], leg_x2 + sw + 4, base_leg, st$size_legend, fill = st$col$text)
  )
  top <- 24 + st$size_value + 6; yb <- top + st$col_h
  vmax <- max(c(spec$v1_frac, spec$v2_frac), na.rm = TRUE)
  wrapped <- lapply(spec$label, wrap_text, size = st$size_label, max_w = gw - 10)
  for (i in seq_len(n)) {
    gc <- m + (i - 0.5) * gw
    for (k in 1:2) {
      frac <- spec[[paste0("v", k, "_frac")]][i]; txt <- spec[[paste0("v", k, "_text")]][i]
      x <- if (k == 1) gc - gap / 2 - cw else gc + gap / 2
      h <- st$col_h * frac / vmax
      els <- c(els, rect_el(x, yb - h, cw, h, if (k == 1) st$col$main else st$col$eu),
               text_path(txt, x + cw / 2, yb - h - 4, st$size_value, 600, TRUE, "middle", if (k == 1) st$col$main else st$col$eu_text))
    }
    step <- st$size_label * 1.22
    els <- c(els, vapply(seq_along(wrapped[[i]]), function(j) {
      text_path(wrapped[[i]][j], gc, yb + 6 + st$size_label * 0.95 + (j - 1) * step, st$size_label, anchor = "middle", fill = st$col$text)
    }, ""))
  }
  els <- c(els, line_el(m, yb, W - m, yb, st$col$axis, 0.6))
  nl <- max(lengths(wrapped))
  chart_out(W, yb + 6 + st$size_label * 0.95 + (nl - 1) * st$size_label * 1.22 + 6, els, st$size_label)
}


## Funnel (charts5.py funnel): centred bars, light connectors, values inside -----------------------------------
# rows: label (may contain "\n"), value text, frac (bar width relative to the widest bar)

chart_funnel <- function(rows, W, st = STD) {
  lines <- lapply(rows$label, function(l) {
    if (grepl("\n", l)) strsplit(l, "\n", fixed = TRUE)[[1]] else wrap_text(l, st$size_label, st$funnel_label_w)
  })
  L <- ceiling(max(unlist(lapply(lines, function(ls) sapply(ls, text_width, size = st$size_label))))) + 2
  x0 <- L + 16; x1 <- W - 4; cx <- (x0 + x1) / 2; full <- x1 - x0
  bh <- st$funnel_bar; gap <- st$funnel_gap; top <- 4
  geo <- lapply(seq_len(nrow(rows)), function(i) {
    w <- full * rows$frac[i]; y <- top + (i - 1) * (bh + gap)
    c(y = y, l = cx - w / 2, r = cx + w / 2)
  })
  els <- character()
  for (i in seq_len(length(geo) - 1)) {
    a <- geo[[i]]; b <- geo[[i + 1]]
    els <- c(els, path_el(sprintf("M%s %sL%s %sL%s %sL%s %sZ", r2(a[["l"]]), r2(a[["y"]] + bh), r2(a[["r"]]), r2(a[["y"]] + bh),
                                  r2(b[["r"]]), r2(b[["y"]]), r2(b[["l"]]), r2(b[["y"]])), st$col$eu, 0.45))
  }
  for (i in seq_along(geo)) {
    g <- geo[[i]]
    els <- c(els, rect_el(g[["l"]], g[["y"]], g[["r"]] - g[["l"]], bh, st$col$main),
             lines_els(lines[[i]], L, g[["y"]] + bh / 2, st$size_label, "end"),
             text_path(rows$value[i], cx, mid_base(g[["y"]] + bh / 2, st$funnel_value), st$funnel_value, 600, TRUE, "middle", "#FFFFFF"))
  }
  chart_out(W, top * 2 + nrow(rows) * bh + (nrow(rows) - 1) * gap, els, st$size_label)
}


## Sankey (charts5.py sankey): three columns of nodes, bands between them, labelled boxes ---------------------
# s: list(C1, LAB1, C2 = list(key = c(value, parent)), LAB2, order2, M (stage 2 -> outcome), order3, LAB3,
#         VAL3, NOTE3, headers); values in % of all respondents.

chart_sankey <- function(s, W, st = STD) {
  out_col <- c(access = st$col$main, no_access = st$col$eu, excluded = st$col$excl)
  TOP <- st$sankey_top; BOT <- TOP + st$sankey_h; GAP <- 9; BAR <- 11
  X1 <- st$sankey_margin; X3 <- W - st$sankey_margin - BAR; X2 <- (X1 + X3) / 2
  order1 <- names(s$C1)
  val2 <- vapply(s$order2, function(k) as.numeric(s$C2[[k]][1]), 0)
  val3 <- vapply(s$order3, function(o) sum(val2[s$order2[unlist(s$M[s$order2]) == o]]), 0)
  scale <- (st$sankey_h - GAP * (length(s$order2) - 1)) / 100
  layout <- function(values, order) {
    used <- sum(values[order]) * scale + GAP * (length(order) - 1)
    y <- TOP + (st$sankey_h - used) / 2; pos <- list()
    for (k in order) { pos[[k]] <- c(y, y + values[[k]] * scale); y <- y + values[[k]] * scale + GAP }
    pos
  }
  p1 <- layout(unlist(s$C1), order1); p2 <- layout(val2, s$order2); p3 <- layout(val3, s$order3)
  band <- function(xa, ya0, ya1, xb, yb0, yb1, fill, op) {
    xm <- (xa + xb) / 2
    path_el(sprintf("M%s %sC%s %s %s %s %s %sL%s %sC%s %s %s %s %s %sZ", r2(xa), r2(ya0), r2(xm), r2(ya0), r2(xm), r2(yb0), r2(xb), r2(yb0),
                    r2(xb), r2(yb1), r2(xm), r2(yb1), r2(xm), r2(ya1), r2(xa), r2(ya1)), fill, op)
  }
  els <- character()
  cur1 <- vapply(p1, `[`, 0, 1)
  for (k in s$order2) {
    h <- val2[[k]] * scale; par <- s$C2[[k]][2]
    els <- c(els, band(X1 + BAR, cur1[[par]], cur1[[par]] + h, X2, p2[[k]][1], p2[[k]][2], out_col[[s$M[[k]]]], 0.30))
    cur1[[par]] <- cur1[[par]] + h
  }
  cur3 <- vapply(p3, `[`, 0, 1)
  for (k in s$order2) {
    o <- s$M[[k]]; h <- val2[[k]] * scale
    els <- c(els, band(X2 + BAR, p2[[k]][1], p2[[k]][2], X3, cur3[[o]], cur3[[o]] + h, out_col[[o]], 0.45))
    cur3[[o]] <- cur3[[o]] + h
  }
  for (k in order1) els <- c(els, rect_el(X1, p1[[k]][1], BAR, diff(p1[[k]]), st$col$node))
  for (k in s$order2) els <- c(els, rect_el(X2, p2[[k]][1], BAR, diff(p2[[k]]), out_col[[s$M[[k]]]]))
  for (o in s$order3) els <- c(els, rect_el(X3, p3[[o]][1], BAR, diff(p3[[o]]), out_col[[o]]))
  hs <- st$sankey_head
  for (j in 1:3) {
    xc <- c(X1, X2, X3)[j] + BAR / 2
    els <- c(els, vapply(seq_along(s$headers[[j]]), function(i) {
      text_path(s$headers[[j]][i], xc, 14 + hs * 1.25 * (i - 1), hs, 700, TRUE, "middle", st$col$main)
    }, ""))
  }
  box <- function(xc, yc, lines, val, note = NULL, val_fill = st$col$main) {
    size <- st$size_label; lh <- size * 1.22; ns <- st$sankey_note
    rows <- c(lines, val, note); kinds <- c(rep("lab", length(lines)), "val", rep("note", length(note)))
    wd <- mapply(function(t, k) switch(k, lab = text_width(t, size), val = text_width(t, size, 600, TRUE), note = text_width(t, ns, 400, TRUE)), rows, kinds)
    w <- max(wd) + 14; h <- lh * length(rows) + 6
    x0 <- xc - w / 2; y0 <- yc - h / 2
    e <- rect_el(x0, y0, w, h, st$col$box, 2.5, st$col$text, 0.6)
    y <- y0 + 3 + lh * 0.8
    for (i in seq_along(rows)) {
      e <- c(e, switch(kinds[i],
        lab  = text_path(rows[i], xc, y, size, anchor = "middle", fill = st$col$text),
        val  = text_path(rows[i], xc, y, size, 600, TRUE, "middle", val_fill),
        note = text_path(rows[i], xc, y, ns, 400, TRUE, "middle", st$col$note)))
      y <- y + lh
    }
    e
  }
  for (k in order1) els <- c(els, box(X1 + BAR / 2, mean(p1[[k]]), s$LAB1[[k]], paste0(round(s$C1[[k]]), "%")))
  for (k in s$order2) els <- c(els, box(X2 + BAR / 2, mean(p2[[k]]), s$LAB2[[k]], paste0(round(val2[[k]]), "%")))
  for (o in s$order3) {
    els <- c(els, box(X3 + BAR / 2, mean(p3[[o]]), s$LAB3[[o]], s$VAL3[[o]], s$NOTE3[[o]],
                      if (o == "excluded") st$col$eu_text else st$col$main))
  }
  chart_out(W, BOT + 10, els, st$size_label)
}


## Scorecards (charts5.py scorecards) ------------------------------------------------------------------------
# vals: matrix rows x columns (%). Differences in percentage points against the first row (ref = "row")
# or the first column (ref = "col"). lohi: range of the colour scale (shared by figures shown together).

mix_col <- function(c1, c2, t) {
  a <- grDevices::col2rgb(c1)[, 1]; b <- grDevices::col2rgb(c2)[, 1]
  sprintf("#%02X%02X%02X", round(a[1] + (b[1] - a[1]) * t), round(a[2] + (b[2] - a[2]) * t), round(a[3] + (b[3] - a[3]) * t))
}
chart_scorecards <- function(row_labels, col_labels, vals, ref = "row", W, lohi = range(round(vals)), st = STD) {
  nr <- length(row_labels); nc <- length(col_labels)
  lw <- st$card_label_w; cg <- 6; ch <- st$card_h
  cw <- (W - lw - 12 - cg * (nc - 1)) / nc
  ls <- st$card_label; hsz <- st$card_label
  heads <- lapply(col_labels, wrap_text, size = hsz, max_w = cw - 8, weight = 700, italic = TRUE)
  hstep <- hsz * 1.2; nl <- max(lengths(heads))
  top <- 6 + nl * hstep + 6
  lo <- lohi[1]; hi <- lohi[2]
  els <- character()
  for (j in seq_len(nc)) {
    x <- lw + 12 + (j - 1) * (cw + cg) + cw / 2
    hl <- heads[[j]]
    els <- c(els, vapply(seq_along(hl), function(k) {
      text_path(hl[k], x, top - 7 - hstep * (length(hl) - k), hsz, 700, TRUE, "middle", st$col$main)
    }, ""))
  }
  for (i in seq_len(nr)) {
    y <- top + (i - 1) * (ch + cg)
    els <- c(els, lines_els(wrap_text(row_labels[i], ls, lw - 4), lw, y + ch / 2, ls, "end"))
    for (j in seq_len(nc)) {
      v <- round(vals[i, j]); t <- 0.12 + 0.88 * (v - lo) / (if (hi > lo) hi - lo else 1)
      dark <- t > 0.55; fc <- if (dark) "#FFFFFF" else st$col$text
      x <- lw + 12 + (j - 1) * (cw + cg)
      els <- c(els, rect_el(x, y, cw, ch, mix_col(st$col$card_low, st$col$main, t), 3))
      is_ref <- (ref == "row" && i == 1) || (ref == "col" && j == 1)
      if (is_ref) {
        els <- c(els, text_path(paste0(v, "%"), x + cw / 2, mid_base(y + ch / 2, st$card_value), st$card_value, 700, FALSE, "middle", fc))
      } else {
        base <- if (ref == "row") round(vals[1, j]) else round(vals[i, 1])
        d <- v - base; ds <- paste0(if (d > 0) "+" else if (d < 0) "−" else "±", abs(d), " points")
        els <- c(els, text_path(paste0(v, "%"), x + cw / 2, y + ch / 2 + 2, st$card_value, 700, FALSE, "middle", fc),
                 text_path(ds, x + cw / 2, y + ch / 2 + 2 + st$card_delta + 4, st$card_delta, 400, TRUE, "middle",
                           if (dark) "#FFFFFF" else st$col$delta))
      }
    }
  }
  chart_out(W, top + nr * ch + (nr - 1) * cg + 4, els, ls)
}
