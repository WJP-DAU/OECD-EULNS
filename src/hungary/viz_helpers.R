## Visualization helpers for the Hungary additional analyses ------------------------------------------------
## All figures follow the report format: purple bars on a grey 100% track, value in bold italic at the
## right, group headers in bold italic purple, National/EU averages highlighted, no titles or notes
## (titles, notes and sources live in the document; methodology lives in the .md notes).

source("src/viz/grouped_bars.R")

# Grouped bars in the report format (reuses gen_grouped_bars() untouched). gen_grouped_bars() only draws an
# EU bar for a fixed list of object names, so the table is exposed under one of those names ("outcome_rp")
# and the EU value is passed with that name's id; the rendered file is then saved under `final`.
render_grouped <- function(tbl, final, w, h, eu_value = NULL, omit = NULL,
                           margin_set = margin(-20, -35, 0, 0), perc = TRUE, decimals = 0) {
  assign("outcome_rp", tbl, envir = .GlobalEnv)
  eu_data <- if (is.null(eu_value)) NULL else tibble(country_name = "European Union", id = "rp_outcome", value = eu_value / 100)
  render_existing(
    function() gen_grouped_bars(
      name = "outcome_rp", country = HU, w = w, h = h, perc = perc, decimals = decimals,
      omit.cats = omit, margin_set = margin_set, eu.avg = !is.null(eu_value), eu.avg.data = eu_data
    ),
    "Hungary_outcome_rp.svg", final
  )
  rm("outcome_rp", envir = .GlobalEnv)
  showtext::showtext_auto(FALSE)
}

highlight_rows <- c("National Average", "EU Average", "Hungary")

# Lay out rows with group headers (report style): returns the rows with a y position and a header flag.
layout_rows <- function(df, rev_within = FALSE) {
  groups <- unique(df$grouping)
  pos <- 0
  out <- list()
  for (g in groups) {
    rows <- df %>% filter(grouping == g)
    if (rev_within && g != "") rows <- rows[rev(seq_len(nrow(rows))), ]
    if (!is.na(g) && g != "") {
      pos <- pos + 1.35
      out[[length(out) + 1]] <- tibble(grouping = g, category = g, header = TRUE, y = pos)
    }
    out[[length(out) + 1]] <- rows %>% mutate(header = FALSE, y = pos + seq_len(n()))
    pos <- pos + nrow(rows)
  }
  res <- bind_rows(out)
  res %>% mutate(y = max(res$y) + 1 - y)
}

label_markup <- function(category, header) {
  case_when(
    header ~ paste0("<span style='color:", COL_MAIN, "'><b><i>", category, "</i></b></span>"),
    category %in% highlight_rows ~ paste0("<span style='color:", COL_MAIN, "'><b><i>", category, "</i></b></span>"),
    TRUE ~ category
  )
}

# Horizontal bars in the report format. df: grouping ("" = top rows), category, pct and, optionally, pct2
# (second stacked segment, drawn lighter; the label shows pct + pct2).
report_bars <- function(df, file_stem, w = 280, perc = TRUE, decimals = 0, xmax = 100, legend = NULL, rev_within = FALSE) {
  stacked <- "pct2" %in% names(df)
  lay <- layout_rows(df %>% mutate(grouping = coalesce(grouping, "")), rev_within)
  bars <- lay %>% filter(!header)
  total <- if (stacked) bars$pct + bars$pct2 else bars$pct
  bars <- bars %>% mutate(
    total = total,
    lab = case_when(is.na(total) ~ "N/A",
                    perc ~ paste0(format(round(total, decimals), nsmall = decimals, trim = TRUE), "%"),
                    TRUE ~ format(round(total, decimals), nsmall = decimals, trim = TRUE))
  )
  p <- ggplot(bars) +
    geom_rect(aes(xmin = 0, xmax = xmax, ymin = y - 0.4, ymax = y + 0.4), fill = COL_BG) +
    geom_rect(aes(xmin = 0, xmax = pct, ymin = y - 0.4, ymax = y + 0.4, fill = "a"), na.rm = TRUE)
  if (stacked) {
    p <- p + geom_rect(aes(xmin = pct, xmax = pct + pct2, ymin = y - 0.4, ymax = y + 0.4, fill = "b"), na.rm = TRUE)
  }
  p <- p +
    geom_text(aes(x = xmax * 1.02, y = y, label = lab), hjust = 0, family = "inter", fontface = "bold.italic",
              color = COL_MAIN, size = 4.6) +
    scale_fill_manual(values = c(a = COL_MAIN, b = COL_LIGHT), labels = legend %||% c("", ""),
                      guide = if (is.null(legend)) "none" else guide_legend()) +
    scale_x_continuous(limits = c(0, xmax * 1.13), expand = c(0, 0)) +
    scale_y_continuous(breaks = lay$y, labels = label_markup(lay$category, lay$header), expand = expansion(add = c(0.7, 1.1))) +
    theme_void(base_family = "inter") +
    theme(
      axis.text.y = ggtext::element_markdown(size = 14, hjust = 1, color = COL_TEXT, margin = margin(r = 6)),
      legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 14),
      plot.margin = margin(5, 5, 5, 5),
      plot.background = element_rect(fill = "white", color = NA)
    )
  save_fig(p, file_stem, w, 14 + 8.2 * max(lay$y))
}

# Dumbbell in the report format (Figs. 6 and 11): perception (purple) vs experience (magenta).
report_dumbbell <- function(df, file_stem, w = 280, rev_within = TRUE) {
  lay <- layout_rows(df %>% mutate(grouping = coalesce(grouping, "")), rev_within)
  pts <- lay %>% filter(!header)
  long <- pts %>% select(y, perception, experience) %>%
    pivot_longer(-y, names_to = "serie", values_to = "pct") %>%
    mutate(serie = factor(serie, c("perception", "experience")))
  p <- ggplot() +
    geom_segment(data = pts, aes(x = perception, xend = experience, y = y, yend = y), color = COL_BG, linewidth = 4, na.rm = TRUE) +
    geom_point(data = long, aes(x = pct, y = y, color = serie), size = 3.5, na.rm = TRUE) +
    geom_text(data = long %>% filter(serie == "perception"), aes(x = pct - 2.5, y = y, label = paste0(round(pct), "%")),
              hjust = 1, family = "inter", color = COL_MAIN, size = 4.6, na.rm = TRUE) +
    geom_text(data = long %>% filter(serie == "experience"), aes(x = pct + 2.5, y = y, label = paste0(round(pct), "%")),
              hjust = 0, family = "inter", color = COL_ACCENT, size = 4.6, na.rm = TRUE) +
    scale_color_manual(values = c(perception = COL_MAIN, experience = COL_ACCENT),
                       labels = c(perception = "General population: perception", experience = "People with a non-trivial problem: experience")) +
    scale_x_continuous(limits = c(-5, 105), breaks = c(0, 25, 50, 75, 100), labels = c("0%", "25%", "50%", "75%", "100%"),
                       position = "top", expand = c(0, 0)) +
    scale_y_continuous(breaks = lay$y, labels = label_markup(lay$category, lay$header), expand = expansion(add = c(0.7, 1.1))) +
    theme_minimal(base_family = "inter") +
    theme(
      axis.title = element_blank(),
      axis.text.y.left = ggtext::element_markdown(size = 14, hjust = 1, color = COL_TEXT),
      axis.text.x = element_text(size = 13, color = COL_TEXT),
      axis.line.x.top = element_line(color = COL_TEXT, linewidth = 0.5),
      panel.grid = element_blank(),
      legend.position = "top", legend.direction = "vertical", legend.title = element_blank(),
      legend.text = element_text(size = 14), legend.justification = "left",
      plot.background = element_rect(fill = "white", color = NA)
    )
  save_fig(p, file_stem, w, 30 + 8.2 * max(lay$y))
}

# Two bars per category (Hungary and EU Average), report colours, value at the right of each bar.
# df: category, hu, eu (eu may be NA). Categories are drawn in the order given.
paired_bars <- function(df, file_stem, w = 280, labels = c("Hungary", "EU Average")) {
  n_rows <- nrow(df)
  d <- df %>% mutate(y = rev(seq_len(n_rows)) * 2.6)
  long <- bind_rows(
    d %>% transmute(category, y = y + 0.55, pct = hu, serie = labels[1]),
    d %>% transmute(category, y = y - 0.55, pct = eu, serie = labels[2])
  ) %>% mutate(serie = factor(serie, labels), lab = if_else(is.na(pct), "N/A", paste0(round(pct), "%")))
  p <- ggplot(long) +
    geom_rect(aes(xmin = 0, xmax = 100, ymin = y - 0.5, ymax = y + 0.5), fill = COL_BG) +
    geom_rect(aes(xmin = 0, xmax = pct, ymin = y - 0.5, ymax = y + 0.5, fill = serie), na.rm = TRUE) +
    geom_text(aes(x = 102, y = y, label = lab, color = serie), hjust = 0, family = "inter", fontface = "bold.italic", size = 4.4, show.legend = FALSE) +
    scale_fill_manual(values = setNames(c(COL_MAIN, COL_LIGHT), labels)) +
    scale_color_manual(values = setNames(c(COL_MAIN, "#7d7da8"), labels)) +
    scale_x_continuous(limits = c(0, 114), expand = c(0, 0)) +
    scale_y_continuous(breaks = d$y, labels = d$category, expand = c(0, 0.8)) +
    theme_void(base_family = "inter") +
    theme(axis.text.y = element_text(size = 14, hjust = 1, color = COL_TEXT, margin = margin(r = 6)),
          legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 13),
          plot.margin = margin(5, 5, 5, 5), plot.background = element_rect(fill = "white", color = NA))
  save_fig(p, file_stem, w, 20 + 12.5 * n_rows)
}
