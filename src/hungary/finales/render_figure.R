## Re-render of the ggplot figures 6 (src/hungary/F01.R) and 29 (src/hungary/F08.R) on a given canvas -----------
## Called by standard_figures.R in its own R process:
##   Rscript src/hungary/finales/render_figure.R <F01|F08> <out.svg> <width_in> <height_in>
## The scripts run unchanged, but their tables and notes go to a temporary folder (OUT and OUT_ABS point
## there) and their own figure saves are intercepted, so this script writes nothing in output/; on a clean
## machine src/hungary/setup.R may still create its own caches (output/tabs/cache_hungary/,
## data/jg_data_*.csv).
##   F01: the matrix plot is captured at its save_fig() call (run_F01() is stopped there) and saved as is
##        on the larger canvas (the full text column).
##   F08: same upset as F08.R with three changes for the larger, taller figure: the percentages above the
##        17 combination bars are vertical (they do not fit side by side at the standard size beside the
##        callout box), the set-size panel is narrower (width_ratio 0.22) with room for its labels
##        (expansion 0.7) and the axis text is 12.8 pt (10 pt once the figure is placed).

args <- commandArgs(TRUE)
which_fig <- args[1]; out_svg <- args[2]; width_in <- as.numeric(args[3]); height_in <- as.numeric(args[4])

suppressWarnings(suppressMessages({
  source("src/hungary/setup.R")
  source("src/hungary/viz_helpers.R")
  source("src/hungary/analysis_helpers.R")
}))

OUT <- file.path(tempdir(), "render_figure")
dir.create(OUT, showWarnings = FALSE)
OUT_ABS <- normalizePath(OUT)
showtext::showtext_auto(FALSE)

if (which_fig == "F01") {
  source("src/hungary/F01.R")
  captured <- NULL
  save_fig <- function(p, file_stem, w, h) {
    captured <<- p
    stop(structure(class = c("plot_captured", "condition"), list(message = "plot captured", call = NULL)))
  }
  tryCatch(suppressWarnings(suppressMessages(run_F01())), plot_captured = function(e) NULL)
  if (is.null(captured)) stop("run_F01() no entregó el gráfico", call. = FALSE)
  p <- captured

} else if (which_fig == "F08") {
  source("src/hungary/F08.R")
  ggsave <- function(filename, plot, ...) invisible(filename)
  data <- suppressWarnings(suppressMessages(run_F08()))

  # Same preparation and plot as run_F08() (src/hungary/F08.R), with the changes listed above
  sets <- c("Access to Assistance", "Access to Information", "Resolution Process", "Problem Resolved")
  upset_data <- data %>%
    mutate(rows = round(pct * 10)) %>%
    slice(rep(row_number(), rows)) %>%
    transmute(`Access to Assistance` = jg_inverse_access2rep == 1, `Access to Information` = jg_inverse_access2info == 1,
              `Resolution Process` = jg_inverse_rp == 1, `Problem Resolved` = jg_inverse_outcome == 1)

  p <- ComplexUpset::upset(
    upset_data, sets,
    name = NULL, width_ratio = 0.22, min_size = 1, sort_sets = FALSE, stripes = "white",
    base_annotations = list(
      "% of people within\nthe justice gap" = ComplexUpset::intersection_size(
        mapping = aes(fill = "bar"),
        bar_number_threshold = 1,
        text = list(size = 3.6, family = "inter", angle = 90, hjust = -0.15, vjust = 0.5),
        text_colors = c(on_background = COL_MAIN, on_bar = COL_MAIN),
        text_mapping = aes(label = paste0(format(round(!!ComplexUpset::get_size_mode("exclusive_intersection") / 10, 1), nsmall = 1), "%"))
      ) +
        scale_fill_manual(values = c(bar = COL_MAIN), guide = "none") +
        scale_y_continuous(expand = expansion(mult = c(0, 0.25)), labels = NULL) +
        theme(panel.grid = element_blank(), axis.text.y = element_blank(), axis.title.y = element_text(family = "inter", size = 11))
    ),
    set_sizes = ComplexUpset::upset_set_size(geom = geom_bar(fill = COL_MAIN, width = 0.7)) +
      geom_text(aes(label = paste0(format(round(after_stat(count) / 10, 1), nsmall = 1), "%")), stat = "count",
                hjust = 1.15, family = "inter", size = 3.6, color = COL_TEXT) +
      scale_y_reverse(expand = expansion(mult = c(0.7, 0))) +
      theme(axis.text.x = element_blank(), axis.title.x = element_blank(), panel.grid = element_blank()),
    matrix = ComplexUpset::intersection_matrix(outline_color = list(active = COL_TEXT, inactive = "#e5e8e8")),
    themes = ComplexUpset::upset_default_themes(text = element_text(size = 12.8, family = "inter"))
  )
} else {
  stop("Figura desconocida: ", which_fig, call. = FALSE)
}

ggplot2::ggsave(out_svg, p, device = svglite::svglite, width = width_in, height = height_in, units = "in")
cat("Figura re-renderizada (", which_fig, "):", out_svg, "\n")
