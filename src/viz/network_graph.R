## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Co-occurrence network graph
## Author(s):         Carlos Toruno (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     July 2026
## ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

gen_network_graph <- function(master, country, w = 320, h = 320) {

  # ============================================================
  # 1. DATA PREPARATION
  # ============================================================

  category_map <- c(
    "A1" = "Consumer",      "A2" = "Consumer",      "A3" = "Consumer",
    "B1" = "Land",          "B2" = "Land",          "B3" = "Land",          "B4" = "Land",
    "C1" = "Housing",       "C2" = "Housing",        "C4" = "Housing",
    "D1" = "Family",        "D2" = "Family",         "D3" = "Family",
    "D4" = "Family",        "D5" = "Family",         "D6" = "Family",
    "E1" = "Education",     "E2" = "Education",
    "F1" = "Accidental",    "F2" = "Accidental",
    "G1" = "Employment",    "G2" = "Employment",     "G3" = "Employment",
    "H1" = "Public Services", "H2" = "Public Services", "H3" = "Public Services", "J4" = "Public Services",
    "I1" = "Law Enforcement",
    "J1" = "Citizenship & ID", "J2" = "Citizenship & ID", "J3" = "Citizenship & ID",
    "K1" = "Money & Debt",  "K2" = "Money & Debt",  "K3" = "Money & Debt",
    "L1" = "Money & Debt",  "L2" = "Money & Debt",
    "E3" = "Community",     "C3" = "Community"
  )

  problem_codes <- names(category_map)
  bin_cols      <- paste0("AJP_", problem_codes, "_bin")

  # AJP_*_bin columns use Stata coding: 1 = Yes, 2 = No → recode to 1/0
  country_data <- master %>%
    filter(country_name_ltn == country) %>%
    select(country_year_id, any_of(bin_cols)) %>%
    mutate(across(-country_year_id, \(x) if_else(x == 1, 1L, 0L, missing = 0L)))

  present_bin_cols <- intersect(bin_cols, names(country_data))
  present_codes    <- str_extract(present_bin_cols, "(?<=AJP_).*(?=_bin)")

  # Aggregate to category level: respondent had ≥1 problem in that category
  cats <- unique(category_map[present_codes])

  consolidated_df <- map_dfc(
    cats,
    function(cat) {
      codes_in_cat  <- names(category_map)[category_map == cat]
      cols_in_cat   <- paste0("AJP_", intersect(codes_in_cat, present_codes), "_bin")
      cols_in_cat   <- intersect(cols_in_cat, names(country_data))
      if (length(cols_in_cat) == 0) return(tibble(!!cat := NA_real_))
      tibble(!!cat := as.integer(rowSums(country_data[cols_in_cat], na.rm = TRUE) > 0))
    }
  ) %>%
    filter(if_any(everything(), \(x) !is.na(x)))

  # Drop categories with zero variance (all 0 or all NA)
  cats <- cats[sapply(cats, function(cat) {
    col <- consolidated_df[[cat]]
    !all(is.na(col)) && var(col, na.rm = TRUE) > 0
  })]
  consolidated_df <- consolidated_df %>% select(all_of(cats))

  n_cats  <- length(cats)
  n_total <- nrow(consolidated_df)

  # ============================================================
  # 2. LIFT MATRIX
  # ============================================================

  lift_matrix        <- matrix(0, n_cats, n_cats, dimnames = list(cats, cats))
  cooccurrence_counts <- matrix(0, n_cats, n_cats, dimnames = list(cats, cats))

  for (i in seq_len(n_cats)) {
    for (j in seq_len(n_cats)) {
      cat1 <- cats[i]
      cat2 <- cats[j]
      both <- sum(consolidated_df[[cat1]] == 1 & consolidated_df[[cat2]] == 1, na.rm = TRUE)
      cooccurrence_counts[i, j] <- both
      if (i == j) {
        lift_matrix[i, j] <- 1
      } else {
        p_both <- both / n_total
        p1     <- mean(consolidated_df[[cat1]], na.rm = TRUE)
        p2     <- mean(consolidated_df[[cat2]], na.rm = TRUE)
        if (p1 > 0 & p2 > 0) lift_matrix[i, j] <- p_both / (p1 * p2)
      }
    }
  }

  prevalence <- sapply(cats, function(cat) mean(consolidated_df[[cat]], na.rm = TRUE) * 100)

  # ============================================================
  # 3. NETWORK & CENTRALITY
  # ============================================================

  edges_all <- map2_dfr(
    rep(seq_len(n_cats - 1), times = rev(seq_len(n_cats - 1))),
    unlist(lapply(seq_len(n_cats - 1), function(i) seq(i + 1, n_cats))),
    function(i, j) {
      tibble(
        from         = cats[i],
        to           = cats[j],
        weight       = lift_matrix[cats[i], cats[j]],
        cooccurrence = cooccurrence_counts[cats[i], cats[j]]
      )
    }
  )

  edges_sig <- edges_all %>%
    filter(weight > 1.2, cooccurrence >= 10)

  nodes <- tibble(name = cats, prevalence = prevalence[cats])

  g_full <- igraph::graph_from_data_frame(edges_all, directed = FALSE, vertices = nodes)
  igraph::E(g_full)$weight <- edges_all$weight

  weighted_degree    <- igraph::strength(g_full, weights = igraph::E(g_full)$weight)
  nodes$centrality   <- weighted_degree[nodes$name]
  cent_range <- max(nodes$centrality) - min(nodes$centrality)
  nodes$centrality_norm <- if (cent_range == 0) {
    rep(0.5, nrow(nodes))
  } else {
    (nodes$centrality - min(nodes$centrality)) / cent_range
  }

  # ============================================================
  # 4. COLOUR GRADIENT
  # ============================================================

  create_purple_gradient <- function(norm) {
    light_rgb <- c(168, 168, 200)
    dark_rgb  <- c(57, 57, 107)
    r <- round(light_rgb[1] + (dark_rgb[1] - light_rgb[1]) * norm)
    g <- round(light_rgb[2] + (dark_rgb[2] - light_rgb[2]) * norm)
    b <- round(light_rgb[3] + (dark_rgb[3] - light_rgb[3]) * norm)
    grDevices::rgb(r, g, b, maxColorValue = 255)
  }

  nodes$color <- sapply(nodes$centrality_norm, create_purple_gradient)

  # ============================================================
  # 5. LAYOUT
  # ============================================================

  set.seed(42)
  layout_fr <- igraph::layout_with_fr(
    g_full, weights = igraph::E(g_full)$weight ^ 2, niter = 1000
  )

  layout_df <- tibble(
    name = igraph::V(g_full)$name,
    x    = layout_fr[, 1],
    y    = layout_fr[, 2]
  )

  nodes <- nodes %>% left_join(layout_df, by = "name")

  # Rescale layout to [0, 1] then add padding so nodes don't clip at edges
  pad <- 0.16
  nodes <- nodes %>%
    mutate(
      x = (x - min(x)) / (max(x) - min(x)) * (1 - 2 * pad) + pad,
      y = (y - min(y)) / (max(y) - min(y)) * (1 - 2 * pad) + pad
    )

  # Overlap resolution: FR layout can place extremely high-lift pairs almost
  # on top of each other (e.g. two rare, tightly-correlated categories); nudge
  # apart any nodes closer than min_dist so labels stay readable
  min_dist <- 0.28
  for (iter in seq_len(100)) {
    moved <- FALSE
    for (i in seq_len(nrow(nodes) - 1)) {
      for (j in seq(i + 1, nrow(nodes))) {
        dx   <- nodes$x[j] - nodes$x[i]
        dy   <- nodes$y[j] - nodes$y[i]
        dist <- sqrt(dx^2 + dy^2)
        if (dist < min_dist) {
          if (dist < 1e-6) { dx <- 1e-3; dy <- 0; dist <- 1e-3 }
          overlap <- (min_dist - dist) / 2
          ux <- dx / dist; uy <- dy / dist
          nodes$x[j] <- nodes$x[j] + ux * overlap
          nodes$y[j] <- nodes$y[j] + uy * overlap
          nodes$x[i] <- nodes$x[i] - ux * overlap
          nodes$y[i] <- nodes$y[i] - uy * overlap
          moved <- TRUE
        }
      }
    }
    if (!moved) break
  }

  # Horizontal stretch: widen spacing along x only; panel width is scaled by
  # the same factor below so node size (mm) and aspect stay circular
  hstretch <- 1.15
  nodes <- nodes %>% mutate(x = 0.5 + (x - 0.5) * hstretch)
  x_half_range <- 0.5 * hstretch

  # ============================================================
  # 6. PLOT
  # ============================================================

  showtext::showtext_auto(FALSE)

  edges_plot <- edges_sig %>%
    left_join(nodes %>% select(name, x, y), by = c("from" = "name")) %>%
    rename(x_from = x, y_from = y) %>%
    left_join(nodes %>% select(name, x, y), by = c("to" = "name")) %>%
    rename(x_to = x, y_to = y) %>%
    mutate(
      edge_width = (weight - 0.8) * 2.5,
      edge_alpha = pmin(0.85, (weight - 1) * 0.35 + 0.2)
    )

  nodes <- nodes %>%
    mutate(node_size = 42)

  label_df <- nodes %>%
    mutate(
      label_name = case_when(
        name == "Public Services"  ~ "Public\nServices",
        name == "Citizenship & ID" ~ "Citizenship\n& ID",
        name == "Money & Debt"     ~ "Money &\nDebt",
        name == "Law Enforcement"  ~ "Law\nEnforcement",
        TRUE ~ name
      ),
      label_full = paste0(label_name, "\n(", round(prevalence, 0), "%)")
    )

  p <- ggplot2::ggplot() +
    ggplot2::geom_segment(
      data = edges_plot,
      ggplot2::aes(
        x = x_from, y = y_from, xend = x_to, yend = y_to,
        linewidth = edge_width, alpha = edge_alpha
      ),
      color    = "#5D6D7E",
      lineend  = "round"
    ) +
    ggplot2::scale_linewidth_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::geom_point(
      data  = nodes,
      ggplot2::aes(x = x, y = y, size = node_size, fill = color),
      shape = 21,
      color = "#2C3E50",
      stroke = 1.5
    ) +
    ggplot2::scale_size_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_text(
      data = label_df,
      ggplot2::aes(x = x, y = y, label = label_full),
      size       = 5.5,
      fontface   = "bold",
      color      = "white",
      lineheight = 0.85
    ) +
    ggplot2::labs(
      caption = "Closer nodes = Stronger co-occurrence (higher Lift)\nNode (%) = Prevalence (%)\nNode color = Centrality (darker = more central)"
    ) +
    ggplot2::theme_void(base_size = 14) +
    ggplot2::theme(
      plot.caption    = ggplot2::element_text(
        hjust = 0.5, size = 18, face = "bold",
        margin = ggplot2::margin(t = 15)
      ),
      plot.margin     = ggplot2::margin(20, 20, 20, 20),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    ) +
    ggplot2::coord_fixed(
      xlim = c(0.5 - x_half_range, 0.5 + x_half_range), ylim = c(0, 1), clip = "off"
    )

  w <- w * hstretch

  output_dir <- glue::glue("output/viz/{country}")
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  ggplot2::ggsave(
    filename = glue::glue("{output_dir}/cooccurrence_network.png"),
    plot     = p,
    width    = w,
    height   = h,
    units    = "mm",
    dpi      = 300,
    bg       = "white"
  )

  ggplot2::ggsave(
    filename = glue::glue("{output_dir}/cooccurrence_network.svg"),
    plot     = p,
    device   = svglite::svglite,
    width    = w,
    height   = h,
    dpi      = 300,
    units    = "mm",
    scale    = 0.75
  )

  invisible(p)
}
