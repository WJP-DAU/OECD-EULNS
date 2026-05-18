gen_dumbbells <- function(name, country, w, h){

  showtext::showtext_auto(FALSE)

  data <- get(paste0(name,"_perceptions")) %>%
    select(grouping, category, perception = all_of(country)) %>%
    left_join(
      get(name) %>%
        select(grouping, category, experience = all_of(country)),
      by = c("grouping", "category")
    ) %>%
    mutate(
      category = case_when(
        grouping == "National" ~ "National Average",
        grouping == "EU" ~ "EU Average",
        TRUE ~ category
      ),
      grouping = case_when(
        grouping %in% c("National", "EU") ~ "",
        TRUE ~ grouping
      )
    )

  if (country == "Malta"){
    data <- data %>%
      filter(
        !(category %in% c("Rural", "18-24"))
      )
  }

  # Create label dataframe for National Average and EU Average
  clabels <- c(
    "", "Gender", "Age Group", "Financial Situation", "Area of Residence"
  )

  national_categories <- data %>%
    filter(grouping == "" & category %in% c("National Average", "EU Average")) %>%
    select(category) %>%
    distinct() %>%
    pull(category)

  national_labels <- tibble(
    category = national_categories,
    label_html = sprintf(
      "<span style='color:#575796;font-weight:700;font-style:italic;'><b><i>%s</i></b></span>",
      national_categories
    ),
    x = 0,
    y = seq_along(national_categories),
    grouping = factor("", levels = clabels)
  )

  # Custom label function that blanks out National/EU Average labels
  custom_labels <- function(x) {
    ifelse(x %in% c("National Average", "EU Average"), " ", x)
  }

  p <- ggplot(
    data,
    aes(
      y = category
    )
  ) +
    geom_segment(
      aes(
        x = perception,
        xend = experience
      ),
      color = "#e5e8e8",
      linewidth = 4,
    ) +
    geom_point(
      aes(
        x = perception
      ),
      size  = 3.5,
      color = "#575796"
    ) +
    geom_point(
      aes(
        x = experience
      ),
      size  = 3.5,
      color = "#b200aa"
    ) +
    geom_text(
      aes(
        x = perception - 7,
        label = paste0(round(perception, 0), "%")
      ),
      family   = "inter",
      fontface = "plain",
      color    = "#575796",
      size     = 5,
      na.rm    = TRUE
    ) +
    geom_text(
      aes(
        x = experience + 7,
        label = paste0(round(experience, 0), "%")
      ),
      family   = "inter",
      fontface = "plain",
      color    = "#b200aa",
      size     = 5,
      na.rm    = TRUE
    ) +
    facet_grid(
      rows   = vars(grouping),
      scales = "free",
      space  = "free_y",
      switch = "y"
    ) +
    scale_x_continuous(
      expand = c(0,0),
      limits = c(-5,105),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0%", "25%", "50%", "75%", "100%"),
      position = "top"
    ) +
    scale_y_discrete(labels = custom_labels) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x  = element_text(
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a",
        size   = 13
      ),
      axis.text.y = element_text(
        size   = 16,
        hjust  = 1,
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a"
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line.x.top = element_line(
        linetype = "solid",
        linewidth = 0.5,
        color = "#1a1a1a"
      ),
      panel.grid.minor.x = element_blank(),
      panel.spacing      = unit(12, "mm"),
      strip.text.y.left  = element_text(
        angle  = 0,
        size   = 16,
        color  = "#575796",
        hjust  = 1,
        vjust  = 1,
        family = "inter",
        face   = "bold.italic",
        margin = margin(-20,-55,0,0)
      ),
      strip.switch.pad.grid = unit(-25, "mm"),
      strip.placement = "outside",
      strip.clip      = "off",
    ) +
    ggtext::geom_richtext(
      data = national_labels,
      aes(x = x, y = y, label = label_html),
      inherit.aes = FALSE,
      fill = NA,
      label.color = NA,
      vjust = 0.5,
      hjust = 1,
      size = 5.623357,
      family = "inter"
    )

  ggsave(
    glue::glue("output/viz/{country}/{country}_{name}_dumbbell.svg"),
    plot   = p,
    device = svglite::svglite,
    width  = w,
    height = h,
    dpi    = 300,
    units  = "mm",
    scale  = 0.75
  )

  showtext::showtext_auto(TRUE)

}
