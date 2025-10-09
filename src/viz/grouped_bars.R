gen_grouped_bars <- function(
    name, country, w, h, 
    perc = TRUE, 
    omit.cats = NULL, 
    margin_set = margin(-20,-35,0,0),
    eu.avg = FALSE,
    eu.avg.data = NULL
  ){
  
  if (eu.avg){
    published_vars <- c(
      "prevalence_nontrivial_problems" = "prevalence2",
      "access2information" = "access2info",
      "access2representation" = "access2rep",
      "access2DRM" = "access2drm",
      "timeliness_rp" = "rp_time",
      "fair_rp" = "rp_fair",
      "cost_rp" = "rp_cost",
      "outcome_rp" = "rp_outcome",
      "trust_judges" = "TRT_judges",
      "trust_prosecutors" = "TRT_prosecutors",
      "trust_pda" = "TRT_pda"
    )
    equivalent_eurovoices_name <- published_vars[name]
  }
  
  # Renaming data frame
  data <- get(name) %>%
    select(
      grouping, category, values2plot = all_of(country)
    ) %>% 
    mutate(
      category = if_else(
        grouping == "National",
        "National Average",
        category
      )
    )
  
  if (eu.avg){
    
    eu_bar <- eu.avg.data %>% 
      select(
        grouping = country_name,
        category = id,
        values2plot = value
      ) %>% 
      filter(
        category %in% equivalent_eurovoices_name
      ) %>% 
      mutate(
        grouping = "National",
        category = "EU Average",
        values2plot = values2plot*100
      )
      
    data <- eu_bar %>% 
      bind_rows(data)
  }
  
  data <- data %>% 
    mutate(
      grouping = if_else(
        grouping == "National",
        "",
        grouping
      ),
      remaining = 100-values2plot
    ) %>% 
    pivot_longer(
      !c(grouping, category),
      names_to = "color",
      values_to = "values2plot"
    ) %>% 
    mutate(
      color = if_else(
        color == "values2plot",
        "primary",
        "secondary"
      ),
      category = if_else(
        grouping == "",
        glue::glue("<span style='color:#575796;'><b><i>{category}</b></i></span>"),
        category
      )
    )
  
  if (perc){
    data <- data %>% 
      mutate(
        values2display = if_else(
          color == "primary",
          paste0(
            format(
              round(values2plot,1),
              nsmall=1
            ),"%"
          ),
          ""
        )
      )
    
  } else {
    data <- data %>% 
      mutate(
        values2display = if_else(
          color == "primary",
          format(
            round(values2plot,1),
            nsmall=1
          ),
          ""
        )
      )
  }
  
  data <- data %>% 
    mutate(
      values2display = if_else(
        is.na(values2plot) & color == "primary",
        "N/A", values2display
      )
    )
  
  if (!is.null(omit.cats)){
    data <- data %>% 
      filter(
        !(grouping %in% omit.cats)
      )
  }
  
  clabels <- c(
    "", "Gender", "Age Group", "Financial Situation", "Area of Residence", 
    "Category", "Co-occurrent Problems", "Problem Status", "Non-Trivial Problem Experienced",
    "Resolution"
  )
  data$grouping <- factor(
    data$grouping,
    levels = clabels
  )
  
  # Drawing ggplot
  plot <- ggplot(
    data = data,
    aes(
      x = values2plot,
      y = category,
      fill = color
    )
  ) +
    geom_bar(
      position = position_stack(reverse = TRUE),
      width = 0.9,
      stat  = "identity",
      na.rm = TRUE
    )
  
  if (perc) {
    plot <- plot +
      geom_text(
        aes(
          x = 101,
          label = values2display
        ),
        family   = "inter",
        fontface = "bold.italic",
        color    = "#575796",
        hjust    = 0,
        size     = 5,
        na.rm    = TRUE
      ) +
      scale_x_continuous(
        expand = c(0,0),
        limits = c(0,115),
        position = "top"
      )
    
  } else {
    plot <- plot +
      geom_text(
        aes(
          x = values2plot + 0.25,
          label = values2display
        ),
        family   = "inter",
        fontface = "bold.italic",
        color    = "#575796",
        hjust    = 0.5,
        size     = 5,
        na.rm    = TRUE
      ) +
      scale_x_continuous(
        expand = c(0,0),
        limits = c(0,10),
        position = "top"
      )
    
  }
  
  plot <- plot +
    facet_grid(
      rows   = vars(grouping),
      scales = "free",
      space  = "free_y",
      switch = "y"
    )
  
  plot <- plot +
    scale_fill_manual(
      values = c(
        "primary"   = "#575796",
        "secondary" = "#e5e8e8"
      )
    ) +
    theme_minimal() +
    theme(
      axis.title.x  = element_blank(),
      axis.title.y  = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = ggtext::element_markdown(
        size   = 16,
        hjust  = 1,
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a"
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
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
        margin = margin_set
      ),
      strip.switch.pad.grid = unit(-35, "mm"),
      strip.placement = "outside",
      strip.clip      = "off",
      legend.position = "none"
    )
  
  ggsave(
    glue::glue("output/viz/{country}/{country}_{name}.svg"), 
    width  = w, 
    height = h,
    dpi    = 300,
    units  = "mm",
    scale  = 0.75 
  )
}
