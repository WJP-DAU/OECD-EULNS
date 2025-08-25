gen_grouped_bars <- function(name, country, w, h, perc = T, omit.cats = NULL){
  
  # Renaming data frame
  data <- get(name) %>%
    select(
      grouping, category, values2plot = all_of(country)
    ) %>%
    mutate(
      category = if_else(
        grouping == "National",
        country,
        category
      ),
      color = if_else(
        grouping == "National",
        "primary",
        "secondary"
      ),
      grouping = if_else(
        grouping == "National",
        "",
        grouping
      ),
      added_NA = if_else(
        is.na(values2plot),
        "NA", ""
      )
    )
  
  if (!is.null(omit.cats)){
    data <- data %>% 
      filter(
        !(grouping %in% omit.cats)
      )
  }
  
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
      width = 0.9,
      stat  = "identity",
      na.rm = TRUE
    )
  
  if (perc) {
    plot <- plot +
      geom_text(
        aes(
          x = values2plot + 5,
          label = paste0(
            format(
              round(values2plot,1),
              nsmall=1
            ),"%"
          )
        ),
        family   = "inter",
        fontface = "italic",
        color    = "grey35",
        size     = 5,
        na.rm    = TRUE
      ) +
      scale_x_continuous(
        expand = c(0,0),
        limits = c(0,100),
        position = "top"
      )
    
  } else {
    plot <- plot +
      geom_text(
        aes(
          x = values2plot + 0.25,
          label = format(
            round(values2plot,1),
            nsmall=1
          )
        ),
        family   = "inter",
        fontface = "italic",
        color    = "grey35",
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
    geom_text(
      aes(
        label = added_NA
      ),
      x        = 0,
      hjust    = 0,
      family   = "inter",
      fontface = "italic",
      color    = "grey35",
      size     = 5,
      na.rm    = TRUE
    ) +
    facet_grid(
      rows   = vars(grouping),
      scales = "free",
      space  = "free_y",
      switch = "y"
    )
  
  plot <- plot +
    scale_fill_manual(
      values = c(
        "primary"   = "#0A69A5",
        "secondary" = "#A0BAC7"
      )
    ) +
    theme_minimal() +
    theme(
      axis.title.x  = element_blank(),
      axis.title.y  = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_text(
        size   = 16,
        hjust  = 0,
        family = "inter",
        face   = "plain",
        color  = "grey35"
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.spacing      = unit(12, "mm"),
      strip.text.y.left  = element_text(
        angle  = 0,
        size   = 18,
        color  = "grey35",
        hjust  = 0, 
        vjust  = 1, 
        family = "inter",
        face   = "bold.italic",
        margin = margin(-18,0,0,0)
      ),
      strip.switch.pad.grid = unit(-25, "mm"),
      strip.placement = "outside",
      strip.clip      = "off",
      legend.position = "none"
    )
  
  ggsave(
    glue::glue("output/viz/{country}/{country}_{name}.svg"), 
    width  = w, 
    height = h,
    dpi    = 300,
    scale  = 0.75 
  )
}
