gen_dumbbells <- function(name, country, w, h){
  
  data <- get(paste0(name,"_perceptions")) %>%
    select(grouping, category, perception = all_of(country)) %>% 
    left_join(
      get(name) %>% 
        select(grouping, category, experience = all_of(country)),
      by = c("grouping", "category")
    ) %>% 
    mutate(
      category = if_else(
        grouping == "National",
        "National Average",
        category
      ),
      grouping = if_else(
        grouping == "National",
        "",
        grouping
      ),
      category = if_else(
        grouping == "",
        glue::glue("<span style='color:#575796;'><b><i>{category}</b></i></span>"),
        category
      )
    )
  
  if (country == "Malta"){
    data <- data %>% 
      filter(
        !(category %in% c("Rural", "18-24"))
      )
  }
  
  ggplot(
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
        label = paste0(
          format(
            round(perception,1),
            nsmall=1
          ),"%"
        )
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
        label = paste0(
          format(
            round(experience,1),
            nsmall=1
          ),"%"
        )
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
      axis.text.y = ggtext::element_markdown(
        size   = 16,
        hjust  = 1,
        family = "inter",
        face   = "plain",
        color  = "#1a1a1a"
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      # panel.grid.major.x = element_line(
      #   linetype = "dashed",
      #   linewidth = 0.15,
      #   color = "#1a1a1a"
      # ),
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
    )
  
  ggsave(
    glue::glue("output/viz/{country}/{country}_{name}_dumbbell.svg"), 
    width  = w, 
    height = h,
    dpi    = 300,
    units  = "mm",
    scale  = 0.75 
  )
  
}
