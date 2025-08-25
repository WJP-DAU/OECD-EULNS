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
        country,
        category
      ),
      grouping = if_else(
        grouping == "National",
        "",
        grouping
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
      color = "#A0BAC7"
    ) +
    geom_point(
      aes(
        x = perception
      ),
      size  = 3.5,
      color = "#0A69A5"
    ) +
    geom_point(
      aes(
        x = experience
      ),
      size  = 3.5,
      color = "#A0BAC7"
    ) +
    geom_text(
      aes(
        x = perception - 5,
        label = paste0(
          format(
            round(perception,1),
            nsmall=1
          ),"%"
        )
      ),
      family   = "inter",
      fontface = "italic",
      color    = "grey35",
      size     = 4,
      na.rm    = TRUE
    ) +
    geom_text(
      aes(
        x = experience + 5,
        label = paste0(
          format(
            round(experience,1),
            nsmall=1
          ),"%"
        )
      ),
      family   = "inter",
      fontface = "italic",
      color    = "grey35",
      size     = 4,
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
      limits = c(0,100),
      breaks = c(0, 25, 50, 75, 100),
      labels = c("0%", "25%", "50%", "75%", "100%"),
      position = "top"
    ) +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x  = element_blank(), 
      # axis.text.x  = element_text(
      #   family = "inter",
      #   face   = "plain",
      #   color  = "grey35",
      #   size   = 13
      # ),
      axis.text.y  = element_text(
        family = "inter",
        face   = "plain",
        color  = "grey35",
        size   = 13
      ),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.spacing      = unit(12, "mm"),
      strip.text.y.left  = element_text(
        angle  = 0,
        color  = "grey35",
        hjust  = 0, 
        vjust  = 1,
        size   = 15,
        family = "inter",
        face   = "bold.italic",
        margin = margin(-18,0,0,0)
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
    scale  = 0.75 
  )
  
}
