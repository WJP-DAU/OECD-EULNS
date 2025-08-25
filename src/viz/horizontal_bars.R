gen_horizontal_bars <- function(name, country, w, h){
  
  # Renaming data frame
  data <- get(name) %>%
    ungroup() %>% 
    select(
      category, 
      values2plot = all_of(country)
    ) %>%
    arrange(desc(values2plot)) %>%
    mutate(
      labels = paste0(
        format(
          round(values2plot, 1),
          nsmall = 1
        ), "%"
      ),
      added_NA = if_else(
        is.na(values2plot),
        "NA", ""
      ),
      lab_pos  = values2plot+7,
      color    = "standard",
      order_no = row_number()
    )
  
  WJPr::wjp_bars(
    data      = data,
    target    = "values2plot",
    grouping  = "category",
    order     = "order_no", 
    direction = "horizontal",
    colors    = "color",
    cvec      = c("standard" = "#0A69A5")
  ) +
    geom_text(
      aes(
        y = data$lab_pos,
        label = labels
      ),
      size     = 5,
      color    = "grey35",
      family   = "inter",
      fontface = "bold.italic"
    ) +
    theme(
      axis.text.x = element_text(
        size   = 14,
        # hjust  = 0,
        family = "inter",
        face   = "plain",
        color  = "grey35"
      ),
      axis.text.y = element_text(
        size   = 14,
        hjust  = 0,
        family = "inter",
        face   = "plain",
        color  = "grey35"
      ),
    )
  
  ggsave(
    glue::glue("output/viz/{country}/{country}_{name}.svg"), 
    width  = w, 
    height = h,
    dpi    = 300,
    scale  = 0.75 
  )
  
}
