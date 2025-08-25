gen_sankey_rdm <- function(master, country){
  
  # Sankey Base Data
  sankey_base_data <- master %>%
    filter(
      country_name_ltn == country
    ) %>% 
    select(
      country_year_id, country_name_ltn, nuts_id, 
      starts_with("sankey_drm_stage_")
    ) %>%
    filter(
      !is.na(sankey_drm_stage_1)
    )
  
  # Stage 1 Data
  sankey_data_stage_1 <- sankey_base_data %>% 
    filter(
      sankey_drm_stage_1 != "Turned to a DRM"
    ) %>% 
    group_by(nuts_id) %>% 
    mutate(
      nuts_sample = n()
    ) %>% 
    group_by(country_name_ltn, nuts_id, nuts_sample, sankey_drm_stage_1) %>% 
    summarise(
      count = n(),
      .groups = "keep"
    ) %>% 
    left_join(
      regions %>%
        select(nuts_id, wgt = regionpoppct),
      by = "nuts_id"
    ) %>% 
    mutate(
      value = (count/nuts_sample)*wgt
    ) %>% 
    group_by(country_name_ltn, sankey_drm_stage_1) %>% 
    summarise(
      national_avg = sum(value),
      .groups = "keep"
    )
  
  # Stage 2 Data
  sankey_data_stage_2 <- sankey_base_data %>% 
    filter(
      !is.na(sankey_drm_stage_2)
    ) %>%
    group_by(nuts_id) %>% 
    mutate(
      nuts_sample = n()
    ) %>%
    group_by(country_name_ltn, nuts_id, nuts_sample, sankey_drm_stage_2) %>% 
    summarise(
      count = n(),
      .groups = "keep"
    ) %>% 
    left_join(
      regions %>%
        select(nuts_id, wgt = regionpoppct),
      by = "nuts_id"
    ) %>% 
    mutate(
      value = (count/nuts_sample)*wgt
    ) %>% 
    group_by(country_name_ltn, sankey_drm_stage_2) %>% 
    summarise(
      national_avg = sum(value),
      .groups = "keep"
    )
  
  # Stage 0 Data
  sankey_data_stage_0 <- sankey_base_data %>% 
    group_by(nuts_id) %>% 
    mutate(
      nuts_sample = n()
    ) %>% 
    group_by(nuts_id) %>% 
    mutate(
      nuts_sample = n()
    ) %>% 
    group_by(country_name_ltn, nuts_id, nuts_sample, sankey_drm_stage_1) %>% 
    summarise(
      count = n(),
      .groups = "keep"
    ) %>% 
    left_join(
      regions %>%
        select(nuts_id, wgt = regionpoppct),
      by = "nuts_id"
    ) %>% 
    mutate(
      value = (count/nuts_sample)*wgt
    ) %>% 
    group_by(country_name_ltn, sankey_drm_stage_1) %>% 
    summarise(
      national_avg = sum(value),
      .groups = "keep"
    ) %>% 
    mutate(
      sankey_drm_stage_0 = if_else(
        sankey_drm_stage_1 == "Turned to a DRM",
        "Contacted a DRM",
        "Did not contact a DRM"
      )
    ) %>%
    group_by(sankey_drm_stage_0) %>% 
    summarise(
      national_avg = sum(national_avg)
    )
  
  # Proportions
  proportions <- bind_rows(
    sankey_data_stage_0 %>% ungroup() %>% mutate(x = "sankey_drm_stage_0") %>% select(x, node = sankey_drm_stage_0, perc = national_avg),
    sankey_data_stage_1 %>% ungroup() %>% mutate(x = "sankey_drm_stage_1") %>% select(x, node = sankey_drm_stage_1, perc = national_avg),
    sankey_data_stage_2 %>% ungroup() %>% mutate(x = "sankey_drm_stage_2") %>% select(x, node = sankey_drm_stage_2, perc = national_avg)
  ) %>% 
    mutate(
      perc = scales::percent(perc, accuracy = 0.1)
    )
  
  # Final Data
  sankey_final_data <- sankey_base_data %>% 
    group_by(nuts_id) %>% 
    mutate(
      nuts_sample = n()
    ) %>% 
    group_by(country_name_ltn, nuts_id, nuts_sample, sankey_drm_stage_1) %>% 
    summarise(
      count = n(),
      .groups = "keep"
    ) %>% 
    left_join(
      regions %>%
        select(nuts_id, wgt = regionpoppct),
      by = "nuts_id"
    ) %>% 
    mutate(
      value = (count/nuts_sample)*wgt
    ) %>% 
    group_by(country_name_ltn, sankey_drm_stage_1) %>% 
    summarise(
      national_avg = sum(value),
      .groups = "keep"
    ) %>%
    left_join(
      tribble(
        ~sankey_drm_stage_0, ~sankey_drm_stage_1, ~sankey_drm_stage_2,
        "Contacted a DRM", "Turned to a DRM", "Had access to a DRM",
        "Did not contact a DRM", "Did not need to turn to a DRM", NA,
        "Did not contact a DRM", "Needed but did not have access to a DRM", "Did not have access to a DRM",
        "Did not contact a DRM", "Unknown Reason", NA
      ),
      by = "sankey_drm_stage_1"
    ) %>% 
    ungroup() %>% 
    ggsankey::make_long(
      sankey_drm_stage_0, sankey_drm_stage_1, sankey_drm_stage_2,
      value = "national_avg"
    ) %>% 
    left_join(
      proportions,
      by = c("x", "node")
    )  %>% 
    filter(
      !is.na(node)
    ) %>%
    mutate(
      
      # next_x = if_else(
      #   next_node == "Turned to a DRM",
      #   "sankey_drm_stage_2",
      #   next_x
      # ),
      # next_node = if_else(
      #   next_node == "Turned to a DRM",
      #   "Had access to a DRM",
      #   next_node
      # ),
      
      node = factor(
        node, 
        levels = c(
          "Did not contact a DRM",
          "Contacted a DRM",
          "Did not have access to a DRM",
          "Did not need to turn to a DRM",
          "Had access to a DRM",
          "Needed but did not have access to a DRM",
          "Unknown Reason",
          "Turned to a DRM"
        )
      ),
      
      label = if_else(
        node == "Turned to a DRM",
        NA_character_,
        paste(
          node, perc, sep = ":\n"
        )
      )
    )
  
  # Drawing ggplot
  ggplot(
    sankey_final_data, 
    aes(
      x = x, 
      next_x    = next_x, 
      node      = node, 
      next_node = next_node,
      label     = label,
      fill      = node,
      value     = value
    )
  ) +
    ggsankey::geom_sankey(
      flow.alpha = .6,
      # node.color = "gray30"
      na.rm = TRUE
    ) +
    ggsankey::geom_sankey_label(
      size   = 3, 
      color  = "gray15", 
      fill   = "gray90",
      family = "inter",
      na.rm  = TRUE
    ) +
    scale_fill_manual(
      values = c(
        "Contacted a DRM" = "#0A69A5",
        "Turned to a DRM" = "#0A69A5",
        "Had access to a DRM" = "#0A69A5",
        "Did not have access to a DRM" = "#710000",
        "Needed but did not have access to a DRM" = "#710000",
        "Did not contact a DRM" = "#A63C06",
        "Did not need to turn to a DRM" = "#A37B67",
        "Unknown Reason" = "#707173"
      )
    ) +
    scale_x_discrete(
      position = "top",
      labels = c("Contacted a DRM", "Reason for No Contact", "Access to a DRM\n(SDG 16.3.3)")
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.x  = element_text(
        size   = 12,
        family = "lato",
        face   = "bold.italic"
      ),
      axis.text.y  = element_blank(),
      axis.title.x = element_blank(),
      plot.margin  = margin(0,-45,-15,-45)
    )
  
  ggsave(
    glue::glue("output/viz/{country}/{country}_sankey_drm.svg"), 
    width  = 8, 
    height = 6,
    dpi    = 300
  )
  
}
