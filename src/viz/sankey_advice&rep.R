gen_sankey_advice <- function(master, country){
  
  # Sankey Base Data
  sankey_base_data <- master %>%
    filter(
      country_name_ltn == country
    ) %>% 
    select(
      country_year_id, country_name_ltn, nuts_id, 
      starts_with("sankey_advice_stage_")
    ) %>%
    filter(
      !is.na(sankey_advice_stage_1) & !is.na(sankey_advice_stage_2)
    ) %>% 
    group_by(nuts_id) %>% 
    mutate(
      nuts_sample = n()
    ) 
  
  # Stage 1 Data
  sankey_data_stage_1 <- sankey_base_data %>% 
    group_by(country_name_ltn, nuts_id, nuts_sample, sankey_advice_stage_1) %>% 
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
    group_by(country_name_ltn, sankey_advice_stage_1) %>% 
    summarise(
      national_avg = sum(value),
      .groups = "keep"
    )
  
  # Stage 2 Data
  sankey_data_stage_2 <- sankey_base_data %>% 
    group_by(country_name_ltn, nuts_id, nuts_sample, sankey_advice_stage_2) %>% 
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
    group_by(country_name_ltn, sankey_advice_stage_2) %>% 
    summarise(
      national_avg = sum(value),
      .groups = "keep"
    )
  
  # Stage 3 Data
  sankey_data_stage_3 <- sankey_data_stage_2 %>% 
    mutate(
      sankey_advice_stage_3 = case_when(
        sankey_advice_stage_2 %in% c(
          "Appropriate Advisor", 
          "Did not need an advisor"
        ) ~ "Had access",
        sankey_advice_stage_2 %in% c(
          "Non-Appropriate Advisor", 
          "Needed but did not have access to an advisor",
          "Unknown Reason"
        ) ~ "Had no access"
      )
    ) %>% 
    group_by(country_name_ltn, sankey_advice_stage_3) %>% 
    summarise(
      national_avg = sum(national_avg),
      .groups = "keep"
    )
  
  # Proportions
  proportions <- bind_rows(
    sankey_data_stage_1 %>% ungroup() %>% mutate(x = "sankey_advice_stage_1") %>% select(x, node = sankey_advice_stage_1, perc = national_avg),
    sankey_data_stage_2 %>% ungroup() %>% mutate(x = "sankey_advice_stage_2") %>% select(x, node = sankey_advice_stage_2, perc = national_avg),
    sankey_data_stage_3 %>% ungroup() %>% mutate(x = "sankey_advice_stage_3") %>% select(x, node = sankey_advice_stage_3, perc = national_avg)
  ) %>% 
    mutate(
      perc = scales::percent(perc, accuracy = 0.1)
    )
  
  # Data for Sankey
  sankey_final_data <- sankey_data_stage_2 %>% 
    ungroup() %>% 
    mutate(
      sankey_advice_stage_1 = case_when(
        sankey_advice_stage_2 %in% c(
          "Appropriate Advisor", 
          "Non-Appropriate Advisor"
        ) ~ "Contacted an advisor",
        sankey_advice_stage_2 %in% c(
          "Did not need an advisor", 
          "Needed but did not have access to an advisor",
          "Unknown Reason"
        ) ~ "Did not contact an advisor"
      ),
      sankey_advice_stage_3 = case_when(
        sankey_advice_stage_2 %in% c(
          "Appropriate Advisor", 
          "Did not need an advisor"
        ) ~ "Had access",
        sankey_advice_stage_2 %in% c(
          "Non-Appropriate Advisor", 
          "Needed but did not have access to an advisor",
          "Unknown Reason"
        ) ~ "Had no access"
      )
    ) %>% 
    ggsankey::make_long(
      sankey_advice_stage_1, sankey_advice_stage_2, sankey_advice_stage_3,
      value = "national_avg"
    ) %>% 
    left_join(
      proportions,
      by = c("x", "node")
    )
  
  # Drawing ggplot
  ggplot(
    sankey_final_data, 
    aes(
      x = x, 
      next_x    = next_x, 
      node      = node, 
      next_node = next_node,
      label     = paste(
        node, perc, sep = ":\n"
      ),
      fill      = node,
      value     = value
    )
  ) +
    ggsankey::geom_sankey(
      flow.alpha = .6,
      # node.color = "gray30"
    ) +
    ggsankey::geom_sankey_label(
      size   = 3, 
      color  = "gray15", 
      fill   = "gray90",
      family = "inter"
    ) +
    scale_fill_manual(
      values = c(
        "Appropriate Advisor" = "#A0BAC7",
        "Contacted an advisor" = "#386641",
        "Did not contact an advisor" = "#A63C06",
        "Did not need an advisor" = "#A0BAC7",
        "Had access" = "#0A69A5",
        "Had no access" = "#710000",
        "Needed but did not have access to an advisor" = "#A37B67",
        "Non-Appropriate Advisor" = "#A37B67",
        "Unknown Reason" = "#707173"
      )
    ) +
    scale_x_discrete(
      position = "top",
      labels = c("Contacted an Advisor", "Type of Advisor /\nReason for No Contact", "Access to Adequate\nAssistance and Representation")
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
      plot.margin  = margin(0,-40,-15,-45)
    )
  
  ggsave(
    glue::glue("output/viz/{country}/{country}_sankey_advice_representation.svg"), 
    width  = 8, 
    height = 6,
    dpi    = 300
  )
}

