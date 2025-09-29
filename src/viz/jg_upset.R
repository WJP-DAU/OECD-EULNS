gen_jg_upset <- function(master, w, h, ylim){
  
  data <- master %>% 
    select(
      country_year_id, country_name_ltn, nuts_id
    ) %>% 
    left_join(
      jg_data %>% 
        mutate(
          jg_inverse_rp = case_when(
            jg_inverse_time == 1 | jg_inverse_fair == 1 | jg_inverse_cost == 1 ~ 1,
            !is.na(jg_inverse_time) | !is.na(jg_inverse_fair) | !is.na(jg_inverse_cost) ~ 0,
            is.na(jg_inverse_time) & is.na(jg_inverse_fair) & is.na(jg_inverse_cost) ~ NA_real_
          )
        ) %>% 
        select(
          country_year_id, 
          jg_inverse_access2info, jg_inverse_access2rep, jg_inverse_rp, jg_inverse_outcome, 
          inside_gap
        ),
      by = "country_year_id"
    ) %>% 
    filter(
      # !is.na(inside_gap)
      inside_gap == 1
    ) %>% 
    select(-inside_gap) %>% 
    mutate(
      across(
        starts_with("jg_"),
        \(x) if_else(
          is.na(x), 0, x
        )
      )
    ) %>% 
    group_by(
      country_name_ltn, nuts_id, 
      jg_inverse_access2info, jg_inverse_access2rep, jg_inverse_rp, jg_inverse_outcome
    ) %>% 
    summarise(
      count_size = n(),
      .groups = "keep"
    ) %>% 
    group_by(country_name_ltn, nuts_id) %>% 
    mutate(total = sum(count_size)) %>% 
    left_join(
      regions %>% select(nuts_id, wgt = regionpoppct),
      by = "nuts_id"
    ) %>% 
    mutate(
      weighted_proportion = (count_size/total)*wgt
    ) %>% 
    group_by(
      country_name_ltn,
      jg_inverse_access2info, jg_inverse_access2rep, jg_inverse_rp, jg_inverse_outcome
    ) %>% 
    summarise(
      national_weighted_proportion = round(sum(weighted_proportion*100),0),
      .groups = "keep"
    )
  
  lapply(
    c("Italy", "Malta"),
    function(country){
      
      upset_data <- data %>%
        filter(
          country_name_ltn == country
        ) %>% 
        ungroup() %>% 
        slice(rep(row_number(), national_weighted_proportion)) %>%
        select(-c(country_name_ltn, national_weighted_proportion))
      names(upset_data) <- c(
        "Access to Information", "Access to Assistance", "Resolution Process", "Problem Resolved"
      )
      
      upset <- ComplexUpset::upset(
        upset_data, 
        names(upset_data), 
        name = "Unmet Justice Needs and\nUnresolved Justice Problems", 
        width_ratio = 0.2, 
        min_size    = 1,
        stripes     = "white", 
        
        base_annotations = list(
          "% of People Within\nthe Justice Gap" = ComplexUpset::intersection_size(
            
            mapping = aes(
              fill  = "bar_color",
            ),
            bar_number_threshold = 1,
            text = list(
              "size" = 4.5
            ),
            text_colors=c(
              on_background = "#575796", 
              on_bar = "white"
            ),
            text_mapping = aes(
              label = paste0(
                !!ComplexUpset::get_size_mode("exclusive_intersection"),
                "%"
              )
            )
            
          ) + scale_fill_manual(
            values = c("bar_color" = "#575796"), 
            guide  = "none"
          ) +
            expand_limits(y = ylim[[country]]) +
            theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.x  = element_blank(),
              axis.text.y  = element_blank(),
            )
        ),
        
        set_sizes = ComplexUpset::upset_set_size(
          geom = geom_bar(
            stat  = "count",
            fill  = "#575796"
          ) 
        ) + 
          geom_text(
            aes(
              label = paste0(
                after_stat(count), "%"
              )
            ),
            hjust = 1.1,
            stat  = "count"
          ) +
          expand_limits(y = 100) +
          theme (
            axis.text.x  = element_blank(),
            axis.text.y  = element_blank(),
            axis.title.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = unit(c(0,-55,0,0), "mm")
          ),
        
        matrix = (
          ComplexUpset::intersection_matrix(
            outline_color=list(
              active="#1a1a1a",
              inactive="#e5e8e8"
            )
          )
        ),
        themes = ComplexUpset::upset_default_themes(
          text = element_text(size=15)
        )
      )
      
      ggsave(
        glue::glue("output/viz/{country}/{country}_jg_upset.svg"), 
        width  = w, 
        height = h,
        dpi    = 300,
        units  = "mm",
        scale  = 0.75
      )
      
    }
  )
}
