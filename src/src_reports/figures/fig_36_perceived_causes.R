## Figura 36 · Perceived Causes of Justice Problems in {country}
## Fuente de datos: perceived_causes (module_4.R:44-143: AJE_description_* ponderado NUTS; sin n<30) → 4_2;
##   orden desc
## Método UE: none
## Código de origen: report_charts.R:224 chart_hbars_axis + standard_figures.R:324-331 spec_causes (diseño de
##   la 13 desde 16-oct)

# origen: module_4.R:44-102 (perceived_causes), literal salvo lo marcado: por región NUTS, menciones / encuestados que
# respondieron esa causa (1 = sí, 2 = no; el resto no cuenta) × regionpoppct, sumado sobre las regiones del país × 100
# (sin renormalizar ni suprimir n < 30). adaptado: master → ctx$national, regions → ctx$regions; sin write_csv
# (4_2_perceived_causes.csv), sin kable ni pivot_wider. Devuelve tibble(category, value). Solo la usa esta figura.
fig36_causes <- function(ctx) {
  regions <- ctx$regions
  ctx$national %>%
    select(country_name_ltn, nuts_id, starts_with("AJE_description")) %>%
    pivot_longer(!c(country_name_ltn, nuts_id), values_to = "mentioned", names_to = "category") %>%
    mutate(
      category = case_when(
        category == "AJE_description_1" ~ "Bad luck / Part of life",
        category == "AJE_description_2" ~ "Bureaucratic",
        category == "AJE_description_3" ~ "Family or private matter",
        category == "AJE_description_4" ~ "Legal",
        category == "AJE_description_5" ~ "Political",
        category == "AJE_description_6" ~ "Social or community matter",
        category == "AJE_description_7" ~ "Economic",
        category == "AJE_description_8" ~ "None of these",
        category == "AJE_description_98" ~ "Don't know",
        category == "AJE_description_99" ~ "No answer"
      )
    ) %>%
    mutate(mentioned = case_when(mentioned == 1 ~ 1, mentioned == 2 ~ 0), counter = if_else(is.na(mentioned), 0, 1)) %>%
    group_by(country_name_ltn, nuts_id, category) %>%
    summarise(total_sample = sum(counter, na.rm = TRUE), mentions = sum(mentioned, na.rm = TRUE), .groups = "keep") %>%
    left_join(regions %>% select(nuts_id, weight = regionpoppct), by = "nuts_id") %>%
    mutate(proportion_weighted = (mentions/total_sample)*weight) %>%
    group_by(country_name_ltn, category) %>%
    summarise(proportion_national = sum(proportion_weighted, na.rm = TRUE)*100, .groups = "keep") %>%
    ungroup() %>%
    filter(!is.na(category)) %>%                       # nuevo: columnas AJE_description_* sin etiqueta, fuera
    select(category, value = proportion_national)
}

fig_36 <- list(

  # Una barra por causa percibida, la más citada primero (rows_hbars, R/05_charts.R).
  data = function(ctx) rows_hbars(fig36_causes(ctx), decimals = 0),

  # Dibujo: WJPr::wjp_bars() horizontal con sus argumentos (wjpr_hbars, R/09_wjpr.R; WJPr 1.2.0), el diseño de la 13.
  # Respaldo sin WJPr: chart_hbars_axis(df, spec) (sin portar: error legible, R/05_charts.R).
  plot = function(df, spec) wjpr_hbars(df, spec)
)
