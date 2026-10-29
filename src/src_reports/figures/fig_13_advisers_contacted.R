## Figura 13 · Who People Contacted to Obtain Assistance and Representation in {country}
## Fuente de datos: contacted_advisers portada en fig_13 (module_2_2.R:107-292): access2rep == 1 (118-121);
##   AJD_adviser_1 → 2 si AJD_expert_adviser ∈ {2, 98, 99} (122-126); filter(none == FALSE) (237); share = Σ_NUTS
##   (casos/n_NUTS)·regionpoppct sin renormalizar ni n < 30 (contacted_shares, 03_estimators.R); orden desc
## Método UE: none
## Código de origen: report_charts.R:224 chart_hbars_axis ← horizontal_bars.R:1-67 gen_horizontal_bars
##   (WJPr::wjp_bars :22; worker-viz.R:268); datos module_2_2.R:107-292 (sin el upset ni el ggsave :145-233)

fig_13 <- list(

  # Entre quienes tuvieron acceso a asistencia apropiada (access2rep == 1), % que acudió a cada tipo de asesor, con el
  # estimador del original (contacted_shares: sin renormalizar ni suprimir). Valores de control (2026-10-27): 54.32 /
  # 22.26 / 21.15 / 16.29 / 11.40 / 9.06 / 7.68 / 3.16.
  data = function(ctx) {
    possible_advisers <- paste0("AJD_adviser_", 1:8)                          # origen: module_2_2.R:109-112
    df_full <- ctx$national %>%                                               # origen: module_2_2.R:118-144
      filter(access2rep == 1) %>%                                             # adaptado: el país ya es ctx$national
      mutate(AJD_adviser_1 = if_else(AJD_adviser_1 == 1 & (AJD_expert_adviser %in% c(2, 98, 99)), 2, AJD_adviser_1)) %>%
      select(country_year_id, country_name_ltn, nuts_id, all_of(possible_advisers)) %>%
      mutate(
        none = case_when(is.na(AJD_adviser_1) ~ TRUE),                       # no necesitó asesor
        # across() ve none (creada arriba en el mismo mutate): TRUE → TRUE, NA → FALSE, como en el original
        across(!c(country_year_id, country_name_ltn, nuts_id), \(x) case_when(x == 1 ~ TRUE, x == 2 ~ FALSE, is.na(x) ~ FALSE))
      )
    contacted_shares(ctx, df_full, possible_advisers) %>%
      mutate(category = case_when(                                            # origen: module_2_2.R:270-279
        category == "AJD_adviser_1" ~ "Friend or Family",
        # corregido: "Advisor" (module_2_2.R:272) → "Adviser", como lo publicado y el resto del reporte
        category == "AJD_adviser_2" ~ "Lawyer or Professional Adviser",
        category == "AJD_adviser_3" ~ "Gvt. Legal Aid",
        category == "AJD_adviser_4" ~ "Gvt. Body or Police",
        category == "AJD_adviser_5" ~ "Health/Welfare Professional",
        category == "AJD_adviser_6" ~ "Trade Union or Employer",
        category == "AJD_adviser_7" ~ "Religious or Community Leader",
        category == "AJD_adviser_8" ~ "Civil Society Organization"
      )) %>%
      rows_hbars(decimals = 0)
  },

  # Dibujo: WJPr::wjp_bars(direction = "horizontal") + complementos (wjpr_hbars, R/09_wjpr.R; metodo wjpr). Respaldo
  # sin WJPr: chart_hbars_axis(df, spec) de R/05_charts.R con metodo svg_r (sin portar: error legible).
  plot = function(df, spec) wjpr_hbars(df, spec)
)

## Prueba (2026-10-28, copia fix5/L2): cd /private/tmp && SRC_REPORTS_LIB=<wjpr_110_lib> Rscript …/main.R --country
## hungary --stage all --figs 13,14,18,19 --strict → ok · 25/25 · parecido f1 1.000 · local 0.083 (la galería con la
## referencia da lo mismo); sin SRC_REPORTS_LIB (WJPr 1.2.0@1664a23): ok · 26/26 · f1 1.000 · local 0.000. Log
## (rows_hbars): 54.32 / 22.26 / 21.15 / … = control del plan; base 250, 250 con none == FALSE, 3 regiones NUTS.
## --country poland | malta (configs de prueba): figures ok (8 filas), verify «sin referencia», en las dos builds.
