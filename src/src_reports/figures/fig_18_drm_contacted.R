## Figura 18 · Dispute Resolution Mechanisms Contacted in {country}
## Fuente de datos: contacted_mechanisms portada en fig_18 (module_2_3.R:77-259): access2drm == 1 (88-91);
##   1 → TRUE, 2/98/99 → FALSE (96-103); filter(none == FALSE) (104-110, 205); share = Σ_NUTS (casos/n_NUTS)·
##   regionpoppct sin renormalizar ni n < 30 (contacted_shares, 03_estimators.R); orden desc
## Método UE: none
## Código de origen: report_charts.R:224 chart_hbars_axis ← gen_horizontal_bars (worker-viz.R:372); datos
##   module_2_3.R:77-259 (sin el upset ni el ggsave :112-201)

fig_18 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "hbars", metodo = "wjpr", w_in = 7.278, h_in = 2.472, callout = FALSE, label_col = "hbars",
              metodo_ue = "none",
              nota = paste("reproducida desde los microdatos (59.75/56.03/51.19/…; antes leída del SVG; sin",
                           "filter(none) daría 55.21/51.84)")),

  # Entre quienes acudieron a un mecanismo de resolución (access2drm == 1), % que acudió a cada uno, con el estimador
  # del original (contacted_shares). Valores de control (2026-10-27): 59.75 / 56.03 / 51.19 / 49.63 / 42.04 / 27.69 /
  # 13.55 (sin filter(none == FALSE) darían 55.21 / 51.84 / …).
  data = function(ctx) {
    possible_mechanisms <- c(                                                 # origen: module_2_3.R:79-82
      "AJR_court_bin", "AJR_police_bin", "AJR_office_bin", "AJR_relig_bin",
      "AJR_arbitration_bin", "AJR_appeal_bin", "AJR_other_bin"
    )
    df_full <- ctx$national %>%                                               # origen: module_2_3.R:88-111
      filter(access2drm == 1) %>%                                             # adaptado: el país ya es ctx$national
      select(country_year_id, country_name_ltn, nuts_id, all_of(possible_mechanisms)) %>%
      mutate(
        across(!c(country_year_id, country_name_ltn, nuts_id), \(x) case_when(x == 1 ~ TRUE, x %in% c(2, 98, 99) ~ FALSE)),
        none = case_when(
          AJR_court_bin == FALSE & AJR_police_bin == FALSE &
            AJR_office_bin == FALSE & AJR_relig_bin == FALSE &
            AJR_arbitration_bin == FALSE & AJR_appeal_bin == FALSE &
            AJR_other_bin == FALSE ~ TRUE,
          TRUE ~ FALSE
        )
      )
    contacted_shares(ctx, df_full, possible_mechanisms) %>%
      mutate(category = case_when(                                            # origen: module_2_3.R:238-246
        category == "AJR_court_bin"       ~ "Court or Tribunal",
        category == "AJR_police_bin"      ~ "Police",
        category == "AJR_office_bin"      ~ "Gvt. Office",
        category == "AJR_relig_bin"       ~ "Rel. or Comm. Leader",
        category == "AJR_arbitration_bin" ~ "Third-party Mediator",
        category == "AJR_appeal_bin"      ~ "Complaints or Appeal Office",
        category == "AJR_other_bin"       ~ "Other"
      )) %>%
      rows_hbars(decimals = 0)
  },

  # Dibujo: WJPr::wjp_bars(direction = "horizontal") con sus argumentos (wjpr_hbars, R/09_wjpr.R; WJPr 1.2.0). Respaldo
  # sin WJPr: chart_hbars_axis(df, spec) de R/05_charts.R con metodo svg_r (sin portar: error legible).
  plot = function(df, spec) wjpr_hbars(df, spec)
)

## Prueba (2026-10-28, copia fix5/L2): cd /private/tmp && SRC_REPORTS_LIB=<wjpr_110_lib> Rscript …/main.R --country
## hungary --stage all --figs 13,14,18,19 --strict → ok · 23/23 · parecido f1 1.000 · local 0.035 (= galería); WJPr
## 1.2.0@1664a23: ok · 24/24 · f1 1.000 · local 0.000. Log (rows_hbars): 59.75 / 56.03 / 51.19 / … = control del plan;
## base 139, 128 con none == FALSE, 3 regiones NUTS. --country poland | malta: figures ok (7 filas), verify «sin
## referencia», en las dos builds.
