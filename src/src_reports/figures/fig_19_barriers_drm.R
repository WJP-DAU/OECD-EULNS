## Figura 19 · Reasons Preventing Access to DRMs in {country}
## Fuente de datos: access2DRM_barriers (module_2_3.R:298-396 → 2_3_5): AJR_noresol_reason entre access2drm == 0;
##   share = Σ_NUTS (casos/n_NUTS)·regionpoppct sin renormalizar ni n < 30 (reason_shares, 03_estimators.R); orden desc
## Método UE: none
## Código de origen: report_charts.R:224 chart_hbars_axis ← gen_horizontal_bars (worker-viz.R:389); datos
##   module_2_3.R:298-396 (sin write_csv :348 ni kable :350-379); orden standard_figures.R:334-339 (spec_hbars)

fig_19 <- list(

  # Entre quienes no acudieron a un mecanismo de resolución aunque lo necesitaban (access2drm == 0), % que dio cada
  # motivo. access2drm == 0 exige AJR_noresol_reason ∈ {3, 5, 6, 7, 8} (02_variables.R:480): solo esos cinco motivos
  # pueden salir. Valores de control (2026-10-27): 37.16 / 22.32 / 20.96 / 11.18 / 8.29.
  data = function(ctx) {
    reasons <- function(target) case_when(                                     # origen: module_2_3.R:307-320
      target == 1 ~ "Issue was not important",
      target == 2 ~ "Respondent was confidence to solve the issue alone",
      target == 3 ~ "Time or bureaucracy",
      target == 4 ~ "No evidence or strong case",
      target == 5 ~ "Fear of retaliation",
      target == 6 ~ "Did not know what to do or where to go",
      target == 7 ~ "Access problems",
      target == 8 ~ "Lack of trust in authorities",
      target == 9 ~ "Fear of consequences",
      target == 10 ~ "It was up to the other party",
      target == 11 ~ "Other",
      target %in% c(98, 99) ~ "DK/NA"
    )
    reason_shares(ctx, target = "AJR_noresol_reason", filter_var = "access2drm", recode = reasons) %>%
      rows_hbars(decimals = 0)
  },

  # Dibujo: WJPr::wjp_bars(direction = "horizontal") + complementos (wjpr_hbars, R/09_wjpr.R; metodo wjpr). Respaldo
  # sin WJPr: chart_hbars_axis(df, spec) de R/05_charts.R con metodo svg_r (hoy pendiente).
  plot = function(df, spec) wjpr_hbars(df, spec)
)

## Prueba (2026-10-28, copia fix5/L2): cd /private/tmp && SRC_REPORTS_LIB=<wjpr_110_lib> Rscript …/main.R --country
## hungary --stage all --figs 13,14,18,19 --strict → ok · 19/19 · parecido f1 1.000 · local 0.125 (= galería); WJPr
## 1.2.0@1664a23: ok · 20/20 · f1 1.000 · local 0.000. Log: base 349, 5 motivos, 3 regiones; 37.16 / 22.32 / 20.96 /
## 11.18 / 8.29 (|Δ| < 0.0001 frente a la referencia). --country poland | malta: figures ok (5 filas), verify
## «sin referencia», en las dos builds.
