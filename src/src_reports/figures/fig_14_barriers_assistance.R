## Figura 14 · Reasons Preventing Access to Assistance and Representation in {country}
## Fuente de datos: access2representation_barriers (module_2_2.R:301-398 → 2_2_5): AJD_noadvice_reason entre
##   access2rep == 0; share = Σ_NUTS (casos/n_NUTS)·regionpoppct sin renormalizar ni n < 30 (reason_shares,
##   03_estimators.R); orden desc
## Método UE: none
## Código de origen: report_charts.R:224 chart_hbars_axis ← gen_horizontal_bars (worker-viz.R:285); datos
##   module_2_2.R:301-398 (sin write_csv :350 ni kable :352-381); orden standard_figures.R:334-339 (spec_hbars)

fig_14 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "hbars", metodo = "wjpr", w_in = 7.278, h_in = 2.778, callout = FALSE, label_col = "hbars",
              metodo_ue = "none",
              nota = paste("8 motivos: access2rep == 0 exige AJD_noadvice_reason ∈ {4-10, 98} (02_variables.R:462),",
                           "1-3 no salen; referencia = fracciones de las barras publicadas (v1_frac·100)")),

  # Entre quienes no tuvieron acceso a asistencia apropiada (access2rep == 0), % que dio cada motivo. Solo aparecen los
  # motivos presentes en la base: access2rep == 0 exige AJD_noadvice_reason ∈ {4, ..., 10, 98} (02_variables.R:462),
  # así que 1-3 nunca salen. Valores de control (2026-10-27): 20.85 / 17.04 / 16.63 / 16.30 / 12.94 / 8.00 / 4.34 / 3.79.
  data = function(ctx) {
    reasons <- function(target) case_when(                                     # origen: module_2_2.R:310-322
      target == 1 ~ "Issue was not important",
      target == 2 ~ "Other side was right",
      target == 3 ~ "Not needed advice",
      target == 4 ~ "Financial cost",
      target == 5 ~ "Previous experiences",
      target == 6 ~ "Did not know who to contact",
      target == 7 ~ "Did not know they could get advice",
      target == 8 ~ "Fear",
      target == 9 ~ "Advisers were too far away",
      target == 10 ~ "Other",
      target %in% c(98, 99) ~ "Don't Know / No Answer"
    )
    reason_shares(ctx, target = "AJD_noadvice_reason", filter_var = "access2rep", recode = reasons) %>%
      rows_hbars(decimals = 0)
  },

  # Dibujo: WJPr::wjp_bars(direction = "horizontal") con sus argumentos (wjpr_hbars, R/09_wjpr.R; WJPr 1.2.0). Respaldo
  # sin WJPr: chart_hbars_axis(df, spec) de R/05_charts.R con metodo svg_r (sin portar: error legible).
  plot = function(df, spec) wjpr_hbars(df, spec)
)

## Prueba (2026-10-28, copia fix5/L2): cd /private/tmp && SRC_REPORTS_LIB=<wjpr_110_lib> Rscript …/main.R --country
## hungary --stage all --figs 13,14,18,19 --strict → ok · 25/25 · parecido f1 1.000 · local 0.056 (= galería); WJPr
## 1.2.0@1664a23: ok · 26/26 · f1 1.000 · local 0.000. Log: base 440 con motivo (531 con access2rep == 0), 8 motivos,
## 3 regiones; 20.85 / 17.04 / 16.63 / … (|Δ| < 0.0001 frente a la referencia). --country poland: 8 filas;
## malta: 7 (sin código 9; el alto de 8 filas del manifiesto deja una fila vacía abajo); verify «sin referencia».
