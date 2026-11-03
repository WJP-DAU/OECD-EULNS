## Figura 04 · Co-occurrence of Non-Trivial Justice Problems in {country}
## Fuente de datos: get_results_table(ctx, "nproblems", ctype = "short", prop = FALSE) con nproblems == 0 → NA:
##   media de problemas no triviales entre quienes tienen al menos uno (module_1.R:412-424 → 1_3)
## Método UE: none
## Código de origen: report_charts.R:131 chart_grouped(track = FALSE, value_at_end = TRUE) ← gen_grouped_bars(perc =
##   FALSE, decimals = 1) (worker-viz.R:136)

fig_04 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "grouped_bars", metodo = "wjpr", w_in = 6.069, h_in = 4.104, callout = TRUE, label_col = "std",
              metodo_ue = "none",
              nota = "valores con 1 decimal ('6.0'); eje 0-10"),

  # Media 0-10 (no porcentaje): una decimal en el texto ("4.9") y barra sobre escala 10
  data = function(ctx) {
    df <- ctx$national %>% mutate(nproblems = if_else(nproblems == 0, NA, nproblems))   # origen: module_1.R:414-418
    tbl <- get_results_table(ctx, target = "nproblems", ctype = "short", prop = FALSE, df = df)
    rows_grouped(tbl, eu = NULL, decimals = 1, scale = 10)
  },

  # Dibujo: wjpr_grouped_bars() detecta la media 0-10 (textos sin "%"): sin pista y valor tras la barra
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
