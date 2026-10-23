## Figura 13 · Who People Contacted to Obtain Assistance and Representation in {country}
## Fuente de datos: contacted_advisers portada en fig_13 (module_2_2.R:107-292): access2rep==1 (118-121);
##   AJD_adviser_1 → 2 si AJD_expert_adviser ∈ {2,98,99} (122-126); filter(none==FALSE) (237); share = Σ_NUTS
##   (casos/n_NUTS)·regionpoppct sin renormalizar; sin n<30; orden desc
## Método UE: none
## Código de origen: report_charts.R:224 chart_hbars_axis ← horizontal_bars.R:1-67 gen_horizontal_bars
##   (WJPr::wjp_bars :22; worker-viz.R:268); datos module_2_2.R:107-292 (sin el ggsave :229-233)
## Estado: pendiente (esqueleto de la Etapa 1; contrato en figures/_template.R)

fig_13 <- list(
  data = function(ctx) stop("pendiente: fig_13 sin implementar", call. = FALSE),
  plot = function(df, spec) stop("pendiente: fig_13 sin implementar", call. = FALSE)
)
