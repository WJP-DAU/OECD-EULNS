## Figura 03 · Prevalence and Severity of Non-Trivial Justice Problems in {country}, by Area of Residence
## Fuente de datos: category_profile('residence') (F02.R:14-25): prevalencia = nat_est sobre problem_flags por
##   residence; severidad media = nuts_mean_long sobre problems_long (todos los problemas, sev 0-10) por
##   category x residence; punto hueco si n<30; rótulos top_cats; facetas Rural/Urban
## Método UE: none
## Código de origen: F02.R:33-50 run_F02 (geom_point + lm dashed + ggrepel seed 7) + svg_text.R:109
##   outline_svg_text + set_size (base 12)
## Estado: pendiente (esqueleto de la Etapa 1; contrato en figures/_template.R)

fig_03 <- list(
  data = function(ctx) stop("pendiente: fig_03 sin implementar", call. = FALSE),
  plot = function(df, spec) stop("pendiente: fig_03 sin implementar", call. = FALSE)
)
