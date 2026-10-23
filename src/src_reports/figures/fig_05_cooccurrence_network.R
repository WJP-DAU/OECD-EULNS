## Figura 05 · Network of Co-occurring Non-Trivial Justice Problems in {country}
## Fuente de datos: recode AJP_*_bin a no trivial 4<=sev<98 (F01.R:46-50) → network_graph.R:35-199:
##   prevalencia sin ponderar, lift, pesos lift>1.2 & n>=10, RNGkind + set.seed(42) layout_with_fr(w^2, niter
##   1000), pad 0.16, min_dist 0.28, hstretch 1.15; 12 nodos con text '(NN%)' + 66 aristas (todas las parejas)
## Método UE: none
## Código de origen: F01.R:43-52 + network_graph.R:35-199 (datos; sin ggsave :281-298) +
##   legacy_python/charts5.py:196-226 network() (diseño)
## Estado: pendiente (esqueleto de la Etapa 1; contrato en figures/_template.R)

fig_05 <- list(
  data = function(ctx) stop("pendiente: fig_05 sin implementar", call. = FALSE),
  plot = function(df, spec) stop("pendiente: fig_05 sin implementar", call. = FALSE)
)
