## Figura 34 · Indicators of Legal Capability in {country}
## Fuente de datos: AJE_legalrights National (4_1: 85.49) + access2info National (2_1_1: 61.46) + percepción
##   JSE_access2info National (F06: 27.19); frac = value/max
## Método UE: none
## Código de origen: report_charts.R:284 chart_funnel + standard_figures.R:108-119 funnel_rows('capability')

# Etapas de la capacidad legal, de arriba abajo: rótulo impreso (con "\n" donde la figura corta la línea).
# origen: standard_figures.R:114-117 (funnel_rows('capability'))
fig34_stages <- c("Reported knowledge of their legal rights", "Had access to information and advice",
                  "Believe people know where\nto obtain advice")

fig_34 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "funnel", metodo = "wjpr", w_in = 6.5, h_in = 2.278, callout = TRUE, label_col = "none",
              metodo_ue = "none",
              nota = "3 valores no contados en los 404; mismos valores que F09.csv"),

  # Promedios nacionales ponderados por NUTS: conocimiento de derechos (AJE_legalrights con binary_agree, la fila
  # National de 4_1), acceso a información (access2info, la de 2_1_1) y la percepción de JSE_access2info (fila National
  # Average de perception_experience, F06.R). frac = value / max la calcula wjpr_funnel().
  data = function(ctx) {
    v <- c(nat_est(ctx, "AJE_legalrights", binary_agree)$value[[1]], nat_est(ctx, "access2info")$value[[1]],
           perception_experience(ctx, "JSE_access2info", "access2info")$perception[[1]])
    log_msg("fig 34: ", paste(sprintf("%.2f", v), collapse = " · "))
    rows_funnel(stats::setNames(v, fig34_stages), decimals = 0)
  },

  # Dibujo: WJPr::wjp_funnel() con sus argumentos (wjpr_funnel, R/09_wjpr.R; WJPr 1.2.0). Respaldo sin WJPr:
  # chart_funnel(df, spec) con metodo svg_r (sin portar: error legible, R/05_charts.R).
  plot = function(df, spec) wjpr_funnel(df, spec)
)
