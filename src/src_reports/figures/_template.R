## figures/_template.R — plantilla y contrato de una figura ------------------------------------------------
## Copie este archivo a figures/fig_NN_<slug>.R (NN y slug = columnas fig y slug de manifest/figures.csv),
## rellene el encabezado y las dos funciones. main.R lo carga con sys.source() en un entorno propio y espera
## que el archivo termine definiendo fig_NN <- list(data, plot). Nada más se ejecuta al cargar.
##
## Encabezado obligatorio (4 líneas, antes del objeto):
##   ## Figura NN · <titulo del manifiesto, con {country}>
##   ## Fuente de datos: <variables y estimador; de dónde sale la media UE si la hay>
##   ## Método UE: none | data4web | eu_est | eu_est_category | eu_est_distribution
##   ## Código de origen: <archivo:líneas del repo de donde se portó>
##
## Contrato (también en CLAUDE.md):
##  - data(ctx) devuelve un tibble con las 8 columnas fijas: order, kind, grouping, label, value, value2, text,
##    text2 (extras permitidas: x, y, n, lo, hi, series). main.R lo escribe en output/<slug>/tables/ antes de
##    dibujar y verify lo compara con reference/<slug>/.
##  - value es el valor crudo (porcentaje 0-100, media 0-10, lift, coordenada); NA si está suprimido (n < 30).
##  - Todo número que aparece en la figura va en text/text2, formateado con fmt_num(x, decimals, pct):
##    "61%", "N/A", "6.0", "(40%)", "". plot() dibuja df$text tal cual y no recalcula ni formatea nada.
##  - Nada de "Hungary" literal: ctx$country, y {country} en el título del manifiesto.
##  - La media UE se declara en código: eu_value(ctx, "data4web", "<id>") o eu_est(ctx, "<var>").
##  - ~80 líneas. Una función auxiliar usada solo por esta figura vive aquí; pasa a R/03_estimators.R cuando
##    la usan dos o más figuras.
##  - plot(df, spec) devuelve un ggplot (metodo ggplot) o un chart_out (metodo svg_r: lo que devuelve
##    chart_<tipo>()). spec es la fila del manifiesto (fig, slug, titulo, tipo, metodo, funcion, w_in, h_in,
##    callout, label_col, base_pt, metodo_ue, nota). plot() no lee cfg ni ctx.
##  - Si el valor reproducido difiere del publicado no se fuerza: se anota en la columna nota del manifiesto.
##
## Qué leer para escribir una figura nueva: este archivo, el bloque de su tipo en R/05_charts.R
## (grep -n "^## Bloque" R/05_charts.R) y una figura aprobada del mismo tipo. Nada más.
##
## Ejemplo completo: barras agrupadas con media UE de data4web (patrón de 1, 9, 12, 17, 20-26, 28, 31, 33, 35,
## 38-40). Sustituya fig_NN por el número real.

## Figura NN · Access to Information and Advice in {country}
## Fuente de datos: get_results_table(ctx, "access2info", ctype = "full") (module_2_1.R:14-29);
##   UE eu_value(ctx, "data4web", "access2info")
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← grouped_bars.R gen_grouped_bars (worker-viz.R:178)

fig_NN <- list(

  # Estimación. get_results_table() devuelve tibble(grouping, category, value) con el promedio nacional
  # ponderado por NUTS (n < 30 → NA). rows_grouped() fija el orden (National Average, EU Average, grupos en el
  # orden de clabels y, dentro de cada grupo, alfabético inverso), kind (avg | header | row) y text con fmt_num().
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "access2info", ctype = "full")
    rows_grouped(tbl, eu = eu_value(ctx, "data4web", "access2info"), decimals = 0)
  },

  # Dibujo: una llamada a la función del tipo (columna funcion del manifiesto). Devuelve chart_out.
  plot = function(df, spec) chart_grouped(df, spec)
)
