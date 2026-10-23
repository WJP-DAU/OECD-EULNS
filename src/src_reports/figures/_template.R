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
##    dibujar y verify lo compara con reference/<slug>/Figure_NN_slug.csv.
##  - value es el valor crudo (porcentaje 0-100, media 0-10, lift, coordenada); NA si está suprimido (n < 30).
##  - Todo número que aparece en la figura va en text/text2, formateado con fmt_num(x, decimals, pct):
##    "61%", "N/A", "6.0", "(40%)", "". plot() dibuja df$text tal cual y no recalcula ni formatea nada.
##  - Nada de "Hungary" literal: ctx$country, y {country} en el título del manifiesto.
##  - La media UE se declara en código: eu_value(ctx, "data4web", "<id>") o eu_est(ctx, "<var>"), y el id o la
##    variable en la columna eu_id del manifiesto. Con data4web es obligatorio: tools/build_reference.R toma de eu_id
##    los ids que guarda en reference/<slug>/eu_values.csv (un id nuevo: eu_id + correr la herramienta).
##  - ~80 líneas. Una función auxiliar usada solo por esta figura vive aquí; pasa a R/03_estimators.R cuando
##    la usan dos o más figuras.
##  - plot(df, spec) llama al motor de dibujo, WJPr: plot = function(df, spec) wjpr_<tipo>(df, spec) (metodo wjpr
##    y funcion wjpr_<tipo> en el manifiesto; R/09_wjpr.R: la función de WJPr del tipo + complementos), que
##    devuelve un ggplot marcado con attr(, "wjpr") (base, complementos, columnas cuyos textos se imprimen). La
##    etapa figures lo exporta a w_in × h_in con los tamaños de WJPr en pt (k = 1) y exige que los textos dibujados
##    sean df$text/text2 (estado "texto" si no); verify mide el parecido con la publicada (f1 ≥ verify$min_f1).
##    Respaldo sin WJPr: el chart_out de chart_<tipo>(df, spec) de R/05_charts.R (metodo svg_r) o un ggplot propio
##    (metodo ggplot, con base_pt). spec es la fila del manifiesto (fig, slug, titulo, tipo, metodo, funcion, w_in,
##    h_in, callout, label_col, base_pt, metodo_ue, eu_id, nota, …) + selected + country (= cfg$country: el país de
##    las leyendas {country} | EU Average). plot() no lee cfg ni ctx.
##  - h_in: con metodo wjpr el SVG sale siempre al w_in × h_in del manifiesto (el de la publicada) y un alto
##    equivocado deforma el dibujo, lo que verify ve en "parecido"; con svg_r es el alto que mide chart_<tipo>() (con
##    uno provisional, --stage figures --figs NN da "tamaño W×H in ≠ …" y H va al manifiesto con 3 decimales).
##  - Si el valor reproducido difiere del publicado no se fuerza: se anota en la columna nota del manifiesto
##    ([conocida: <label>=<text>]).
##
## Qué leer para escribir una figura nueva: este archivo, el bloque de su tipo en R/05_charts.R (rows_<tipo>() para
## data(); grep -n "^## Bloque" R/05_charts.R), el de R/09_wjpr.R (wjpr_<tipo>() para plot(); grep -n "^## Bloque
## wjpr" R/09_wjpr.R), una figura aprobada del mismo tipo y reference/<slug>/Figure_NN_slug.csv (labels, textos y
## orden exactos que compara verify). Nada más.
##
## Ejemplo completo: barras agrupadas con media UE de data4web (patrón de 1, 9, 17, 20, 22, 23, 25 y 38-40).
## Sustituya fig_NN por el número real. corregido: la lista anterior mezclaba figuras con eu_est o sin media UE.
## Las demás agrupadas cambian la media UE: eu = eu_est(ctx, "<var>") en 12, 28 y 31, y con transform =
## binary_agree en 33 y 35; eu = NULL en 4, 21, 24 y 26. La 4 es además una media 0-10 (decimals = 1 y scale = 10
## en rows_grouped) y la 27 no usa rows_grouped (sin fila National; ver su encabezado): wjpr_grouped_bars(df, spec)
## reconoce las dos por spec$fig (respaldo svg_r de la 4: chart_grouped(df, spec, track = FALSE, value_at_end =
## TRUE, scale = 10)).

## Figura NN · Access to Information and Advice in {country}
## Fuente de datos: get_results_table(ctx, "access2info", ctype = "full") (module_2_1.R:14-29);
##   UE eu_value(ctx, "data4web", "access2info")
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← grouped_bars.R gen_grouped_bars (worker-viz.R:178)

fig_NN <- list(

  # Estimación. get_results_table() devuelve tibble(grouping, category, value) con el promedio nacional
  # ponderado por NUTS (n < 30 → NA). rows_grouped() fija el orden (National Average, EU Average, grupos en el
  # orden de clabels y, dentro de cada grupo, alfabético inverso), kind (avg | header | row) y text con fmt_num().
  # ctype (grupos de filas): short = National + Gender, Age Group, Financial Situation, Area of Residence; full =
  # short + Category + Co-occurrent Problems; trust = short + Problem Status + Non-Trivial Problem Experienced;
  # custom_1 = short + Problem Status + Resolution. omit = c("Category") quita grupos por su nombre (clabel).
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "access2info", ctype = "full")
    rows_grouped(tbl, eu = eu_value(ctx, "data4web", "access2info"), decimals = 0)
  },

  # Dibujo: una llamada al adaptador WJPr del tipo (columna funcion del manifiesto, metodo wjpr). Devuelve un
  # ggplot; el respaldo sin WJPr es chart_grouped(df, spec) con metodo svg_r.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
