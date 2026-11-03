## figures/_template.R — plantilla y contrato de una figura ------------------------------------------------
## Copie este archivo a figures/fig_NN_<slug>.R (NN = número de la figura en el reporte publicado, que es el
## inventario de figuras; slug en minúsculas_con_guiones: nombra tabla, SVG y referencia), rellene el encabezado, la
## spec y las dos funciones. main.R lo carga con sys.source() en un entorno propio y espera que el archivo termine
## definiendo fig_NN <- list(spec, data, plot). Nada más se ejecuta al cargar.
##
## Encabezado obligatorio (4 campos antes del objeto; un campo largo sigue en líneas «##   »):
##   ## Figura NN · <título del reporte publicado, con {country}> (primera línea: es spec$titulo)
##   ## Fuente de datos: <variables y estimador; de dónde sale la media UE si la hay>
##   ## Método UE: none | data4web | eu_est | eu_est_category | eu_est_distribution
##   ## Código de origen: <archivo:líneas del repo de donde se portó> (figura nueva: «nuevo» + las funciones que usa)
##
## Contrato (también en CLAUDE.md):
##  - spec = lo que el código necesita para dibujar y verificar (read_specs, R/00_setup.R; campos en SPEC_FIELDS):
##    tipo (grouped_bars, dumbbell, hbars, …: elige wjpr_<tipo>), metodo (wjpr | svg_r | ggplot), w_in y h_in
##    (tamaño final en pulgadas, el de la publicada, 3 decimales), callout (TRUE junto a un recuadro: w_in ≤
##    layout$max_w_callout_in; FALSE a página completa: ≤ max_w_full_in), label_col (std | dumb | hbars | own |
##    none), base_pt (metodo ggplot y los adaptadores de 3, 6, 7 y 29), metodo_ue (= Método UE), eu_id (data4web) y
##    nota. Por país, figures: figNN: de config/<pais>.yml cambia cualquiera de ellos (p. ej. h_in: 5.250).
##  - data(ctx) devuelve un tibble con las 8 columnas fijas: order, kind, grouping, label, value, value2, text,
##    text2 (extras permitidas: x, y, n, lo, hi, series). main.R lo escribe en output/<slug>/tables/ antes de
##    dibujar y verify lo compara con reference/<slug>/Figure_NN_slug.csv.
##  - value es el valor crudo (porcentaje 0-100, media 0-10, lift, coordenada); NA si está suprimido (n < 30).
##  - Todo número que aparece en la figura va en text/text2, formateado con fmt_num(x, decimals, pct):
##    "61%", "N/A", "6.0", "(40%)", "". plot() dibuja df$text tal cual y no recalcula ni formatea nada.
##  - Nada de "Hungary" literal: ctx$country, y {country} en el título del encabezado.
##  - La media UE se declara en código: eu_value(ctx, "data4web", "<id>") o eu_est(ctx, "<var>"), y el id o la
##    variable en eu_id de la spec. Con data4web es obligatorio: tools/build_reference.R toma de eu_id
##    los ids que guarda en reference/<slug>/eu_values.csv (un id nuevo: eu_id + correr la herramienta).
##  - ~80 líneas. Una función auxiliar usada solo por esta figura vive aquí; pasa a R/03_estimators.R cuando
##    la usan dos o más figuras.
##  - plot(df, spec) llama al motor de dibujo, WJPr: plot = function(df, spec) wjpr_<tipo>(df, spec) (metodo wjpr
##    en la spec; R/09_wjpr.R: la función de WJPr del tipo + complementos), que
##    devuelve un ggplot marcado con attr(, "wjpr") (base, complementos, columnas cuyos textos se imprimen). La
##    etapa figures lo exporta a w_in × h_in con los tamaños de WJPr en pt (k = 1) y exige que los textos dibujados
##    sean df$text/text2 (estado "texto" si no); si el adaptador expone lo dibujado por fila (drawn.csv), también el
##    nombre impreso en cada posición (label_drawn/grouping_drawn = label/grouping: "texto") y el valor de la
##    geometría (val/val2 = value/value2: "dibujo"); verify mide el parecido con la publicada (f1 ≥ verify$min_f1).
##    Respaldo sin WJPr: el chart_out de chart_<tipo>(df, spec) de R/05_charts.R (metodo svg_r) o un ggplot propio
##    (metodo ggplot, con base_pt). spec en plot() = fig y slug del nombre del archivo, titulo del encabezado, los
##    campos de fig_NN$spec, funcion (wjpr_<tipo>), selected y country (= cfg$country: el país de las leyendas
##    {country} | EU Average). plot() no lee cfg ni ctx.
##  - h_in: con metodo wjpr el SVG sale siempre al w_in × h_in de la spec (el de la publicada) y un alto
##    equivocado deforma el dibujo, lo que verify ve en "parecido"; con svg_r es el alto que mide chart_<tipo>() (con
##    uno provisional, --stage figures --figs NN da "tamaño W×H in ≠ …" y H va a la spec con 3 decimales).
##  - Si el valor reproducido difiere del publicado no se fuerza: se anota en la nota de la spec
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
## binary_agree en 33 y 35; sin media UE (21, 24, 26) rows_grouped(tbl, decimals = 0), como fig_21. Muestra pequeña:
## las de las secciones 2.1-2.5 quitan grupos con small_sample(ctx) (9, 12: Category; 17-25: + Age Group; 26: Age
## Group y Problem Status); una figura nueva sigue la regla de su sección o lo anota en la nota de la spec. La 4 es además una media 0-10 (decimals = 1 y scale = 10
## en rows_grouped) y la 27 no usa rows_grouped (sin fila National; ver su encabezado): wjpr_grouped_bars(df, spec)
## reconoce las dos por spec$fig (respaldo svg_r de la 4: chart_grouped(df, spec, track = FALSE, value_at_end =
## TRUE, scale = 10)).

## Figura NN · Access to Information and Advice in {country}
## Fuente de datos: get_results_table(ctx, "access2info", ctype = "full") (module_2_1.R:14-29);
##   UE eu_value(ctx, "data4web", "access2info")
## Método UE: data4web
## Código de origen: report_charts.R:131 chart_grouped ← grouped_bars.R gen_grouped_bars (worker-viz.R:178)

fig_NN <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (alto de las agrupadas:
  # CLAUDE.md, «Cómo añadir o arreglar una figura», punto 4)
  spec = list(tipo = "grouped_bars", metodo = "wjpr", w_in = 6.069, h_in = 8.375, callout = TRUE, label_col = "std",
              metodo_ue = "data4web", eu_id = "access2info"),

  # Estimación. get_results_table() devuelve tibble(grouping, category, value) con el promedio nacional
  # ponderado por NUTS (n < 30 → NA). rows_grouped() fija el orden (National Average, EU Average, grupos en el
  # orden de clabels y, dentro de cada grupo, alfabético inverso), kind (avg | header | row) y text con fmt_num().
  # ctype (grupos de filas): short = National + Gender, Age Group, Financial Situation, Area of Residence; full =
  # short + Category + Co-occurrent Problems; trust = short + Problem Status + Non-Trivial Problem Experienced;
  # custom_1 = short + Problem Status + Resolution. omit = c("Category") quita grupos por su nombre (clabel).
  data = function(ctx) {
    tbl <- get_results_table(ctx, target = "access2info", ctype = "full", omit = if (small_sample(ctx)) "Category")
    rows_grouped(tbl, eu = eu_value(ctx, "data4web", "access2info"), decimals = 0)
  },

  # Dibujo: una llamada al adaptador WJPr del tipo (wjpr_<tipo> de la spec, metodo wjpr). Devuelve un
  # ggplot; el respaldo sin WJPr es chart_grouped(df, spec) con metodo svg_r.
  plot = function(df, spec) wjpr_grouped_bars(df, spec)
)
