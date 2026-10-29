# src_reports — guía para Claude Code

Carpeta autocontenida que produce las figuras del reporte *Paths to Justice* desde los microdatos (carga → variables →
estimación → figura → verificación). Un solo punto de entrada, desde cualquier directorio:

    Rscript src/src_reports/main.R [--stage data|figures|verify|compare|all] [--figs 09,12] [--country hungary]
                                   [--force] [--no-png] [--strict] [--help]

Rutas de la máquina en `config/local.yml` (copiar de `local.yml.example`; está en .gitignore). Nada aquí hace `source()`
fuera de la carpeta ni usa `Sys.info()["user"]`.

## Flujo y archivos
- `data`: lee el `.dta` una vez → `cache/master.rds` (27 países) → `cache/ctx_<slug>.rds`; comprobaciones en el log.
- `figures`: por figura escribe `output/<slug>/tables/Figure_NN_slug.csv` (salida de `data()`) ANTES de dibujar, y luego
  `figures/Figure_NN_slug.svg|png` (300 dpi; texto en trazos; tamaño del manifiesto).
- `verify`: tablas y dibujo frente a `reference/<slug>/` → `tables/00-verificacion.csv`. `compare`: hojas publicada | nueva
  en `output/<slug>/compare/` (+ `index.html`).
- Consola: solo la tabla resumen (`fig | metodo | estado | verificacion | archivo`). Detalle: `output/<slug>/log/`.

| Archivo | Qué contiene |
|---|---|
| `R/00_setup.R` | config, deps.csv (versión y commit), fuentes (Inter Tight en `fonts/`), log, tabla de consola |
| `R/01_load.R` | lectura del `.dta`, master, `ctx`, caché, comprobaciones de `--stage data` |
| `R/02_variables.R` | variables derivadas (copias de `special-vars.R`, `setup.R`; `add_report_vars`) |
| `R/03_estimators.R` | `get_results_table`, `nat_est`, `eu_est`, `journey`, `perception_experience`, `xt`, `eu_value`, `memo`, `problems_long`, `cooc_flags`, `contacted_shares`, `reason_shares` |
| `R/04_svg.R` | primitivas SVG, texto en trazos, `chart_out`, `svg_size`, `outline_svg_text`, `set_size` |
| `R/05_charts.R` | un bloque por tipo: `rows_<tipo>()` (df del contrato) + `chart_<tipo>(df, spec)` (respaldo svg_r) |
| `R/06_export.R` | `run_figures`: tablas, SVG, PNG (Chrome), textos dibujados, `figures_status.csv` |
| `R/07_verify.R` · `R/08_compare.R` | `run_verify` (tablas, parecido con la publicada), `run_compare` |
| `R/09_wjpr.R` · `tools/wjpr_gallery.R` | el motor: `wjpr_<tipo>(df, spec)` = la función de WJPr 1.2.0 del tipo (+ lo que le falta); su galería |
| `figures/fig_NN_slug.R` | una figura; `_template.R` es el contrato comentado |
| `manifest/figures.csv` | la verdad de cada figura (columnas en README, «Manifiesto»); `figures_<slug>.csv` las cambia por país |
| `reference/<slug>/` | valores y fracciones publicadas (verdad para verify); `tools/build_reference.R` los regenera |

## Motor de dibujo: WJPr (detalle en README, «WJPr»)
- Por defecto `metodo wjpr`: `plot = function(df, spec) wjpr_<tipo>(df, spec)` = la función de WJPr 1.2.0 del tipo con
  sus argumentos (`layout = wjp_layout(…)`, `value_*`, `label_*`, `ptheme = WJP_theme(…) + theme(…)`), medidas de
  `STD`/`ST_*`/`LABEL_COL` y de spec; el ggplot sale a `w_in × h_in` exactos (k = 1). Respaldo: `chart_<tipo>()` con
  `metodo svg_r`. Lo que 1.2.0 aún no cubre: `# complemento: <qué falta>` + su propuesta de argumento.
- `wjpr_require()` exige WJPr ≥ 1.2.0; `deps.csv` fija 1.2.0@1664a23 (exacta); otra con `SRC_REPORTS_LIB=<librería>` da
  `aviso deps.csv: …`; `log/figures_status.csv` guarda la build y los complementos de cada figura.
- Textos visibles (y `rotulos`) ≠ `df$text/text2` → `texto`. Lo dibujado por fila (`wjpr_meta(…, drawn =)`, las 42;
  `drawn.csv`): nombre impreso (`label_drawn`, `grouping_drawn`) ≠ df → `texto`; valor de la geometría (`val`) o marca
  fija ausente (`marcas =`) → `dibujo`. verify: `parecido` (f1 ≥ 0.995 frente a `published/`), `parecido_local` (≤ 0.5)
  y `drawn.csv` frente a la referencia; un estado de figures distinto de ok sale `fallo`; sin PNG, `sin dibujo` (`--strict` sale con 1).

## Contrato de una figura (detalle en `figures/_template.R`)
`fig_NN <- list(data = function(ctx) ..., plot = function(df, spec) ...)`
- `data(ctx)` → tibble `order, kind, grouping, label, value, value2, text, text2` (+ `x, y, n, lo, hi, series`).
  `value` crudo (NA si n < 30); `text`/`text2` = lo impreso, exacto, vía `fmt_num()`. `plot()` no formatea ni recalcula.
- `plot(df, spec)` → ggplot de `wjpr_<tipo>()` (respaldo: `chart_out` de `chart_<tipo>()`); no lee cfg ni ctx (país de las
  leyendas: `spec$country`). Encabezado: título, fuente de datos, método UE, código de origen (archivo:líneas). ~80 líneas.
- Nada de `"Hungary"` literal (`ctx$country`, `{country}` en el manifiesto); una elección editorial del país (p. ej. las
  destacadas de la 3) va en `figures:` de `config/<pais>.yml` y `data()` la pone en el df. Media UE en código
  (`eu_value(ctx, "data4web", id)` o `eu_est(ctx, var)`) y en `eu_id` del manifiesto (lo lee `tools/build_reference.R`).
- `get_results_table(ctx, target, ctype, …, omit)`: `short` = National + Gender, Age Group, Financial Situation, Area of
  Residence; `full` = short + Category + Co-occurrent Problems; `trust` = short + Problem Status + Non-Trivial Problem
  Experienced; `custom_1` = short + Problem Status + Resolution; `omit = "Category"` quita grupos por nombre.

## Cómo añadir o arreglar una figura
1. Leer solo `figures/_template.R`, el bloque del tipo en `R/05_charts.R` y en `R/09_wjpr.R` (`grep -n "^## Bloque"`),
   una figura aprobada del mismo tipo y `reference/<slug>/Figure_NN_slug.csv` (labels, textos y orden exactos).
2. Escribir `figures/fig_NN_slug.R` y su fila del manifiesto (la de una figura del mismo tipo; `status`, `verificada` y
   `wjpr` los actualiza el cierre de cada etapa).
3. Iterar con `… main.R --stage figures --figs NN` hasta `ok` (da el motivo de `error`, `tamaño`, `texto` o `png`).
4. Fijar `h_in` (3 decimales): con wjpr el SVG sale siempre al tamaño del manifiesto y un alto equivocado lo ve
   `parecido`; con svg_r es el que mide `chart_<tipo>()`. Agrupadas: (6 + 15.5·filas + 22·encabezados) / 72 in;
   horizontales: (24 + 22·filas) / 72 in. Si cambia solo para un país (la 5 y la 11: el log da el que pide), va en `manifest/figures_<slug>.csv` (`fig,h_in`).
5. `Rscript src/src_reports/main.R --stage all --figs NN` → `ok` en verify; mirar `output/<slug>/compare/`. 6. Un valor
   que difiere del publicado no se fuerza: va a la `nota` del manifiesto (`[conocida: <label>=<text>]` → conocida).

## Reglas duras
- No modificar, mover ni borrar nada fuera de `src/src_reports/`. Microdatos solo en el caché (`cache/`, `cache_dir` o
  `SRC_REPORTS_CACHE`; en .gitignore con `output/` y `config/local.yml`): nunca en commits, en consola ni en el log.
- Paquetes fuera de base/tidyverse siempre con `::`; solo los de `deps.csv` (sin targets/R6/paquetes nuevos sin proponer).
- Código portado tal cual con `# origen: archivo:líneas` (correcciones: `# corregido:`). Determinismo: locale C
  (`arrange(.locale = "C")`), showtext apagado, `set.seed()` explícito.
- Una figura que falla no detiene a las demás (estado `error`); `verify` sale con 0 salvo `--strict`. Git: un commit por
  etapa con `git add src/src_reports`; sin push salvo que se pida.
