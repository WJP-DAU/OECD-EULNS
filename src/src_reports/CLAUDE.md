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
- `verify`: compara tablas y dibujo con `reference/<slug>/` → `tables/00-verificacion.csv`.
- `compare`: hojas publicada | nueva en `output/<slug>/compare/` (+ `index.html`).
- Consola: solo la tabla resumen (`fig | metodo | estado | verificacion | archivo`). Detalle: `output/<slug>/log/`.

| Archivo | Qué contiene |
|---|---|
| `R/00_setup.R` | config, deps.csv, fuentes (Inter Tight en `fonts/`), log, tabla de consola |
| `R/01_load.R` | lectura del `.dta`, master, `ctx`, caché, comprobaciones de `--stage data` |
| `R/02_variables.R` | variables derivadas (copias de `special-vars.R`, `setup.R`; `add_report_vars`) |
| `R/03_estimators.R` | `get_results_table`, `nat_est`, `eu_est`, `journey`, `perception_experience`, `xt`, `eu_value`, `memo` |
| `R/04_svg.R` | primitivas SVG, texto en trazos, `chart_out`, `svg_size`, `outline_svg_text`, `set_size` |
| `R/05_charts.R` | un bloque por tipo de gráfica: `rows_<tipo>()` (df del contrato) + `chart_<tipo>(df, spec)` |
| `R/06_export.R` | `run_figures`: tablas, SVG, PNG (Chrome), `figures_status.csv` |
| `R/07_verify.R` · `R/08_compare.R` | `run_verify`, `run_compare` |
| `R/09_wjpr.R` · `tools/wjpr_gallery.R` | `wjpr_<tipo>(df, spec)`: las figuras sobre WJPr + complementos (experimento, aún fuera de `main.R`) y su galería de fidelidad |
| `figures/fig_NN_slug.R` | una figura; `_template.R` es el contrato comentado |
| `manifest/figures.csv` | la verdad de cada figura (columnas en README, «Manifiesto»); `figures_<slug>.csv` las cambia por país |
| `reference/<slug>/` | valores y fracciones publicadas (verdad para verify); `tools/build_reference.R` los regenera |

## Contrato de una figura (detalle en `figures/_template.R`)
`fig_NN <- list(data = function(ctx) ..., plot = function(df, spec) ...)`
- `data(ctx)` → tibble `order, kind, grouping, label, value, value2, text, text2` (+ `x, y, n, lo, hi, series`).
  `value` crudo (NA si n < 30); `text`/`text2` = lo impreso, exacto, vía `fmt_num()`. `plot()` no formatea ni recalcula.
- `plot(df, spec)` → `chart_out` (svg_r, salida de `chart_<tipo>()`) o un ggplot (metodo ggplot). No lee cfg ni ctx: el
  país de las leyendas es `spec$country`.
- Encabezado obligatorio: título, fuente de datos, método UE, código de origen (archivo:líneas). ~80 líneas.
- Nada de `"Hungary"` literal: `ctx$country`, `{country}` en el manifiesto. Media UE declarada en código,
  `eu_value(ctx, "data4web", id)` o `eu_est(ctx, var)`, y en la columna `eu_id` del manifiesto (con data4web es de
  ahí de donde `tools/build_reference.R` saca los ids de `eu_values.csv`: un id nuevo exige eu_id + correrla).
- `get_results_table(ctx, target, ctype, …, omit)`: `short` = National + Gender, Age Group, Financial Situation, Area
  of Residence; `full` = short + Category + Co-occurrent Problems; `trust` = short + Problem Status + Non-Trivial
  Problem Experienced; `custom_1` = short + Problem Status + Resolution; `omit = "Category"` quita grupos por nombre.

## Cómo añadir o arreglar una figura
1. Leer solo: `figures/_template.R`, el bloque de su tipo en `R/05_charts.R` (`grep -n "^## Bloque" R/05_charts.R`), una
   figura aprobada del mismo tipo y `reference/<slug>/Figure_NN_slug.csv` (labels, textos y orden exactos que compara
   verify, p. ej. los `\n` de las etapas de los embudos). No hace falta leer más.
2. Escribir `figures/fig_NN_slug.R` y su fila del manifiesto (copiar la de una figura del mismo tipo; `status`,
   `verificada` y `wjpr` no se tocan: los actualiza el cierre de cada etapa).
3. Iterar con `Rscript src/src_reports/main.R --stage figures --figs NN` hasta `ok`: esa etapa muestra el motivo de
   `error`, `tamaño`, `texto` o `png` (con `--stage all` verify lo resume y el detalle queda en el log).
4. Fijar `h_in`: es el alto que mide `plot()`. Con uno provisional, `--stage figures` da `tamaño W×H in ≠ W×h del
   manifiesto`; copiar H (3 decimales). Barras agrupadas con STD: (6 + 15.5·filas + 22·encabezados) / 72 in. Si
   cambia solo para un país, va en `manifest/figures_<slug>.csv` (columnas `fig`, `h_in`).
5. `Rscript src/src_reports/main.R --stage all --figs NN` → `ok` en verify; mirar `output/<slug>/compare/`.
6. Si el valor reproducido difiere del publicado, no se fuerza: se anota en la columna `nota` del manifiesto
   (`[conocida: <label>=<text>]` hace que verify lo cuente como conocida).

## Reglas duras
- No modificar, mover ni borrar nada fuera de `src/src_reports/`. Microdatos solo en el caché (`cache/`, o `cache_dir` /
  `SRC_REPORTS_CACHE`): nunca en commits ni fuera del caché. `cache/`, `output/` y `config/local.yml` están en
  .gitignore, pero por defecto `cache/` vive dentro de esta carpeta (OneDrive): `cache_dir` permite sacarlo. No
  imprimir filas de microdatos en consola ni en el log.
- Paquetes fuera de base/tidyverse siempre con `::`; solo los de `deps.csv` (sin targets/R6/paquetes nuevos sin proponer).
- El código portado se copia tal cual con `# origen: archivo:líneas`; las únicas correcciones llevan `# corregido:`.
- Determinismo: locale C (`arrange(.locale = "C")`), showtext apagado, `set.seed()` explícito.
- Una figura que falla no detiene a las demás (estado `error`); `verify` sale con 0 salvo `--strict`.
- Git: un commit por etapa con `git add src/src_reports`; sin push salvo que se pida.
