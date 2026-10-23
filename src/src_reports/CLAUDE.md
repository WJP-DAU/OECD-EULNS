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
| `figures/fig_NN_slug.R` | una figura; `_template.R` es el contrato comentado |
| `manifest/figures.csv` | la verdad de cada figura: tamaño, tipo, método, función, fuente de datos, método UE, nota |
| `reference/<slug>/` | valores y fracciones publicadas (verdad para verify); `tools/build_reference.R` los regenera |

## Contrato de una figura (detalle en `figures/_template.R`)
`fig_NN <- list(data = function(ctx) ..., plot = function(df, spec) ...)`
- `data(ctx)` → tibble `order, kind, grouping, label, value, value2, text, text2` (+ `x, y, n, lo, hi, series`).
  `value` crudo (NA si n < 30); `text`/`text2` = lo impreso, exacto, vía `fmt_num()`. `plot()` no formatea ni recalcula.
- `plot(df, spec)` → `chart_out` (svg_r, salida de `chart_<tipo>()`) o un ggplot (metodo ggplot). No lee cfg ni ctx.
- Encabezado obligatorio: título, fuente de datos, método UE, código de origen (archivo:líneas). ~80 líneas.
- Nada de `"Hungary"` literal: `ctx$country`, `{country}` en el manifiesto. Media UE declarada en código:
  `eu_value(ctx, "data4web", id)` o `eu_est(ctx, var)`.

## Cómo añadir o arreglar una figura
1. Leer solo: `figures/_template.R`, el bloque de su tipo en `R/05_charts.R` (`grep -n "^## Bloque" R/05_charts.R`) y una
   figura aprobada del mismo tipo. No hace falta leer más.
2. Escribir `figures/fig_NN_slug.R` y su fila del manifiesto (o editarla).
3. `Rscript src/src_reports/main.R --stage all --figs NN` → `ok` en verify; mirar `output/<slug>/compare/`.
4. Si el valor reproducido difiere del publicado, no se fuerza: se anota en la columna `nota` del manifiesto
   (`[conocida: <label>=<text>]` hace que verify lo cuente como conocida).

## Reglas duras
- No modificar, mover ni borrar nada fuera de `src/src_reports/`. Microdatos nunca en la carpeta ni en commits
  (`cache/`, `output/`, `config/local.yml` están ignorados). No imprimir filas de microdatos en consola ni en el log.
- Paquetes fuera de base/tidyverse siempre con `::`; solo los de `deps.csv` (sin targets/R6/paquetes nuevos sin proponer).
- El código portado se copia tal cual con `# origen: archivo:líneas`; las únicas correcciones llevan `# corregido:`.
- Determinismo: locale C (`arrange(.locale = "C")`), showtext apagado, `set.seed()` explícito.
- Una figura que falla no detiene a las demás (estado `error`); `verify` sale con 0 salvo `--strict`.
- Git: un commit por etapa con `git add src/src_reports`; sin push salvo que se pida.
