# src_reports — guía para Claude Code

Carpeta autocontenida que produce las figuras del reporte *Paths to Justice* desde los microdatos (carga → variables →
estimación → figura → verificación). Un solo punto de entrada, desde cualquier directorio:

    Rscript src/src_reports/main.R [--stage data|figures|verify|compare|all] [--figs 09,12] [--country hungary]   # o <carpeta>/main.R
                                   [--force] [--no-png] [--strict] [--help]

Paquetes: renv (`renv.lock`; `renv::restore()` una vez; `R/00_setup.R` lo activa desde cualquier cwd). Rutas de la
máquina: `config/local.yml` (en .gitignore). Sin `source()` de fuera ni `Sys.info()`. Inventario = el PDF publicado (fuera).

## Flujo y archivos
- `data`: lee el `.dta` una vez → `data/master.rds` (27 países) → `data/ctx_<slug>.rds`; comprobaciones en el log.
- `figures`: por figura escribe `output/<slug>/tables/Figure_NN_slug.csv` (salida de `data()`) ANTES de dibujar, y luego
  `figures/Figure_NN_slug.svg|png` (300 dpi; texto en trazos; tamaño de la spec).
- `verify`: tablas y dibujo frente a `reference/<slug>/` → `tables/00-verificacion.csv`. `compare`: publicada | nueva en
  `output/<slug>/compare/`. Consola: solo la tabla (`fig | metodo | estado | verificacion | archivo`); detalle en `log/`.

| Archivo | Qué contiene |
|---|---|
| `R/00_setup.R` | renv, config, `renv.lock` (versión y commit), fuentes (`fonts/`), `read_specs`, log, consola |
| `R/01_load.R` | lectura del `.dta`, master, `ctx`, caché, comprobaciones de `--stage data` |
| `R/02_variables.R` | variables derivadas (copias de `special-vars.R`, `setup.R`; `add_report_vars`) |
| `R/03_estimators.R` | `get_results_table`, `nat_est`, `eu_est`, `journey`, `perception_experience`, `xt`, `eu_value`, `memo`, `problems_long`, `cooc_flags`, `contacted_shares`, `reason_shares` |
| `R/04_svg.R` | primitivas SVG, texto en trazos, `chart_out`, `svg_size`, `outline_svg_text`, `set_size` |
| `R/05_charts.R` | un bloque por tipo: `rows_<tipo>()` (df del contrato) + `chart_<tipo>(df, spec)` (respaldo svg_r) |
| `R/06_export.R` | `run_figures`: tablas, SVG, PNG (Chrome), textos dibujados, `figures_status.csv` |
| `R/07_verify.R` · `R/08_compare.R` | `run_verify` (tablas, parecido con la publicada), `run_compare` |
| `R/09_wjpr.R` · `tools/wjpr_gallery.R` | el motor: `wjpr_<tipo>(df, spec)` = la función de WJPr 1.2.0 del tipo (+ lo que le falta); su galería |
| `figures/fig_NN_slug.R` | una figura: `spec` (tipo, metodo, w_in, h_in…; README, «Spec de cada figura»), `data`, `plot`; `_template.R` es el contrato |
| `data/` | `regions.csv` (versionado) y el caché de microdatos (ignorado) |
| `reference/<slug>/` | valores y fracciones publicadas (verdad para verify); `tools/build_reference.R` los regenera |
| `tools/inyecciones/` | errores inyectados en el dibujo; `correr.sh` comprueba que figures y verify los ven |

## Motor de dibujo: WJPr (detalle en README, «WJPr»)
- Por defecto `metodo wjpr`: `plot = function(df, spec) wjpr_<tipo>(df, spec)` = la función de WJPr 1.2.0 del tipo con
  sus argumentos (`layout = wjp_layout(…)`, `value_*`, `label_*`, `ptheme = WJP_theme(…) + theme(…)`), medidas de
  `STD`/`ST_*`/`LABEL_COL` y de spec; el ggplot sale a `w_in × h_in` exactos (k = 1). Respaldo: `chart_<tipo>()` con
  `metodo svg_r`. Lo que 1.2.0 aún no cubre: `# complemento: <qué falta>` + su propuesta de argumento.
- `wjpr_require()` exige WJPr ≥ 1.2.0; `renv.lock` fija 1.2.0@1664a23 (otra: `aviso renv.lock`); build en `figures_status`.
- Textos visibles ≠ `df$text/text2` → `texto`; lo dibujado por fila (`drawn.csv`, las 42): nombre impreso ≠ df → `texto`,
  valor de la geometría o marca fija ausente → `dibujo`. verify: `parecido` (f1 ≥ 0.995), `parecido_local` (≤ 0.5) y
  `drawn.csv` frente a la referencia; figures no ok → `fallo`; sin PNG, `sin dibujo` (`--strict` sale con 1).

## Contrato de una figura (detalle en `figures/_template.R`)
`fig_NN <- list(spec = list(tipo, metodo, w_in, h_in, …), data = function(ctx) ..., plot = function(df, spec) ...)`
- `data(ctx)` → tibble `order, kind, grouping, label, value, value2, text, text2` (+ `x, y, n, lo, hi, series`).
  `value` crudo (NA si n < 30); `text`/`text2` = lo impreso, exacto, vía `fmt_num()`. `plot()` no formatea ni recalcula.
- `plot(df, spec)` → ggplot de `wjpr_<tipo>()` (respaldo: `chart_out` de `chart_<tipo>()`); no lee cfg ni ctx (país de las
  leyendas: `spec$country`). Encabezado: título, fuente de datos, método UE, código de origen (archivo:líneas). ~80 líneas.
- Nada de `"Hungary"` literal (`ctx$country`, `{country}` en el título); una elección editorial del país (p. ej. las
  destacadas de la 3) va en `figures:` de `config/<pais>.yml` y `data()` la pone en el df. Media UE en código
  (`eu_value(ctx, "data4web", id)` o `eu_est(ctx, var)`) y en `eu_id` de la spec (lo lee `tools/build_reference.R`).
- `get_results_table(ctx, target, ctype, …, omit)`: `short` = National + Gender, Age Group, Financial Situation, Area of
  Residence; `full` = short + Category + Co-occurrent Problems; `trust` = short + Problem Status + Non-Trivial Problem
  Experienced; `custom_1` = short + Problem Status + Resolution; `omit = "Category"` quita grupos por nombre.

## Cómo añadir o arreglar una figura
1. Leer solo `figures/_template.R`, el bloque del tipo en `R/05_charts.R` y en `R/09_wjpr.R` (`grep -n "^## Bloque"`),
   una figura del mismo tipo (las 42 verifican) y `reference/<slug>/Figure_NN_slug.csv` (textos y orden exactos).
2. Escribir `figures/fig_NN_slug.R` con su `spec` (copia de una del mismo tipo; NN = número en el PDF). Figura nueva:
   slug nuevo en minúsculas_con_guiones (nombra tabla, SVG y referencia), código de origen «nuevo» + las funciones que
   usa, y la regla de muestra pequeña de su sección (`small_sample(ctx)`).
3. Iterar con `… main.R --stage figures --figs NN` hasta `ok` (da el motivo de `error`, `tamaño`, `texto` o `png`).
4. `h_in` (3 decimales; el SVG sale a ese alto). Agrupadas: (6 + 15.5·filas + 22·encabezados) / 72 in, filas = `avg` +
   `row` con las N/A (short: 12 + 4 → 4.104 sin UE, 4.319 con UE): si no cabe, `tamaño` con el h_in; si sobra, aviso.
   Horizontales: (24 + 22·filas) / 72. Por país (5, 11, agrupadas; el log lo da): `figures: figNN: h_in:` del yml.
5. `… --stage all --figs NN` → `ok` en verify (sin publicada: «sin referencia»/«sin publicada», y el criterio es `ok`
   en `log/figures_status.csv`); mirar `output/<slug>/compare/`. 6. Un valor que difiere del publicado no se fuerza: va
   a la `nota` de la spec (`[conocida: <label>=<text>]` → conocida).

## Reglas duras
- No modificar, mover ni borrar nada fuera de `src/src_reports/`. Microdatos solo en el caché (`data/`, `cache_dir` o
  `SRC_REPORTS_CACHE`; en .gitignore con `output/` y `config/local.yml`): nunca en commits, en consola ni en el log.
- Paquetes fuera de base/tidyverse siempre con `::`; solo los de `renv.lock` (uno nuevo: proponerlo, `renv::install` y
  `renv::snapshot()` tras verificar; los indirectos en `_dependencies.R`).
- Código portado tal cual con `# origen: archivo:líneas` (correcciones: `# corregido:`). Determinismo: locale C
  (`arrange(.locale = "C")`), showtext apagado, `set.seed()` explícito.
- Una figura que falla no detiene a las demás (estado `error`); `verify` sale con 0 salvo `--strict`. Git: un commit por
  etapa con `git add src/src_reports`; sin push salvo que se pida.
