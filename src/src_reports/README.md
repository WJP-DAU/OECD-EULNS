# src_reports — figuras del reporte *Paths to Justice* desde los microdatos

Carpeta autocontenida: con `Rscript src/src_reports/main.R` se producen las 42 figuras del reporte (hoy Hungría) a partir
de `EU_GPP_2024.dta`, con el mismo diseño, tamaño y tipografía que las publicadas, y se verifican contra los valores
publicados. Reutilizable para otro país cambiando `config/<pais>.yml`; copiable tal cual a
`automatic-reports/reports/pathways-to-justice-hungary/pipeline` (no hace `source()` de nada fuera de la carpeta).

> Estado: **Etapa 1 (esqueleto)**. Las 42 figuras están en el manifiesto y tienen archivo propio, pero ninguna está
> implementada todavía. Etapa 2 = piloto 09/10/11 + prueba WJPr; Etapa 3 = las 42 por lotes; Etapa 4 = cierre.

## Requisitos
- R 4.6.x con los paquetes de `deps.csv` (librería del sistema; no hace falta renv). `check_deps()` detiene si falta uno
  en modo `minima` y avisa si una versión `exacta` difiere (igraph, ComplexUpset, ggplot2, svglite, systemfonts,
  textshaping, ggrepel: cambian el dibujo aunque no los valores).
- Chrome o Chromium para los PNG (se busca en `CHROMOTE_CHROME`, `config/local.yml`, rutas habituales y `PATH`; sin él
  los PNG se omiten con aviso, `--no-png` los omite a propósito).
- Las fuentes Inter Tight (OFL) ya están en `fonts/`.
- Acceso al `.dta` (SharePoint *EU Subnational*) solo para `--stage data`; después todo corre desde `cache/`.

## Puesta en marcha
```bash
cp src/src_reports/config/local.yml.example src/src_reports/config/local.yml   # y editar path2EU
Rscript src/src_reports/main.R --stage data            # lee el .dta (3-5 min la primera vez) y construye cache/
Rscript src/src_reports/main.R --stage all --figs 09   # una figura: tabla, SVG, PNG, verificación y comparación
Rscript src/src_reports/main.R --stage all --strict    # todo; exit 1 si alguna figura no verifica
```
Si el repo donde se copie activa renv por `.Rprofile`, use `Rscript --no-init-file …/main.R` o defina `SRC_REPORTS_LIB`
con la librería que tiene los paquetes. La cabecera de la consola imprime `.libPaths()[1]` y si renv está activo.

Variables de entorno opcionales: `SRC_REPORTS_LIB`, `SRC_REPORTS_CACHE` (caché fuera de OneDrive), `SRC_REPORTS_ROOT`,
`CHROMOTE_CHROME`. Precedencia: entorno > `config/local.yml` > defectos.

## Qué hace cada etapa
| Etapa | Entrada | Salida |
|---|---|---|
| `data` | `.dta`, `data/regions.csv`, `reference/<slug>/eu_values.csv` | `cache/master.rds` (27 países, un encuestado por fila), `cache/ctx_<slug>.rds`; comprobaciones de `config/<pais>.yml` → log |
| `figures` | `cache/`, `figures/fig_NN_slug.R`, `manifest/figures.csv` | `output/<slug>/tables/Figure_NN_slug.csv` (+ `.drawn.csv`), `figures/Figure_NN_slug.svg|png`, `log/figures_status.csv` |
| `verify` | tablas + `reference/<slug>/` | `tables/00-verificacion.csv`; estados ok / fallo / sin salida / sin referencia / desactualizada |
| `compare` | PNG nuevos + `reference/<slug>/published/` | `compare/Figure_NN_slug.png`, `compare/index.html` |

La consola muestra solo la tabla resumen; el detalle va a `output/<slug>/log/run_<id>.log`.

## Método (igual al reporte publicado)
- Estimaciones nacionales = promedio de las regiones NUTS ponderado por `regionpoppct` (`data/regions.csv`); celdas con
  n < 30 suprimidas (`N/A`). Quality check: encuestados con más de 25 problemas no triviales excluidos de las experiencias.
- Media UE: según la figura, `data4web` (valores cacheados en `reference/<slug>/eu_values.csv`) o `eu_est()` (media simple
  de las estimaciones nacionales de los 27 países). La fuente se declara en el código de cada figura y en el manifiesto.
- Diseño: SVG propio con texto en trazos (`R/04_svg.R`, `R/05_charts.R`, portados de `src/hungary/finales/`) para 38
  figuras; ggplot2 + contorneado para 3, 6, 7 y 29. Tamaños exactos del `00-INDICE` publicado.

## Arquitectura y contratos
`main.R` hace `source()` de `R/00..08` en orden en el entorno global; `figures/fig_NN_slug.R` se cargan con `sys.source()`
y deben terminar en `fig_NN <- list(data, plot)`. Los archivos de `R/` solo definen funciones (nada corre al cargar).

**`ctx`** (lo construye `R/01_load.R`): `cfg` (sin rutas), `country`, `slug`, `master` (27 países, un encuestado por fila:
`data/master.csv` + `non_trivial_problem_broader` + `jg_inverse_*_keepdk` + columnas de `add_report_vars()`), `national`
(filas del país), `regions`, `jg` (tabla keepDK del país), `eu_values` (id, value), `meta` (clave de caché, versiones).
Estimadores (`R/03_estimators.R`, siempre `ctx` primero): `get_results_table(ctx, target, ctype, prop, transform, omit, df)`
→ tibble(`grouping` factor con niveles = clabels; primera fila `National`/`National Average`, `category`, `value`);
`nat_est(ctx, target, transform, by, df)` → tibble(country_name_ltn, by…, value, n); `eu_est(ctx, target, transform)` →
escalar; `journey(ctx)` → lista (`adv_contacted`, `rep_c_published`, `drm_c_published`, `no_need_r1`, `unknown_r1`,
`drm_no_need_r1`, `drm_unknown_r1`, …); `perception_experience(ctx, jse, exp)`; `xt()`, `diff_stratified()`;
`eu_value(ctx, "data4web" | "eu_est", id)`; `memo(ctx, key, expr)`.

**`df = data(ctx)`**: `order, kind, grouping, label, value, value2, text, text2` (+ `x, y, n, lo, hi, series`). Convención por tipo:

| tipo | kind | grouping | label | value / value2 | text / text2 |
|---|---|---|---|---|---|
| grouped_bars | avg / header / row | "" en avg; grupo | National Average / EU Average; grupo; categoría | % (media 0-10 en la 4) | "61%", "N/A", "6.0" |
| paired_bars (2), columns (30, 37) | row | "" | categoría | % país / % UE | "61%" / "49%" |
| dumbbell (10, 15) | avg / header / row | "" / grupo | categoría | percepción / experiencia | "27%" o "" |
| hbars (13, 14, 18, 19, 36) | row | "" | categoría | % | "54%" |
| funnel (8, 34) | stage | "" | etapa | % | "61%" |
| sankey (11, 16) | node | "1"/"2"/"3" | clave del nodo | % | "29%" |
| cards (32, 41, 42) | card | fila impresa | columna impresa | % / % de referencia | "41%" / "+16 points" (U+2212 en negativos) |
| matrix (6) | cell | categoría fila | categoría columna | % o NA | "62%", "N/A", "–" / banda |
| upset (29) | row / set | "" | "A + B" / conjunto | % | "12.3%" |
| network (5) | node / edge | "" | problema / "A\|B" | prevalencia / lift (+ x, y, n) | "(40%)" |
| scatter (3) | point | Rural / Urban | categoría | prevalencia (x) / severidad (y) | "" |
| severity_bars (7) | bar | serie | "0".."10" | % | "" |

Orden en barras agrupadas: National Average, EU Average, cada grupo en el orden de clabels y dentro del grupo alfabético
inverso (`arrange(desc(label), .locale = "C")`). `rows_<tipo>()` construye el df; `chart_<tipo>(df, spec)` devuelve un
`chart_out` (`svg`, `width_in`, `height_in`, `text_pt`, `drawn` = order, kind, grouping, label, frac, frac2, text, text2).

**Salidas y verificación**: `tables/Figure_NN_slug.csv` (= df), `.drawn.csv`, `log/figures_status.csv` (md5, tamaños,
estado por corrida); `reference/<slug>/Figure_NN_slug.csv` (order, kind, grouping, label, value, value2, text, text2,
frac, frac2, tol) es la verdad; `verify` une por (kind, grouping, label): texto exacto, `|value − ref| ≤ tol` (0.6 pp;
0.06 escala 0-10; 0.05 lift; 0.01 coordenadas), dibujo `|frac − ref| ≤ 0.006`, mismo orden, ancho exacto, alto ± 0.01 in,
PNG `round(in·300)` px (±1 ancho, ±3 alto), 0 `<text>`. Estados: figures ok | error | png | tamaño | texto | omitida;
verify ok | fallo | error | sin salida | sin referencia | desactualizada | estatica | omitida; compare ok | sin salida |
sin publicada | error | omitida. `[conocida: <label>=<text>]` en la nota del manifiesto marca una discrepancia aceptada.

**Decisiones del porte** (Etapa 1): `add_a2j_vars()` devuelve las tablas de brecha como `attr(, "jg")` en vez de escribir
CSV; `add_report_vars()` = `build_analysis_frame()` + recodificaciones de A03/A04/A05 con sus nombres originales
(`has_nt`, `resuelto`, `favor`, `exposicion`, `escalera_hardship`, `rep_c_published`, …); `get_results_table()` etiqueta la
fila nacional `National Average` (el original: `National`); `first_line()` vive en `00_setup.R`; el log se escribe por
mensaje (OneDrive); `main.R` decodifica los `~+~` de `--file=`; tarjetas con escala de color por figura (publicado 2/62,
29/55, 30/55); `data/master.csv` equivale a 200 columnas (53 + 147 AJ*).

## Discrepancias conocidas con lo publicado (no se fuerzan)
- Fig 16: "excluded" = 56 % (partes a 1 decimal, método publicado); la aritmética sin redondear da 55 %.
- Fig 20: v9 imprimía "5 or more problems"; las figuras finales (16-oct) "6 or more problems" (la etiqueta correcta para
  nproblems ≥ 6; `# corregido:` en `R/02_variables.R`).
- Fig 22: media UE 54 % (data4web); `eu_est()` daría 53 %.
- Fig 12: barra UE = 37.68 → 38 (R03 `rep_c_published`), no la de data4web (54.2).

## Añadir un país
1. `config/<pais>.yml` (country, slug, rutas relativas, comprobaciones). 2. `tools/build_reference.R` para
`reference/<slug>/` (si existe reporte publicado) o dejar `verify` en "sin referencia". 3. Las figuras agrupadas cambian de
alto con el número de filas: revisar `h_in` en el manifiesto. Las etiquetas, el orden y los colores no están en el yml a
propósito (son del diseño, no del país).

## Caché
`cache/master.key` = tamaño y fecha del `.dta` + md5 de `regions.csv`, `01_load.R`, `02_variables.R`, `deps.csv` +
versiones de haven/dplyr/tidyr/readr + versión de R. `cache/ctx_<slug>.key` añade `eu_values.csv` y el país. `--force`
reconstruye. Sin acceso al `.dta`, `figures/verify/compare` usan el caché existente (aviso "sin comprobar").

## WJPr
Pendiente del piloto de la Etapa 2 (`tools/pilot_wjpr.R`): columna `wjpr` del manifiesto (adoptada | descartada |
sin_equivalente) y tabla de criterios aquí.

## Tiempos (medidos el 2026-10-22, MacBook, .dta ya descargado)
| Etapa | Tiempo |
|---|---|
| `--stage data` en frío (`--force`) | 33 s (lectura del `.dta` 3 s; variables y master de 27 países 28 s) · la primera vez OneDrive tarda además ~50 s en descargar los 295 MB |
| `--stage data` con caché | 3 s |
| `--stage verify` / `--stage compare` | 1-3 s |
| `--stage figures` | por medir con figuras implementadas (Etapa 2) |

## OneDrive
La carpeta vive en OneDrive: una conexión de archivo abierta durante toda la corrida pierde escrituras y falla al
cerrar ("Operation timed out"), por eso el log se escribe mensaje a mensaje y la salida capturada pasa por un
temporal local. Si un `.ttf` o el `.dta` pesan 0 bytes es que no están descargados. `cache_dir` en `config/local.yml`
permite sacar el caché de OneDrive.
