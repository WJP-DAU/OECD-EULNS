# src_reports — figuras del reporte *Paths to Justice* desde los microdatos

Carpeta autocontenida: con `Rscript src/src_reports/main.R` se producen las 42 figuras del reporte (hoy Hungría) a partir
de `EU_GPP_2024.dta`, con el mismo diseño, tamaño y tipografía que las publicadas, y se verifican contra los valores
publicados. Pensada para reutilizarse con otro país (`config/<pais>.yml` + lo que lista «Añadir un país»); copiable tal
cual a `automatic-reports/reports/pathways-to-justice-hungary/pipeline` (no hace `source()` de nada fuera de la carpeta).

> Estado: **Etapa 2 (piloto)**, 2026-10-23: figuras 09, 10 y 11 implementadas y verificadas (3/3, idénticas a las
> publicadas); WJPr adoptado como motor de dibujo de las 42 (`metodo wjpr`, con complementos; ver «WJPr»); revisión
> adversarial del esqueleto aplicada (89 hallazgos de 7 revisores). Etapa 3 = las 39 restantes por lotes (1-7, 8-19,
> 20-27, 28-37, 38-42); Etapa 4 = cierre.

## Requisitos
- R 4.6.x con los paquetes de `deps.csv` (librería del sistema; no hace falta renv). `check_deps()` detiene si falta uno
  en modo `minima` (ggtext entre ellos), avisa si una versión `exacta` difiere (WJPr, igraph, ComplexUpset, ggplot2,
  svglite, systemfonts, textshaping, ggrepel: cambian el dibujo aunque no los valores; en WJPr, además, el commit
  `@382aee9` de `origen` frente al `RemoteSha` instalado, "local" si se instaló desde un clon) y solo registra los
  `opcional` (showtext). Las figuras con `metodo wjpr` no se dibujan sin WJPr ni ggtext: la etapa figures se detiene
  con un error que dice cómo instalarlos (`check_method_deps()`).
- Chrome o Chromium para los PNG (se busca en `CHROMOTE_CHROME`, `config/local.yml`, rutas habituales y `PATH`; sin él
  los PNG se omiten con aviso, `--no-png` los omite a propósito).
- Las fuentes Inter Tight (OFL) ya están en `fonts/`.
- Acceso a `path2EU` (SharePoint *EU Subnational*): el `.dta` para `--stage data` sin caché y `data4web_gpp.csv` para
  `tools/build_reference.R`; después todo corre desde `cache/`.

## Puesta en marcha
```bash
cp src/src_reports/config/local.yml.example src/src_reports/config/local.yml   # y editar path2EU
Rscript src/src_reports/main.R --stage data            # lee el .dta (~35 s; 1-2 min si OneDrive aún no lo descargó)
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
| `figures` | `cache/`, `figures/fig_NN_slug.R`, manifiesto | `output/<slug>/tables/Figure_NN_slug.csv` (+ `.drawn.csv`), `figures/Figure_NN_slug.svg|png`, `log/figures_status.csv` |
| `verify` | tablas + `reference/<slug>/` (+ `published/` para el parecido de `metodo wjpr`) | `tables/00-verificacion.csv`; estados ok / fallo / error / sin salida / sin referencia / desactualizada |
| `compare` | PNG nuevos + `reference/<slug>/published/` | `compare/Figure_NN_slug.png`, `compare/index.html` |

La consola muestra solo la tabla resumen; el detalle va a `output/<slug>/log/run_<id>.log`.

## Consola, errores y códigos de salida
- La consola muestra la cabecera (país, etapa, id de corrida, librería, renv, Chrome, locale y avisos), la tabla
  `fig | metodo | estado | verificacion | archivo` y una línea de cierre con la ruta del log; el detalle va a
  `output/<slug>/log/run_<id>.log` (el id lleva milisegundos y PID: dos corridas a la vez no comparten log).
- Un error que detiene una etapa se ve como `Error en la etapa <x>: <primera línea>` + `Detalle: <log>`, y la salida
  es 1. Una figura que falla no detiene a las demás: queda en `error` con su motivo (en `--stage all`, verify la
  muestra como `error · figures: <motivo>` y la línea de cierre la cuenta).
- Salida 0 salvo error de etapa o argumentos inválidos; con `--strict`, 1 si alguna figura seleccionada no es `ok` en
  figures o verify (o da error en compare).
- Argumentos estrictos: una opción repetida, un valor ausente o un `--figs` que no sea de la forma `09,12` dan error; una
  figura que no está en el manifiesto se avisa en la cabecera.
- Desde RStudio: `Sys.setenv(SRC_REPORTS_ARGS = "--stage figures --figs 09"); source("src/src_reports/main.R")`. Con
  `source()` la sesión no se cierra y `source(...)$value` es el código de salida; sin argumentos solo muestra el uso.
- Raíz de la carpeta: la del archivo que recibe `source()` o `--file` (resolviendo enlaces simbólicos); si ahí no está
  `R/00_setup.R`, `SRC_REPORTS_ROOT`; si tampoco, el directorio actual.
- Rutas de `config/local.yml` y `SRC_REPORTS_CACHE`: se expande `~` y las relativas se resuelven contra esta carpeta,
  no contra el directorio desde el que se lanza `Rscript`.
- Locale: si `LC_CTYPE` no es UTF-8 (cron, launchd, Docker sin `LANG`) se cambia a `C.UTF-8` o `en_US.UTF-8` con una
  nota en la cabecera; si no hay ninguno, la corrida se detiene con un mensaje claro.

## Método (igual al reporte publicado)
- Estimaciones nacionales = promedio de las regiones NUTS ponderado por `regionpoppct` (`data/regions.csv`); celdas con
  n < 30 suprimidas (`N/A`). Quality check: encuestados con más de 25 problemas no triviales excluidos de las experiencias.
- Media UE: según la figura, `data4web` (ids en la columna `eu_id` del manifiesto; valores cacheados en
  `reference/<slug>/eu_values.csv`) o `eu_est()` (media simple de las estimaciones nacionales de los 27 países). La
  fuente se declara en el código de cada figura y en el manifiesto (`metodo_ue`, `eu_id`).
- Diseño: WJPr, el paquete de diseño de WJP (`R/09_wjpr.R`: la función de WJPr del tipo, o `WJP_theme()`/`wjp_palette()`
  donde no tiene una, + complementos), exportado con svglite y texto en trazos para las 42. El SVG propio (`R/04_svg.R`,
  `R/05_charts.R`, portado de `src/hungary/finales/`) queda de respaldo (`metodo svg_r`). Tamaños exactos del `00-INDICE`.

## Arquitectura y contratos
`main.R` hace `source()` de `R/00..09` en orden en el entorno global; `figures/fig_NN_slug.R` se cargan con `sys.source()`
y deben terminar en `fig_NN <- list(data, plot)`. Los archivos de `R/` solo definen funciones (nada corre al cargar).

**`ctx`** (lo construye `R/01_load.R`): `cfg` (sin rutas), `country`, `slug`, `master` (27 países, un encuestado por fila:
`data/master.csv` + `non_trivial_problem_broader` + `jg_inverse_*_keepdk` + columnas de `add_report_vars()`), `national`
(filas del país), `regions`, `jg` (tabla keepDK del país), `eu_values` (id, value), `meta` (clave de caché, versiones).
Estimadores (`R/03_estimators.R`, siempre `ctx` primero): `get_results_table(ctx, target, ctype, prop, transform, omit, df)`
→ tibble(`grouping` factor con niveles = clabels; primera fila `National`/`National Average`, `category`, `value`);
`nat_est(ctx, target, transform, by, df)` → tibble(country_name_ltn, by…, value, n); `eu_est(ctx, target, transform)` →
escalar; `journey(ctx)` → lista (`adv_contacted`, `rep_c_published`, `drm_c_published`, `no_need_r1`, `unknown_r1`,
`drm_no_need_r1`, `drm_unknown_r1`, …); `perception_experience(ctx, jse, exp)`; `xt()`, `diff_stratified()`;
`eu_value(ctx, "data4web" | "eu_est", id)`; `memo(ctx, key, expr)`. **`spec`** = una fila del manifiesto (lista) +
`selected` + `country` (= `cfg$country`: `plot(df, spec)` no recibe ctx y las leyendas {country} | EU Average lo leen de ahí).

**`df = data(ctx)`**: `order, kind, grouping, label, value, value2, text, text2` (+ `x, y, n, lo, hi, series`). Convención por tipo:

| tipo | kind | grouping | label | value / value2 | text / text2 |
|---|---|---|---|---|---|
| grouped_bars | avg / header / row | "" en avg; grupo | National Average / EU Average; grupo; categoría | % (media 0-10 en la 4) | "61%", "N/A", "6.0" |
| paired_bars (2), columns (30, 37) | row | "" | categoría | % país / % UE | "61%" / "49%" |
| dumbbell (10, 15) | avg / header / row | "" / grupo | categoría | percepción / experiencia | "27%" o "" |
| hbars (13, 14, 18, 19, 36) | row | "" | categoría | % | "54%" |
| funnel (8, 34) | stage | "" | etapa (con `\n` donde corta la línea) | % | "61%" |
| sankey (11, 16) | node | "1"/"2"/"3" | clave del nodo | % | "29%" |
| cards (32, 41, 42) | card | fila impresa | columna impresa | % / % de la tarjeta de referencia (NA en ella) | "41%" / "+16 points" ("−" U+2212, "±"; "" en la de referencia) + `lo`, `hi` por figura |
| matrix (6) | cell | categoría fila | categoría columna | % o NA | "62%", "N/A", "–" / banda |
| upset (29) | row / set | "" | "A + B" (conjuntos en el orden fijo) / conjunto | % | "12.3%" (+ `n`) |
| network (5) | node / edge | "" | problema / "A\|B" | prevalencia / lift (+ `x`, `y` del layout, sin renormalizar) | "(40%)" |
| scatter (3) | point | Rural / Urban | categoría | prevalencia (x) / severidad (y) | "" (+ `n`) |
| severity_bars (7) | bar | serie (país primero) | "0".."10" | % | "" |

Orden en barras agrupadas: National Average, EU Average, cada grupo en el orden de clabels y dentro del grupo alfabético
inverso (`arrange(desc(label), .locale = "C")`); la 27 es la excepción (sin avg; ver su encabezado). Upset: intersecciones
de izquierda a derecha como las dibuja ComplexUpset. `rows_<tipo>()` construye el df y `wjpr_<tipo>(df, spec)` (`R/09_wjpr.R`)
lo dibuja: un ggplot con `attr(, "wjpr")` = `list(base, complementos, compara)` (función de WJPr de partida, complementos
aplicados y columnas cuyos textos se imprimen tal cual, por defecto `text` y `text2`). El respaldo, `chart_<tipo>(df,
spec)`, devuelve un `chart_out` (`svg`, `width_in`, `height_in`, `text_pt`, `drawn` = order, kind, grouping, label, frac,
frac2, text, text2; `frac` = fracción dibujada: valor/escala de la pista, barra/la más ancha en los embudos, valor/máximo
de las dos series en las columnas 30 y 37). La verdad exacta de cada figura está en `reference/<slug>/Figure_NN_slug.csv`.

**Salidas y verificación**: `tables/Figure_NN_slug.csv` (= df), `.drawn.csv`, `log/figures_status.csv` (md5, tamaños,
estado por corrida); `reference/<slug>/Figure_NN_slug.csv` (order, kind, grouping, label, value, value2, text, text2,
frac, frac2, tol + `tol2`, `tol_xy`, `x`, `y`, `n`, `lo`, `hi` si la figura los usa) es la verdad; `verify` une por (kind,
grouping, label): texto exacto, `|value − ref| ≤ tol` (0.6 pp; 0.06 escala 0-10; 0.05 lift), `value2` con `tol2` (si
falta, `tol`; 0.06 en la severidad de la 3), `x`, `y` con `tol_xy` (si falta, 0.01), `lo`, `hi`, `n` exactos, dibujo
`|frac − ref| ≤ 0.006`, mismo orden, ancho exacto, alto ± 0.01 in, PNG `round(in·300)` px (±1 ancho, ±3 alto), 0
`<text>`. Con `metodo wjpr` no hay drawn.csv que comparar (WJPr no expone las barras dibujadas): en su lugar `parecido` =
f1 de tinta del PNG nuevo frente a `reference/<slug>/published/Figure_NN_slug.png` a 96 dpi ≥ `verify$min_f1` (0.995;
`wjpr_similarity()`; sin uno de los dos PNG, `suprimido`) y `textos` = los textos dibujados frente a `df$text/text2`, que
figures mide sobre el ggplot (`wjpr_texts()`; solo cuentan como sobrantes los valores que df no trae) y guarda en
`figures_status.csv` (columnas `wjpr_build`, `wjpr_complementos`, `wjpr_textos`); una salida dibujada con otro metodo
queda `desactualizada`. Estados: figures ok | error | png | tamaño | texto | omitida; verify ok | fallo | error | sin salida | sin
referencia | desactualizada | estatica | omitida; compare ok | sin salida | sin publicada | error | omitida.
`[conocida: <label>=<text>]` en la nota del manifiesto marca una discrepancia aceptada.

**Decisiones del porte** (Etapa 1): `add_a2j_vars()` devuelve las tablas de brecha como `attr(, "jg")` en vez de escribir
CSV; `add_report_vars()` = `build_analysis_frame()` + recodificaciones de A03/A04/A05 con sus nombres originales
(`has_nt`, `resuelto`, `favor`, `exposicion`, `escalera_hardship`, `rep_c_published`, …); `get_results_table()` etiqueta la
fila nacional `National Average` (el original: `National`); `first_line()` vive en `00_setup.R`; el log se escribe por
mensaje (OneDrive); `main.R` decodifica los `~+~` de `--file=`; tarjetas con escala de color por figura (publicado 2/62,
29/55, 30/55); `data/master.csv` equivale a 200 columnas (53 + 147 AJ*). `AJP_problem` conserva el `""` del `.dta`
donde `data/master.csv` tiene NA (el viaje write_csv/read_csv del original convertía `""` en NA); ninguna figura lo usa.

## Manifiesto
`manifest/figures.csv` (una fila por figura) es común a todos los países. `manifest/figures_<slug>.csv` (opcional) lo
ajusta para un país: columna `fig` y las columnas a cambiar (p. ej. `h_in`, `nota`); una celda vacía conserva el valor
común y una columna o figura que no existe en el manifiesto es un error. `read_manifest()` lo aplica al leer.

| columna | valores | la lee |
|---|---|---|
| `fig`, `slug` | número; nombre de archivo (`fig_NN_slug.R`, `Figure_NN_slug.*`) | todo el código |
| `titulo`, `seccion`, `pagina` | título con `{country}`; sección y página del PDF | 08 (`index.html`) |
| `tipo` | grouped_bars, paired_bars, dumbbell, hbars, columns, funnel, sankey, cards, matrix, upset, network, scatter, severity_bars | 08 y `tools/build_reference.R` |
| `metodo` | wjpr (las 42) · svg_r (respaldo) · ggplot · static | 06 (cómo guardar: wjpr y ggplot exportan el ggplot, wjpr con k = 1; textos dibujados) y 07 (wjpr → parecido y textos; static → estatica) |
| `funcion` | `wjpr_<tipo>` (el adaptador de `R/09_wjpr.R` que llama `plot()`); de respaldo `chart_<tipo>` o `fig_NN$plot` | solo documentación |
| `w_in`, `h_in` | pulgadas, 3 decimales | 05 (ancho del lienzo), 06 y 07 (tamaño), 08 |
| `callout` | true · false (junto a un recuadro) | `read_manifest()`: avisa si `w_in` supera `max_w_callout_in` / `max_w_full_in` |
| `label_col` | std · dumb · hbars · own · none | 05 (columna de etiquetas) |
| `base_pt` | tamaño base con que se dibujó la figura publicada cuando era ggplot: 12, 11, 13 y 10.24 en 3, 6, 7 y 29; vacío en el resto | `wjpr_scatter`, `wjpr_matrix` y `wjpr_upset` dividen sus tamaños por base_pt/10 (sin él el texto sale al tamaño por defecto); 06 solo en `metodo ggplot` (lienzo × base_pt / text_pt) |
| `fuente_datos`, `codigo_origen` | texto | nadie: se copian al encabezado de la figura |
| `metodo_ue` | none · data4web · eu_est · eu_est_category · eu_est_distribution | `tools/build_reference.R` (con `eu_id`) |
| `eu_id` | id de la media UE (varios con `;`) o "" | `tools/build_reference.R`: ids de data4web → `eu_values.csv` |
| `wjpr` | adoptada (33: parten de `wjp_groupbars`, `wjp_bars` o `wjp_dumbbells`) · tema (9: solo `WJP_theme()`/`wjp_palette()`, WJPr no tiene esa gráfica) | documentación (`main.R` la lee para una nota de cabecera) |
| `status`, `verificada` | plan de la figura (hoy `todo`) y fecha de su verificación (vacía) | nadie: los actualiza el cierre de cada etapa |
| `nota` | texto libre + `[conocida: <label>=<text>]` | 07 (discrepancias conocidas) |

## Discrepancias conocidas con lo publicado (no se fuerzan)
- Fig 16: "excluded" = 56 % (partes a 1 decimal, método publicado); la aritmética sin redondear da 55 %.
- Fig 20: v9 imprimía "5 or more problems"; las figuras finales (16-oct) "6 or more problems" (la etiqueta correcta para
  nproblems ≥ 6; `# corregido:` en `R/02_variables.R`).
- Fig 22: media UE 54 % (data4web); `eu_est()` daría 53 %.
- Fig 12: barra UE = 37.68 → 38 (R03 `rep_c_published`), no la de data4web (54.2).

## Añadir un país
Hoy solo Hungría tiene reporte publicado y referencia; para otro país hace falta, además del yml:
1. `config/<pais>.yml`: `country` (= `country_name_ltn` del `.dta`), `slug`, rutas; los valores de `checks` son de
   Hungría: cámbielos o bórrelos (solo se registran en el log). `data.eu_values` puede apuntar a un `eu_values.csv` ya
   generado: las medias UE de data4web no dependen del país.
2. Referencia: solo si el país tiene reporte publicado. `published_dir` y `reference_src_dir` están en `config/local.yml`
   (son de la máquina, no del país): apúntelos a las figuras y los datos publicados de ese país y corra
   `tools/build_reference.R --country <slug>`. La herramienta se detiene sin escribir en `reference/<slug>/` si las
   fuentes no son del país (carpeta `output/viz/<country>`, `<country>_jg_upset.svg`, columna del país en F03.csv, títulos de
   `00-INDICE.csv`). Sin referencia, verify da "sin referencia" y compare "sin publicada".
3. Manifiesto del país (`manifest/figures_<slug>.csv`): el `h_in` de las figuras cuyo número de filas cambia (ver
   CLAUDE.md, «Fijar h_in») y las notas. Las notas `[conocida: …]` y los números de `fuente_datos` del común son de
   Hungría; los slugs `*_hu_eu` (2, 7, 30) son nombres de archivo y se conservan.
4. Lo que no es configurable: grupos omitidos por muestra pequeña (`omit` de `get_results_table`, como `omit.cats`
   para Malta en `src/viz`) y relabels están en el `data()` de cada figura; etiquetas, orden y colores son del diseño.

## Caché
`cache/master.key` = tamaño y fecha del `.dta` + md5 de `regions.csv`, `01_load.R`, `02_variables.R`, `deps.csv` +
versiones de haven/dplyr/tidyr/readr + versión de R. `cache/ctx_<slug>.key` añade `eu_values.csv` y el país. `--force`
reconstruye. Sin acceso al `.dta`: si la clave difiere solo en el `.dta`, se usa el caché con aviso "sin comprobar"; si
difiere en el código, `regions.csv`, `deps.csv`, los paquetes o R, se detiene y pide `--stage data` con acceso al `.dta`.

## WJPr
**Motor de dibujo (adoptado el 2026-10-23).** Las 42 figuras se dibujan con WJPr, el paquete de diseño de WJP: `metodo
wjpr` y `funcion wjpr_<tipo>` en el manifiesto, `plot = function(df, spec) wjpr_<tipo>(df, spec)` en cada figura.
`R/09_wjpr.R` tiene un adaptador por tipo (13) que parte de la función de WJPr del tipo (`wjp_groupbars`, `wjp_bars`,
`wjp_dumbbells`: columna `wjpr` = `adoptada`, 33 figuras) o de `WJP_theme()`/`wjp_palette()` donde WJPr no tiene esa
gráfica (`tema`, 9: tarjetas, matriz, dispersión, UpSet, red y sankey) y la complementa hasta el diseño publicado; cada
complemento lleva `# complemento: <qué le falta a WJPr>` (la fuente del documento de brechas del paquete). Los
`chart_<tipo>()` de `R/05_charts.R` quedan de respaldo: `metodo svg_r` en la fila del manifiesto y `plot =
function(df, spec) chart_<tipo>(df, spec)` vuelven a la figura propia, que verify compara con su `drawn.csv`.
- **Exportación** (`R/06_export.R`): svglite a `w_in × h_in` con k = 1 (WJPr ya da los tamaños en pt; sin `base_pt`),
  texto en trazos y PNG con Chrome, como `wjpr_render()` de la galería. Los textos dibujados (`wjpr_texts()`) deben ser
  los de `df` en las columnas `attr(p, "wjpr")$compara`, con la regla de la galería (solo sobran los valores que df no
  trae): si no, estado `texto`. `log/figures_status.csv` guarda `wjpr_build` (versión@commit), `wjpr_complementos` y
  `wjpr_textos`.
- **Verificación** (`R/07_verify.R`): `parecido` (f1 de tinta frente a la publicada a 96 dpi ≥ `verify$min_f1` = 0.995)
  y `textos` en lugar de `drawn.csv` (detalle en «Arquitectura y contratos»).
- **Build fijada**: `deps.csv` declara WJPr 1.1.0 en modo `exacta` con el commit `@382aee9` en `origen` (la build del
  13-jul con la que la galería da 42/42) y ggtext en `minima`. Dos builds con el mismo número de versión dibujan
  distinto, así que `check_deps()` compara también el commit con el `RemoteSha` instalado ("local" si se instaló desde
  un clon con `R CMD INSTALL`).
- **Otra build** (p. ej. la rama main del paquete): instalarla en una librería aparte y correr
  `SRC_REPORTS_LIB=<librería> Rscript src/src_reports/main.R --stage all --figs 09,10,11` (o `tools/wjpr_gallery.R`
  con `--out` para no mezclar salidas). La cabecera dice `aviso deps.csv: WJPr 1.1.0@local (esperada 1.1.0@382aee9:
  otra build con la misma versión)`, verify añade la fila `entorno` y, si la build cambia el dibujo, la figura queda
  `fallo · parecido: ≥ 0.995→<f1>` aunque la tabla y los textos estén bien. Así se vio el 23-oct con la rama main
  (a46c831) y los complementos escritos para 1.1.0: 09 → 0.907 (`wjp_groupbars` ordena las categorías de arriba abajo,
  f4f3b8f, y el complemento las dejaba invertidas) y 10 → 0.419 (`wjp_dumbbells` activa su leyenda por defecto). Tras
  adaptar los complementos (detectan el sentido del orden y usan `show_legend`, `label_offset` y la fuente nativa), las
  42 dan lo mismo con las dos builds. Una build nueva se adopta cuando verify vuelve a dar `ok`
  (y la galería 42/42): los adaptadores usan `wjpr_has_fun()`/`wjpr_has_arg()` para lo que la build ya trae (p. ej. la
  fuente nativa de main, `options(wjpr.family = )`, que hace innecesario `wjpr_font()`) y se cambia el commit de
  `deps.csv`.
- `options(wjpr.complementos = FALSE)` dibuja con WJPr sola (la línea de base de la galería, `--sin-complementos`).

**Galería** (`Rscript src/src_reports/tools/wjpr_gallery.R [--figs …] [--out …] [--sin-complementos]`): dibuja las 42 (con
`data()` si la figura existe; si no, desde la referencia) y mide textos, fuente y parecido con lo publicado →
`output/<slug>/wjpr/00-wjpr.csv`, `index.html` y `compare/`. Resultado del 23-oct, igual con 1.1.0@382aee9 y con main
(a46c831): 42/42 con textos idénticos, 0 textos dudosos y f1 0.996-1.000 (≥ 0.999 en 40; 0.997 la 16 desde la
referencia y 0.996 la 29). Complementos: 367 con 1.1.0 (5-13 por figura) y 333 con main (5-12): main ahorra 34, casi
todos la fuente. WJPr sola: mediana 0.647 con 1.1.0 (41 figuras con la fuente de reemplazo) y 0.623 con main (fuente
resuelta). Las brechas del paquete están en el documento «WJPr: brechas para automatizar las figuras de Paths to
Justice».

**Piloto sin complementos (histórico, `tools/pilot_wjpr.R`, 2026-10-23).** Para cada tipo con posible equivalente se
dibujó la figura publicada con la función de WJPr 1.1.0 (`worldjusticeproject-org/WJPr@382aee9`) usando solo argumentos
documentados y el tema por `ptheme`, y se midieron 45 criterios sobre el ggplot construido y el SVG. Ninguna los cumplía
todos: la familia "Lato Full" está fijada dentro de las capas (sin esa fuente el texto cae en una de reemplazo con otras
métricas); `wjp_groupbars` admite un solo promedio y no dibuja las «N/A»; `wjp_dumbbells` no trae leyenda ni
encabezados y pierde el valor de experiencia de la fila sin percepción; `wjp_bars` centra los valores sobre el final de
la barra y fija `position = "stack"`. Esos huecos son los que cubren hoy los complementos.

| Tipo (figuras) | Figura probada | Función | Criterios cumplidos | Fallados | Decisión del piloto |
|---|---|---|---|---|---|
| paired_bars (2) | 02 | `wjp_bars` | 3/6 | país y UE lado a lado (horizontal); fuente Inter Tight solo con argumentos (ptheme); métrica del texto = Inter Tight (0 dudosos) | **descartada** |
| severity_bars (7) | 07 | `wjp_bars` | 4/6 | dos series por severidad lado a lado; fuente Inter Tight solo con argumentos (ptheme) | **descartada** |
| grouped_bars (1, 4, 9, 12, 17, 20-28, 31, 33, 35, 38-40) | 09 | `wjp_groupbars` | 2/9 | dos filas de promedio (National + EU Average); encabezados como filas de la columna de etiquetas; filas de barra (promedios + categorías); celdas suprimidas impresas 'N/A'; fuente Inter Tight solo con argumentos (ptheme); textos de valor = publicados; métrica del texto = Inter Tight (0 dudosos) | **descartada** |
| dumbbell (10, 15) | 10 | `wjp_dumbbells` | 3/9 | encabezados de grupo; eje 0-100 % cada 25; leyenda Perception / Experience; fuente Inter Tight solo con argumentos (ptheme); textos de valor = publicados; métrica del texto = Inter Tight (0 dudosos) | **descartada** |
| hbars (13, 14, 18, 19, 36) | 13 | `wjp_bars` | 5/9 | valor a continuación del final de la barra; valores en semibold cursiva; fuente Inter Tight solo con argumentos (ptheme); métrica del texto = Inter Tight (0 dudosos) | **descartada** |
| columns (30, 37) | 30 | `wjp_bars` | 3/6 | país y UE lado a lado (vertical); fuente Inter Tight solo con argumentos (ptheme); métrica del texto = Inter Tight (0 dudosos) | **descartada** |
| funnel (8, 34) | — | — | — | WJPr no tiene función de este tipo | **sin_equivalente** |
| sankey (11, 16) | — | — | — | WJPr no tiene función de este tipo | **sin_equivalente** |
| cards (32, 41, 42) | — | — | — | WJPr no tiene función de este tipo | **sin_equivalente** |
| matrix (6) | — | — | — | WJPr no tiene función de este tipo | **sin_equivalente** |
| upset (29) | — | — | — | WJPr no tiene función de este tipo | **sin_equivalente** |
| network (5) | — | — | — | WJPr no tiene función de este tipo | **sin_equivalente** |
| scatter (3) | — | — | — | WJPr no tiene función de este tipo | **sin_equivalente** |

Salidas del piloto en `output/<slug>/compare/`: `pilot_wjpr_figNN.svg|png`, `pilot_wjpr_figNN_comparacion.png` y
`pilot_wjpr_criterios.csv` (fig, tipo, función, criterio, esperado, obtenido, cumple); `Rscript
src/src_reports/tools/pilot_wjpr.R` lo repite.

## Tiempos (medidos el 2026-10-22 y 23, MacBook, .dta ya descargado)
| Etapa | Tiempo |
|---|---|
| `--stage data` en frío (`--force`) | 33 s (lectura del `.dta` 3 s; variables y master de 27 países 28 s) · la primera vez OneDrive tarda además ~50 s en descargar los 295 MB |
| `--stage data` con caché | 3 s |
| `--stage data` tras cambiar 01/02 o `deps.csv` (reconstruye el master, 23-oct) | 40-63 s |
| `--stage all --figs 09,10,11` con caché | 21 s con el dibujo propio (svg_r); 15 s con WJPr (23-oct: figures 09 4.5 s, 10 2 s, 11 1.9 s con PNG; verify con el parecido 1 s) |
| `tools/pilot_wjpr.R` | 15 s |
| `--stage verify` / `--stage compare` | 1-3 s |
| `--stage figures --figs 09,10,11` | 7 s (09: 4 s, 10: 1.8 s, 11: 1.5 s, con PNG; medido el 2026-10-23) |

## OneDrive
La carpeta vive en OneDrive: una conexión de archivo abierta durante toda la corrida pierde escrituras y falla al
cerrar ("Operation timed out"), por eso el log se escribe mensaje a mensaje y la salida capturada pasa por un
temporal local. Si un `.ttf` o el `.dta` pesan 0 bytes es que no están descargados. `cache_dir` en `config/local.yml`
permite sacar el caché de OneDrive.
