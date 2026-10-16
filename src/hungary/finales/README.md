# Figuras finales · Paths to Justice in Hungary

Genera las 42 figuras del reporte final de Hungría con la numeración del reporte (SVG + PNG a 300 dpi) y arma la carpeta de entrega con el reporte. Las figuras muestran los mismos valores que el reporte, pero estandarizadas para el ensamblaje:

- Todo el texto va en trazos con Inter Tight. Así se ven igual aunque la máquina que arma el reporte no tenga la fuente.
- El texto base mide 10 pt cuando la figura se coloca al 100 % de su tamaño.
- Las figuras de barras comparten medidas (y, según su tipo, una maqueta común); embudos, sankeys y tarjetas se redibujan con su diseño original.
- Los anchos caben en la página del reporte ensamblado.

## Orden de ejecución (desde la raíz del proyecto)

1. `Rscript main.R --data` produce `data/master.csv` y `data/jg_data_*.csv`. Requiere `path2EU` en `src/utils/config.R`.
2. `Rscript src/hungary/worker-hungary.R` deja las salidas R en `output/viz/Hungary/analisis adicionales/`, entre ellas las fuentes de las figuras 3, 6, 7 y 27. Necesita `path2EU` o la caché `output/tabs/cache_hungary/eu_master.rds`.
3. `Rscript src/hungary/finales/extract_report_figures.R` se corre **solo cuando se emite una nueva versión final del reporte**. Antes hay que actualizar `REPORT_DOCX` / `REPORT_PDF` en `paths.R` y revisar `provenance.csv`.
4. `Rscript src/hungary/finales/build_finales.R` escribe `output/viz/Hungary/finales/` y `output/finales/Hungria/`. Termina con `Verificación OK` o sale con estado 1.
5. `Rscript src/hungary/finales/proof_assembly.R "<reporte ensamblado.pdf>"` genera la prueba de ensamblaje en `output/viz/Hungary/prueba_ensamblaje/`. El argumento es opcional; sirve para tomar el texto de los recuadros.

El reporte final (DOCX y PDF) vive en `output/viz/Hungary/analisis adicionales/`, que no está versionado: en un clon limpio hay que copiarlo ahí.

Si cambia a propósito alguna fuente fuera del reporte (SVG del pipeline, salidas R, tablas; ver `fuentes.csv`), revise el cambio y corra `Rscript src/hungary/finales/build_finales.R --registrar-fuentes`.

## Dependencias

- Archivos TTF de Inter Tight en `{path2DA}/6. Country Reports/0. Fonts/` (no hace falta instalarlos). Se usan para convertir el texto en trazos.
- Paquetes R del `renv` del proyecto: `tidyverse`, `xml2`, `png`, `systemfonts`, `textshaping`, `svglite`, `showtext`, `ragg` y `ComplexUpset`.
- Chrome o Chromium para rasterizar los PNG. La ruta está en `paths.R` o en `CHROMOTE_CHROME`.
- poppler (`pdftotext`, `pdfinfo`, `pdftoppm`) para `extract_report_figures.R` y `proof_assembly.R`.
- Las Figuras 6 y 29 se vuelven a dibujar desde los datos (`src/hungary/F01.R`, `src/hungary/F08.R`). Necesitan `data/master.csv`, `data/jg_data_keepdk.csv`, `data/regions.csv` y la caché UE (o `path2EU`).
- Las medias UE de `data4web` se verifican contra `{path2EU}/reports/eu-thematic-reports/data-viz/output/data4web_gpp.csv`. Sin acceso a ese archivo, esas filas quedan «sin dato» y el build lo avisa.

## Archivos

| Archivo | Qué es |
|---|---|
| `paths.R` | Rutas compartidas: reporte final, carpetas de salida, `DPI`, Chrome |
| `extract_report_figures.R` | Lee el DOCX, asocia cada pie «Figure N.» con su imagen y escribe `svg/Figure_NN.svg` (SVG tal como está en el reporte), `svg/Figure_NN_report.png` (figuras solo PNG) y `manifest.csv` |
| `manifest.csv` | Número, título, página del PDF, recuadro en la página (in), medios y md5 de cada activo y del DOCX/PDF del reporte |
| `provenance.csv` | Procedencia de la figura publicada: `titulo` (debe coincidir con el pie del reporte), `slug`, `fuente`, `r_stem`, `metodo_ue`, `origen` |
| `fuentes.csv` | md5 de las fuentes que no están en `svg/`: SVG del pipeline (4, 20, 38–40), salidas R (3, 6, 7, 27), tablas de las figuras dibujadas desde datos (8, 11, 14, 16, 19, 32, 34, 36, 41, 42) y datos de 6 y 29 |
| `svg/` | Activos versionados extraídos del reporte (fuente de verdad del contenido) |
| `standard_figures.R` | Plan por figura (`FIGURE_PLAN`): cómo se estandariza, contra qué se verifica, `TEXT_PT` y anchos máximos |
| `report_charts.R` | Dibuja en SVG: barras agrupadas, pares Hungría/UE, dumbbells, barras horizontales con eje (diseño de `gen_horizontal_bars()`), columnas Hungría/UE, embudos, sankeys y tarjetas (diseño de `charts5.py`). `STD`: medidas comunes |
| `figure_specs.R` | Lee el contenido de las figuras publicadas (orden, grupos, etiquetas, valores, barras) y lo verifica contra los datos (`check_spec`) |
| `svg_text.R` | Convierte texto en trazos de Inter Tight (`text_path`, `outline_svg_text`) |
| `render_figure.R` | Vuelve a dibujar las Figuras 6 (F01) y 29 (F08) en otro lienzo, en su propio proceso de R, sin escribir en `output/` |
| `build_finales.R` | Construye, verifica y publica las figuras finales |
| `proof_assembly.R` | Prueba de ensamblaje con la maqueta del reporte del 16-oct |
| `legacy_python/` | Copias de los scripts Python que produjeron algunas figuras fuera del repo (solo trazabilidad; ver su README) |

## Qué hace `build_finales.R`

**Antes de escribir nada**, se detiene si ocurre cualquiera de estas cosas:

- el DOCX/PDF cambió desde la extracción (md5);
- `provenance.csv` no tiene una fila por figura, o su `titulo` no coincide con el pie del reporte;
- un SVG de `svg/` no es el del reporte;
- el PNG que produce `worker-hungary.R` para 6, 7 o 27 ya no es idéntico al del reporte;
- una fuente de `fuentes.csv` cambió.

Después construye cada figura en una carpeta temporal según su método (`FIGURE_PLAN`):

| Método | Figuras | Qué se hace |
|---|---|---|
| Barras agrupadas y pares | 1, 2, 4, 9, 12, 17, 20–26, 28, 31, 33, 35, 38–40 | Se lee el contenido de la figura publicada y se redibuja con la maqueta común. Para 4, 20 y 38–40 se usa el SVG del pipeline del 9-jul (el reporte trae su texto en trazos) y se comprueba que las barras sean idénticas a las publicadas. |
| Dumbbells y Figura 27 | 10, 15, 27 | Igual, pero llenando el ancho junto al recuadro (eje o barra más largos). La 27 sale de `A01.csv` y se comprueba contra el SVG de A01. |
| Barras horizontales con eje | 13, 14, 18, 19, 36 | Diseño de `gen_horizontal_bars()` con medidas comunes. 14, 19 y 36 salen de sus tablas y se comprueban contra las barras publicadas; en 13 y 18 los valores se leen de las barras publicadas y las etiquetas se transcriben (`FIGURE_PLAN`). |
| Columnas Hungría/UE | 30, 37 | Columnas verticales con el contenido de la figura publicada. |
| Embudos, sankeys y tarjetas | 8, 34, 11, 16, 32, 41, 42 | Diseño de `charts5.py` redibujado en R, más largo y grande, con los valores de las tablas que leyó ese script (R03, A03, A04, A05); se comprueban contra la figura publicada (proporciones, valores impresos, número de tarjetas). |
| Reescalada | 5 | El texto ya estaba en trazos: solo cambian el tamaño y el fondo blanco. |
| Texto a trazos | 3, 7 | Se usa el SVG de R (la 3 se comprueba punto por punto contra la publicada). |
| Re-renderizada | 6, 29 | Se dibujan desde `F01.R` y `F08.R` en un lienzo mayor (29: porcentajes en vertical). Se comprueba que los textos y valores sean idénticos a los publicados. |

**Antes de publicar** verifica lo siguiente:

- cada valor impreso coincide con su tabla (pipeline, `data4web` para las medias UE, R03, F04, F06, F07, F10), y la longitud de cada barra con su valor (tolerancia de 0.6 pp). El detalle queda en `datos/00-verificacion_valores.csv`;
- se cumplen las comprobaciones adicionales;
- no queda texto vivo;
- ningún texto se midió con otra fuente;
- el ancho cabe en la página;
- el texto base mide `TEXT_PT`;
- el PNG tiene el tamaño correcto a 300 dpi.

Si algo falla, no publica nada.

**Al publicar** borra solo lo que generó una corrida anterior (`Figure_NN_*.svg/png`, `00-INDICE.*`, `datos/`) y copia figuras, índice, PDF y DOCX a la carpeta de entrega, donde además arma `figuras.zip` con las mismas figuras (lista para enviar). Los archivos ajenos que encuentre en la carpeta de entrega solo los reporta como aviso. Por último, verifica las copias.

## Estándar (ensamblaje del 16-oct)

El reporte se ensambla en HTML impreso con Chrome:

- toda la página sale al 82.2 %;
- cada figura va a su tamaño declarado;
- el cuerpo de texto mide 16 px (unos 10 pt en papel);
- no está Inter Tight.

Por eso se aplican estas medidas:

- `TEXT_PT = 10`: unos 8.2 pt en papel, un 11 % más que la Figura 31 de esa versión (9 pt). En las barras, los valores van a 9 pt y las leyendas a 9.5 pt (`STD` en `report_charts.R`, con las proporciones de `gen_grouped_bars()`).
- Ancho máximo de 6.5 in para las figuras con recuadro de texto a la derecha (`CALLOUT_FIGS`) y de 8.4 in para las demás. La Figura 5 llega al límite y su texto queda en 8 pt. Las etiquetas de las tarjetas (32, 41, 42) van a 11 pt, a pedido.
- Barras: columna de etiquetas común (la etiqueta más ancha de todas las figuras; las barras horizontales 13, 14, 18 y 19, de etiquetas largas, comparten otra más ancha), barra de 250 pt sobre fondo gris, filas de 15.5 pt (26 pt por par Hungría/UE), etiquetas alineadas a la derecha y valores a la derecha de la barra en la misma posición.

## Cambios de contenido respecto al reporte del 15-oct

- Figura 20: «5 or more problems» pasa a «6 or more problems». Es la categoría `nproblems >= 6`, igual que en las demás figuras.
- Figuras 30 y 37: columnas verticales. Figura 36: diseño de la Figura 13.
- Embudos (8, 34): la primera etapa ocupa todo el ancho. Sankeys (11, 16): a lo ancho de la columna y centrados. Tarjetas (32, 41, 42) más grandes, 41 y 42 con la misma escala de color. Figuras 6, 10, 15, 27 y 29 más grandes.
- Figura 29: los porcentajes van en vertical.
- Figura 4: el eje va de 0 a 10.
- Figuras 10 y 15: los valores van en negrita cursiva.

Ningún valor cambia. Queda pendiente de decisión la Figura 16 (dice 56 % donde el código da 55 %).

## Procedencia (`provenance.csv`)

- `fuente` describe la figura publicada: `svg_reporte` (el reporte embebe un SVG) o `codigo_r` (el reporte solo trae el PNG producido por la salida de R indicada en `r_stem`). El método con el que se construye la figura final está en `FIGURE_PLAN` y en `00-INDICE`.
- `metodo_ue` puede ser:
  - `data4web`: media UE publicada en `data4web_gpp.csv`, leída por `worker-viz.R`;
  - `eu_est`: media UE recalculada con `eu_est()` de `src/hungary/setup.R`;
  - `R03`: media UE de `R03.csv`, con la misma definición que el valor nacional;
  - `sin media UE`.
- `origen` indica de dónde sale la figura publicada:
  - el pipeline (`gen_grouped_bars()`, `gen_horizontal_bars()`, salidas del 9-jul);
  - scripts de `src/hungary` (28-sep);
  - scripts Python externos (`legacy_python/`);
  - ediciones hechas a mano fuera del repo: la etiqueta «6 or more problems» del 15-oct en 9, 12, 17, 21–25 y 31; los lienzos ensanchados sin documentar en 2, 10, 15, 28–31, 33, 35 y 37; y «Without a non-trivial problem» en 41 y 42.
