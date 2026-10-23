# static/

Vacía a propósito. Las 42 figuras se producen desde los microdatos (`metodo` svg_r o ggplot en
`manifest/figures.csv`). Una figura solo entra aquí como `metodo = static` con justificación escrita en la
columna `nota` del manifiesto y aprobación previa. Lo que hace falta:

1. `static/Figure_NN_slug.svg` con el texto ya en trazos (0 `<text>`; `outline_svg_text()` de `R/04_svg.R`) y el
   tamaño del manifiesto (ancho exacto, alto ± `verify$size_tol_in`).
2. `figures/fig_NN_slug.R` sigue siendo obligatorio: `data()` devuelve un df válido del contrato (las 8 columnas y al
   menos 1 fila, p. ej. la del título; `validate_df()` lo exige) y `plot = function(df, spec) NULL` (no se usa).
3. `--stage figures` escribe la tabla, copia el SVG a `output/<slug>/figures/`, comprueba tamaño y `<text>` (estados
   `tamaño` / `texto`) y lo rasteriza con Chrome. `verify` da `estatica` (no se verifica; sin tabla, `sin salida`) y
   `compare` la pone junto a la publicada como a las demás.
