## Figura 06 · Co-occurrence of Non-Trivial Justice Problems in {country}, by Problem Category
## Fuente de datos: cooc_flags(ctx) (03_estimators.R ← F01.R:6-7: problema no trivial por categoría) → F01.R:8-19:
##   orden = prevalencia no trivial ponderada (nat_est) de mayor a menor; celda = nat_est(flags[nt[[col]], ], fila) =
##   P(fila | columna) con el promedio NUTS del reporte (n < 30 → "N/A"); "–" en la diagonal; banda (text2) =
##   cut(right = FALSE) sobre el % sin redondear. Contraste: analisis adicionales/F01.csv (las 144 celdas)
## Método UE: none
## Código de origen: F01.R:5-21 (datos de run_F01); dibujo F01.R:23-41 (geom_tile + geom_text; theme_minimal) +
##   render_figure.R:29-38 (lienzo 8.4 in × base_pt 11 / 10)

# Bandas de la matriz de la más baja a la más alta (F01.R:17): son las etiquetas del cut() y los nombres con que
# wjpr_matrix fija el color de cada banda (COL_BG, COL_LIGHT, COL_MAIN, F01.R:26) y el texto blanco (la última)
fig06_bands <- c("Less than 25%", "25% to 50%", "50% or more")

fig_06 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "matrix", metodo = "wjpr", w_in = 8.4, h_in = 7.56, callout = FALSE, label_col = "none",
              base_pt = 11, metodo_ue = "none",
              nota = paste("144 celdas verificadas por tabla (text + banda); no contadas en los 404; color de banda",
                           "por nombre (F01.R:26)")),

  # Una fila por celda: filas de arriba abajo y, en cada una, columnas de izquierda a derecha, las dos en el orden de
  # prevalencia (F01.R:12 recorre columna × fila; el df va por filas, como la referencia). grouping = fila («People
  # who also experienced»), label = columna («Among people who experienced problems with»). value = % sin redondear
  # (NA en la diagonal y en las suprimidas); text = "62%" | "N/A" | "–"; text2 = banda ("" donde no hay valor).
  data = function(ctx) {
    flags <- cooc_flags(ctx)
    # origen: F01.R:8-9 (prevalencia no trivial ponderada por categoría; sort decreciente)
    prev <- map_dbl(categories, \(k) nat_est(ctx, k, df = flags)$value) %>% setNames(categories) %>%
      sort(decreasing = TRUE)
    ord <- names(prev)
    # origen: F01.R:12-19 — nat_est(flags[nt[[x]], ], y) con x = columna, y = fila; nt[[x]] ≡ flags[[x]] == 1
    # corregido: $value[1] → NA ("N/A") si nadie tiene la categoría de la columna (el original fallaba en map_dbl)
    p_cond <- function(x, y) if (x == y) NA_real_ else nat_est(ctx, y, df = flags[flags[[x]] == 1, ])$value[1]
    expand_grid(grouping = ord, label = ord) %>%
      mutate(
        value = map2_dbl(label, grouping, p_cond),
        band = cut(value, c(-Inf, 25, 50, Inf), labels = fig06_bands, right = FALSE),
        text = if_else(label == grouping, "–", fmt_num(value, 0)),     # F01.R:18: paste0(round(pct), "%")
        text2 = if_else(is.na(band), "", as.character(band))
      ) %>%
      transmute(order = row_number(), kind = "cell", grouping, label, value, value2 = NA_real_, text, text2)
  },

  # Dibujo: WJPr::wjp_heatmap() con sus argumentos (wjpr_matrix, R/09_wjpr.R; revisión 5, WJPr 1.2.0): filas =
  # grouping de arriba abajo, columnas = label en el orden de df$order, relleno = banda (text2) con la escala ordinal
  # del reporte asignada por nombre de banda (fig06_bands), títulos de eje fijos del diseño y tamaños divididos por
  # base_pt / 10.
  plot = function(df, spec) wjpr_matrix(df, spec, bands = fig06_bands)
)
