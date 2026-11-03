# tools/inyecciones — errores inyectados en el dibujo

Prueban que la verificación ve errores que no cambian el conjunto de textos ni casi el parecido con la figura
publicada: dos etiquetas o celdas intercambiadas, un valor oculto, una barra unos puntos más larga, una arista quitada,
un punto hueco de más, la línea de corte de la 7 quitada. Cada inyección cambia solo el dibujo (el df de `data()` queda
intacto), así que una verificación sana tiene que dar `texto` o `dibujo` en figures y `fallo` en verify (exit 1 con
`--strict`).

    bash src/src_reports/tools/inyecciones/correr.sh

Corre en una copia temporal de la carpeta (sin `output/`): no toca `R/09_wjpr.R` ni las salidas del proyecto. Tarda
30-60 min. Al final da «N de M detectadas» para las que pasan por `main.R` y exit 0 si son todas.

| Banco (`bancos/`) | Figuras | Cómo corre |
|---|---|---|
| `iny_gb.R` | 01 (agrupadas) | main.R |
| `iny_m2.R` | 02 (pareadas), 07 (severidad), 13 (barras horizontales) | main.R |
| `iny_g1_M3.R` | 10 (pesas) | main.R |
| `iny_g2_m4.R` | 03 (dispersión), 06 (matriz) | main.R |
| `iny_g3_m4.R` | 05 (red) | main.R |
| `iny_m5.R` | 11 (sankey) por main.R; 29 (UpSet) con `iny_fig.R` | main.R y simulada |
| `t_iny_ref.R` | 8, 34 (embudo), 15 (pesas), 32, 41, 42 (tarjetas) | simulada desde la referencia |
| `iny_m2_sim.R` | 30, 37 (columnas) | simulada desde la referencia |

Un banco envuelve el adaptador (`wjpr_<tipo> <- function(df, spec, ...) { p <- orig(df, spec, ...); <cambio>; p }`),
conserva `attr(p, "wjpr")` y elige la inyección con `Sys.getenv("INY")`. Con ggplot2 4, para cambiar el mapeo o los
datos de una capa hay que rehacerla con `ggplot2::layer(…)`: asignar `l$mapping` a un `ggproto(NULL, l)` desborda la
pila al construir. Al cambiar un adaptador, sus inyecciones pueden dejar de aplicar (dependen de sus capas): se portan
con el mismo error semántico. Resultado de referencia (2026-10-29, WJPr 1.2.0@1664a23): 47 de 47 por main.R y 42 de 43
simuladas (la que no: series cambiadas en la 37, donde las dos valen 99 %).
