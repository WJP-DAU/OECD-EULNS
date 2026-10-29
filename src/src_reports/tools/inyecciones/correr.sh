#!/bin/bash
## tools/inyecciones/correr.sh — corre los bancos de errores inyectados en el dibujo y comprueba que la verificación los
## detecta. Cada inyección cambia solo el dibujo (df intacto): dos etiquetas o celdas intercambiadas, un valor oculto,
## una barra +5 pp, una arista quitada, la línea de corte quitada… Trabaja en una copia temporal de la carpeta (sin
## output/; con cache/ y config/local.yml), así que no toca R/09_wjpr.R ni las salidas del proyecto.
## Uso: bash src/src_reports/tools/inyecciones/correr.sh        (unos 30-60 min; WJPr y Chrome como en main.R)
## Salida: una línea por inyección y, al final, «N de M detectadas»; exit 0 si se detectan todas las de main.R.
## Bancos (bancos/): se añaden al final de R/09_wjpr.R de la copia y envuelven el adaptador; INY = nombre de la
## inyección. Por main.R --stage all --figs NN --strict (detectada = exit 1 y la figura en «fallo»): iny_gb.R (01),
## iny_m2.R (02, 07, 13), iny_g1_M3.R (10), iny_g2_m4.R (03, 06), iny_g3_m4.R (05), iny_m5.R (11). Simuladas desde la
## referencia (save_figure + la regla de verify; imprimen su propio resultado): t_iny_ref.R (8, 15, 32, 34, 41, 42),
## iny_m2_sim.R (30, 37) e iny_fig.R con iny_m5.R (29).
set -u
SRC="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TMP="$(mktemp -d "${TMPDIR:-/tmp}/inyecciones.XXXXXX")"
trap 'rm -rf "$TMP"' EXIT
echo "copia de trabajo: $TMP/src_reports"
rsync -a --exclude output "$SRC/" "$TMP/src_reports/"
R="$TMP/src_reports"; F="$R/R/09_wjpr.R"; BK="$TMP/09_wjpr.R.orig"; B="$R/tools/inyecciones/bancos"
cp "$F" "$BK"; m0=$(md5 -q "$F" 2>/dev/null || md5sum "$F" | cut -d' ' -f1)
cd "$TMP"
n=0; det=0
por_main() {                                   # banco fig iny…
  local banco=$1 fig=$2; shift 2
  for iny in "$@"; do
    cp "$BK" "$F"; cat "$B/$banco" >> "$F"
    out=$(INY=$iny Rscript "$R/main.R" --stage all --figs "$fig" --strict 2>&1); ex=$?
    fila=$(grep -E "^$fig " <<< "$out" | head -1)
    n=$((n + 1))
    if [ "$ex" -eq 1 ] && grep -q " fallo " <<< "$fila"; then det=$((det + 1)); r="detectada"; else r="NO DETECTADA"; fi
    echo "fig $fig · $iny · $r · $(cut -c1-110 <<< "$fila" | sed -E 's/ +/ /g')"
  done
  cp "$BK" "$F"
}
por_main iny_gb.R 01 male_female edad area encabezado valor_oculto fuente barra sin_pista orden
por_main iny_m2.R 02 pb_textos pb_oculto pb_barra pb_series pb_series_todo pb_etiquetas pb_leyenda
por_main iny_m2.R 07 sev_barra sev_leyenda sev_ejes sev_corte
por_main iny_m2.R 13 hb_textos hb_oculto hb_barra hb_etiquetas
por_main iny_g1_M3.R 10 db_textos db_etiquetas db_punto db_oculto db_leyenda
por_main iny_g2_m4.R 06 mx_textos mx_filas mx_traspuesta mx_banda
por_main iny_g2_m4.R 03 sc_x3 sc_hueco sc_rotulos sc_rotulo_otro
por_main iny_g3_m4.R 05 red_nombres red_porcentajes red_gruesa red_fina red_nodo
por_main iny_m5.R 11 sk_valores sk_etiquetas sk_alto sk_encabezados sk_oculto
echo "== simuladas desde la referencia"
INY_ROOT="$R" Rscript "$B/t_iny_ref.R" 2>&1 | grep -E "^fig "
INY_ROOT="$R" Rscript "$B/iny_m2_sim.R" 30 ninguna col_textos col_oculto col_barra col_series col_series_todo col_etiquetas col_leyenda 2>&1 | grep -E "^== |verify"
INY_ROOT="$R" Rscript "$B/iny_m2_sim.R" 37 ninguna col_textos col_barra col_series col_etiquetas 2>&1 | grep -E "^== |verify"
for iny in ninguna up_valores up_nombres up_alto up_conjunto up_punto up_oculto; do
  cp "$BK" "$F"; cat "$B/iny_m5.R" >> "$F"
  echo "== 29 $iny"; INY=$iny INY_ROOT="$R" Rscript "$B/iny_fig.R" 29 2>&1 | grep -E "fig 29|verify"
done
cp "$BK" "$F"
m1=$(md5 -q "$F" 2>/dev/null || md5sum "$F" | cut -d' ' -f1); [ "$m0" = "$m1" ] || echo "ERROR: R/09_wjpr.R de la copia no quedó igual"
echo "por main.R: $det de $n detectadas"
[ "$det" -eq "$n" ]
