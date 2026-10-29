## Figura 29 · Co-occurrence of Unmet Legal Needs and Unresolved Justice Problems in {country}
## Fuente de datos: run_F08 literal (F08.R:3-23): jg_inverse_rp = time|fair|cost (derivado), inside_gap==1 (jg
##   keepDK), NA→0, share ponderado NUTS sin n<30; filas = round(pct·10); totales por conjunto = Σ
##   round(pct·10)/10 (71.2, no 71.4); text fmt_num(value, 1)
## Método UE: none
## Código de origen: F08.R:3-23 (datos) + render_figure.R:53-75 (ComplexUpset; % vertical angle 90;
##   width_ratio 0.22; expansión 0.25/0.7; texto 12.8) + outline (base 10.24; lienzo 6.5×5.4 × 1.024)

# Conjuntos (rótulo impreso = indicador de necesidad insatisfecha de la tabla keepDK). origen: render_figure.R:44-48
fig29_sets <- c("Access to Assistance" = "jg_inverse_access2rep", "Access to Information" = "jg_inverse_access2info",
                "Resolution Process" = "jg_inverse_rp", "Problem Resolved" = "jg_inverse_outcome")

# origen: F08.R:6-21, literal salvo lo marcado: dentro de la brecha (inside_gap == 1, keepDK), una fila por combinación
# de necesidades insatisfechas con pct = Σ_NUTS casos/encuestados de la región × regionpoppct × 100 (sin n < 30) y n =
# casos sin ponderar; jg_inverse_rp = tiempo, equidad o costo. adaptado: hu → ctx$national, hu_jg → ctx$jg (la tabla
# keepDK del país, 01_load.R), regions → ctx$regions. Solo la usa esta figura.
fig29_combos <- function(ctx) {
  regions <- ctx$regions
  ctx$national %>%
    select(country_year_id, country_name_ltn, nuts_id) %>%
    left_join(
      ctx$jg %>%
        mutate(jg_inverse_rp = case_when(
          jg_inverse_time == 1 | jg_inverse_fair == 1 | jg_inverse_cost == 1 ~ 1,
          !is.na(jg_inverse_time) | !is.na(jg_inverse_fair) | !is.na(jg_inverse_cost) ~ 0
        )) %>%
        select(country_year_id, jg_inverse_access2info, jg_inverse_access2rep, jg_inverse_rp, jg_inverse_outcome, inside_gap),
      by = "country_year_id"
    ) %>%
    filter(inside_gap == 1) %>%
    mutate(across(starts_with("jg_"), \(x) coalesce(x, 0))) %>%
    count(nuts_id, jg_inverse_access2info, jg_inverse_access2rep, jg_inverse_rp, jg_inverse_outcome, name = "count_size") %>%
    group_by(nuts_id) %>% mutate(total = sum(count_size)) %>% ungroup() %>%
    left_join(regions %>% select(nuts_id, wgt = regionpoppct), by = "nuts_id") %>%
    group_by(jg_inverse_access2info, jg_inverse_access2rep, jg_inverse_rp, jg_inverse_outcome) %>%
    summarise(pct = sum(count_size / total * wgt) * 100, n = sum(count_size), .groups = "drop")
}

fig_29 <- list(

  # Una fila «row» por barra de intersección (label = sus conjuntos en el orden de fig29_sets unidos por " + ", value =
  # pct sin redondear, text con un decimal), de izquierda a derecha como las dibuja ComplexUpset con las réplicas
  # round(pct · 10) de lo publicado (render_figure.R:44-57: upset_data(min_size = 1, sort_sets = FALSE), que ordena por
  # réplicas y desempata a su manera; el mismo orden que tools/build_reference.R); y una fila «set» por conjunto, de
  # arriba abajo como la matriz, con su total como lo dibuja la figura: Σ round(pct · 10) / 10 (71.2, no 71.4).
  data = function(ctx) {
    d <- fig29_combos(ctx)
    sets <- names(fig29_sets)
    member <- as.matrix(d[, unname(fig29_sets)]) == 1
    colnames(member) <- sets
    combo <- apply(member, 1, \(m) paste(sets[m], collapse = " + "))
    if (any(combo == "")) {                              # nuevo: sin necesidad marcada no hay barra que dibujar
      log_msg("fig 29 aviso: ", sum(d$n[combo == ""]), " casos dentro de la brecha sin ninguna necesidad marcada (fuera)")
      d <- d[combo != "", ]; member <- member[combo != "", , drop = FALSE]; combo <- combo[combo != ""]
    }
    reps <- round(d$pct * 10)
    ud_in <- stats::setNames(as.data.frame(member[rep(seq_len(nrow(d)), reps), , drop = FALSE]), sets)
    ud <- ComplexUpset::upset_data(ud_in, sets, min_size = 1, sort_sets = FALSE)
    mf <- ud$matrix_frame %>% mutate(across(c(intersection, group), as.character))
    left_right <- rev(as.character(ud$sorted$intersections))
    lab_lr <- vapply(left_right, \(id) paste(sets[sets %in% mf$group[mf$intersection == id & mf$value]], collapse = " + "), "")
    keep <- match(lab_lr, combo)
    if (anyNA(keep)) stop("fig 29: ComplexUpset dibuja intersecciones que no están en los datos", call. = FALSE)
    top_bottom <- rev(as.character(ud$sorted$groups))
    tot <- colSums(member * reps) / 10
    log_msg("fig 29: ", length(keep), " intersecciones (", sum(reps == 0), " sin réplicas, no se dibujan) · conjuntos ",
            paste(sprintf("%s %.1f", top_bottom, tot[top_bottom]), collapse = " / "))
    bind_rows(
      tibble(kind = "row", label = combo[keep], value = d$pct[keep], text = fmt_num(d$pct[keep], 1), n = as.integer(d$n[keep])),
      tibble(kind = "set", label = top_bottom, value = unname(tot[top_bottom]), text = fmt_num(tot[top_bottom], 1), n = NA_integer_)
    ) %>%
      transmute(order = row_number(), kind, grouping = "", label, value, value2 = NA_real_, text, text2 = "", n)
  },

  # Dibujo: WJPr::wjp_upset() con sus argumentos (wjpr_upset, R/09_wjpr.R; WJPr 1.2.0): barras de intersección, matriz
  # y tamaños de conjunto en un solo ggplot. ComplexUpset solo da el orden de las intersecciones (data()).
  plot = function(df, spec) wjpr_upset(df, spec)
)
