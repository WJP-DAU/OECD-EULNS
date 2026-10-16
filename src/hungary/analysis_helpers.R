## Helpers for the A0x cross-tabulations (report weighting: NUTS averages, n < 30 suppression) -------------

trust_actors <- c("Judges" = "TRT_judges", "Prosecutors" = "TRT_prosecutors", "Public defense attorneys" = "TRT_pda")

# Person-level analysis frame for Hungary with every derived variable the A-series needs
build_analysis_frame <- function() {
  nt <- problem_flags(hu)
  journey_vars(hu) %>%
    mutate(
      across(all_of(unname(trust_actors)), binary_agree, .names = "trust_{.col}"),
      has_nt = non_trivial_problem %in% 1,
      resolved = rp_outcome,
      in_favor = case_when(resolution_favor == "In favor of respondent" ~ 1, resolution_favor == "Not in favor / Unknown" ~ 0),
      cooc3 = case_when(nproblems == 1 ~ "1 problem", nproblems %in% 2:3 ~ "2-3 problems", nproblems >= 4 ~ "4 or more problems"),
      housing_land_nt = as.numeric(nt[["Housing"]] | nt[["Land"]])
    )
}

# % of a binary outcome by one grouping variable (report method), with unweighted n.
xt <- function(df, outcome, group, label_outcome = outcome, label_group = group) {
  nat_est(df %>% mutate(.g = as.character(.data[[group]])), outcome, by = ".g") %>%
    transmute(desenlace = label_outcome, corte = label_group, grupo = .g, pct, n, n_menor_30 = n < 30)
}
xt_nat <- xt

# Outcome by a binary exposure (1 vs 0), raw and within strata (report method in every cell). The
# standardised difference averages within-stratum differences weighted by stratum size.
diff_stratified <- function(df, outcome, exposure, strata = NULL) {
  est <- function(d) {
    r <- nat_est(d %>% mutate(.e = as.character(.data[[exposure]])), outcome, by = ".e")
    pick <- function(col, lev, default) { v <- r[[col]][r$.e == lev]; if (length(v) == 0) default else v }
    tibble(con = pick("pct", "1", NA_real_), sin = pick("pct", "0", NA_real_),
           n_con = pick("n", "1", 0L), n_sin = pick("n", "0", 0L))
  }
  d <- df %>% filter(!is.na(.data[[outcome]]), !is.na(.data[[exposure]]))
  out <- est(d) %>% mutate(estrato = "Sin control", .before = 1)
  if (!is.null(strata)) {
    levels <- sort(unique(na.omit(as.character(d[[strata]]))))
    by_s <- map_dfr(levels, \(s) est(d %>% filter(.data[[strata]] == s)) %>% mutate(estrato = paste0(strata, ": ", s), .before = 1))
    valid <- by_s %>% filter(!is.na(con), !is.na(sin))
    out <- bind_rows(
      out %>% mutate(diferencia = con - sin), by_s %>% mutate(diferencia = con - sin),
      tibble(estrato = paste0("Estandarizada por ", strata), con = NA_real_, sin = NA_real_,
             n_con = sum(valid$n_con), n_sin = sum(valid$n_sin),
             diferencia = if (nrow(valid) > 0) weighted.mean(valid$con - valid$sin, valid$n_con + valid$n_sin) else NA_real_)
    )
  } else {
    out <- out %>% mutate(diferencia = con - sin)
  }
  out %>% mutate(celda_menor_30 = n_con < 30 | n_sin < 30)
}

caveat_association <- "Asociación, no efecto: quienes acceden no son iguales a quienes no acceden (ingreso, educación, tipo y complejidad del problema). La versión estratificada controla solo por las variables indicadas."
