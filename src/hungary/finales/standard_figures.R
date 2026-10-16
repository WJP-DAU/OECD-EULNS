## Standard version of every figure of the final Hungary report -----------------------------------------------
## Sourced by build_finales.R. One rule for the whole report (feedback on the assembled report of 16 Oct 2026:
## figures placed at very different scales, Figures 1 and 9 tiny, "NationalAverage" because the assembler
## lacks Inter Tight), plus the per-figure requests of the same day (larger and longer figures):
##   - text drawn as Inter Tight outlines in every figure (no live text, no font dependency);
##   - base text (labels, group headers) at TEXT_PT when the figure is placed at 100 %: 10 pt, 11 % larger
##     than Figure 31 of that report (9 pt); values at 9 pt and legends at 9.5 pt (STD in report_charts.R);
##     scorecard labels at 11 pt (requested); Figure 5 stays below TEXT_PT because its width is capped;
##   - the bar figures redrawn with the measures of report_charts.R; content read from the published figure
##     or from the table it was drawn from, and checked against the data or against the published figure:
##       grouped bars and Hungary/EU pairs: one common label column, bar length and value position;
##       dumbbells (10, 15) and Figure 27: wider, filling the room beside the callout box;
##       horizontal bars 13, 14, 18, 19 and 36: design of the pipeline's gen_horizontal_bars() (axis 0-100 %);
##       Hungary/EU comparisons 30 and 37: vertical columns;
##   - funnels (8, 34), sankeys (11, 16) and scorecards (32, 41, 42) redrawn with the design of the external
##     charts5.py (legacy_python/), longer and larger; the network (5) only rescaled;
##   - the ggplot figures outlined and sized so that their base text measures TEXT_PT; the matrix (6) and the
##     upset plot (29) re-rendered from their scripts (render_figure.R) on a canvas that makes them larger.
## Width limits come from the report layout: figures with a callout box at their right must fit beside it.

source("src/hungary/finales/report_charts.R")
source("src/hungary/finales/figure_specs.R")

TEXT_PT          <- 10     # base text size at 100 % (pt)
MAX_W_CALLOUT_IN <- 6.5    # beside the callout box
MAX_W_FULL_IN    <- 8.4    # full text column
CALLOUT_FIGS     <- c(1, 4, 5, 7, 8, 9, 10, 12, 15, 17, 20:35, 38:40)
FINALES_SOURCES  <- file.path(FINALES_SRC, "fuentes.csv")   # md5 of the sources outside the report
W_CALLOUT        <- MAX_W_CALLOUT_IN * 72                  # pt

PIPE <- file.path("output", "viz", "Hungary")
TABS <- file.path("output", "tabs", "csv")
pub  <- function(fig) file.path(FINALES_SVG, sprintf("Figure_%02d.svg", fig))
add  <- function(file) file.path(R_OUTPUTS, file)
tab  <- function(file) file.path(TABS, file)

# Measures of the figures that differ from STD
ST_DUMB   <- with_std(pitch = 18)                                         # track: fills W_CALLOUT
ST_A01    <- with_std(pitch = 22, bar = 18, header = 26, header_base = 20) # track: fills W_CALLOUT
ST_HBARS  <- with_std(pitch = 22, bar = 18, track = 300, value_w = 34)
ST_COLS   <- with_std(col_w = 46, col_h = 180)
ST_FUNNEL <- with_std(funnel_label_w = 190, funnel_bar = 40, funnel_gap = 18, funnel_value = 11)
ST_SANKEY <- with_std(sankey_top = 62, sankey_h = 300, sankey_margin = 92, sankey_head = 10.8, sankey_note = 8.75)
ST_CARDS  <- with_std(card_label = 11, card_label_w = 170, card_h = 48, card_value = 16, card_delta = 9)


## Tables the printed values are checked against ---------------------------------------------------------------

tab_pipeline <- function(file) {
  read_csv(tab(file), show_col_types = FALSE) %>% transmute(grouping, category, value = Hungary)
}
tab_additional <- function(file, filter_col = NULL, filter_val = NULL, value = "hungria") {
  d <- read_csv(add(file), show_col_types = FALSE)
  if (!is.null(filter_col)) d <- d %>% filter(.data[[filter_col]] == filter_val)
  d %>% transmute(grouping = if ("grouping" %in% names(d)) grouping else NA_character_, category, value = .data[[value]])
}
SIX <- c("5 or more problems" = "6 or more problems")

# EU averages, as one "EU Average" row
eu_row <- function(value) tibble(grouping = "National", category = "EU Average", value = value)
# data4web (the EU averages worker-viz.R draws; read from path2EU). NULL when it is not reachable: the EU
# rows are then reported as not verified
DATA4WEB <- file.path(path2EU, "reports", "eu-thematic-reports", "data-viz", "output", "data4web_gpp.csv")
eu_data4web <- function(id) {
  if (!file.exists(DATA4WEB)) return(NULL)
  d <- read_csv(DATA4WEB, show_col_types = FALSE) %>%
    filter(id == !!id, demographic == "Total Sample", level == "eu") %>% distinct(value)
  if (nrow(d) != 1) stop("data4web: no hay un único valor UE para ", id, call. = FALSE)
  eu_row(100 * d$value)
}
eu_additional <- function(file, filter_col, filter_val, value = "media_ue") {
  d <- read_csv(add(file), show_col_types = FALSE) %>% filter(.data[[filter_col]] == filter_val)
  if ("grouping" %in% names(d)) d <- d %>% filter(grouping == "National")
  if (nrow(d) != 1) stop(file, ": no hay una única fila para ", filter_val, call. = FALSE)
  eu_row(d[[value]])
}
with_eu <- function(national, eu) if (is.null(eu)) national else bind_rows(national, eu)

# Hardships (Figure 30): national rows of F07.csv, labelled as in the figure
tab_hardships <- function(value) {
  read_csv(add("F07.csv"), show_col_types = FALSE) %>%
    filter(grouping == "National", indicador != "hardship", indicador != "inside_justice_gap_keepdk") %>%
    transmute(category = recode(indicador, hardships_health = "Health issues", hardships_income = "Economic issues",
                                hardships_emotional = "Relationship breakdown", hardships_drugs = "Alcohol/Drug problems"),
              value = .data[[value]])
}
# Legal documentation (Figure 37): HU and EU means of src/hungary/F10.R
tab_section4 <- function(value) {
  read_csv(add("F10_seccion4_media_ue.csv"), show_col_types = FALSE) %>% transmute(category = indicador, value = .data[[value]])
}
grouped_check <- function(file, eu = NULL) list(with_eu(tab_pipeline(file), eu))
national <- function(file, value = "Hungary") {
  read_csv(tab(file), show_col_types = FALSE) %>% filter(grouping == "National") %>% pull(all_of(value))
}
r03 <- function(variable) {
  read_csv(add("R03.csv"), show_col_types = FALSE) %>% filter(variable == !!variable) %>% pull(hungria)
}
f06_perception <- function(dimension) {
  read_csv(add("F06.csv"), show_col_types = FALSE) %>%
    filter(category == "National Average", dimension == !!dimension) %>% pull(perception)
}


## Content of the funnels, sankeys and scorecards (values from the tables charts5.py read) ---------------------

# Funnels: label, value; bars relative to the widest stage
funnel_rows <- function(which) {
  d <- switch(which,
    pathway = tibble(label = c("Had access to information and advice",
                               "Obtained appropriate assistance\nwhen they needed it",
                               "Turned to a dispute resolution\nmechanism when they needed it"),
                     v = c(national("2_1_1_access_to_information.csv"), r03("rep_c_published"), r03("drm_c_published"))),
    capability = tibble(label = c("Reported knowledge of their legal rights", "Had access to information and advice",
                                  "Believe people know where\nto obtain advice"),
                        v = c(national("4_1_knowledge_rights.csv"), national("2_1_1_access_to_information.csv"),
                              f06_perception("Information and advice"))))
  d %>% mutate(value = paste0(round(v), "%"), frac = v / max(v))
}
# Published funnel: same proportions between the bars
same_funnel <- function(rows, published, tol = 0.01) {
  w <- read_svg_items(published)$rects %>% filter(fill == MAIN) %>% arrange(y) %>% pull(w)
  length(w) == nrow(rows) && max(abs(w / max(w) - rows$frac)) < tol
}

# Sankeys: values of R03 rounded to one decimal, as drawn by charts5.py; the outcome of the excluded column
# is the sum of its rounded parts (as published: Figure 16 shows 56 %, R03 gives 55.5 % unrounded parts)
sankey_def <- function(which) {
  r1 <- function(v) round(v, 1)
  if (which == "assistance") {
    c1 <- r1(r03("adv_contacted"))
    v2 <- c(appropriate = r1(r03("adv_s_appropriate")), non_appropriate = r1(r03("adv_s_nonappr")),
            needed_no = r1(r03("adv_s_needed_no")), no_need = r1(r03("adv_s_noneed")), unknown = r1(r03("adv_s_unknown")))
    list(
      C1 = list(contacted = c1, not_contacted = 100 - c1),
      LAB1 = list(contacted = "Contacted an adviser:", not_contacted = "Did not contact an adviser:"),
      C2 = list(appropriate = c(v2[["appropriate"]], "contacted"), non_appropriate = c(v2[["non_appropriate"]], "contacted"),
                needed_no = c(v2[["needed_no"]], "not_contacted"), no_need = c(v2[["no_need"]], "not_contacted"),
                unknown = c(v2[["unknown"]], "not_contacted")),
      LAB2 = list(appropriate = "Appropriate adviser:", non_appropriate = "Non-appropriate adviser:",
                  needed_no = c("Needed but did not have", "access to an adviser:"), no_need = "Did not need an adviser:",
                  unknown = "Unknown reason:"),
      order2 = c("appropriate", "non_appropriate", "needed_no", "no_need", "unknown"),
      M = list(appropriate = "access", non_appropriate = "no_access", needed_no = "no_access", no_need = "excluded", unknown = "excluded"),
      order3 = c("access", "no_access", "excluded"),
      LAB3 = list(access = "Had access:", no_access = "Had no access:", excluded = c("Did not need or", "unknown reason:")),
      VAL3 = list(access = paste0(round(r03("rep_c_published")), "%"), no_access = paste0(100 - round(r03("rep_c_published")), "%"),
                  excluded = paste0(round(v2[["no_need"]] + v2[["unknown"]]), "%")),
      NOTE3 = list(excluded = "of all respondents"),
      headers = list(c("Contacted", "an adviser"), c("Type of adviser or", "reason for no contact"),
                     c("Access among those", "who needed assistance", "(SDG 16.3.3)")))
  } else {
    c1 <- r1(r03("drm_s_turned"))
    v2 <- c(turned = c1, needed_no = r1(r03("drm_s_needed_no")), no_need = r1(r03("drm_s_noneed")), unknown = r1(r03("drm_s_unknown")))
    list(
      C1 = list(contacted = c1, not_contacted = 100 - c1),
      LAB1 = list(contacted = "Contacted a DRM:", not_contacted = "Did not contact a DRM:"),
      C2 = list(turned = c(v2[["turned"]], "contacted"), needed_no = c(v2[["needed_no"]], "not_contacted"),
                no_need = c(v2[["no_need"]], "not_contacted"), unknown = c(v2[["unknown"]], "not_contacted")),
      LAB2 = list(turned = "Turned to a DRM:", needed_no = c("Needed but did not have", "access to a DRM:"),
                  no_need = c("Did not need to", "turn to a DRM:"), unknown = "Unknown reason:"),
      order2 = c("turned", "needed_no", "no_need", "unknown"),
      M = list(turned = "access", needed_no = "no_access", no_need = "excluded", unknown = "excluded"),
      order3 = c("access", "no_access", "excluded"),
      LAB3 = list(access = "Had access:", no_access = "Had no access:", excluded = c("Did not need or", "unknown reason:")),
      VAL3 = list(access = paste0(round(r03("drm_c_published")), "%"), no_access = paste0(100 - round(r03("drm_c_published")), "%"),
                  excluded = paste0(round(v2[["no_need"]] + v2[["unknown"]]), "%")),
      NOTE3 = list(excluded = "of all respondents"),
      headers = list(c("Contacted", "a DRM"), c("Reason for", "no contact"), c("Access among those", "who needed a DRM", "(SDG 16.3.3)")))
  }
}
# Published sankey: same printed values as the definition (the published values are text outlines; they are
# compared with those charts5.py drew, kept here as the record of the published figure)
PUBLISHED_SANKEY <- list(
  assistance = c("29%", "71%", "21%", "8%", "38%", "25%", "8%", "32%", "68%", "33%"),
  drm = c("13%", "87%", "13%", "32%", "47%", "8%", "28%", "72%", "56%")
)
sankey_values <- function(s) {
  c(paste0(round(unlist(s$C1)), "%"), paste0(round(vapply(s$order2, function(k) as.numeric(s$C2[[k]][1]), 0)), "%"), unlist(s$VAL3[s$order3]))
}

# Scorecards: A03 (Figure 32), A04 (41), A05 (42); labels as published
ACTORS <- c("Judges", "Prosecutors", "Public defense attorneys")
scorecard_def <- function(which) {
  switch(which,
    hardship_outcome = {
      a3 <- read_csv(add("A03.csv"), show_col_types = FALSE) %>% filter(corte == "Resolución")
      hs <- c("At least one hardship", "Health issues", "Economic issues", "Relationship breakdown", "Alcohol/Drug problems")
      list(rows = c("At least one hardship", "Health issues", "Economic issues", "Relationship breakdown", "Alcohol or drug problems"),
           cols = c("Fully resolved", "Not resolved"), ref = "col",
           vals = t(sapply(hs, function(h) sapply(c("Fully resolved", "Not resolved"), function(g) a3$pct[a3$desenlace == h & a3$grupo == g]))))
    },
    exposure = {
      a4 <- read_csv(add("A04.csv"), show_col_types = FALSE)
      g <- c("No non-trivial problem", "Problem, did not need a DRM", "Needed a DRM, had access", "Needed a DRM, no access")
      list(rows = c("Without a non-trivial problem", g[-1]), cols = ACTORS, ref = "row",
           vals = t(sapply(g, function(gr) sapply(ACTORS, function(a) a4$pct[a4$actor == a & a4$grupo == gr]))))
    },
    hardship = {
      a5 <- read_csv(add("A05.csv"), show_col_types = FALSE) %>% filter(corte == "Escalera con hardship")
      g <- c("No non-trivial problem", "Problem, no hardship", "Problem, with hardship")
      list(rows = c("Without a non-trivial problem", g[-1]), cols = ACTORS, ref = "row",
           vals = t(sapply(g, function(gr) sapply(ACTORS, function(a) a5$pct[a5$actor == a & a5$grupo == gr]))))
    })
}
# Number of scorecards of the published figure (cards = rounded rects of the colour scale)
published_cards <- function(published) {
  doc <- xml2::read_xml(published)
  r <- xml2::xml_find_all(doc, "//*[local-name()='rect'][@rx and @rx != '0']")
  length(r)
}

# Printed values of a figure drawn from a table, against that table
check_values <- function(labels, printed, data, decimals = 0) {
  tibble(label = labels, grupo = NA_character_, serie = "v1", printed = printed,
         data = formatC(round(data, decimals), format = "f", digits = decimals), bar = NA_real_) %>%
    mutate(estado = if_else(sub("%$", "", printed) == data, "ok", "distinto"), ok = estado == "ok")
}


## Figure plan ---------------------------------------------------------------------------------------------------
# How each figure is standardised and what its printed values are checked against.
#   type  grouped | paired | dumbbell | spec (bars built from a table) | hbars_axis | columns | funnel |
#         sankey | scorecards | rescale (outlined SVG) | outline (SVG with live text) | rerender (render_figure.R)
#   src   source SVG (or table); sources = tables read by the builders (for fuentes.csv)
#   column  label column shared by a group of figures ("std" by default)
#   bars_like / points_like  published SVG (text outlined) whose bars / points must equal those of src
#   base  size of the source text, in its own units (rescale / outline / rerender)
FIGURE_PLAN <- list(
  list(fig = 1,  type = "grouped", src = pub(1),  check = grouped_check("1_2_prevalence_nontrivial_problems.csv", eu_data4web("prevalence2"))),
  list(fig = 2,  type = "paired",  src = pub(2),  check = list(tab_additional("F04.csv", value = "hu"), tab_additional("F04.csv", value = "eu"))),
  list(fig = 3,  type = "outline", src = add("F02_prevalencia_severidad_zona.svg"), base = 12, points_like = pub(3)),
  list(fig = 4,  type = "grouped", src = file.path(PIPE, "Hungary_cooccurence_nproblems.svg"), bars_like = pub(4),
       axis_max = 10, track = FALSE, value_at_end = TRUE, decimals = 1, scale = 10,
       check = grouped_check("1_3_cooccurence_nproblems.csv")),
  list(fig = 5,  type = "rescale", src = pub(5),  base = 10.5),
  list(fig = 6,  type = "rerender", which = "F01", base = 11, width_in = MAX_W_FULL_IN, aspect = 0.9,
       published = add("F01_matriz_coocurrencia_condicional.svg"), sources = c("data/master.csv", "data/regions.csv")),
  list(fig = 7,  type = "outline", src = add("F03_distribucion_severidad_HU_UE.svg"), base = 13),
  list(fig = 8,  type = "funnel", which = "pathway", published = pub(8),
       sources = c(tab("2_1_1_access_to_information.csv"), add("R03.csv"))),
  list(fig = 9,  type = "grouped", src = pub(9),  check = grouped_check("2_1_1_access_to_information.csv", eu_data4web("access2info")), relabel = SIX),
  list(fig = 10, type = "dumbbell", src = pub(10), column = "dumb", st = ST_DUMB, fill_w = W_CALLOUT,
       check = list(tab_additional("F06.csv", "dimension", "Information and advice", "perception"),
                    tab_additional("F06.csv", "dimension", "Information and advice", "experience"))),
  list(fig = 11, type = "sankey", which = "assistance", published_values = PUBLISHED_SANKEY$assistance, sources = add("R03.csv")),
  # EU average of R03 (same definition as the national value), as published
  list(fig = 12, type = "grouped", src = pub(12), relabel = SIX,
       check = grouped_check("2_2_1_access_to_representation.csv", eu_additional("R03.csv", "variable", "rep_c_published"))),
  # Pipeline horizontal bars (gen_horizontal_bars): the report has their text as outlines
  list(fig = 13, type = "hbars_axis", src = pub(13), column = "hbars", st = ST_HBARS,
       labels = c("Lawyer or Professional Adviser", "Friend or Family", "Health/Welfare Professional", "Gvt. Body or Police",
                  "Gvt. Legal Aid", "Trade Union or Employer", "Civil Society Organization", "Religious or Community Leader")),
  list(fig = 14, type = "hbars_axis", src = tab("2_2_5_access2representation_barriers.csv"), published = pub(14), column = "hbars", st = ST_HBARS),
  list(fig = 15, type = "dumbbell", src = pub(15), column = "dumb", st = ST_DUMB, fill_w = W_CALLOUT,
       check = list(tab_additional("F06.csv", "dimension", "Assistance and representation", "perception"),
                    tab_additional("F06.csv", "dimension", "Assistance and representation", "experience"))),
  list(fig = 16, type = "sankey", which = "drm", published_values = PUBLISHED_SANKEY$drm, sources = add("R03.csv")),
  list(fig = 17, type = "grouped", src = pub(17), check = grouped_check("2_3_1_access_to_DRM.csv", eu_data4web("access2drm")), relabel = SIX),
  list(fig = 18, type = "hbars_axis", src = pub(18), column = "hbars", st = ST_HBARS,
       labels = c("Court or Tribunal", "Other", "Police", "Gvt. Office", "Third-party Mediator",
                  "Complaints or Appeal Office", "Rel. or Comm. Leader")),
  list(fig = 19, type = "hbars_axis", src = tab("2_3_5_access2DRM_barriers.csv"), published = pub(19), column = "hbars", st = ST_HBARS),
  # The report kept "5 or more problems" only here (its text was already outlined); the category is
  # nproblems >= 6 (src/data/special-vars.R), labelled "6 or more problems" in every other figure
  list(fig = 20, type = "grouped", src = file.path(PIPE, "Hungary_timeliness_rp.svg"), bars_like = pub(20), label_map = SIX,
       check = grouped_check("2_4_1_timeliness.csv", eu_data4web("rp_time")), relabel = SIX),
  list(fig = 21, type = "grouped", src = pub(21), check = grouped_check("2_4_2_quickness.csv"), relabel = SIX),
  list(fig = 22, type = "grouped", src = pub(22), check = grouped_check("2_4_3_fairness.csv", eu_data4web("rp_fair")), relabel = SIX),
  list(fig = 23, type = "grouped", src = pub(23), check = grouped_check("2_4_4_costliness.csv", eu_data4web("rp_cost")), relabel = SIX),
  list(fig = 24, type = "grouped", src = pub(24), check = grouped_check("2_4_5_expensive.csv"), relabel = SIX),
  list(fig = 25, type = "grouped", src = pub(25), check = grouped_check("2_5_1_outcome.csv", eu_data4web("rp_outcome")), relabel = SIX),
  list(fig = 26, type = "grouped", src = pub(26), check = grouped_check("2_5_2_satisfaction.csv")),
  list(fig = 27, type = "spec", builder = "a01", src = add("A01.csv"), st = ST_A01, fill_w = W_CALLOUT,
       spec_like = add("A01_resolucion_segun_acceso.svg")),
  list(fig = 28, type = "grouped", src = pub(28),
       check = grouped_check("3_1_justice_gap_keepdk.csv", eu_additional("F07.csv", "indicador", "inside_justice_gap_keepdk"))),
  list(fig = 29, type = "rerender", which = "F08", base = 10.24, width_in = MAX_W_CALLOUT_IN, height_in = 5.4,
       published = pub(29), sources = c("data/master.csv", "data/jg_data_keepdk.csv", "data/regions.csv")),
  list(fig = 30, type = "columns", src = pub(30), st = ST_COLS, check = list(tab_hardships("hungria"), tab_hardships("media_ue"))),
  list(fig = 31, type = "grouped", src = pub(31), relabel = SIX,
       check = list(with_eu(tab_additional("F07.csv", "indicador", "hardship"), eu_additional("F07.csv", "indicador", "hardship")))),
  list(fig = 32, type = "scorecards", which = "hardship_outcome", published = pub(32), sources = add("A03.csv")),
  list(fig = 33, type = "grouped", src = pub(33),
       check = grouped_check("4_1_knowledge_rights.csv", eu_additional("F10_seccion4_media_ue.csv", "indicador", "Knowledge of rights (Fig. 26)"))),
  list(fig = 34, type = "funnel", which = "capability", published = pub(34),
       sources = c(tab("4_1_knowledge_rights.csv"), tab("2_1_1_access_to_information.csv"), add("F06.csv"))),
  list(fig = 35, type = "grouped", src = pub(35),
       check = grouped_check("4_3_expert_support.csv", eu_additional("F10_seccion4_media_ue.csv", "indicador", "Expert support (Fig. 27)"))),
  list(fig = 36, type = "hbars_axis", builder = "causes", src = tab("4_2_perceived_causes.csv"), bar_fracs_like = pub(36),
       column = "hbars", st = ST_HBARS),
  # same width as Figure 36, which shares its page
  list(fig = 37, type = "columns", src = pub(37), st = ST_COLS, width_like = "hbars",
       check = list(tab_section4("hungria"), tab_section4("media_ue"))),
  list(fig = 38, type = "grouped", src = file.path(PIPE, "Hungary_trust_judges.svg"), bars_like = pub(38),
       check = grouped_check("5_1_trust_judges.csv", eu_data4web("TRT_judges"))),
  list(fig = 39, type = "grouped", src = file.path(PIPE, "Hungary_trust_prosecutors.svg"), bars_like = pub(39),
       check = grouped_check("5_1_trust_prosecutors.csv", eu_data4web("TRT_prosecutors"))),
  list(fig = 40, type = "grouped", src = file.path(PIPE, "Hungary_trust_pda.svg"), bars_like = pub(40),
       check = grouped_check("5_1_trust_pda.csv", eu_data4web("TRT_pda"))),
  # 41 and 42 go together on the same page: same colour scale
  list(fig = 41, type = "scorecards", which = "exposure", published = pub(41), shared_scale = c("exposure", "hardship"), sources = add("A04.csv")),
  list(fig = 42, type = "scorecards", which = "hardship", published = pub(42), shared_scale = c("exposure", "hardship"), sources = add("A05.csv"))
)


## Content of the bar figures ---------------------------------------------------------------------------------

# Figure 27: resolution by access at each stage (src/hungary/A01.R, rows "Sin control" = no adjustment)
spec_a01 <- function() {
  d <- read_csv(add("A01.csv"), show_col_types = FALSE) %>% filter(estrato == "Sin control")
  pct <- function(v) if (is.na(v)) "N/A" else paste0(round(v), "%")
  map_dfr(seq_len(nrow(d)), function(i) {
    tibble(kind = c("header", "row", "row"), label = c(d$escalon[i], "With access", "Without access"),
           v1_frac = c(NA, d$con[i] / 100, d$sin[i] / 100), v1_text = c(NA, pct(d$con[i]), pct(d$sin[i])))
  }) %>% mutate(order = row_number(), v2_frac = NA_real_, v2_text = NA_character_) %>%
    select(order, kind, label, v1_frac, v1_text, v2_frac, v2_text)
}

# Figure 36: perceived causes (pipeline table 4_2), ordered from the most to the least cited
spec_causes <- function() {
  read_csv(tab("4_2_perceived_causes.csv"), show_col_types = FALSE) %>%
    filter(country_name_ltn == "Hungary") %>%
    arrange(desc(proportion_national)) %>%
    transmute(order = row_number(), kind = "row", label = category, v1_frac = proportion_national / 100,
              v1_text = paste0(round(proportion_national), "%"), v2_frac = NA_real_, v2_text = NA_character_)
}

# Horizontal bars: from the pipeline table (ordered as gen_horizontal_bars does, most cited first) or, when
# there is no table, values from the bars of the published figure and its transcribed labels
spec_hbars <- function(p) {
  if (identical(p$builder, "causes")) return(spec_causes())
  if (grepl("\\.csv$", p$src)) {
    d <- read_csv(p$src, show_col_types = FALSE) %>% filter(country_name_ltn == "Hungary") %>% arrange(desc(proportion_national))
    spec_rows(d$target, d$proportion_national / 100)
  } else {
    fr <- hbar_fracs(p$src)
    if (length(fr) != length(p$labels)) stop("Figura ", p$fig, ": ", length(fr), " barras y ", length(p$labels), " etiquetas", call. = FALSE)
    spec_rows(p$labels, fr)
  }
}

figure_spec <- function(p) {
  spec <- switch(p$type,
    hbars_axis = spec_hbars(p),
    grouped    = parse_grouped(p$src, axis_max = p$axis_max),
    paired     = ,
    columns    = parse_paired(p$src),
    dumbbell   = parse_dumbbell(p$src),
    spec       = switch(p$builder, a01 = spec_a01()),
    NULL)
  if (!is.null(spec) && !is.null(p$label_map)) spec$label <- recode(spec$label, !!!p$label_map)
  spec
}

# Points of the published SVG (outlined text) equal to those of the source SVG (live text)
same_points <- function(src, published) {
  pts <- function(f) read_svg_items(f)$circles %>% arrange(round(cx, 1), round(cy, 1))
  a <- pts(src); b <- pts(published)
  nrow(a) == nrow(b) && nrow(a) > 0 && max(abs(a$cx - b$cx), abs(a$cy - b$cy)) < 0.05 && identical(a$fill, b$fill)
}

# Bars of the published SVG (outlined text) equal to those of the source SVG (live text)
same_bars <- function(src, published) {
  w <- function(f) read_svg_items(f)$rects %>% filter(fill %in% c(MAIN, TRACK)) %>% arrange(y, x) %>% pull(w)
  a <- w(src); b <- w(published)
  length(a) == length(b) && max(abs(a - b)) < 0.05
}


## Sources outside the report ------------------------------------------------------------------------------------
# Files read to build the figures that are not the versioned report SVGs (pipeline SVGs, R outputs, tables).
# Their md5 is recorded in fuentes.csv (versioned) and build_finales.R stops if one changed.
source_files <- function(plan = FIGURE_PLAN) {
  map_dfr(plan, function(p) {
    f <- unique(c(p$src, p$spec_like, p$sources, if (p$type == "rerender" && !startsWith(p$published, FINALES_SVG)) p$published))
    f <- f[!startsWith(f %||% character(), FINALES_SVG)]
    if (length(f) == 0) return(NULL)
    tibble(fig = p$fig, archivo = f)
  })
}
register_sources <- function() {
  s <- source_files() %>% mutate(md5 = unname(tools::md5sum(archivo)))
  write_csv(s, FINALES_SOURCES)
  invisible(s)
}
changed_sources <- function() {
  if (!file.exists(FINALES_SOURCES)) return(tibble(fig = NA_integer_, archivo = FINALES_SOURCES, problema = "no existe"))
  rec <- read_csv(FINALES_SOURCES, show_col_types = FALSE)
  now <- source_files() %>% mutate(md5_now = if_else(file.exists(archivo), unname(tools::md5sum(archivo)), NA_character_))
  full_join(rec, now, by = c("fig", "archivo")) %>%
    filter(is.na(md5) | is.na(md5_now) | md5 != md5_now) %>%
    transmute(fig, archivo, problema = case_when(is.na(md5) ~ "no registrado", is.na(md5_now) ~ "falta o ya no se usa",
                                                 TRUE ~ "cambió"))
}


## SVG helpers ------------------------------------------------------------------------------------------------

svg_viewbox <- function(path) {
  root <- xml2::xml_root(xml2::read_xml(path))
  vb <- xml2::xml_attr(root, "viewBox")
  if (is.na(vb)) vb <- paste(0, 0, sub("pt$", "", xml2::xml_attr(root, "width")), sub("pt$", "", xml2::xml_attr(root, "height")))
  as.numeric(strsplit(trimws(vb), "[ ,]+")[[1]])
}

# Same drawing at a new physical size (inches), with a white background over the whole viewBox.
# pad = c(left, right) widens the viewBox
set_size <- function(svg_in, svg_out, width_in, pad = c(0, 0)) {
  doc <- xml2::read_xml(svg_in)
  root <- xml2::xml_root(doc)
  vb <- svg_viewbox(svg_in) + c(-pad[1], 0, sum(pad), 0)
  xml2::xml_set_attr(root, "viewBox", paste(fmt(vb), collapse = " "))
  xml2::xml_set_attr(root, "width", sprintf("%.3fin", width_in))
  xml2::xml_set_attr(root, "height", sprintf("%.3fin", width_in * vb[4] / vb[3]))
  first <- xml2::xml_child(root, 1)
  bg <- xml2::xml_add_sibling(first, "rect", .where = "before")
  xml2::xml_set_attrs(bg, c(x = fmt(vb[1]), y = fmt(vb[2]), width = fmt(vb[3]), height = fmt(vb[4]), style = "fill:#FFFFFF;stroke:none"))
  xml2::write_xml(doc, svg_out, options = c("format", "no_declaration"))
  c(width_in = width_in, height_in = width_in * vb[4] / vb[3])
}

live_texts <- function(path) {
  doc <- xml2::read_xml(path)
  str_squish(xml2::xml_text(xml2::xml_find_all(doc, "//*[local-name()='text']")))
}

# Width at which the source text measures TEXT_PT, limited by the room on the page
width_for_text <- function(path, base, fig, pad = c(0, 0)) {
  vb <- svg_viewbox(path) + c(-pad[1], 0, sum(pad), 0)
  w <- vb[3] * TEXT_PT / base / 72
  min(w, if (fig %in% CALLOUT_FIGS) MAX_W_CALLOUT_IN else MAX_W_FULL_IN)
}

# Outlines the text of an SVG; returns the number of texts outlined with a doubtful font or width
outline_checked <- function(svg_in, svg_out) {
  log <- outline_svg_text(svg_in, svg_out)
  sum(log$doubtful)
}


## Standardise every figure -----------------------------------------------------------------------------------

METHOD_NAMES <- c(grouped = "barras agrupadas", paired = "barras Hungría/UE", dumbbell = "dumbbell", spec = "barras desde tabla",
                  hbars_axis = "barras horizontales con eje", columns = "columnas Hungría/UE", funnel = "embudo",
                  sankey = "sankey", scorecards = "tarjetas")

# Returns one row per figure (size, effective text size, checks) and writes Figure_NN.svg into out_dir
standardize_figures <- function(out_dir, plan = FIGURE_PLAN) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  specs <- map(plan, figure_spec)
  names(specs) <- map_chr(plan, ~ as.character(.x$fig))
  # label columns: one for the grouped bars and pairs; the dumbbells and the horizontal bars (labels starting
  # at the left) have their own, shared within each group
  column_of <- map_chr(plan, ~ .x$column %||% "std")
  L_col <- c(std = label_column(compact(specs[column_of == "std"])), dumb = label_column(compact(specs[column_of == "dumb"])),
             hbars = ceiling(max(unlist(map(compact(specs[column_of == "hbars"]), ~ sapply(.x$label, text_width, size = STD$size_label)))) + 2))
  card_scale <- range(round(unlist(map(c("exposure", "hardship"), ~ scorecard_def(.x)$vals))))
  # width of the horizontal bars with axis (chart_hbars_axis), for the figures that share their page
  W_hbars <- L_col[["hbars"]] + 12 + ST_HBARS$track + ST_HBARS$value_w + 6
  rscript <- file.path(R.home("bin"), "Rscript")

  rows <- map_dfr(plan, function(p) {
    out <- file.path(out_dir, sprintf("Figure_%02d.svg", p$fig))
    spec <- specs[[as.character(p$fig)]]
    L <- L_col[[p$column %||% "std"]]
    st <- p$st %||% STD
    if (!is.null(p$fill_w)) st$track <- p$fill_w - L - st$gap_label - st$gap_value - st$value_w
    checks <- check_spec(spec, NULL)
    extra_ok <- TRUE; extra_note <- ""; doubtful <- 0L; text_pt <- TEXT_PT; larger_text_ok <- FALSE
    chart <- NULL
    if (p$type %in% c("grouped", "paired", "dumbbell", "spec", "hbars_axis", "columns")) {
      chart <- switch(p$type,
        paired     = chart_paired(spec, L, attr(spec, "legend"), st),
        dumbbell   = chart_dumbbell(spec, L, attr(spec, "legend"), st),
        hbars_axis = chart_hbars_axis(spec, L, st),
        columns    = chart_columns(spec, if (identical(p$width_like, "hbars")) W_hbars else W_CALLOUT, attr(spec, "legend"), st),
        chart_grouped(spec, L, track = p$track %||% TRUE, value_at_end = p$value_at_end %||% FALSE, st = st))
      write_csv(spec, file.path(out_dir, sprintf("Figure_%02d_datos.csv", p$fig)), na = "")
      if (!is.null(p$check)) {
        checks <- imap_dfr(p$check, ~ check_spec(spec, .x, series = paste0("v", .y), decimals = p$decimals %||% 0,
                                                  relabel = p$relabel %||% c(), scale = p$scale %||% 100))
      }
      if (!is.null(p$bars_like)) {
        extra_ok <- same_bars(p$src, p$bars_like)
        extra_note <- paste0("barras iguales a las del SVG del reporte: ", if (extra_ok) "sí" else "NO")
      }
      if (!is.null(p$spec_like)) {
        extra_ok <- same_spec(spec, parse_grouped(p$spec_like))
        extra_note <- paste0("mismo contenido que ", basename(p$spec_like), " (PNG idéntico al del reporte): ", if (extra_ok) "sí" else "NO")
      }
      if (p$type == "hbars_axis" && is.null(p$bar_fracs_like)) {
        if (!is.null(p$published)) {
          extra_ok <- same_hbars(spec, p$published)
          extra_note <- paste0("mismas barras, orden y valores que el SVG del reporte: ", if (extra_ok) "sí" else "NO")
        } else {
          extra_note <- "valores leídos de las barras de la figura del reporte; etiquetas transcritas de ella"
        }
      }
      if (!is.null(p$bar_fracs_like)) {
        extra_ok <- same_bar_fracs(spec, p$bar_fracs_like)
        extra_note <- paste0("mismas barras, orden y valores que el SVG del reporte: ", if (extra_ok) "sí" else "NO")
      }
    } else if (p$type == "funnel") {
      fr <- funnel_rows(p$which)
      chart <- chart_funnel(fr, W_CALLOUT, ST_FUNNEL)
      write_csv(fr, file.path(out_dir, sprintf("Figure_%02d_datos.csv", p$fig)))
      extra_ok <- same_funnel(fr, p$published)
      extra_note <- paste0("mismas proporciones entre etapas que el embudo del reporte: ", if (extra_ok) "sí" else "NO")
    } else if (p$type == "sankey") {
      s <- sankey_def(p$which)
      chart <- chart_sankey(s, MAX_W_FULL_IN * 72, ST_SANKEY)
      vals <- sankey_values(s)
      write_csv(tibble(valor = vals), file.path(out_dir, sprintf("Figure_%02d_datos.csv", p$fig)))
      extra_ok <- identical(unname(vals), p$published_values)
      extra_note <- paste0("mismos valores que el sankey del reporte (R03 redondeado como se publicó): ", if (extra_ok) "sí" else "NO")
    } else if (p$type == "scorecards") {
      d <- scorecard_def(p$which)
      chart <- chart_scorecards(d$rows, d$cols, d$vals, d$ref, W_CALLOUT,
                                lohi = if (!is.null(p$shared_scale)) card_scale else range(round(d$vals)), st = ST_CARDS)
      write_csv(as_tibble(round(d$vals, 2), rownames = "fila"), file.path(out_dir, sprintf("Figure_%02d_datos.csv", p$fig)))
      extra_ok <- published_cards(p$published) == length(d$vals)
      extra_note <- paste0("mismo número de tarjetas que la figura del reporte: ", if (extra_ok) "sí" else "NO")
      larger_text_ok <- TRUE
    }

    if (!is.null(chart)) {
      writeLines(chart$svg, out)
      size <- c(width_in = chart$width_pt / 72, height_in = chart$height_pt / 72)
      text_pt <- chart$text_pt
      metodo <- paste0("redibujada (", METHOD_NAMES[[p$type]], ")")
    } else if (p$type == "rescale") {
      if (length(live_texts(p$src)) > 0) stop("Figura ", p$fig, ": se esperaba texto en trazos", call. = FALSE)
      pad <- c(p$pad_left %||% 0, p$pad_right %||% 0)
      w <- width_for_text(p$src, p$base, p$fig, pad)
      size <- set_size(p$src, out, w, pad)
      text_pt <- p$base * w * 72 / (svg_viewbox(p$src)[3] + sum(pad))
      metodo <- "reescalada (texto ya en trazos)"
    } else if (p$type == "outline") {
      tmp <- tempfile(fileext = ".svg")
      doubtful <- outline_checked(p$src, tmp)
      if (!is.null(p$points_like)) {
        extra_ok <- same_points(p$src, p$points_like)
        extra_note <- paste0("puntos iguales a los del SVG del reporte: ", if (extra_ok) "sí" else "NO")
      }
      w <- width_for_text(p$src, p$base, p$fig)
      size <- set_size(tmp, out, w)
      text_pt <- p$base * w * 72 / svg_viewbox(p$src)[3]
      metodo <- "texto convertido a trazos y reescalada"
    } else if (p$type == "rerender") {
      tmp <- tempfile(fileext = ".svg")
      w <- p$width_in
      h <- p$height_in %||% (w * p$aspect)
      # canvas on which the plot's base text (p$base pt) measures TEXT_PT once declared at width w
      k <- p$base / TEXT_PT
      status <- system2(rscript, c("src/hungary/finales/render_figure.R", p$which, shQuote(tmp), w * k, h * k))
      if (!identical(as.integer(status), 0L) || !file.exists(tmp)) stop("No se pudo re-renderizar la Figura ", p$fig, call. = FALSE)
      extra_ok <- identical(sort(live_texts(tmp)), sort(live_texts(p$published)))
      extra_note <- paste0("mismos textos y valores que la figura del reporte: ", if (extra_ok) "sí" else "NO")
      tmp2 <- tempfile(fileext = ".svg")
      doubtful <- outline_checked(tmp, tmp2)
      size <- set_size(tmp2, out, w)
      text_pt <- p$base / k
      metodo <- paste0("re-renderizada desde ", c(F01 = "src/hungary/F01.R", F08 = "src/hungary/F08.R")[[p$which]],
                       " en un lienzo mayor, texto en trazos")
    }
    fuente <- p$src %||% paste(c(p$sources), collapse = "; ")
    tibble(fig = p$fig, metodo = metodo, fuente = fuente,
           width_in = round(size[["width_in"]], 3), height_in = round(size[["height_in"]], 3),
           text_pt = round(text_pt, 2), larger_text_ok = larger_text_ok, callout = p$fig %in% CALLOUT_FIGS,
           n_printed = sum(checks$estado != "suprimido"), n_checked = sum(checks$estado %in% c("ok", "distinto", "barra")),
           n_check_fail = sum(checks$estado %in% c("distinto", "barra", "ambiguo")),
           n_unverified = sum(checks$estado == "sin dato"), check_detail = list(checks),
           extra_ok = extra_ok, extra_note = extra_note, outline_doubtful = doubtful)
  })
  attr(rows, "label_column_pt") <- L_col
  rows
}
