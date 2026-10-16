## R03 · Denominadores del recorrido: asistencia y DRM ----------------------------------------------------

# Base común: quienes tuvieron al menos un problema no trivial y tienen etapa del recorrido definida.
journey_vars <- function(df) {
  df %>%
    mutate(
      adv = sankey_advice_stage_2,
      drm = sankey_drm_stage_1,
      # Asistencia
      adv_contacted       = if_else(!is.na(adv), as.numeric(sankey_advice_stage_1 == "Contacted an advisor"), NA_real_),
      adv_s_appropriate   = if_else(!is.na(adv), as.numeric(adv == "Appropriate Advisor"), NA_real_),
      adv_s_nonappr       = if_else(!is.na(adv), as.numeric(adv == "Non-Appropriate Advisor"), NA_real_),
      adv_s_needed_no     = if_else(!is.na(adv), as.numeric(adv == "Needed but did not have access to an advisor"), NA_real_),
      adv_s_noneed        = if_else(!is.na(adv), as.numeric(adv == "Did not need an advisor"), NA_real_),
      adv_s_unknown       = if_else(!is.na(adv), as.numeric(adv == "Unknown Reason"), NA_real_),
      # Variantes del indicador de acceso a asistencia
      rep_a_all        = adv_s_appropriate,                                                    # (a) sobre todos
      rep_b_need_unk   = if_else(adv %in% c("Did not need an advisor"), NA_real_, adv_s_appropriate),      # (b) excluye no necesitó
      rep_c_published  = if_else(adv %in% c("Did not need an advisor", "Unknown Reason"), NA_real_, adv_s_appropriate), # (c) excluye no necesitó y desconocida = publicado
      rep_d_textdef    = case_when(adv == "Unknown Reason" ~ NA_real_,
                                   adv %in% c("Appropriate Advisor", "Did not need an advisor") ~ 1,
                                   !is.na(adv) ~ 0),                                           # (d) definición del texto p.22
      # DRM
      drm_s_turned     = if_else(!is.na(drm), as.numeric(drm == "Turned to a DRM"), NA_real_),
      drm_s_needed_no  = if_else(!is.na(drm), as.numeric(drm == "Needed but did not have access to a DRM"), NA_real_),
      drm_s_noneed     = if_else(!is.na(drm), as.numeric(drm == "Did not need to turn to a DRM"), NA_real_),
      drm_s_unknown    = if_else(!is.na(drm), as.numeric(drm == "Unknown Reason"), NA_real_),
      drm_nc_needed_no = if_else(drm %in% c("Turned to a DRM") | is.na(drm), NA_real_, drm_s_needed_no),
      drm_nc_noneed    = if_else(drm %in% c("Turned to a DRM") | is.na(drm), NA_real_, drm_s_noneed),
      drm_nc_unknown   = if_else(drm %in% c("Turned to a DRM") | is.na(drm), NA_real_, drm_s_unknown),
      drm_a_all        = drm_s_turned,
      drm_b_need_unk   = if_else(drm %in% c("Did not need to turn to a DRM"), NA_real_, drm_s_turned),
      drm_c_published  = if_else(drm %in% c("Did not need to turn to a DRM", "Unknown Reason"), NA_real_, drm_s_turned),
      drm_d_textdef    = case_when(drm == "Unknown Reason" ~ NA_real_,
                                   drm %in% c("Turned to a DRM", "Did not need to turn to a DRM") ~ 1,
                                   !is.na(drm) ~ 0)
    )
}

run_R03 <- function() {

  hj <- journey_vars(hu)
  ej <- journey_vars(eu)

  rows <- tribble(
    ~bloque,        ~variable,          ~etiqueta,                                              ~base,
    "Asistencia · etapa 1", "adv_contacted",     "Contactó a un asesor",                         "Con problema no trivial",
    "Asistencia · etapa 2", "adv_s_appropriate", "Asesor apropiado",                             "Con problema no trivial",
    "Asistencia · etapa 2", "adv_s_nonappr",     "Asesor no apropiado",                          "Con problema no trivial",
    "Asistencia · etapa 2", "adv_s_needed_no",   "Necesitó y no tuvo acceso",                    "Con problema no trivial",
    "Asistencia · etapa 2", "adv_s_noneed",      "No necesitó asesor",                           "Con problema no trivial",
    "Asistencia · etapa 2", "adv_s_unknown",     "Razón desconocida",                            "Con problema no trivial",
    "Acceso a asistencia",  "rep_a_all",         "(a) Asesor apropiado / todos",                 "Con problema no trivial",
    "Acceso a asistencia",  "rep_b_need_unk",    "(b) Excluye 'no necesitó'",                    "Necesitaban (incluye razón desconocida)",
    "Acceso a asistencia",  "rep_c_published",   "(c) Excluye 'no necesitó' y 'desconocida' = PUBLICADO (Fig. 8)", "Necesitaban, razón conocida",
    "Acceso a asistencia",  "rep_d_textdef",     "(d) 'No necesitó' cuenta como acceso = definición del texto p.22", "Todos salvo razón desconocida",
    "DRM · etapa 1",        "drm_s_turned",      "Acudió a un DRM",                              "Con problema no trivial",
    "DRM · etapa 1 (total)", "drm_s_needed_no",  "Necesitó y no tuvo acceso",                    "Con problema no trivial",
    "DRM · etapa 1 (total)", "drm_s_noneed",     "No necesitó un DRM",                           "Con problema no trivial",
    "DRM · etapa 1 (total)", "drm_s_unknown",    "Razón desconocida",                            "Con problema no trivial",
    "DRM · etapa 2 (Fig. 12)", "drm_nc_needed_no", "Necesitó y no tuvo acceso",                  "No contactaron un DRM",
    "DRM · etapa 2 (Fig. 12)", "drm_nc_noneed",   "No necesitó un DRM",                          "No contactaron un DRM",
    "DRM · etapa 2 (Fig. 12)", "drm_nc_unknown",  "Razón desconocida",                           "No contactaron un DRM",
    "Acceso a DRM",         "drm_a_all",         "(a) Acudió / todos",                           "Con problema no trivial",
    "Acceso a DRM",         "drm_b_need_unk",    "(b) Excluye 'no necesitó'",                    "Necesitaban (incluye razón desconocida)",
    "Acceso a DRM",         "drm_c_published",   "(c) Excluye 'no necesitó' y 'desconocida' = PUBLICADO (Fig. 13)", "Necesitaban, razón conocida",
    "Acceso a DRM",         "drm_d_textdef",     "(d) 'No necesitó' cuenta como acceso",         "Todos salvo razón desconocida"
  )

  tbl <- rows %>%
    mutate(
      est = map(variable, \(v) nat_est(hj, v)),
      hungria = map_dbl(est, "pct"),
      n = map_int(est, "n"),
      media_ue = map_dbl(variable, \(v) eu_est(ej, v)$eu_pct)
    ) %>%
    select(-est)

  save_table(tbl, "R03")

  g <- function(v, col = "hungria") round(tbl[[col]][tbl$variable == v], 1)
  save_note(
    "R03",
    "# R03 · Denominadores del recorrido: asistencia y DRM",
    "",
    "**Población:** entrevistados con al menos un problema no trivial y etapa del recorrido definida (variables `sankey_*` de `special-vars.R`). Hungría: ponderación NUTS. UE: promedio de los promedios nacionales ponderados, misma definición.",
    "",
    md_table(tbl),
    "",
    "## Reconciliación",
    "",
    paste0("- **Figura 7:** las columnas 1 y 2 están sobre todos los que tuvieron un problema no trivial (", g("adv_contacted"), " / ", g("adv_s_appropriate"), " / ", g("adv_s_nonappr"), " / ", g("adv_s_needed_no"), " / ", g("adv_s_noneed"), " / ", g("adv_s_unknown"), "). ",
           "La columna 3 (32 %) está sobre una base distinta: excluye a quienes no necesitaron asesor y a los de razón desconocida → variante (c) = ", g("rep_c_published"), " %."),
    paste0("- **Figura 12:** el 13 % está sobre el total (", g("drm_s_turned"), " %); el 36/54/10 está sobre quienes **no** contactaron; el 28 % excluye a quienes no necesitaron y a los de razón desconocida → variante (c) = ", g("drm_c_published"), " %."),
    paste0("- **Resumen ejecutivo (suma 113 %):** sobre una sola base (todos con problema no trivial) las cuatro cifras de DRM son ",
           g("drm_s_turned"), " / ", g("drm_s_needed_no"), " / ", g("drm_s_noneed"), " / ", g("drm_s_unknown"), " = 100 %."),
    "- **Página 22:** la definición escrita («It also includes respondents who did not seek assistance because they did not need it») corresponde a la variante (d), no a la figura.",
    paste0("- **Bases no declaradas en el borrador:** quienes no saben si acudieron a un DRM (`AJR_resolution = 98`, n = ", sum(hu$non_trivial_problem %in% 1 & hu$AJR_resolution %in% 98), ") no tienen etapa de DRM y quedan fuera de todas las bases de DRM; en asistencia, «no sabe» como razón de no contacto (`AJD_noadvice_reason = 98`) se cuenta como «necesitó y no tuvo acceso»."),
    "",
    "## Comparabilidad con la media UE publicada (importante)",
    "",
    paste0("La media UE publicada de asistencia (54,2 %) sale de `data4web_gpp.csv`, que usa una definición anterior en la que 'no necesitó' cuenta como acceso (cercana a la variante d): con ella Hungría da 46,6 %, no 32 %. ",
           "Con la definición de la Figura 8 (variante c) la media UE es **", g("rep_c_published", "media_ue"), " %**. La comparación publicada «32 % contra 54 %» mezcla dos definiciones. ",
           "Por decisión del equipo, las figuras publicadas se mantienen con data4web mientras se actualiza; esta tabla deja las dos cifras comparables."),
    "",
    "## Recomendación",
    "",
    "- Para el **embudo (F05)** y cualquier comparación entre escalones: variante (a), todo sobre quienes tuvieron un problema no trivial — es la única en la que los escalones se pueden restar.",
    "- Para el **indicador de acceso** (Figs. 8 y 13, SDG 16.3.3): variante (c), declarando en el pie que excluye a quienes no lo necesitaron y a los de razón desconocida, y comparándola con la media UE de la misma variante."
  )
  invisible(tbl)
}
