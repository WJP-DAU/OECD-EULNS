## F09 · La escalera de capacidad legal 85 → 61 → 27 -------------------------------------------------------

run_F09 <- function() {

  steps <- tribble(
    ~orden, ~escalon, ~variable, ~transform, ~poblacion,
    1, "Understand their legal rights", "AJE_legalrights", list(binary_agree), "Respondents who reported a problem (any severity), self-reported",
    2, "Had access to information and advice", "access2info", list(NULL), "Respondents with a non-trivial problem",
    3, "Believe people know where to get advice", "JSE_access2info", list(binary_agree), "General population, about others"
  ) %>%
    mutate(
      est = map2(variable, transform, \(v, t) nat_est(hu, v, t[[1]])),
      hu_pct = map_dbl(est, "pct"), n = map_int(est, "n"),
      eu_pct = map2_dbl(variable, transform, \(v, t) eu_est(eu, v, t[[1]])$eu_pct)
    ) %>%
    select(-est, -transform) %>%
    mutate(brecha_desde_anterior = hu_pct - lag(hu_pct))
  save_table(steps, "F09")

  paired_bars(steps %>% transmute(category = escalon, hu = hu_pct, eu = eu_pct), "F09_escalera_capacidad_legal", w = 290)

  save_note(
    "F09",
    "# F09 · La escalera de capacidad legal",
    "",
    "**Ponderación del reporte.** Cada escalón tiene su propia población (va en el pie de la figura del documento):",
    "",
    md_table(steps),
    "",
    "## Corrección al brief",
    "",
    "El brief y la planilla (S4-05) tratan el 85 % como **población general**. No lo es: `AJE_legalrights` solo se preguntó a quienes seleccionaron un problema, de cualquier severidad.",
    "",
    "1. 85 % — entrevistados que reportaron un problema (cualquier severidad), autodeclarado, sobre sí mismos.",
    "2. 61 % — entrevistados con un problema no trivial, experiencia real.",
    "3. 27 % — población general, sobre los demás.",
    "",
    paste0("Brechas: ", round(abs(steps$brecha_desde_anterior[2])), " puntos (85 → 61) y ", round(abs(steps$brecha_desde_anterior[3])), " puntos (61 → 27)."),
    "",
    "## Encuadre y pie sugeridos",
    "",
    "> Saber que se tienen derechos no es saber qué hacer con ellos. El escalón del medio —la experiencia real— sostiene el argumento porque no depende del sesgo de calificarse por encima de los demás.",
    "",
    "> Note: The first step is self-reported and asked of respondents who reported a problem; the second is actual access among respondents with a non-trivial problem; the third asks the general population about other people.",
    "",
    paste0("Contra la UE: el conocimiento autodeclarado está ", round(steps$hu_pct[1] - steps$eu_pct[1]), " puntos por encima de la media europea, el acceso real está en la media y la percepción sobre los demás ",
           round(steps$eu_pct[3] - steps$hu_pct[3]), " puntos por debajo.")
  )
  invisible(steps)
}
