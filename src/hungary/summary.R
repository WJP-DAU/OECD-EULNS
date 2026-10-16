## 00-AUDITORIA · inconsistencies between the draft (v3 JUL14) and the data/code ----------------------------

write_audit <- function() {
  r <- function(id, ...) read_csv(file.path(OUT, paste0(id, ".csv")), show_col_types = FALSE, ...)
  r03 <- r("R03"); r04 <- r("R04"); f07 <- r("F07"); f10 <- r("F10"); r02 <- r("R02"); f03 <- r("F03_resumen")
  v <- function(df, col, key, val = "hungria") round(df[[val]][df[[col]] == key], 1)
  n_nr <- nrow(hu %>% filter(!is.na(rp_time), AJR_solvingtime == -8888))
  n_rt <- nrow(hu %>% filter(!is.na(rp_time)))
  rt_answered <- nat_est(hu %>% filter(!(AJR_solvingtime %in% -8888)), "rp_time")$pct
  m8 <- eu %>% filter(!is.na(rp_time)) %>% group_by(country_name_ltn) %>% summarise(p = 100 * mean(AJR_solvingtime %in% -8888), .groups = "drop")
  m8_txt <- paste0(" El código -8888 aparece en ", sum(m8$p > 0), " de ", nrow(m8), " países (", paste0(m8$country_name_ltn[m8$p >= 20], " ", round(m8$p[m8$p >= 20]), " %", collapse = ", "),
                   ") y en ", sum(m8$p == 0), " no aparece nunca: la comparación 45 % contra 74 % UE depende de un código que se usa distinto según el país.")

  audit <- tribble(
    ~id, ~gravedad, ~donde, ~que_dice_el_borrador, ~que_muestran_los_datos, ~estado,
    "AU-01", "Crítica", "Resumen ejecutivo p.5, §2.2, Fig. 8",
    "Asistencia 32 % contra media UE 54 %",
    paste0("El 54 % (data4web) usa una definición en la que 'no necesitó' cuenta como acceso; con esa definición Hungría da 46,6 %. Con la definición de la Fig. 8, la media UE es ", v(r03, "variable", "rep_c_published", "media_ue"), " %."),
    "Documentado (R03). Figura publicada sin cambiar hasta actualizar data4web (decisión del equipo).",
    "AU-02", "Crítica", "Resumen ejecutivo p.8, §2.4.1, recuadro de la Fig. 16",
    "Timeliness 73 % contra UE 72 %; mujeres 75/70; financiera 76/64; 1 problema 81 / 5+ 65",
    "Esos valores son los de la Fig. 21 (resolución). La Fig. 16 muestra correctamente 45 % contra UE 74 %. Hay que corregir el texto y el recuadro.",
    "Corrección de texto (sin cálculo).",
    "AU-03", "Alta", "Fig. 16 (indicador de oportunidad)",
    "45 % finalizados en menos de un año",
    paste0(n_nr, " de ", n_rt, " casos de la base tienen AJR_solvingtime = -8888 (no responde) y cuentan como «más de un año». Entre quienes respondieron la duración, el indicador sería ", round(rt_answered, 1),
           " %. Además, en la brecha de justicia el mismo código se trata distinto (jg_time: NA; jg_inverse_time: satisfecho).", m8_txt),
    "Se mantiene la codificación (decisión del 28 sep).",
    "AU-04", "Crítica", "Fig. 23 y §3.1",
    "48 % presentado como déficit, sin media UE",
    paste0("Media UE con la misma metodología: ", round(r04$media_ue[r04$regla == "Publicada"], 1), " %. Hungría está en la media europea."),
    "Corregido: Hungary_justice_gap_keepdk.svg con media UE.",
    "AU-05", "Alta", "§3.1, resumen ejecutivo p.8",
    "«at least one substantial unmet need»",
    paste0("La regla es a2j_score < 0,67 (promedio de 4 componentes). «Al menos una» daría ", v(r04, "regla", "Al menos una"), " %."),
    "Documentado (R04). Corrección de texto.",
    "AU-06", "Alta", "Fig. 4 y §1.2",
    "Red de problemas no triviales; lifts 11,8 / 5,4 / …",
    "El dibujo usaba problemas de cualquier severidad (nodos 62 %, 49 %…) y el texto lifts no triviales. Los dos lifts «imposibles» eran artefacto de esa mezcla.",
    "Corregido: cooccurrence_network.svg con problemas no triviales (lifts = texto).",
    "AU-07", "Alta", "Figs. 2 y 4, §1.1, resumen ejecutivo",
    "Prevalencia por categoría «of respondents with non-trivial problems»",
    "La Fig. 2 es sobre toda la muestra; la Fig. 4 antigua era cualquier severidad sobre toda la muestra.",
    "Documentado (R01). Corrección de texto.",
    "AU-08", "Alta", "Fig. 27, §4.2, resumen ejecutivo",
    "«Fully satisfied with expert support», 47 % de quienes tuvieron problemas no triviales",
    "AJE_advice es una escala de acuerdo sobre la disponibilidad de ayuda experta (muy de acuerdo + de acuerdo). Se preguntó a quienes seleccionaron un problema de cualquier severidad.",
    "Documentado (F10). Corrección de título y pie.",
    "AU-09", "Media", "Fig. 26, S4-05 (F09)",
    "Conocimiento de derechos 85 % (población general)",
    "Solo se preguntó a quienes seleccionaron un problema (cualquier severidad). La escalera F09 rotula esa población.",
    "Corregido en F09. Corrección de pie en la Fig. 26.",
    "AU-10", "Media", "Figs. 7 y 12, resumen ejecutivo p.7",
    "Bases distintas por columna; DRM 13/36/54/10 suma 113 %",
    paste0("Sobre una sola base: ", v(r03, "variable", "drm_s_turned"), " / ", v(r03, "variable", "drm_s_needed_no"), " / ", v(r03, "variable", "drm_s_noneed"), " / ", v(r03, "variable", "drm_s_unknown"), " %."),
    "Documentado (R03).",
    "AU-11", "Media", "p.22 (definición de asistencia)",
    "Incluye a quienes no necesitaron asistencia",
    "La Fig. 8 los excluye (variante c de R03).",
    "Corrección de texto.",
    "AU-12", "Media", "Fig. 29",
    "«General population» para las tres documentaciones",
    "El contrato escrito solo aplica a quienes trabajan, y el código (vulnerability3) cuenta como «con contrato» a empleados públicos sin respuesta sobre el tipo de contrato.",
    "Nota en Hungary_legal_empowerment.svg. La regla de vulnerability3 queda para revisión del equipo.",
    "AU-13", "Baja", "Notas de las Figs. 30-32",
    "«26 out of 27 EU Member States»",
    "La media UE de confianza incluye a Irlanda (27 países): es pregunta del sondeo general, no del módulo GLNS.",
    "Corrección de nota.",
    "AU-14", "Baja", "Fig. 10, §2.2 y resumen ejecutivo",
    "Lista de barreras sin «Other»",
    "«Other» es 17 %, empatada en segundo lugar.",
    "Corrección de texto.",
    "AU-15", "Baja", "Pipeline: eu_main_indicators.R",
    "(no aparece en el borrador)",
    "Las medias UE por categoría de problema y de causas percibidas promedian 27 países contando a Irlanda (sin módulo LNS) como 0 %. En el código nuevo se excluye.",
    "Pendiente en el pipeline (no se modificó).",
    "AU-16", "Baja", "Pipeline: --eu_main_indicators",
    "(no aparece en el borrador)",
    "La rutina reescribe data/jg_data_*.csv con los 27 países (efecto secundario de add_a2j_vars). No altera resultados porque el análisis une por ID, pero cambia el archivo.",
    "Pendiente en el pipeline. El código nuevo respalda y restaura esos archivos.",
    "AU-17", "Informativa", "Objeción del umbral (S1-06b)",
    "(el borrador no la aborda)",
    paste0("La brecha Hungría − UE se mantiene con umbral 6 y 8 (", paste(round(r02$brecha_hu_menos_ue, 1), collapse = " / "), " pts) y la severidad media húngara es MENOR que la europea (",
           round(f03$severidad_media[1], 2), " contra ", round(f03$severidad_media[2], 2), "). El 68 % no es efecto de anclaje."),
    "Documentado (R02, F03).",
    "AU-18", "Informativa", "Figs. 30-32",
    "Confianza «a lot or some»",
    paste0("La parte «mucha» es pequeña: jueces ", round(f10$hu_a_lot[1], 1), " de ", round(f10$hu_total[1], 1), " puntos (UE ", round(f10$eu_a_lot[1], 1), " de ", round(f10$eu_total[1], 1), ")."),
    "Corregido: figuras de confianza con «mucha» y «algo» separadas.",
    "AU-19", "Baja", "§1.2",
    "Lift Citizenship & ID ↔ Land = 4,0",
    "El valor es 3,95 y redondea a 3,9 (F01_lifts_texto.csv).",
    "Corrección de texto."
  )
  write_csv(audit, file.path(OUT, "00-AUDITORIA.csv"))
  save_note(
    "00-AUDITORIA",
    "# Auditoría del borrador «Paths to Justice in Hungary» (v3 JUL14) contra los datos y el código",
    "",
    "Complementa la planilla `correcciones-reporte-hungria.xlsx`. Cada fila indica qué dice el borrador, qué muestran los datos y si quedó corregido en esta carpeta, documentado en una nota o pendiente.",
    "",
    md_table(audit)
  )
}


## 00-RESUMEN · index of every output with its type ---------------------------------------------------------

write_summary <- function() {

  corrections <- c(
    "cooccurrence_network.svg" = "Figura 4 — red de co-ocurrencia con problemas NO triviales (lifts = texto de §1.2)",
    "Hungary_prevalence_nontrivial_problems_by_category.svg" = "Figura 2 — con media UE por categoría",
    "Hungary_access2information_dumbbell.svg" = "Figura 6 — series nombradas por población + brecha",
    "Hungary_access2representation_dumbbell.svg" = "Figura 11 — series nombradas por población + brecha",
    "Hungary_justice_gap_keepdk.svg" = "Figura 23 — con media UE",
    "Hungary_jg_upset.svg" = "Figura 24 — legible, valores completos y nota de lectura",
    "Hungary_hardships.svg" = "Figura 25 — con media UE",
    "Hungary_knowledge_rights.svg" = "Figura 26 — con media UE",
    "Hungary_expert_support.svg" = "Figura 27 — con media UE",
    "Hungary_perceived_causes.svg" = "Figura 28 — con media UE y nota de respuesta múltiple",
    "Hungary_legal_empowerment.svg" = "Figura 29 — con media UE",
    "Hungary_trust_judges.svg" = "Figura 30 — «mucha» y «algo» separadas, media UE desglosada",
    "Hungary_trust_prosecutors.svg" = "Figura 31 — «mucha» y «algo» separadas, media UE desglosada",
    "Hungary_trust_pda.svg" = "Figura 32 — «mucha» y «algo» separadas, media UE desglosada"
  )

  files <- sort(list.files(OUT))
  files <- files[!str_starts(files, "00-")]
  idx <- tibble(archivo = files) %>%
    mutate(
      tipo = case_when(
        archivo %in% names(corrections) ~ "Corrección (mismo nombre que el original)",
        str_detect(archivo, "\\.(svg|png)$") & archivo %in% paste0(tools::file_path_sans_ext(names(corrections)), ".png") ~ "Corrección (versión png)",
        str_detect(archivo, "\\.(svg|png)$") ~ "Adicional · figura",
        str_detect(archivo, "\\.md$") ~ "Nota (denominador, n y lectura)",
        TRUE ~ "Adicional · tabla"
      ),
      brief = str_extract(archivo, "^[RFA][0-9]{2}"),
      descripcion = unname(corrections[archivo])
    )
  write_csv(idx, file.path(OUT, "00-RESUMEN.csv"), na = "")

  save_note(
    "00-RESUMEN",
    "# Análisis adicionales y correcciones · Hungría",
    "",
    paste0("Generado con `Rscript src/hungary/worker-hungary.R` el ", format(Sys.Date(), "%Y-%m-%d"), ". El código existente no se modificó (salvo la ruta de `config.R`); todo se construye en `src/hungary/` reutilizando sus funciones."),
    "",
    "## Convenciones",
    "",
    "- **Correcciones** llevan el mismo nombre de archivo que la figura original de `output/viz/Hungary/`, que no se toca.",
    "- **Adicionales** llevan el ID del brief (`R0x`, `F0x`, `A0x`).",
    "- Cada brief tiene su `.md` con la población, el denominador, los `n` y la lectura.",
    "- Ponderación: la del reporte en todas las cifras (promedio por región NUTS, regiones con n < 30 suprimidas → N/A, ponderado por población regional); `n` sin ponderar. Lifts y red de co-ocurrencia sin ponderar, como en el reporte.",
    "- Figuras: formato del reporte, sin títulos ni notas (van en el documento); la metodología de cada una está en su `.md`.",
    "- Medias UE nuevas: promedio simple de los promedios nacionales, con la misma definición y control de calidad que Hungría (26 países con módulo LNS; 27 en preguntas del sondeo general). Las figuras publicadas que ya tenían media UE conservan la de `data4web` (pendiente de actualizar por el equipo).",
    "",
    "## Correcciones de figuras",
    "",
    md_table(tibble(archivo = names(corrections), cambio = unname(corrections))),
    "",
    "## Todos los archivos",
    "",
    md_table(idx %>% select(archivo, tipo, brief))
  )
}
