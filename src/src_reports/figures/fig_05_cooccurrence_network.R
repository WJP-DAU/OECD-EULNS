## Figura 05 · Network of Co-occurring Non-Trivial Justice Problems in {country}
## Fuente de datos: cooc_flags(ctx) (03_estimators.R ← F01.R:6-7; = AJP_*_bin recodificado a no trivial, F01.R:46-50)
##   → network_graph.R:35-199 sin ponderar: prevalencia por categoría (12 nodos, text "(40%)"), lift y coocurrencias
##   por pareja; el df lleva todas las parejas (series "sig" = lift > 1.2 y n >= 10, las que se dibujan: 66 de 66 en
##   lo publicado; "no" = solo cuentan para la centralidad de los nodos), layout_with_fr(lift², niter 1000) sobre las
##   parejas con lift > 0, con RNGkind + set.seed(42), pad 0.16, min_dist 0.28, hstretch 1.15. Alto: el del
##   spec es el de lo publicado; el que pide el layout de otro país lo da el log (figures: figNN: de config/<pais>.yml).
##   Contraste: tabs/csv/1_5_lift_matrix_<país>.csv
## Método UE: none
## Código de origen: F01.R:43-52 + network_graph.R:35-199 (datos; sin el dibujo ni el ggsave :200-300) +
##   legacy_python/charts5.py:196-226 network() (diseño)

# origen: network_graph.R:143-199 (layout). g = grafo de las parejas con lift > 0 (vértices en el orden de cats); RNG
# fijado
net_layout <- function(g) {
  layout_fr <- igraph::layout_with_fr(g, weights = igraph::E(g)$weight^2, niter = 1000)
  nodes <- tibble(name = igraph::V(g)$name, x = layout_fr[, 1], y = layout_fr[, 2])
  pad <- 0.16                                                            # reescala a [0, 1] con margen
  nodes <- nodes %>% mutate(x = (x - min(x)) / (max(x) - min(x)) * (1 - 2 * pad) + pad,
                            y = (y - min(y)) / (max(y) - min(y)) * (1 - 2 * pad) + pad)
  min_dist <- 0.28                                                       # separa nodos más cercanos que min_dist
  for (iter in seq_len(100)) {
    moved <- FALSE
    for (i in seq_len(nrow(nodes) - 1)) for (j in seq(i + 1, nrow(nodes))) {
      dx <- nodes$x[j] - nodes$x[i]; dy <- nodes$y[j] - nodes$y[i]; dist <- sqrt(dx^2 + dy^2)
      if (dist < min_dist) {
        if (dist < 1e-6) { dx <- 1e-3; dy <- 0; dist <- 1e-3 }
        overlap <- (min_dist - dist) / 2; ux <- dx / dist; uy <- dy / dist
        nodes$x[j] <- nodes$x[j] + ux * overlap; nodes$y[j] <- nodes$y[j] + uy * overlap
        nodes$x[i] <- nodes$x[i] - ux * overlap; nodes$y[i] <- nodes$y[i] - uy * overlap
        moved <- TRUE
      }
    }
    if (!moved) break
  }
  hstretch <- 1.15                                                       # estira solo x (sin renormalizar)
  nodes %>% mutate(x = 0.5 + (x - 0.5) * hstretch)
}

fig_05 <- list(

  # Spec: tipo y motor de dibujo, tamaño final (in), recuadro, columna de etiquetas y media UE (figures/_template.R)
  spec = list(tipo = "network", metodo = "wjpr", w_in = 6.5, h_in = 5.869, callout = TRUE, label_col = "none",
              metodo_ue = "none",
              nota = paste("12 valores impresos (prevalencia sin ponderar) no contados en los 404; referencia desde",
                           "analisis adicionales/cooccurrence_network.svg (12 circle + 66 line); layout depende de",
                           "igraph 2.3.1 (renv.lock); texto 7.96 pt por tope de ancho; h_in = el de lo publicado: el",
                           "alto que pide el layout de otro país sale en el log (figures: fig05: h_in de",
                           "config/<pais>.yml)")),

  # Nodos (12: label = categoría, value = prevalencia % sin ponderar, text "(40%)", x, y del layout; series "") y
  # todas las parejas (label "A|B" en el orden de edges_all: A antes que B en cats; value = lift; x, y NA; series
  # "sig" si es arista significativa, la que se dibuja, o "no"); sin n.
  data = function(ctx) {
    # origen: network_graph.R:14-29, 44 — cats = unique(category_map): orden de primera aparición en category_map
    cats <- c("Consumer", "Land", "Housing", "Family", "Education", "Accidental", "Employment", "Public Services",
              "Law Enforcement", "Citizenship & ID", "Money & Debt", "Community")
    cons <- cooc_flags(ctx)[cats] %>% mutate(across(everything(), as.integer))     # network_graph.R:53 (as.integer)
    cats <- cats[sapply(cats, function(cat) {                                        # origen: network_graph.R:58-63
      col <- cons[[cat]]; !all(is.na(col)) && var(col, na.rm = TRUE) > 0               # sin varianza: fuera
    })]
    cons <- cons %>% select(all_of(cats))
    n_cats <- length(cats); n_total <- nrow(cons)
    # origen: network_graph.R:72-92 — lift = P(A y B) / (P(A) · P(B)), coocurrencias y prevalencia, sin ponderar
    lift <- matrix(0, n_cats, n_cats, dimnames = list(cats, cats)); cooc <- lift
    for (i in seq_len(n_cats)) for (j in seq_len(n_cats)) {
      both <- sum(cons[[cats[i]]] == 1 & cons[[cats[j]]] == 1, na.rm = TRUE); cooc[i, j] <- both
      p1 <- mean(cons[[cats[i]]], na.rm = TRUE); p2 <- mean(cons[[cats[j]]], na.rm = TRUE)
      lift[i, j] <- if (i == j) 1 else if (p1 > 0 & p2 > 0) (both / n_total) / (p1 * p2) else 0
    }
    prevalence <- sapply(cats, function(k) mean(cons[[k]], na.rm = TRUE) * 100)
    # origen: network_graph.R:98-116 — todas las parejas i < j (centralidad y layout) y las significativas (aristas)
    edges_all <- expand_grid(i = seq_len(n_cats), j = seq_len(n_cats)) %>% filter(i < j) %>%
      transmute(from = cats[i], to = cats[j], weight = lift[cbind(i, j)], cooccurrence = cooc[cbind(i, j)])
    # corregido: el grafo del layout no lleva las parejas de lift 0 (ninguna coocurrencia): layout_with_fr exige pesos
    # positivos y se detenía con «Weights must be positive» en 6 de los 26 países con módulo LNS (Malta, Bulgaria,
    # Grecia, Letonia, Lituania y Portugal); el original (network_graph.R:148-150) falla igual. Una pareja de lift 0
    # no es significativa (no se dibuja) y suma 0 a la centralidad; lo publicado no tiene ninguna: mismo grafo y layout
    # (revisión V1-02, V4-01)
    lay <- edges_all %>% filter(weight > 0)
    if (nrow(lay) < nrow(edges_all)) {
      z <- edges_all %>% filter(!(weight > 0))
      log_msg("fig 05 aviso: ", ctx$country, ": ", nrow(z), " de ", nrow(edges_all), " parejas sin coocurrencias ",
              "(lift 0) fuera del layout: ", paste(z$from, z$to, sep = "|", collapse = ", "))
    }
    if (nrow(lay) == 0) {
      stop("fig 05: ", ctx$country, " sin coocurrencias entre categorías para el layout (las ", nrow(edges_all),
           " parejas con lift 0)", call. = FALSE)
    }
    g <- igraph::graph_from_data_frame(lay, directed = FALSE, vertices = tibble(name = cats, prevalence))
    # origen: network_graph.R:147 — RNG del original; el estado del RNG de la sesión se restaura al salir
    seed0 <- get0(".Random.seed", globalenv(), inherits = FALSE)
    on.exit(if (is.null(seed0)) rm(".Random.seed", envir = globalenv()) else assign(".Random.seed", seed0, globalenv()))
    RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(42)
    xy <- net_layout(g)
    nodes <- tibble(kind = "node", label = cats, value = unname(prevalence),
                    text = paste0("(", fmt_num(prevalence, 0), ")"),             # network_graph.R:228 round(·, 0)
                    x = xy$x[match(cats, xy$name)], y = xy$y[match(cats, xy$name)], series = "")
    # corregido: el df lleva TODAS las parejas con series "sig" (edges_sig, network_graph.R:111-112: lift > 1.2 y
    # n >= 10, las que se dibujan) o "no": la centralidad que rellena los nodos suma los lifts de todas
    # (network_graph.R:116-126, g_full) y con solo las significativas el adaptador la calculaba mal (revisión V1-01).
    # En lo publicado las 66 son "sig": mismas filas que antes más la columna series
    edges <- edges_all %>% transmute(kind = "edge", label = paste(from, to, sep = "|"), value = weight, text = "",
                                     x = NA_real_, y = NA_real_,
                                     series = if_else(weight > 1.2 & cooccurrence >= 10, "sig", "no"))
    bind_rows(nodes, edges) %>%
      transmute(order = row_number(), kind, grouping = "", label, value, value2 = NA_real_, text, text2 = "", x, y,
                series)
  },

  # Dibujo: WJPr::wjp_network() con sus argumentos (wjpr_network, R/09_wjpr.R; revisión 5, WJPr 1.2.0): aristas (las
  # "sig") con ancho y opacidad según el lift, nodos con relleno por centralidad (suma de los lifts de todas las
  # parejas; complemento si alguna no se dibuja) y rótulos nombre + text sobre las coordenadas x, y del df (lienzo =
  # caja de los nodos), al ancho de la spec; aviso en el log si el alto que pide el layout no es el h_in del
  # spec.
  plot = function(df, spec) wjpr_network(df, spec)
)
