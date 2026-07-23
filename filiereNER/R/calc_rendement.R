#' Calculer le rendement du Mil (kg/ha)
#'
#' Calcule le rendement des parcelles de Mil en kg par hectare, en utilisant
#' le facteur de conversion des unités locales (s16cq16c). Application d'une
#' winsorisation (P1/P99) pour éliminer les valeurs extrêmes.
#'
#' @param data_parcels Base des parcelles (b5 - s16a_me_ner2021.dta).
#' @param data_crops Base des cultures (b6 - s16c_me_ner2021.dta).
#' @param code_cult Code culture du Mil (défaut = 1).
#'
#' @return Une liste contenant : `df` (les données nettoyées) et `stats` (le résumé statistique).
#' @export
calc_rendement <- function(data_parcels, data_crops, code_cult = 1) {

  library(dplyr)
  library(ggplot2)

  # 1. Uniformisation surface en Ha
  data_parcels <- data_parcels %>%
    mutate(surface_ha = case_when(
      as.numeric(s16aq09b) == 1 ~ as.numeric(s16aq09a),
      as.numeric(s16aq09b) == 2 ~ as.numeric(s16aq09a) / 100,
      TRUE ~ NA_real_
    ))

  # 2. Filtrer parcelles de Mil
  parcelles_mil <- data_crops %>%
    filter(!is.na(vague)) %>%
    filter(as.numeric(s16cq04) == code_cult) %>%
    filter(!is.na(s16cq16a) & !is.na(s16cq16c))

  # 3. Calcul quantité en kg (Quantité locale * conversion)
  parcelles_mil <- parcelles_mil %>%
    mutate(quantite_kg = as.numeric(s16cq16a) * as.numeric(s16cq16c))

  # 4. Jointure avec surface
  parcelles_mil <- parcelles_mil %>%
    left_join(data_parcels %>% select(grappe, menage, s16aq02, s16aq03, surface_ha),
              by = c("grappe" = "grappe", "menage" = "menage",
                     "s16cq02" = "s16aq02", "s16cq03" = "s16aq03")) %>%
    filter(!is.na(surface_ha) & surface_ha > 0)

  # 5. Calcul rendement
  parcelles_mil <- parcelles_mil %>%
    mutate(rendement = quantite_kg / surface_ha) %>%
    filter(as.numeric(s16cq11) != 3 | is.na(s16cq11)) # Exclure les pertes totales

  # 6. Winsorisation
  p1 <- quantile(parcelles_mil$rendement, 0.01, na.rm = TRUE)
  p99 <- quantile(parcelles_mil$rendement, 0.99, na.rm = TRUE)
  parcelles_mil <- parcelles_mil %>%
    filter(rendement >= p1 & rendement <= p99)

  # 7. Statistiques descriptives
  stats <- parcelles_mil %>%
    summarise(
      n_parcelles = n(),
      rendement_moyen = mean(rendement, na.rm = TRUE),
      rendement_median = median(rendement, na.rm = TRUE),
      rendement_sd = sd(rendement, na.rm = TRUE),
      rendement_min = min(rendement, na.rm = TRUE),
      rendement_max = max(rendement, na.rm = TRUE)
    )

  return(list(df = parcelles_mil, stats = stats))
}
