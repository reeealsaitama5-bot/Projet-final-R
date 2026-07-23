#' Profilage des ménages (Typologie 4 groupes et indicateurs)
#'
#' Cette fonction construit la typologie Producteur/Consommateur pour le produit
#' stratégique, fusionne les données démographiques (S01), de consommation (S7B),
#' de production (S16C) et de sécurité alimentaire (FIES) pour créer un tableau
#' de profilage complet.
#'
#' @param data_conso La base de consommation (b4 - s07b_me_ner2021.dta).
#' @param data_prod La base de production (b6 - s16c_me_ner2021.dta).
#' @param data_welfare La base de pondération et milieu (b2 - ehcvm_conso_ner2021.dta).
#' @param data_demo La base démographique (s01_me_ner2021.dta).
#' @param data_fies La base FIES (s08a_me_ner2021.dta).
#' @param code_prod Le code produit du Mil dans la base consommation (défaut = 7).
#' @param code_cult Le code culture du Mil dans la base production (défaut = 1).
#'
#' @return Un dataframe contenant les statistiques agrégées par groupe typologique.
#' @export
profil_menage <- function(data_conso, data_prod, data_welfare, data_demo, data_fies,
                          code_prod = 7, code_cult = 1) {

  library(dplyr)
  library(tidyr)

  # 1. Ménages CONSOMMATEURS
  consommateurs <- data_conso %>%
    filter(as.numeric(s07bq01) == code_prod & as.numeric(s07bq02) == 1) %>%
    distinct(grappe, menage) %>% mutate(est_consommateur = 1L)

  # 2. Ménages PRODUCTEURS (vague valide)
  data_prod_clean <- data_prod %>% filter(!is.na(vague))
  producteurs <- data_prod_clean %>%
    filter(as.numeric(s16cq04) == code_cult) %>%
    distinct(grappe, menage) %>% mutate(est_producteur = 1L)

  # 3. Base unifiée
  base_menages <- data_welfare %>%
    mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
    distinct(grappe, menage, milieu, region, hhweight) %>%
    left_join(consommateurs, by = c("grappe", "menage")) %>%
    left_join(producteurs, by = c("grappe", "menage")) %>%
    replace_na(list(est_consommateur = 0L, est_producteur = 0L)) %>%
    mutate(
      groupe_typologie = factor(case_when(
        est_producteur == 1 & est_consommateur == 1 ~ "Producteur-Consommateur",
        est_producteur == 1 & est_consommateur == 0 ~ "Producteur uniquement",
        est_producteur == 0 & est_consommateur == 1 ~ "Consommateur uniquement",
        TRUE ~ "Ni producteur ni consommateur"
      ),
      levels = c("Producteur-Consommateur", "Producteur uniquement",
                 "Consommateur uniquement", "Ni producteur ni consommateur"))
    )

  # 4. Indicateurs démographiques (taille, âge, sexe chef)
  membres_presents <- data_demo %>%
    filter(as.numeric(s01q11) == 1) %>%
    mutate(grappe = as.numeric(grappe), menage = as.numeric(menage))

  taille_hh <- membres_presents %>% group_by(grappe, menage) %>% summarise(taille = n(), .groups = "drop")
  chef_hh <- membres_presents %>%
    filter(as.numeric(s01q02) == 1) %>%
    select(grappe, menage, sexe = s01q01, age = s01q04a) %>%
    mutate(across(c(sexe, age), as.numeric))

  # 5. Score FIES
  # Fonction convert_fies pour convertir "Oui/Non" ou 1/2 en 0/1
  convert_fies <- function(col) {
    vals <- suppressWarnings(as.numeric(col))
    if (!all(is.na(vals))) return(ifelse(vals == 1, 1, 0))
    else return(ifelse(col == "Oui" | col == "1", 1, 0))
  }
  fies_cols <- c("s08aq01", "s08aq02", "s08aq03", "s08aq04",
                 "s08aq05", "s08aq06", "s08aq07", "s08aq08")
  fies_data <- data_fies %>%
    mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
    select(grappe, menage, all_of(fies_cols)) %>%
    mutate(across(all_of(fies_cols), convert_fies)) %>%
    rowwise() %>%
    mutate(score_fies = sum(c_across(all_of(fies_cols)), na.rm = TRUE)) %>%
    ungroup() %>%
    select(grappe, menage, score_fies)

  # 6. Fusion finale
  base_analyse <- base_menages %>%
    left_join(taille_hh, by = c("grappe", "menage")) %>%
    left_join(chef_hh, by = c("grappe", "menage")) %>%
    left_join(fies_data, by = c("grappe", "menage")) %>%
    mutate(across(c(taille, age, sexe, score_fies), ~ ifelse(is.na(.), 0, .)))

  # 7. Moyennes pondérées par groupe
  stats_groupes <- base_analyse %>%
    group_by(groupe_typologie) %>%
    summarise(
      n_menages = n(),
      taille_moy = weighted.mean(taille, w = hhweight, na.rm = TRUE),
      age_moy = weighted.mean(age, w = hhweight, na.rm = TRUE),
      prop_femme = weighted.mean(sexe == 2, w = hhweight, na.rm = TRUE),
      prop_urbain = weighted.mean(as.numeric(milieu) == 1, w = hhweight, na.rm = TRUE),
      fies_moy = weighted.mean(score_fies, w = hhweight, na.rm = TRUE),
      .groups = "drop"
    )

  return(stats_groupes)
}
