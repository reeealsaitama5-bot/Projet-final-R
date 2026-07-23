#' Régression d'impact de la filière sur la sécurité alimentaire
#'
#' Estime l'impact du statut de producteur de Mil sur le score FIES en utilisant
#' une méthode des variables instrumentales (2SLS) pour corriger l'endogénéité,
#' ainsi que des modèles de robustesse (HDDS) et des interactions spatiales.
#'
#' @param data_fies Base FIES.
#' @param data_hdds Base HDDS (produit par calc_hdds()).
#' @param data_ventes Base S16D (ventes et revenus).
#' @param data_prod Base S16C (producteur).
#' @param data_welfare Base des contrôles (taille, région, éducation, etc.).
#'
#' @return Une liste contenant les modèles `ols`, `iv`, `hdds_model`.
#' @export
reg_filiere <- function(data_fies, data_hdds, data_ventes, data_prod, data_welfare) {

  library(dplyr)
  library(haven)

  # 1. Construction de la base de régression
  base_reg <- data_fies %>%
    select(grappe, menage, score_fies) %>%
    left_join(data_hdds, by = c("grappe", "menage")) %>%
    left_join(data_ventes %>%
                filter(as.numeric(s16dq01) == 1) %>%
                group_by(grappe, menage) %>%
                summarise(log_revenu = log(sum(as.numeric(s16dq06), na.rm = TRUE) + 1), .groups = "drop"),
              by = c("grappe", "menage")) %>%
    left_join(data_prod %>%
                filter(as.numeric(s16cq04) == 1) %>%
                distinct(grappe, menage) %>% mutate(producteur = 1L),
              by = c("grappe", "menage")) %>%
    left_join(data_welfare %>% select(grappe, menage, hhweight, hhsize, region, milieu, hgender, heduc),
              by = c("grappe", "menage")) %>%
    mutate(across(c(producteur, hhsize, hgender), ~ replace_na(., 0L)))

  # 2. Modèle OLS
  ols_model <- lm(score_fies ~ producteur + log_revenu + hhsize + as.factor(region),
                  data = base_reg, weights = hhweight)

  # 3. Modèle IV (Instrument : Milieu rural)
  # La variable instrumentale utilisée ici est le milieu rural (exogène au FIES individuel)
  iv_model <- ivreg::ivreg(score_fies ~ producteur + log_revenu + hhsize + as.factor(region) |
                             milieu + log_revenu + hhsize + as.factor(region),
                           data = base_reg, weights = hhweight)

  # 4. Modèle HDDS (Robustesse)
  hdds_model <- lm(score_hdds ~ producteur + log_revenu + hhsize + as.factor(region),
                   data = base_reg, weights = hhweight)

  return(list(ols = ols_model, iv = iv_model, hdds = hdds_model))
}
