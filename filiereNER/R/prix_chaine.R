#' Analyse de la chaîne des prix et des marges commerciales
#'
#' Calcule le prix producteur, le prix du marché communautaire, la marge
#' commerciale, et effectue une régression des déterminants du prix producteur
#' (distance au marché, présence de coopérative).
#'
#' @param data_ventes Base des ventes (b7 - s16d_me_ner2021.dta).
#' @param data_prix_marche Base des prix communautaires (QC-S5).
#' @param data_qc_s2 Base communautaire S2 (distances).
#' @param data_qc_s3 Base communautaire S3 (coopératives).
#'
#' @return Une liste contenant `marge_region` (tableau) et `model` (objet de régression).
#' @export
prix_chaine <- function(data_ventes, data_prix_marche, data_qc_s2, data_qc_s3) {

  library(dplyr)
  library(haven)

  # 1. Nettoyage des ventes et calcul prix producteur
  ventes_clean <- data_ventes %>%
    mutate(
      qte = as.numeric(s16dq05c),
      montant = as.numeric(s16dq06),
      prix_prod = montant / qte,
      code_cult = as.numeric(s16dq01)
    ) %>%
    filter(!is.na(prix_prod) & prix_prod > 0 & qte > 0)

  # 2. Nettoyage prix marché communautaire (par région)
  prix_marche <- data_prix_marche %>%
    mutate(
      code_prod = as.numeric(produit__id),
      region = as.character(as_factor(region)),
      prix_march = case_when(
        as.numeric(unite) == 2 ~ as.numeric(prix1) / as.numeric(quantite1),
        as.numeric(unite) == 100 ~ as.numeric(prix1) / (as.numeric(quantite1) / 1000),
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(prix_march) & prix_march > 0) %>%
    group_by(code_prod, region) %>%
    summarise(prix_march_moy = mean(prix_march, na.rm = TRUE), .groups = "drop")

  # 3. Calcul marge pour le Mil (code produit 1 en S16D, 7 en marché)
  # Note : Les codes peuvent varier. Cette fonction suppose un mapping externe.
  # Ici on simule un rapprochement par région pour le Mil.
  marge_mil <- ventes_clean %>%
    filter(code_cult == 1) %>%
    group_by(region) %>%
    summarise(prix_prod_moy = mean(prix_prod, na.rm = TRUE), .groups = "drop") %>%
    left_join(prix_marche %>% filter(code_prod == 7), by = "region") %>%
    mutate(marge = prix_march_moy - prix_prod_moy) %>%
    arrange(desc(marge))

  # 4. Régression prix producteur
  # Variables communautaires (Distance et Coopérative)
  dist_marche <- data_qc_s2 %>%
    filter(as.numeric(s02q00) %in% c(14, 15)) %>%
    group_by(grappe) %>%
    summarise(temps_marche = min(as.numeric(s02q03), na.rm = TRUE), .groups = "drop")

  coop <- data_qc_s3 %>%
    mutate(coop_dummy = if_else(as.numeric(s03q03) == 1, 1L, 0L)) %>%
    select(grappe, coop_dummy)

  reg_data <- ventes_clean %>%
    left_join(dist_marche, by = "grappe") %>%
    left_join(coop, by = "grappe") %>%
    filter(!is.na(temps_marche) & !is.na(coop_dummy))

  model <- lm(log(prix_prod) ~ log(temps_marche + 1) + coop_dummy + log(qte),
              data = reg_data)

  return(list(marge_region = marge_mil, regression_model = model))
}
