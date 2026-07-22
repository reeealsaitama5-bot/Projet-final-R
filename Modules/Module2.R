# =========================================================================
# MODULE 2 : Profilage des ménages - SCRIPT COMPLET ET AUTONOME
# (Avec sauvegarde de tous les résultats)
# =========================================================================

# 1. CHARGEMENT DES BIBLIOTHÈQUES
# -------------------------------------------------------------------------
library(haven)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)

# 2. CHARGEMENT DES BASES DU NIGER
# -------------------------------------------------------------------------
# Bases principales
b2 <- read_dta(here("data", "ehcvm_conso_ner2021.dta"))
b4 <- read_dta(here("data", "s07b_me_ner2021.dta"))
b5 <- read_dta(here("data", "s16a_me_ner2021.dta"))
b6 <- read_dta(here("data", "s16c_me_ner2021.dta"))
b9 <- read_dta(here("data", "s16b_me_ner2021.dta"))

# Bases auxiliaires (Démographie S01 et Sécurité Alimentaire S08)
s01_me <- read_dta(here("data", "s01_me_ner2021.dta"))
s08a <- read_dta(here("data", "s08a_me_ner2021.dta"))

cat("✔ Bases du Niger (S01, S08, b2, b4, b5, b6, b9) chargées avec succès.\n")


# =========================================================================
# ÉTAPE 0 : UNIFORMISATION DE LA SURFACE EN HECTARES (b5)
# =========================================================================
if(!"surface_ha" %in% names(b5)){
  b5 <- b5 %>%
    mutate(surface_ha = case_when(
      as.numeric(s16aq09b) == 1 ~ as.numeric(s16aq09a),       # 1 = Hectare
      as.numeric(s16aq09b) == 2 ~ as.numeric(s16aq09a) / 100, # 2 = Are -> Hectare
      TRUE ~ NA_real_
    ))
}


# =========================================================================
# ÉTAPE 1 : TYPOLOGIE DES MÉNAGES (Producteurs vs Consommateurs)
# =========================================================================

code_consommation_mil <- 7
code_production_mil <- 1

# 1.1 Ménages CONSOMMATEURS (base b4)
menages_consommateurs <- b4 %>%
  filter(as.numeric(s07bq01) == code_consommation_mil & as.numeric(s07bq02) == 1) %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  distinct(grappe, menage) %>%
  mutate(est_consommateur = 1)

# 1.2 Ménages PRODUCTEURS (base b6, UNIQUEMENT sur les vagues valides)
b6_valide <- b6 %>% filter(!is.na(vague))

menages_producteurs <- b6_valide %>%
  filter(as.numeric(s16cq04) == code_production_mil) %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  distinct(grappe, menage) %>%
  mutate(est_producteur = 1)

# 1.3 Base unifiée de tous les ménages (avec poids, région, milieu et dépenses annuelles)
base_menages <- b4 %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  distinct(grappe, menage) %>%
  left_join(
    b2 %>% 
      mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
      select(grappe, menage, milieu, region, hhweight, depan),
    by = c("grappe", "menage")
  ) %>%
  left_join(menages_consommateurs, by = c("grappe", "menage")) %>%
  left_join(menages_producteurs, by = c("grappe", "menage")) %>%
  replace_na(list(est_consommateur = 0, est_producteur = 0))

# 1.4 Classification en 4 groupes
base_menages <- base_menages %>%
  mutate(
    groupe_typologie = case_when(
      est_producteur == 1 & est_consommateur == 1 ~ "Producteur-Consommateur",
      est_producteur == 1 & est_consommateur == 0 ~ "Producteur uniquement",
      est_producteur == 0 & est_consommateur == 1 ~ "Consommateur uniquement",
      TRUE ~ "Ni producteur ni consommateur"
    ),
    groupe_typologie = factor(groupe_typologie,
                              levels = c("Producteur-Consommateur", "Producteur uniquement",
                                         "Consommateur uniquement", "Ni producteur ni consommateur"))
  )


# =========================================================================
# ÉTAPE 2 : CALCUL DES INDICATEURS
# =========================================================================

# 2.1 PROFIL SOCIODÉMOGRAPHIQUE (Depuis s01_me_ner2021.dta)
# On filtre les membres présents (s01q11 == 1)
membres_presents <- s01_me %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  filter(as.numeric(s01q11) == 1)

taille_hh <- membres_presents %>%
  group_by(grappe, menage) %>%
  summarise(taille_menage = n(), .groups = "drop")

chef_hh <- membres_presents %>%
  filter(as.numeric(s01q02) == 1) %>% # 1 = Chef de ménage
  select(grappe, menage, sexe_chef = s01q01, age_chef = s01q04a) %>%
  mutate(across(c(sexe_chef, age_chef), as.numeric))

base_menages <- base_menages %>%
  left_join(taille_hh, by = c("grappe", "menage")) %>%
  left_join(chef_hh, by = c("grappe", "menage"))

# 2.2 SCORE FIES (Depuis s08a_me_ner2021.dta) - Conversion robuste
convert_fies <- function(col) {
  vals_nums <- suppressWarnings(as.numeric(col))
  if (!all(is.na(vals_nums))) {
    return(ifelse(vals_nums == 1, 1, 0))
  } else {
    return(ifelse(col == "Oui" | col == "1", 1, 0))
  }
}

fies_cols <- c("s08aq01", "s08aq02", "s08aq03", "s08aq04", 
               "s08aq05", "s08aq06", "s08aq07", "s08aq08")

fies_data <- s08a %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  select(grappe, menage, all_of(fies_cols)) %>%
  mutate(across(all_of(fies_cols), convert_fies)) %>%
  rowwise() %>%
  mutate(
    fies_score = sum(c_across(all_of(fies_cols)), na.rm = TRUE),
    fies_moderee = ifelse(fies_score >= 3 & !is.na(fies_score), 1, 0),
    fies_severe = ifelse(fies_score >= 6 & !is.na(fies_score), 1, 0)
  ) %>%
  ungroup() %>%
  select(grappe, menage, fies_score, fies_moderee, fies_severe)

# 2.3 SCORE HDDS (Depuis b4 - S7B)
hdds_data <- b4 %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  filter(as.numeric(s07bq02) == 1) %>%
  group_by(grappe, menage) %>%
  summarise(hdds_score = n_distinct(as.numeric(s07bq01)), .groups = "drop")

# 2.4 DÉPENSES ALIMENTAIRES (Depuis b4 - S7B)
depenses_alim <- b4 %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  group_by(grappe, menage) %>%
  summarise(depense_alim_totale = sum(as.numeric(s07bq08), na.rm = TRUE), .groups = "drop")

# 2.5 SUPERFICIE AGRICOLE (Depuis b5 - S16A)
surface_agri <- b5 %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  group_by(grappe, menage) %>%
  summarise(surface_tot_ha = sum(as.numeric(surface_ha), na.rm = TRUE), .groups = "drop") %>%
  mutate(surface_tot_ha = ifelse(is.na(surface_tot_ha), 0, surface_tot_ha))

# 2.6 VALEUR DES INTRANTS AGRICOLES (Depuis b9 - S16B)
# On utilise la valeur des intrants achetés (s16bq09c)
intrants <- b9 %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  group_by(grappe, menage) %>%
  summarise(valeur_intrants_tot = sum(as.numeric(s16bq09c), na.rm = TRUE), .groups = "drop") %>%
  mutate(valeur_intrants_tot = ifelse(is.na(valeur_intrants_tot), 0, valeur_intrants_tot))


# =========================================================================
# ÉTAPE 3 : FUSION FINALE ET CALCUL DE LA CONSOMMATION PAR TÊTE
# =========================================================================

base_analyse <- base_menages %>%
  left_join(fies_data, by = c("grappe", "menage")) %>%
  left_join(hdds_data, by = c("grappe", "menage")) %>%
  left_join(depenses_alim, by = c("grappe", "menage")) %>%
  left_join(surface_agri, by = c("grappe", "menage")) %>%
  left_join(intrants, by = c("grappe", "menage")) %>%
  mutate(
    fies_score = ifelse(is.na(fies_score), 0, fies_score),
    fies_moderee = ifelse(is.na(fies_moderee), 0, fies_moderee),
    fies_severe = ifelse(is.na(fies_severe), 0, fies_severe),
    hdds_score = ifelse(is.na(hdds_score), 0, hdds_score),
    depense_alim_totale = ifelse(is.na(depense_alim_totale), 0, depense_alim_totale),
    depan = ifelse(is.na(depan), 0, depan),
    valeur_intrants_tot = ifelse(is.na(valeur_intrants_tot), 0, valeur_intrants_tot)
  ) %>%
  # Calcul de la consommation par tête (Dépense annuelle totale / Taille du ménage)
  mutate(
    conso_par_tete = depan / taille_menage
  )


# =========================================================================
# ÉTAPE 4 : STATISTIQUES RÉCAPITULATIVES PAR GROUPE (AVEC PONDÉRATION)
# =========================================================================

stats_groupes <- base_analyse %>%
  group_by(groupe_typologie) %>%
  summarise(
    # Profil socio-démographique
    taille_menage_moy = weighted.mean(taille_menage, w = hhweight, na.rm = TRUE),
    age_chef_moy = weighted.mean(age_chef, w = hhweight, na.rm = TRUE),
    prop_chef_femme = weighted.mean(sexe_chef == 2, w = hhweight, na.rm = TRUE),
    prop_urbain = weighted.mean(milieu == 1, w = hhweight, na.rm = TRUE),
    
    # Sécurité Alimentaire
    fies_score_moy = weighted.mean(fies_score, w = hhweight, na.rm = TRUE),
    fies_moderee_prop = weighted.mean(fies_moderee, w = hhweight, na.rm = TRUE),
    fies_severe_prop = weighted.mean(fies_severe, w = hhweight, na.rm = TRUE),
    hdds_score_moy = weighted.mean(hdds_score, w = hhweight, na.rm = TRUE),
    
    # Niveau de vie et Accès aux facteurs
    conso_par_tete_moy = weighted.mean(conso_par_tete, w = hhweight, na.rm = TRUE),
    depense_alim_moy = weighted.mean(depense_alim_totale, w = hhweight, na.rm = TRUE),
    surface_moy_ha = weighted.mean(surface_tot_ha, w = hhweight, na.rm = TRUE),
    valeur_intrants_moy = weighted.mean(valeur_intrants_tot, w = hhweight, na.rm = TRUE),
    
    # Nombre de ménages
    n_menages = n()
  ) %>%
  arrange(desc(n_menages))


# =========================================================================
# ÉTAPE 5 : NETTOYAGE FINAL, AFFICHAGE ET SAUVEGARDE COMPLÈTE
# =========================================================================

# 5.1 Nettoyer les éventuels NaN
stats_groupes <- stats_groupes %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA_real_, .)))

# 5.2 Affichage dans la console
cat("\n======================================================\n")
cat("TABLEAU DE PROFILAGE - TYPOLOGIE DES MÉNAGES (Module 2)\n")
cat("======================================================\n")
print(stats_groupes)
cat("======================================================\n")

# 5.3 Sauvegardes dans le dossier figures/
dir.create("figures", showWarnings = FALSE)

# A. Sauvegarde du tableau récapitulatif
write.csv(stats_groupes, "figures/tableau_profilage_module2_complet.csv", row.names = FALSE)
cat("\n✔ Tableau complet sauvegardé : figures/tableau_profilage_module2_complet.csv")

# B. Sauvegarde de la base brute fusionnée (pour analyses ultérieures)
saveRDS(base_analyse, file = "figures/base_analyse_module2.rds")
cat("\n✔ Base de données brute sauvegardée : figures/base_analyse_module2.rds")

# C. Sauvegarde des indicateurs de niveau de vie (format allégé)
niveau_vie_stats <- stats_groupes %>%
  select(groupe_typologie, conso_par_tete_moy, depense_alim_moy, surface_moy_ha, valeur_intrants_moy)
write.csv(niveau_vie_stats, "figures/indicateurs_niveau_vie_par_groupe.csv", row.names = FALSE)
cat("\n✔ Indicateurs de niveau de vie sauvegardés : figures/indicateurs_niveau_vie_par_groupe.csv")

# D. Graphique FIES
if(max(stats_groupes$fies_score_moy, na.rm = TRUE) > 0) {
  p_fies <- ggplot(stats_groupes, aes(x = groupe_typologie, y = fies_score_moy, fill = groupe_typologie)) +
    geom_col(width = 0.7) + coord_flip() +
    ylim(0, 8) +
    geom_text(aes(label = round(fies_score_moy, 2)), hjust = -0.2, fontface = "bold") +
    labs(title = "Sécurité Alimentaire (Score FIES) selon le profil des ménages",
         x = NULL, y = "Score FIES moyen (0 à 8)") +
    scale_fill_brewer(palette = "Set2") +
    theme_minimal() + theme(legend.position = "none", plot.title = element_text(face = "bold"))
  
  print(p_fies)
  ggsave("figures/fies_score_par_groupe.png", plot = p_fies, width = 8, height = 5, dpi = 300)
  cat("\n✔ Graphique FIES sauvegardé : figures/fies_score_par_groupe.png")
}

# E. Résumé textuel formaté (pour le rapport ou le carnet de bord)
sink("figures/synthese_module2.txt")
cat("\n========================================================\n")
cat("SYNTHÈSE DU MODULE 2 - PROFILAGE DES MÉNAGES\n")
cat("========================================================\n\n")

cat("TAILLE MOYENNE DES MÉNAGES PAR GROUPE :\n")
print(stats_groupes %>% select(groupe_typologie, taille_menage_moy))

cat("\n\nÂGE MOYEN DES CHEFS DE MÉNAGE PAR GROUPE :\n")
print(stats_groupes %>% select(groupe_typologie, age_chef_moy))

cat("\n\nSCORE FIES MOYEN ET PRÉVALENCES PAR GROUPE :\n")
print(stats_groupes %>% select(groupe_typologie, fies_score_moy, fies_moderee_prop, fies_severe_prop))

cat("\n\nCONSOMMATION PAR TÊTE ET DÉPENSES ALIMENTAIRES PAR GROUPE :\n")
print(stats_groupes %>% select(groupe_typologie, conso_par_tete_moy, depense_alim_moy))

cat("\n\nACCÈS AUX FACTEURS (SUPERFICIE ET INTRANTS) PAR GROUPE :\n")
print(stats_groupes %>% select(groupe_typologie, surface_moy_ha, valeur_intrants_moy))

cat("\n\n========================================================\n")
cat("FIN DE LA SYNTHÈSE\n")
cat("========================================================\n")
sink()
cat("\n✔ Résumé textuel sauvegardé : figures/synthese_module2.txt\n")


# ---------- NOTE SUR L'INCIDENCE DE LA PAUVRETÉ ----------
cat("\n\n➡️ NOTE SUR L'INCIDENCE DE LA PAUVRETÉ :\n")
cat("Pour calculer l'incidence de la pauvreté, vous avez besoin du seuil de pauvreté national (par exemple 1,90 $/jour converti en FCFA).\n")
cat("Si vous avez ce seuil, exécutez ce code dans votre console :\n")
cat("seuil_pauvrete <- 1000  # À remplacer par votre seuil en FCFA\n")
cat("stats_groupes <- stats_groupes %>%\n")
cat("  mutate(pauvrete_incidence = weighted.mean(conso_par_tete_moy < seuil_pauvrete, w = n_menages, na.rm = TRUE))\n")

cat("\n\n🎯 FÉLICITATIONS ! LE MODULE 2 EST DÉFINITIVEMENT TERMINÉ.\n")
cat("✔ Tous les tableaux, graphiques et bases de données ont été sauvegardés dans le dossier 'figures/'.\n")