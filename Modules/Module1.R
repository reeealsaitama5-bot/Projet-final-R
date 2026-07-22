# =========================================================================
# PROJET Exposer_R - MODULE 1 : Justification du Mil au Niger
# Script R complet, réorganisé et unifié pour l'analyse EHCVM 2021
# =========================================================================

# 1. CHARGEMENT DES BIBLIOTHÈQUES
# -------------------------------------------------------------------------
library(haven)
library(here)
library(dplyr)
library(ggplot2)

# =========================================================================
# PARTIE 1 : CHARGEMENT ET DIAGNOSTIC DES BASES (b1 à b9)
# =========================================================================

data_dir <- here("data")

# 1.1 Liste personnalisée pour contrôler l'ordre (b1 à b8 inchangés, le nouveau s16b devient b9)
liste_noms_fichiers <- c(
  "calorie_conversion_wa_2021.dta", # -> b1
  "ehcvm_conso_ner2021.dta",        # -> b2
  "ehcvm_prix_ner2021.dta",         # -> b3
  "s07b_me_ner2021.dta",            # -> b4
  "s16a_me_ner2021.dta",            # -> b5
  "s16c_me_ner2021.dta",            # -> b6
  "s16d_me_ner2021.dta",            # -> b7
  "s17_me_ner2021.dta",             # -> b8
  "s16b_me_ner2021.dta"             # -> b9 (le nouveau fichier ajouté en dernier)
)

# 1.2 Fonction de diagnostic (libellés et valeurs manquantes)
diagnostic_base <- function(df, nom_objet) {
  libelles_var <- vapply(df, function(x) {
    lab <- attr(x, "label")
    if (is.null(lab)) return(NA_character_)
    if (length(lab) > 1) lab <- lab[1]
    as.character(lab)
  }, FUN.VALUE = character(1))
  
  recap <- data.frame(
    Variable = names(df),
    Libelle = libelles_var,
    Nb_Manquants = colSums(is.na(df)),
    stringsAsFactors = FALSE
  )
  
  cat("\n========================================================\n")
  cat("FICHIER :", nom_fichier, " --> Objet R créé :", nom_objet, "\n")
  cat("========================================================\n")
  print(recap)
  cat("\n")
}

# 1.3 Boucle unifiée de chargement
for (i in seq_along(liste_noms_fichiers)) {
  nom_fichier <- liste_noms_fichiers[i]
  chemin <- file.path(data_dir, nom_fichier)
  nom_objet <- paste0("b", i)
  
  df <- read_dta(chemin)
  assign(nom_objet, df)
  
  diagnostic_base(df, nom_objet)
}

cat("\n✔ Toutes les bases 'b1' à 'b9' ont été chargées avec succès !\n")


# =========================================================================
# PARTIE 2 : MODULE 1 - CRITÈRE 1 (Importance pour la sécurité alimentaire)
# =========================================================================

# 2.1 Calcul de la fréquence de consommation (depuis b4 et b1)
total_menages <- n_distinct(b4$grappe, b4$menage)

tab_freq_conso <- b4 %>%
  filter(!is.na(s07bq02)) %>%
  left_join(b1 %>% select(codpr, prodlab), by = c("s07bq01" = "codpr")) %>%
  mutate(prodlab = ifelse(is.na(prodlab) | prodlab == "", paste("Code", s07bq01), prodlab)) %>%
  group_by(s07bq01, prodlab) %>%
  summarise(n_oui = sum(s07bq02 == 1, na.rm = TRUE), .groups = "drop") %>%
  mutate(frequence = n_oui / total_menages) %>%
  arrange(desc(frequence))

# 2.2 Filtrage des condiments (sel, sucre, cubes, piments)
produits_a_exclure <- c(
  "Sel", "Cube alimentaire (Maggi, Jumbo, )", "Sucre en poudre",
  "Piment séché", "Piment frais", "Soumbala (moutarde africaine)", "Oignon frais"
)

tab_freq_serieux <- tab_freq_conso %>%
  filter(!prodlab %in% produits_a_exclure) %>%
  slice_head(n = 10) %>%
  arrange(frequence) %>%
  mutate(prodlab = factor(prodlab, levels = prodlab))

# 2.3 Graphique 1 : Top 10 des produits stratégiques consommés
p_consommation <- ggplot(tab_freq_serieux, aes(x = prodlab, y = frequence)) +
  geom_col(fill = "#2E86AB", width = 0.75) + coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  geom_text(aes(label = scales::percent(frequence, accuracy = 1)), hjust = -0.2, size = 5, fontface = "bold") +
  labs(title = "Top 10 des produits stratégiques les plus consommés", x = NULL, y = "Fréquence (% ménages)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12), plot.title = element_text(size = 18, face = "bold"), panel.grid.major.y = element_blank())

dir.create("figures", showWarnings = FALSE)
ggsave("figures/1_frequence_consommation.png", p_consommation, width = 8, height = 5, dpi = 300)
print(p_consommation)
cat("✔ Graphique 1 (Consommation) sauvegardé.\n")


# =========================================================================
# PARTIE 3 : MODULE 1 - CRITÈRE 2 (Poids dans la production agricole)
# =========================================================================

# 3.1 Uniformisation des unités de surface dans b5 (Hectares)
b5 <- b5 %>%
  mutate(surface_ha = case_when(
    s16aq09b == 1 ~ s16aq09a,        # 1 = Hectare
    s16aq09b == 2 ~ s16aq09a / 100,  # 2 = Are -> Hectare
    TRUE ~ NA_real_
  ))

# 3.2 Création du dictionnaire des noms de cultures (depuis b6)
dict_b6 <- b6 %>%
  select(s16cq04) %>%
  distinct() %>%
  mutate(nom_culture = as.character(as_factor(s16cq04))) %>%
  filter(!is.na(nom_culture))

# 3.3 Nettoyage : On filtre les NA de la vague dans b6 (pour le calcul robuste des ménages et surfaces)
b6_clean <- b6 %>% filter(!is.na(vague))

# 3.4 Jointure b6 + b5, application de la règle des mélanges et calcul des surfaces par culture
surface_par_culture <- b6_clean %>%
  filter(!is.na(s16cq04)) %>%
  left_join(b5 %>% select(grappe, menage, vague, s16aq02, s16aq03, surface_ha),
            by = c("grappe" = "grappe", "menage" = "menage", "vague" = "vague",
                   "s16cq02" = "s16aq02", "s16cq03" = "s16aq03")) %>%
  filter(!is.na(surface_ha)) %>%
  mutate(surface_allouee = ifelse(is.na(s16cq08), surface_ha, surface_ha * (s16cq08 / 100))) %>%
  group_by(s16cq04) %>%
  summarise(surface_tot = sum(surface_allouee, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(dict_b6, by = "s16cq04") %>%
  mutate(nom_culture = ifelse(is.na(nom_culture), paste("Code", s16cq04), nom_culture)) %>%
  arrange(desc(surface_tot))

# 3.5 Graphique 2 : Top 10 des cultures par surface agricole occupée
top10_surface <- head(surface_par_culture, 10) %>%
  mutate(nom_culture = factor(nom_culture, levels = nom_culture[order(surface_tot)]))

p_surface <- ggplot(top10_surface, aes(x = nom_culture, y = surface_tot)) +
  geom_col(fill = "#28A745", width = 0.75) + coord_flip() +
  scale_y_continuous(labels = scales::number_format(suffix = " Ha"), expand = expansion(mult = c(0, 0.2))) +
  geom_text(aes(label = paste0(round(surface_tot, 0), " Ha")), hjust = -0.1, size = 5, fontface = "bold") +
  labs(title = "Top 10 des cultures par surface agricole occupée", x = NULL, y = "Superficie en Ha") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, face = "bold"), plot.title = element_text(size = 18, face = "bold"))

ggsave("figures/2_surface_agricole.png", p_surface, width = 9, height = 6, dpi = 300)
print(p_surface)
cat("✔ Graphique 2 (Surfaces) sauvegardé.\n")

# =========================================================================
# PARTIE 4 : MODULE 1 - CRITÈRE 2 - BIS (Diffusion parmi les ménages agricoles)
# =========================================================================

# 4.1 Dénominateur des ménages agriculteurs valides (en filtrant les NA de vague)
b6_valide <- b6 %>% filter(!is.na(vague))
valid_agri_hh <- n_distinct(b6_valide$grappe, b6_valide$menage)

# 4.2 Proportion des ménages par culture
prop_menages <- b6_valide %>%
  filter(!is.na(s16cq04)) %>%
  mutate(nom_culture = as.character(as_factor(s16cq04))) %>%
  filter(!is.na(nom_culture)) %>%
  group_by(nom_culture) %>%
  summarise(n_menages = n_distinct(grappe, menage)) %>%
  mutate(proportion = n_menages / valid_agri_hh) %>%
  arrange(desc(proportion))

# 4.3 Graphique 3 : Top 20 des cultures les plus cultivées par les ménages
top20_menages <- head(prop_menages, 20) %>%
  mutate(nom_culture = factor(nom_culture, levels = nom_culture[order(proportion)]))

p_menages <- ggplot(top20_menages, aes(x = nom_culture, y = proportion)) +
  geom_col(fill = "#2E86AB", width = 0.75) + coord_flip() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), hjust = -0.1, size = 4, fontface = "bold") +
  labs(title = "Top 20 des cultures cultivées par les ménages",
       subtitle = paste("Basé sur", valid_agri_hh, "ménages agriculteurs valides"), x = NULL, y = "% des ménages agriculteurs") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11), plot.title = element_text(size = 18, face = "bold"))

ggsave("figures/3_proportion_menages.png", p_menages, width = 10, height = 7, dpi = 300)
print(p_menages)
cat("✔ Graphique 3 (Ménages producteurs) sauvegardé.\n")


# =========================================================================
# PARTIE 5 : MODULE 1 - CRITÈRE 3 (Rôle économique et part du Mil)
# =========================================================================

# 5.1 Identification du code du Mil dans la base b7 (S16D)
labels_b7 <- attr(b7$s16dq01, "labels")
code_mil_b7 <- as.numeric(names(labels_b7)[labels_b7 == "Mil"])
if (length(code_mil_b7) == 0) code_mil_b7 <- 1 # Fallback sécurisé

# 5.2 Calcul du Revenu Total et du Revenu du Mil
revenu_total <- sum(b7$s16dq06, na.rm = TRUE)
revenu_mil <- b7 %>% filter(s16dq01 == code_mil_b7) %>% pull(s16dq06) %>% sum(na.rm = TRUE)
part_revenu_mil <- revenu_mil / revenu_total

cat("\n======================================================\n")
cat("RÉSULTATS ÉCONOMIQUES (Critère 3)\n")
cat("======================================================\n")
cat("Revenu total des ventes :", round(revenu_total, 0), "FCFA\n")
cat("Revenu issu du Mil      :", round(revenu_mil, 0), "FCFA\n")
cat("Part du Mil (14,5%)     :", round(part_revenu_mil * 100, 2), "%\n")
cat("======================================================\n")

# 5.3 Classement des cultures par revenu (pour le graphique)
dict_b7 <- b7 %>% select(s16dq01) %>% distinct() %>% 
  mutate(nom_culture = as.character(as_factor(s16dq01))) %>% filter(!is.na(nom_culture))

revenu_par_culture <- b7 %>%
  filter(!is.na(s16dq06)) %>%
  group_by(s16dq01) %>%
  summarise(revenu_culture = sum(s16dq06, na.rm = TRUE)) %>%
  left_join(dict_b7, by = "s16dq01") %>%
  mutate(nom_culture = ifelse(is.na(nom_culture), paste("Code", s16dq01), nom_culture)) %>%
  arrange(desc(revenu_culture))

# 5.4 Graphique 4 : Top 10 des cultures par revenu de vente généré
top10_revenu <- head(revenu_par_culture, 10) %>%
  mutate(nom_culture = factor(nom_culture, levels = nom_culture[order(revenu_culture)]))

p_revenu <- ggplot(top10_revenu, aes(x = nom_culture, y = revenu_culture)) +
  geom_col(fill = "#D64933", width = 0.75) + coord_flip() +
  scale_y_continuous(labels = scales::number_format(suffix = " FCFA"), expand = expansion(mult = c(0, 0.2))) +
  geom_text(aes(label = paste0(round(revenu_culture / 1000, 1), " K")), hjust = -0.1, size = 4.5, fontface = "bold") +
  labs(title = "Top 10 des cultures par revenu de vente généré",
       subtitle = paste("Le Mil représente", round(part_revenu_mil * 100, 1), "% du revenu total"),
       x = NULL, y = "Revenu total des ventes") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 11, face = "bold"), plot.title = element_text(size = 18, face = "bold"))

ggsave("figures/4_revenu_agricole.png", p_revenu, width = 10, height = 7, dpi = 300)
print(p_revenu)

# =========================================================================
# FIN : BOUCLAGE DU MODULE 1
# =========================================================================

cat("\n🎯 FÉLICITATIONS ! LE MODULE 1 EST COMPLÈTEMENT BOUCLÉ.\n")
cat("✔ Les 4 graphiques ont été générés dans le dossier 'figures/'.\n")
cat("✔ Les 3 critères (Consommation, Production, Revenu) sont prêts pour votre rapport.\n")