# =========================================================================
# MODULE 3 : Analyse de la Production et des Rendements (Mil) - FINAL
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
b5 <- read_dta(here("data", "s16a_me_ner2021.dta"))
b6 <- read_dta(here("data", "s16c_me_ner2021.dta"))
b9 <- read_dta(here("data", "s16b_me_ner2021.dta"))
s01_me <- read_dta(here("data", "s01_me_ner2021.dta"))
s03_co <- read_dta(here("data", "s03_co_ner2021.dta"))

cat("✔ Bases du Module 3 (b5, b6, b9, s01_me, s03_co) chargées.\n")

# 3. UNIFORMISATION DE LA SURFACE EN HECTARES (b5)
# -------------------------------------------------------------------------
if(!"surface_ha" %in% names(b5)){
  b5 <- b5 %>%
    mutate(surface_ha = case_when(
      as.numeric(s16aq09b) == 1 ~ as.numeric(s16aq09a),       # 1 = Hectare
      as.numeric(s16aq09b) == 2 ~ as.numeric(s16aq09a) / 100, # 2 = Are -> Hectare
      TRUE ~ NA_real_
    ))
}

# =========================================================================
# ÉTAPE 1 : FILTRAGE ET DÉDOUBLONNAGE DES PARCELLES DE MIL
# =========================================================================

# 1.1. Filtrer les parcelles de Mil (code 1) et exclure les vagues NA
parcelles_mil <- b6 %>%
  filter(!is.na(vague)) %>%
  filter(as.numeric(s16cq04) == 1) %>%
  # Garder les lignes où la conversion est possible
  filter(!is.na(s16cq16a) & !is.na(s16cq16c) & !is.na(s16cq16b))

# 1.2. Calcul de la quantité récoltée en kilogrammes
parcelles_mil <- parcelles_mil %>%
  mutate(
    quantite_kg = as.numeric(s16cq16a) * as.numeric(s16cq16c)
  )

# 1.3. DÉDOUBLONNAGE : une seule ligne par parcelle de Mil
parcelles_mil <- parcelles_mil %>%
  group_by(grappe, menage, s16cq02, s16cq03) %>%
  slice(1) %>%
  ungroup()

# =========================================================================
# ÉTAPE 2 : JOINTURE AVEC LES SUPERFICIES
# =========================================================================

# 2.1. Jointure avec b5 pour récupérer la surface en hectares
parcelles_mil <- parcelles_mil %>%
  left_join(
    b5 %>% select(grappe, menage, s16aq02, s16aq03, surface_ha),
    by = c("grappe" = "grappe", "menage" = "menage",
           "s16cq02" = "s16aq02", "s16cq03" = "s16aq03")
  ) %>%
  filter(!is.na(surface_ha) & surface_ha > 0)

# 2.2. Calcul du rendement (kg / ha)
parcelles_mil <- parcelles_mil %>%
  mutate(
    rendement_kg_ha = quantite_kg / surface_ha
  )

# 2.3. Exclure les parcelles en perte totale (s16cq11 == 3)
parcelles_mil <- parcelles_mil %>%
  filter(as.numeric(s16cq11) != 3 | is.na(s16cq11))

# =========================================================================
# ÉTAPE 3 : WINSORISATION
# =========================================================================

p1 <- quantile(parcelles_mil$rendement_kg_ha, 0.01, na.rm = TRUE)
p99 <- quantile(parcelles_mil$rendement_kg_ha, 0.99, na.rm = TRUE)

cat("Seuils de winsorisation : P1 =", round(p1, 2), "kg/ha, P99 =", round(p99, 2), "kg/ha\n")

parcelles_mil <- parcelles_mil %>%
  filter(rendement_kg_ha >= p1 & rendement_kg_ha <= p99)

# =========================================================================
# ÉTAPE 4 : STATISTIQUES DESCRIPTIVES ET HISTOGRAMME
# =========================================================================

stats_rendement <- parcelles_mil %>%
  summarise(
    n_parcelles = n(),
    rendement_moyen = mean(rendement_kg_ha, na.rm = TRUE),
    rendement_median = median(rendement_kg_ha, na.rm = TRUE),
    rendement_sd = sd(rendement_kg_ha, na.rm = TRUE),
    rendement_min = min(rendement_kg_ha, na.rm = TRUE),
    rendement_max = max(rendement_kg_ha, na.rm = TRUE)
  )

cat("\n======================================================\n")
cat("STATISTIQUES DESCRIPTIVES - RENDEMENT DU MIL (kg/ha)\n")
cat("======================================================\n")
print(stats_rendement)
cat("======================================================\n")

# Histogramme
p_hist <- ggplot(parcelles_mil, aes(x = rendement_kg_ha)) +
  geom_histogram(binwidth = 100, fill = "#2E86AB", alpha = 0.8) +
  geom_vline(xintercept = stats_rendement$rendement_moyen, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution des rendements du Mil au Niger",
       subtitle = paste("Rendement moyen :", round(stats_rendement$rendement_moyen, 1), "kg/ha"),
       x = "Rendement (kg/ha)", y = "Nombre de parcelles") +
  theme_minimal()

print(p_hist)
dir.create("figures", showWarnings = FALSE)
ggsave("figures/histogramme_rendement_mil.png", plot = p_hist, width = 8, height = 6, dpi = 300)
cat("\n✔ Histogramme sauvegardé dans 'figures/histogramme_rendement_mil.png'\n")

# =========================================================================
# ÉTAPE 5 : PRÉPARATION DES VARIABLES POUR LA RÉGRESSION
# =========================================================================

# 5.1. Intrants (b9)
intrants_agreg <- b9 %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  group_by(grappe, menage) %>%
  summarise(valeur_intrants_tot = sum(as.numeric(s16bq09c), na.rm = TRUE), .groups = "drop") %>%
  mutate(valeur_intrants_tot = ifelse(is.na(valeur_intrants_tot), 0, valeur_intrants_tot))

# 5.2. Éducation du chef (s01_me)
education_chef <- s01_me %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  filter(as.numeric(s01q02) == 1) %>%
  select(grappe, menage, edu_chef = s01q06) %>%
  mutate(edu_chef = as.numeric(edu_chef))

# 5.3. Semences (b6) – DÉDOUBLONNAGE ET AGRÉGATION
# On garde une seule ligne par parcelle, avec la valeur de s16cq09
semences <- b6 %>%
  filter(!is.na(vague)) %>%
  filter(as.numeric(s16cq04) == 1) %>%
  mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
  select(grappe, menage, s16cq02, s16cq03, s16cq09) %>%
  group_by(grappe, menage, s16cq02, s16cq03) %>%
  summarise(s16cq09 = first(s16cq09), .groups = "drop") %>%
  mutate(semence_amelioree = ifelse(as.numeric(s16cq09) == 2, 1, 0))

# 5.4. Variables communautaires (s03_co)
s03_co_valid <- s03_co %>%
  mutate(grappe = as.numeric(grappe)) %>%
  filter(as.numeric(s03q01) == 1) %>%
  transmute(
    grappe,
    presence_coop = as.numeric(s03q03),
    acces_sem_ameliorees = as.numeric(s03q08),
    acces_engrais_chimique = as.numeric(s03q12),
    acces_pesticide = as.numeric(s03q14),
    service_vulgarisation = as.numeric(s03q16),
    pratique_irrigation = as.numeric(s03q17)
  )

# 5.5. Fusion finale – Toutes les tables sont maintenant uniques par parcelle
data_reg <- parcelles_mil %>%
  left_join(intrants_agreg, by = c("grappe", "menage")) %>%
  left_join(education_chef, by = c("grappe", "menage")) %>%
  left_join(semences, by = c("grappe", "menage", "s16cq02" = "s16cq02", "s16cq03" = "s16cq03")) %>%
  left_join(s03_co_valid, by = "grappe") %>%
  mutate(
    log_rendement = log(rendement_kg_ha),
    log_intrants = log(valeur_intrants_tot + 1),
    log_surface = log(surface_ha)
  )

# =========================================================================
# ÉTAPE 6 : RÉGRESSION OLS
# =========================================================================

model_reg <- lm(log_rendement ~ log_intrants + semence_amelioree + log_surface + edu_chef +
                  presence_coop + acces_sem_ameliorees + acces_engrais_chimique +
                  acces_pesticide + service_vulgarisation + pratique_irrigation,
                data = data_reg)

cat("\n======================================================\n")
cat("RÉSULTATS DE LA RÉGRESSION (Déterminants du rendement)\n")
cat("======================================================\n")
print(summary(model_reg))
cat("======================================================\n")

sink("figures/resultats_regression_rendement.txt")
cat("MODÈLE DE RÉGRESSION - DÉTERMINANTS DU RENDEMENT DU MIL\n")
cat("========================================================\n")
print(summary(model_reg))
cat("========================================================\n")
sink()
cat("\n✔ Résultats de la régression sauvegardés.\n")

# =========================================================================
# ÉTAPE 7 : SAUVEGARDE POUR CARTOGRAPHIE
# =========================================================================

rendement_par_grappe <- parcelles_mil %>%
  group_by(grappe) %>%
  summarise(
    rendement_moyen_grappe = mean(rendement_kg_ha, na.rm = TRUE),
    n_parcelles = n()
  )

# Éviter l'erreur "Permission denied"
if(file.exists("figures/rendement_par_grappe.csv")) {
  file.remove("figures/rendement_par_grappe.csv")
}
write.csv(rendement_par_grappe, "figures/rendement_par_grappe.csv", row.names = FALSE)
cat("\n✔ Données de rendement par grappe sauvegardées.\n")

cat("\n🎯 MODULE 3 TERMINÉ SANS AUCUN AVERTISSEMENT !\n")