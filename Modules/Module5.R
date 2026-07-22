# =============================================================================
# MODULE 4 : Analyse de la commercialisation, des prix et cartographie
# (Version avec jointure par REGION pour les prix communautaires - pas de clé grappe disponible)
# EHCVM Niger 2021 - Projet filière Mil
# =============================================================================

# ---- 1. Chargement des données (déjà fait dans 00_load_data.R) ----
if (!exists("b16")) source("00_load_data.R")

library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
library(haven)
library(tidyr)

cat("\n=== MODULE 4 : Commercialisation et Prix ===\n")

# ---- 2. Nettoyage des données de commercialisation (S16D) ----
b16_clean <- b16 %>%
  mutate(
    qte_vendue_kg = as.numeric(s16dq05c),
    montant_vente = as.numeric(s16dq06),
    prix_prod_kg = if_else(qte_vendue_kg > 0 & !is.na(qte_vendue_kg) & qte_vendue_kg > 0,
                           montant_vente / qte_vendue_kg, NA_real_),
    culture_code = as.numeric(s16dq01),
    acheteur = as_factor(s16dq08),
    stockage = as_factor(s16dq11),
    but_stock = as_factor(s16dq14)
  ) %>%
  filter(!is.na(qte_vendue_kg) & qte_vendue_kg > 0 & 
           !is.na(montant_vente) & montant_vente > 0 &
           !is.na(prix_prod_kg) & prix_prod_kg > 0) %>%
  # Joindre les poids, région et coordonnées GPS
  left_join(welfare %>% select(grappe, menage, hhweight, region), by = c("grappe", "menage")) %>%
  left_join(s00 %>% select(grappe, menage, lat, lon), by = c("grappe", "menage")) %>%
  # Harmoniser region en texte pour permettre des jointures futures cohérentes
  mutate(region = trimws(as.character(as_factor(region))))
# NOTE : si s00 contient encore GPS__Latitude / GPS__Longitude au lieu de lat/lon,
# utilisez plutôt : left_join(s00 %>% select(grappe, menage, lat = GPS__Latitude, lon = GPS__Longitude), by = c("grappe","menage"))

cat("Nombre de lignes après nettoyage S16D :", nrow(b16_clean), "\n")

# ---- 3. Taux de commercialisation par culture (pondéré) ----
taux_commer <- b16_clean %>%
  group_by(culture_code) %>%
  summarise(
    nb_producteurs_pond = sum(hhweight, na.rm = TRUE),
    nb_vendeurs_pond   = sum(hhweight * (qte_vendue_kg > 0), na.rm = TRUE),
    qte_vendue_pond    = sum(qte_vendue_kg * hhweight, na.rm = TRUE),
    montant_vente_pond = sum(montant_vente * hhweight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    taux_commercialisation = round(nb_vendeurs_pond / nb_producteurs_pond * 100, 1),
    prix_moyen_kg = round(montant_vente_pond / qte_vendue_pond, 0)
  ) %>%
  arrange(desc(taux_commercialisation))

cat("\n--- Taux de commercialisation par culture ---\n")
print(taux_commer)

# ---- 4. Canaux de vente et stockage (graphiques) ----
dir.create("figures", showWarnings = FALSE)

# Canaux de vente
canaux <- b16_clean %>%
  group_by(acheteur) %>%
  summarise(effectif_pond = sum(hhweight, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct = round(effectif_pond / sum(effectif_pond) * 100, 1))

p_canaux <- ggplot(canaux, aes(x = fct_reorder(acheteur, effectif_pond), y = effectif_pond, fill = effectif_pond)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Canaux de vente (pondéré)", x = NULL, y = "Effectif pondéré") +
  theme_minimal()
print(p_canaux)
ggsave("figures/canaux_vente.png", p_canaux, width = 10, height = 6, dpi = 150)

# Stockage
stock <- b16_clean %>%
  group_by(stockage) %>%
  summarise(effectif_pond = sum(hhweight, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct = round(effectif_pond / sum(effectif_pond) * 100, 1))

p_stock <- ggplot(stock, aes(x = fct_reorder(stockage, effectif_pond), y = effectif_pond, fill = effectif_pond)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Méthodes de stockage (pondéré)", x = NULL, y = "Effectif pondéré") +
  theme_minimal()
print(p_stock)
ggsave("figures/methodes_stockage.png", p_stock, width = 10, height = 6, dpi = 150)

# ---- 5. Prix producteur moyen par culture et région (pondéré) ----
prix_prod_region <- b16_clean %>%
  group_by(culture_code, region) %>%
  summarise(
    prix_prod_moy = weighted.mean(prix_prod_kg, hhweight, na.rm = TRUE),
    .groups = "drop"
  )

# ---- 6. Prix marchés communautaires (prix_communautaire) ----
# IMPORTANT : prix_communautaire ne partage AUCUNE clé (ni grappe, ni key commune)
# avec les autres modules chargés. La seule jointure possible ici est par REGION.
# ⚠️ Adapter le nom de la variable région ci-dessous si ce n'est pas "region"
# (vérifier avec names(prix_communautaire) - ex: s00q01, codregion, etc.)

prix_marche <- prix_communautaire %>%
  mutate(
    codpr = as.numeric(produit__id),
    region = trimws(as.character(as_factor(region))),   # <-- ADAPTER le nom de variable si nécessaire
    prix_kg = case_when(
      as.numeric(unite) == 2   ~ as.numeric(prix1) / as.numeric(quantite1),
      as.numeric(unite) == 100 ~ as.numeric(prix1) / (as.numeric(quantite1) / 1000),
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(prix_kg) & prix_kg > 0) %>%
  group_by(codpr, region) %>%
  summarise(
    prix_marche_moy = mean(prix_kg, na.rm = TRUE),
    n_obs_marche = n(),
    .groups = "drop"
  )

cat("\nNombre de lignes prix_marche (par région) :", nrow(prix_marche), "\n")

# ---- Vérification de cohérence des libellés région avant jointure ----
cat("\nRégions dans b16_clean  :", paste(sort(unique(b16_clean$region)), collapse = " | "), "\n")
cat("Régions dans prix_marche :", paste(sort(unique(prix_marche$region)), collapse = " | "), "\n")
cat("Régions non appariées :", 
    paste(setdiff(unique(b16_clean$region), unique(prix_marche$region)), collapse = " | "), "\n")

# ---- 7. Correspondance culture_code <-> codpr (pour le mil et autres) ----
correspondance <- data.frame(
  culture_code = c(1, 2, 3, 4, 8, 10, 11, 12, 13, 33, 37),
  codpr        = c(7, 8, 1, 6, 112, 114, 98, 102, 120, 100, 91),
  label        = c("Mil", "Sorgho", "Riz Paddy", "Maïs", "Niébé", "Arachide",
                   "Gombo", "Oseille", "Sésame", "Oignon", "Haricot vert")
)

# ---- 8. Base des prix par grappe pour le Mil (avec marge, prix marché au niveau région) ----
prix_par_grappe <- b16_clean %>%
  filter(culture_code == 1) %>%  # Mil
  group_by(grappe) %>%
  summarise(
    prix_prod_moy = weighted.mean(prix_prod_kg, hhweight, na.rm = TRUE),
    region = first(region),
    lat = first(lat),
    lon = first(lon),
    n_observations = n(),
    .groups = "drop"
  ) %>%
  # Jointure par région (chaque grappe hérite du prix de marché moyen de sa région)
  left_join(
    prix_marche %>% filter(codpr == 7) %>% select(region, prix_marche_moy),
    by = "region"
  ) %>%
  mutate(
    marge = prix_marche_moy - prix_prod_moy,
    marge_pct = round(marge / prix_prod_moy * 100, 1)
  ) %>%
  filter(!is.na(lat) & !is.na(lon))

cat("Nombre de grappes avec marge calculée :", sum(!is.na(prix_par_grappe$marge)), 
    "sur", nrow(prix_par_grappe), "\n")

# Sauvegarde pour cartographie
write.csv(prix_par_grappe, "figures/prix_par_grappe.csv", row.names = FALSE)
saveRDS(prix_par_grappe, "figures/prix_par_grappe.rds")
cat("\n✔ Base prix par grappe sauvegardée (jointure marché par région).\n")

# ---- 9. Marges commerciales par région (toutes cultures) ----
marge_region <- prix_par_grappe %>%
  group_by(region) %>%
  summarise(
    marge_moy = mean(marge, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(marge_moy))

cat("\n--- Marge commerciale moyenne par région (FCFA/kg) ---\n")
print(marge_region)

# ---- 10. Régression du prix producteur sur distance au marché et coopératives ----
# Distance au marché (QC-S2)
dist_marche <- qc_s2 %>%
  filter(as.numeric(s02q00) %in% c(14, 15)) %>%  # 14=Marché permanent, 15=Marché périodique
  group_by(grappe) %>%
  summarise(temps_marche = min(as.numeric(s02q03), na.rm = TRUE), .groups = "drop")

# Existence de coopérative (QC-S3)
coop <- qc_s3 %>%
  mutate(coop_dummy = if_else(as.numeric(s03q03) == 1, 1L, 0L)) %>%
  select(grappe, coop_dummy)

# Base de régression
reg_data <- b16_clean %>%
  left_join(dist_marche, by = "grappe") %>%
  left_join(coop, by = "grappe") %>%
  filter(!is.na(temps_marche) & !is.na(prix_prod_kg) & prix_prod_kg > 0)

cat("\nNombre d'observations pour la régression :", nrow(reg_data), "\n")
# ---- Distance au marché (QC-S2) — CORRIGÉ pour éviter les Inf ----
dist_marche <- qc_s2 %>%
  filter(as.numeric(s02q00) %in% c(14, 15)) %>%  # 14=Marché permanent, 15=Marché périodique
  group_by(grappe) %>%
  summarise(
    temps_marche = suppressWarnings(min(as.numeric(s02q03), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(temps_marche = if_else(is.infinite(temps_marche), NA_real_, temps_marche))

cat("Nombre de grappes avec temps_marche valide :", sum(!is.na(dist_marche$temps_marche)), 
    "sur", nrow(dist_marche), "\n")
reg_data <- b16_clean %>%
  left_join(dist_marche, by = "grappe") %>%
  left_join(coop, by = "grappe") %>%
  filter(
    !is.na(temps_marche) & is.finite(temps_marche) &
    !is.na(prix_prod_kg) & prix_prod_kg > 0 & is.finite(prix_prod_kg) &
    !is.na(qte_vendue_kg) & qte_vendue_kg > 0 & is.finite(qte_vendue_kg) &
    !is.na(coop_dummy) &
    !is.na(hhweight) & hhweight > 0
  )

cat("\nNombre d'observations pour la régression :", nrow(reg_data), "\n")
summary(reg_data$temps_marche)  # vérifier qu'il n'y a plus d'Inf
# Modèle OLS pondéré
model_prix <- lm(log(prix_prod_kg) ~ log(temps_marche + 1) + coop_dummy + 
                   as_factor(s16dq08) + log(qte_vendue_kg),
                 data = reg_data, weights = hhweight)


cat("\n--- Résultats de la régression du prix producteur ---\n")
print(summary(model_prix))

# Sauvegarde des résultats
sink("figures/regression_prix_producteur.txt")
cat("MODÈLE : log(prix_prod_kg) ~ log(temps_marche+1) + coop_dummy + canal_vente + log(qte_vendue_kg)\n")
print(summary(model_prix))
sink()

cat("\n✔ Module 4 terminé. Résultats sauvegardés dans figures/\n")