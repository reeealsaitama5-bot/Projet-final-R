# =============================================================================
# MODULE INDÉPENDANT : Pluriactivité et sécurité alimentaire (Niger)
# Question de recherche : 
# "La pluriactivité (agriculture + élevage) protège-t-elle les ménages 
#  nigériens producteurs/éleveurs de mil contre l'insécurité alimentaire sévère 
#  (score FIES) lors des mauvaises campagnes ?"
# =============================================================================

# ---- 1. Chargement des bibliothèques ----
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(here)

cat("\n=== MODULE : PLURIACTIVITÉ ET INSÉCURITÉ ALIMENTAIRE (NIGER) ===\n")

# ---- 2. Chargement des données (autonome) ----
cat("Chargement des données EHCVM Niger 2021...\n")

# Bases nécessaires
s08  <- read_dta(here("data", "s08a_me_ner2021.dta"))
b4   <- read_dta(here("data", "s07b_me_ner2021.dta"))
b6   <- read_dta(here("data", "s16c_me_ner2021.dta"))
b16  <- read_dta(here("data", "s16d_me_ner2021.dta"))
s17  <- read_dta(here("data", "s17_me_ner2021.dta"))
qc_s2 <- read_dta(here("data", "s02_co_ner2021.dta"))
qc_s3 <- read_dta(here("data", "s03_co_ner2021.dta"))
welfare <- read_dta(here("data", "ehcvm_welfare_ner2021.dta"))

# Nettoyage des types
convert <- function(df) {
  df %>% mutate(across(c(grappe, menage), ~ as.numeric(as.character(.))))
}
s08  <- convert(s08)
b4   <- convert(b4)
b6   <- convert(b6)
b16  <- convert(b16)
s17  <- convert(s17)
qc_s2 <- qc_s2 %>% mutate(grappe = as.numeric(as.character(grappe)))
qc_s3 <- qc_s3 %>% mutate(grappe = as.numeric(as.character(grappe)))
welfare <- convert(welfare)

# ---- 3. Construction du score FIES (section S08) ----
cat("Construction du score FIES...\n")
fies <- s08 %>%
  mutate(across(starts_with("s08aq"),
                ~ case_when(
                  as.numeric(.) == 1 ~ 1L,
                  as.numeric(.) == 2 ~ 0L,
                  TRUE ~ NA_integer_
                ))) %>%
  rowwise() %>%
  mutate(score_fies = sum(c_across(starts_with("s08aq")), na.rm = TRUE),
         n_reponses = sum(!is.na(c_across(starts_with("s08aq"))))) %>%
  ungroup() %>%
  filter(n_reponses >= 6) %>%
  select(grappe, menage, score_fies)

# ---- 4. Variables de pluriactivité ----
# 4.1. Producteur de mil (agriculture)
prod_mil <- b6 %>%
  filter(as.numeric(s16cq04) == 1 & !is.na(vague)) %>%
  distinct(grappe, menage) %>%
  mutate(producteur = 1L)

# 4.2. Élevage (au moins un animal)
elevage <- s17 %>%
  mutate(effectif = as.numeric(s17q03)) %>%
  group_by(grappe, menage) %>%
  summarise(a_elevage = if_else(sum(effectif, na.rm = TRUE) > 0, 1L, 0L),
            .groups = "drop")

# 4.3. Pluriactif = producteur ET élevage
pluriactif <- prod_mil %>%
  left_join(elevage, by = c("grappe", "menage")) %>%
  mutate(pluriactif = if_else(producteur == 1 & a_elevage == 1, 1L, 0L)) %>%
  select(grappe, menage, pluriactif, a_elevage)

# ---- 5. Variables de revenu agricole et commercialisation ----
revenu <- b16 %>%
  mutate(montant = as.numeric(s16dq06)) %>%
  group_by(grappe, menage) %>%
  summarise(revenu_agri = sum(montant, na.rm = TRUE), .groups = "drop")

taux_vente <- b16 %>%
  mutate(qte = as.numeric(s16dq05c)) %>%
  filter(!is.na(qte) & qte > 0) %>%
  distinct(grappe, menage) %>%
  mutate(a_vendu = 1L)

# ---- 6. Variables communautaires (instruments et proxy chocs) ----
irrigation <- qc_s3 %>%
  mutate(irrig = if_else(as.numeric(s03q17) == 1, 1L, 0L)) %>%
  select(grappe, irrig)

cooperative <- qc_s3 %>%
  mutate(coop = if_else(as.numeric(s03q03) == 1, 1L, 0L)) %>%
  select(grappe, coop)

# Distance au marché (proxy d'accès)
dist_marche <- qc_s2 %>%
  filter(as.numeric(s02q00) %in% c(14, 15)) %>%
  group_by(grappe) %>%
  summarise(temps_marche = min(as.numeric(s02q03), na.rm = TRUE), .groups = "drop")

# ---- 7. Variables de contrôle (welfare) ----
controles <- welfare %>%
  mutate(
    milieu_rural = if_else(as.numeric(milieu) == 2, 1L, 0L),
    educ_primaire = if_else(as.numeric(heduc) >= 3, 1L, 0L),
    log_pcexp = log(pcexp + 1)
  ) %>%
  select(grappe, menage, hhweight, region, hhsize, hage, hgender,
         milieu_rural, educ_primaire, log_pcexp)

# ---- 8. Fusion en une base unique ----
base_reg <- fies %>%
  left_join(pluriactif, by = c("grappe", "menage")) %>%
  left_join(revenu, by = c("grappe", "menage")) %>%
  left_join(taux_vente, by = c("grappe", "menage")) %>%
  left_join(irrigation, by = "grappe") %>%
  left_join(cooperative, by = "grappe") %>%
  left_join(dist_marche, by = "grappe") %>%
  left_join(controles, by = c("grappe", "menage")) %>%
  mutate(
    pluriactif = replace_na(pluriactif, 0L),
    a_elevage = replace_na(a_elevage, 0L),
    producteur = if_else(pluriactif == 1 | a_elevage == 1, 1L, 0L), # producteur si pluriactif ou élevage seul
    a_vendu = replace_na(a_vendu, 0L),
    revenu_agri = replace_na(revenu_agri, 0),
    log_revenu = log(revenu_agri + 1),
    log_temps_marche = log(temps_marche + 1)
  ) %>%
  filter(!is.na(hhweight) & !is.na(score_fies))

cat("Base de régression :", nrow(base_reg), "ménages.\n")

# ---- 9. MODÈLE PRINCIPAL : Effet de la pluriactivité sur FIES ----
modele_pluri <- lm(score_fies ~ pluriactif + producteur + log_revenu + a_vendu +
                     hhsize + hage + hgender + milieu_rural + educ_primaire +
                     log_pcexp + as.factor(region),
                   data = base_reg, weights = hhweight)

cat("\n=== MODÈLE 1 : Effet de la pluriactivité sur FIES ===\n")
print(summary(modele_pluri))

# ---- 10. MODÈLE AVEC INTERACTION : Pluriactivité × Irrigation (proxy mauvaises campagnes) ----
modele_interact <- lm(score_fies ~ pluriactif * irrig + 
                        producteur + log_revenu + a_vendu +
                        hhsize + hage + hgender + milieu_rural + educ_primaire +
                        log_pcexp + as.factor(region),
                      data = base_reg, weights = hhweight)

cat("\n=== MODÈLE 2 : Interaction pluriactivité × irrigation ===\n")
print(summary(modele_interact))

# ---- 11. MODÈLE AVEC INTERACTION : Pluriactivité × Coopérative ----
modele_coop <- lm(score_fies ~ pluriactif * coop + 
                    producteur + log_revenu + a_vendu +
                    hhsize + hage + hgender + milieu_rural + educ_primaire +
                    log_pcexp + as.factor(region),
                  data = base_reg, weights = hhweight)

cat("\n=== MODÈLE 3 : Interaction pluriactivité × coopérative ===\n")
print(summary(modele_coop))

# ---- 12. MODÈLE AVEC EFFET FIXE RÉGIONAL ET SANS variable producteur (robustesse) ----
modele_robuste <- lm(score_fies ~ pluriactif + a_elevage + log_revenu + a_vendu +
                       hhsize + hage + hgender + milieu_rural + educ_primaire +
                       log_pcexp + as.factor(region),
                     data = base_reg, weights = hhweight)

cat("\n=== MODÈLE 4 : Robustesse (sans producteur, avec élevage séparé) ===\n")
print(summary(modele_robuste))

# ---- 13. Graphique des coefficients ----
vars_interet <- c("pluriactif", "irrig", "producteur", "a_elevage", "log_revenu", "a_vendu")
coefs <- bind_rows(
  tidy(modele_pluri, conf.int = TRUE) %>% filter(term %in% vars_interet) %>% mutate(Modele = "Principal"),
  tidy(modele_interact, conf.int = TRUE) %>% filter(term %in% vars_interet) %>% mutate(Modele = "Interaction Irrig"),
  tidy(modele_coop, conf.int = TRUE) %>% filter(term %in% vars_interet) %>% mutate(Modele = "Interaction Coop"),
  tidy(modele_robuste, conf.int = TRUE) %>% filter(term %in% vars_interet) %>% mutate(Modele = "Robuste")
) %>%
  mutate(
    term = recode(term,
                  pluriactif = "Pluriactif",
                  irrig = "Irrigation",
                  producteur = "Producteur",
                  a_elevage = "Élevage",
                  log_revenu = "Log(Revenu agricole)",
                  a_vendu = "A vendu"),
    sig = if_else(p.value < 0.05, "p<0.05", "p>=0.05")
  )

p_coef <- ggplot(coefs, aes(x = term, y = estimate, color = Modele, shape = sig)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(title = "Effet des variables sur le score FIES",
       x = NULL, y = "Coefficient", color = "Modèle", shape = "Significativité") +
  theme_minimal()
print(p_coef)

# ---- 14. Sauvegarde des résultats ----
dir.create("figures", showWarnings = FALSE)

# Sauvegarde des modèles
saveRDS(modele_pluri, "figures/modele_pluri_principal.rds")
saveRDS(modele_interact, "figures/modele_pluri_irrig.rds")
saveRDS(modele_coop, "figures/modele_pluri_coop.rds")
saveRDS(modele_robuste, "figures/modele_pluri_robuste.rds")

# Sauvegarde du graphique
ggsave("figures/coef_pluriactivite.png", p_coef, width = 10, height = 6, dpi = 150)

# Sauvegarde de la base utilisée
write.csv(base_reg, "figures/base_pluriactivite.csv", row.names = FALSE)

# Sauvegarde des résumés dans un fichier texte
sink("figures/resultats_pluriactivite.txt")
cat("=== MODULE PLURIACTIVITÉ : RÉSULTATS ===\n\n")
cat("MODÈLE 1 : Effet principal\n")
print(summary(modele_pluri))
cat("\n\nMODÈLE 2 : Interaction avec irrigation\n")
print(summary(modele_interact))
cat("\n\nMODÈLE 3 : Interaction avec coopérative\n")
print(summary(modele_coop))
cat("\n\nMODÈLE 4 : Robustesse\n")
print(summary(modele_robuste))
sink()

cat("\n✔ Module pluriactivité terminé. Résultats sauvegardés dans figures/\n")
cat("   - figures/modele_pluri_principal.rds\n")
cat("   - figures/modele_pluri_irrig.rds\n")
cat("   - figures/modele_pluri_coop.rds\n")
cat("   - figures/modele_pluri_robuste.rds\n")
cat("   - figures/coef_pluriactivite.png\n")
cat("   - figures/base_pluriactivite.csv\n")
cat("   - figures/resultats_pluriactivite.txt\n")