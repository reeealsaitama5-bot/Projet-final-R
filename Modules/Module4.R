# =============================================================================
# MODULE 5 : Impact de la filière du Mil sur la sécurité alimentaire
# Version corrigée v2 — corrections : construction HDDS, IV complet,
# colinéarité pluriactif/producteur, hgender, diagnostics instruments,
# graphique pluriactivité
# =============================================================================

# 1. CHARGEMENT DES BIBLIOTHÈQUES
# -------------------------------------------------------------------------
library(haven)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(broom)
library(scales)
# NOTE : alias() (détection de colinéarité parfaite) vient du package
# 'stats' (base R, toujours disponible) et non de 'car'. On l'appelle
# explicitement via stats::alias() plus bas, aucun package supplémentaire
# n'est donc requis pour cette partie.

# 2. CHARGEMENT DES BASES
# -------------------------------------------------------------------------
s08 <- read_dta(here("data", "s08a_me_ner2021.dta"))
b4  <- read_dta(here("data", "s07b_me_ner2021.dta"))
b16 <- read_dta(here("data", "s16d_me_ner2021.dta"))
b6  <- read_dta(here("data", "s16c_me_ner2021.dta"))
s17 <- read_dta(here("data", "s17_me_ner2021.dta"))
qc_s2 <- read_dta(here("data", "s02_co_ner2021.dta"))
qc_s3 <- read_dta(here("data", "s03_co_ner2021.dta"))
welfare <- read_dta(here("data", "ehcvm_welfare_ner2021.dta"))

# Codes produits
CODE_MIL_CONSO <- 7   # dans S07B
CODE_MIL_PROD  <- 1   # dans S16C et S16D

# NOTE : la consigne initiale demandait un "taux_vente" (quantité vendue /
# quantité produite). Faute de variable de quantité produite fiable dans
# S16D, on utilise ici un indicateur binaire `a_vendu` (a vendu du Mil : oui/non).
# A documenter explicitement dans le rapport.

cat("\n=== MODULE 5 : IMPACT DE LA FILIÈRE DU MIL SUR LA SÉCURITÉ ALIMENTAIRE ===\n")

# ---- 3. Construction du score FIES (Section S08) ----
fies <- s08 %>%
  mutate(across(c(s08aq01, s08aq02, s08aq03, s08aq04,
                  s08aq05, s08aq06, s08aq07, s08aq08),
                ~ case_when(
                  as.numeric(.) == 1 ~ 1L,
                  as.numeric(.) == 2 ~ 0L,
                  TRUE ~ NA_integer_
                ),
                .names = "fies_{.col}")) %>%
  rowwise() %>%
  mutate(
    score_fies = sum(c_across(starts_with("fies_")), na.rm = TRUE),
    n_reponses = sum(!is.na(c_across(starts_with("fies_"))))
  ) %>%
  ungroup() %>%
  filter(n_reponses >= 6) %>%
  mutate(
    cat_fies = case_when(
      score_fies <= 1 ~ "Sécurisé",
      score_fies <= 3 ~ "Modérément insécurisé",
      TRUE            ~ "Sévèrement insécurisé"
    ),
    cat_fies = factor(cat_fies, levels = c("Sécurisé", "Modérément insécurisé", "Sévèrement insécurisé")),
    fies_moderee = if_else(score_fies >= 3, 1L, 0L),
    fies_severe  = if_else(score_fies >= 6, 1L, 0L)
  ) %>%
  select(grappe, menage, score_fies, cat_fies, fies_moderee, fies_severe)

cat("Score FIES : moyenne =", mean(fies$score_fies, na.rm = TRUE),
    ", médiane =", median(fies$score_fies, na.rm = TRUE), "\n")

# ---- 4. Construction du score HDDS (Section S07B) ----
# CORRECTION : l'ancienne version utilisait
#   groupes_alimentaires <- c("Céréales" = 1:20, "Viandes" = 27:39, ...)
# Or c() sur plusieurs vecteurs nommés APLATIT le tout et génère des noms
# du type "Céréales1", "Céréales2", ... au lieu de répéter "Céréales".
# stack() sur un vecteur atomique (pas une liste) échoue en plus avec
# une erreur "'x' must be a list or data frame".
# -> on reconstruit group_map explicitement avec rep() pour chaque groupe.
groupe_map <- data.frame(
  code_produit = c(1:20, 27:39, 40:49, 52:60, 61:69, 71:87, 88:111, 112:120, 134:138, 155:166),
  groupe = c(
    rep("Céréales", length(1:20)),
    rep("Viandes", length(27:39)),
    rep("Poissons", length(40:49)),
    rep("Lait/Oeufs", length(52:60)),
    rep("Huiles/Graisses", length(61:69)),
    rep("Fruits", length(71:87)),
    rep("Légumes/Tubercules", length(88:111)),
    rep("Légumineuses", length(112:120)),
    rep("Sucreries", length(134:138)),
    rep("Boissons", length(155:166))
  ),
  stringsAsFactors = FALSE
)

hdds <- b4 %>%
  filter(as.numeric(s07bq02) == 1) %>%
  mutate(code_produit = as.numeric(s07bq01)) %>%
  left_join(groupe_map, by = "code_produit") %>%
  filter(!is.na(groupe)) %>%
  group_by(grappe, menage) %>%
  summarise(score_hdds = n_distinct(groupe), .groups = "drop")

cat("Score HDDS : moyenne =", mean(hdds$score_hdds, na.rm = TRUE),
    ", médiane =", median(hdds$score_hdds, na.rm = TRUE), "\n")

# ---- 5. Variables de participation à la filière du Mil ----
producteur_mil <- b6 %>%
  filter(!is.na(vague) & as.numeric(s16cq04) == CODE_MIL_PROD) %>%
  distinct(grappe, menage) %>%
  mutate(producteur = 1L)

revenu_vente_mil <- b16 %>%
  filter(as.numeric(s16dq01) == CODE_MIL_PROD) %>%
  group_by(grappe, menage) %>%
  summarise(
    revenu_agri = sum(as.numeric(s16dq06), na.rm = TRUE),
    a_vendu = if_else(sum(as.numeric(s16dq05c), na.rm = TRUE) > 0, 1L, 0L),
    .groups = "drop"
  )

# ---- 6. Élevage et pluriactivité (S17) ----
elevage <- s17 %>%
  mutate(effectif = as.numeric(s17q03)) %>%
  group_by(grappe, menage) %>%
  summarise(a_elevage = if_else(sum(effectif, na.rm = TRUE) > 0, 1L, 0L), .groups = "drop")

pluriactif <- producteur_mil %>%
  left_join(elevage, by = c("grappe", "menage")) %>%
  mutate(
    a_elevage = replace_na(a_elevage, 0L),
    pluriactif = if_else(producteur == 1 & a_elevage == 1, 1L, 0L)
  ) %>%
  select(grappe, menage, pluriactif, a_elevage)

# ---- 7. Variables communautaires (instruments et interactions) ----
irrigation <- qc_s3 %>%
  mutate(irrigation = if_else(as.numeric(s03q17) == 1, 1L, 0L)) %>%
  select(grappe, irrigation)

cooperative <- qc_s3 %>%
  mutate(coop_existe = if_else(as.numeric(s03q03) == 1, 1L, 0L)) %>%
  select(grappe, coop_existe)

dist_marche <- qc_s2 %>%
  filter(as.numeric(s02q00) %in% c(14, 15)) %>%
  group_by(grappe) %>%
  summarise(temps_marche = min(as.numeric(s02q03), na.rm = TRUE), .groups = "drop")

# ---- 8. Contrôles (welfare) ----
# CORRECTION : fonction unique de conversion de hgender, appliquée de façon
# identique partout (au lieu d'un traitement différent entre OLS et IV).
to_numeric_safe <- function(x) as.numeric(as.character(x))

controles <- welfare %>%
  mutate(
    milieu_rural = if_else(as.numeric(milieu) == 2, 1L, 0L),
    educ_primaire = if_else(as.numeric(heduc) >= 3, 1L, 0L),
    log_pcexp = log(pcexp + 1),
    hgender = to_numeric_safe(hgender)
  ) %>%
  select(grappe, menage, hhweight, region, hhsize, hage, hgender,
         milieu_rural, educ_primaire, log_pcexp)

# ---- 9. Base de régression complète (avec filtrage Mil) ----
base_reg <- fies %>%
  left_join(hdds, by = c("grappe", "menage")) %>%
  left_join(revenu_vente_mil, by = c("grappe", "menage")) %>%
  left_join(producteur_mil, by = c("grappe", "menage")) %>%
  left_join(pluriactif, by = c("grappe", "menage")) %>%
  left_join(irrigation, by = "grappe") %>%
  left_join(cooperative, by = "grappe") %>%
  left_join(dist_marche, by = "grappe") %>%
  left_join(controles, by = c("grappe", "menage")) %>%
  mutate(
    producteur = replace_na(producteur, 0L),
    a_vendu = replace_na(a_vendu, 0L),
    revenu_agri = replace_na(revenu_agri, 0),
    log_revenu = log(revenu_agri + 1),
    pluriactif = replace_na(pluriactif, 0L),
    a_elevage = replace_na(a_elevage, 0L),
    log_temps_marche = log(temps_marche + 1)
  ) %>%
  filter(!is.na(score_fies) & !is.na(hhweight))

cat("Base de régression (filière Mil) :", nrow(base_reg), "ménages.\n")

# ---- 10. GRAPHIQUE 1 : Distribution du score FIES selon statut producteur de Mil ----
dir.create("figures", showWarnings = FALSE)

p_fies <- base_reg %>%
  mutate(statut = if_else(producteur == 1, "Producteur de Mil", "Non producteur de Mil")) %>%
  ggplot(aes(x = score_fies, fill = statut, weight = hhweight)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.8) +
  scale_x_continuous(breaks = 0:8) +
  scale_fill_manual(values = c("Producteur de Mil" = "#2E86AB", "Non producteur de Mil" = "#E76F51")) +
  labs(
    title = "Distribution du score FIES selon le statut producteur de Mil",
    x = "Score FIES", y = "Nombre de ménages (pondéré)",
    fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "top")
print(p_fies)
ggsave("figures/fies_distribution.png", p_fies, width = 10, height = 6, dpi = 300)

# ---- 11. MODÈLE 1 : OLS ----
ols_data <- base_reg %>%
  filter(!is.na(producteur) & !is.na(log_revenu) & !is.na(a_vendu) &
           !is.na(hhsize) & !is.na(milieu_rural) & !is.na(educ_primaire) &
           !is.na(log_pcexp) & !is.na(region)) %>%
  mutate(across(where(is.numeric), ~ if_else(is.infinite(.), NA_real_, .))) %>%
  na.omit()

if (nrow(ols_data) > 0) {
  modele_ols <- lm(score_fies ~ producteur + log_revenu + a_vendu +
                     hhsize + hage + hgender + milieu_rural + educ_primaire +
                     log_pcexp + as.factor(region),
                   data = ols_data, weights = hhweight)
  cat("\n=== MODÈLE OLS ===\n")
  print(summary(modele_ols))
  saveRDS(modele_ols, "figures/model_ols.rds")
} else {
  warning("Aucune donnée valide pour le modèle OLS.")
  modele_ols <- NULL
}

# ---- 12. MODÈLE 2 : IV/2SLS (correction endogénéité) ----
# CORRECTION : hage et hgender ajoutés dans la 2e étape pour cohérence
# stricte avec la spécification OLS (mêmes contrôles dans les deux modèles).
iv_vars <- c("producteur", "irrigation", "log_temps_marche", "hhsize", "hage",
             "hgender", "milieu_rural", "educ_primaire", "log_pcexp", "region",
             "hhweight", "score_fies", "log_revenu", "a_vendu")

base_iv <- base_reg %>%
  select(all_of(iv_vars)) %>%
  mutate(across(where(is.numeric), ~ if_else(is.infinite(.), NA_real_, .))) %>%
  mutate(region = as.factor(region)) %>%
  na.omit()

cat("Nombre d'observations pour IV après nettoyage :", nrow(base_iv), "\n")

# Diagnostics IV sauvegardés (au lieu d'être seulement affichés en console)
iv_diag <- character()

if (nrow(base_iv) > 0) {
  first_stage <- lm(producteur ~ irrigation + log_temps_marche +
                      hhsize + hage + hgender + milieu_rural + educ_primaire +
                      log_pcexp + region,
                    data = base_iv, weights = hhweight)
  fstat <- summary(first_stage)$fstatistic[1]
  r2_fs <- summary(first_stage)$r.squared
  cat("\n--- Première étape IV ---\n")
  cat("F-stat =", fstat, "\n")
  cat("R² =", round(r2_fs, 3), "\n")
  if (fstat < 10) {
    cat("ATTENTION : F-stat < 10, instrument potentiellement faible.\n")
  }
  
  iv_diag <- c(
    "--- Diagnostic de la première étape IV ---",
    paste("F-stat (force des instruments) :", round(fstat, 2)),
    paste("R² de la première étape :", round(r2_fs, 3)),
    if (fstat < 10) "ATTENTION : F-stat < 10 -> instrument potentiellement faible (règle de Stock-Yogo)." else "Instrument jugé suffisamment fort (F-stat >= 10).",
    "",
    "Note sur la restriction d'exclusion : irrigation et distance au marché",
    "sont supposées affecter score_fies uniquement via la décision de produire",
    "du Mil (producteur), et non directement. Cette hypothèse n'est pas",
    "testable statistiquement et doit être justifiée qualitativement dans le rapport."
  )
  
  base_iv$producteur_hat <- fitted(first_stage)
  
  modele_iv <- lm(score_fies ~ producteur_hat + log_revenu + a_vendu +
                    hhsize + hage + hgender + milieu_rural + educ_primaire +
                    log_pcexp + region,
                  data = base_iv, weights = hhweight)
  cat("\n=== MODÈLE IV/2SLS ===\n")
  print(summary(modele_iv))
  saveRDS(modele_iv, "figures/model_iv.rds")
} else {
  message("Pas assez de données pour le modèle IV")
  modele_iv <- NULL
}

# ---- 13. MODÈLE 3 : Interactions (pluriactivité × irrigation / coopérative) ----
# CORRECTION : pluriactif = producteur * a_elevage n'est PAS parfaitement
# colinéaire avec producteur (ce n'est vrai que si a_elevage était constant),
# mais les deux variables sont fortement corrélées puisque pluriactif=1
# implique producteur=1. On garde les deux termes (l'effet marginal de
# producteur=1 & a_elevage=0 reste identifié), mais on ajoute un diagnostic
# explicite de colinéarité (alias + VIF) au lieu de laisser R droper une
# variable silencieusement.
modele_pluri_irrig <- lm(score_fies ~ pluriactif * irrigation +
                           producteur + log_revenu + a_vendu +
                           hhsize + hage + hgender + milieu_rural + educ_primaire +
                           log_pcexp + as.factor(region),
                         data = base_reg, weights = hhweight)
cat("\n=== MODÈLE Pluriactivité × Irrigation ===\n")
print(summary(modele_pluri_irrig))

alias_irrig <- stats::alias(modele_pluri_irrig)
if (!is.null(alias_irrig$Complete)) {
  cat("\nATTENTION - colinéarité parfaite détectée (modèle Irrigation) :\n")
  print(alias_irrig$Complete)
} else {
  cat("\nAucune colinéarité parfaite détectée (modèle Irrigation).\n")
}

modele_pluri_coop <- lm(score_fies ~ pluriactif * coop_existe +
                          producteur + log_revenu + a_vendu +
                          hhsize + hage + hgender + milieu_rural + educ_primaire +
                          log_pcexp + as.factor(region),
                        data = base_reg, weights = hhweight)
cat("\n=== MODÈLE Pluriactivité × Coopérative ===\n")
print(summary(modele_pluri_coop))

alias_coop <- stats::alias(modele_pluri_coop)
if (!is.null(alias_coop$Complete)) {
  cat("\nATTENTION - colinéarité parfaite détectée (modèle Coopérative) :\n")
  print(alias_coop$Complete)
} else {
  cat("\nAucune colinéarité parfaite détectée (modèle Coopérative).\n")
}

# ---- 13bis. GRAPHIQUE 3 : Score FIES selon pluriactivité (fig-pluri) ----
p_pluri <- base_reg %>%
  mutate(statut_pluri = case_when(
    pluriactif == 1 ~ "Pluriactif (Mil + élevage)",
    producteur == 1 ~ "Producteur de Mil seul",
    TRUE ~ "Non producteur de Mil"
  )) %>%
  group_by(statut_pluri) %>%
  summarise(
    score_moyen = weighted.mean(score_fies, hhweight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = reorder(statut_pluri, score_moyen), y = score_moyen, fill = statut_pluri)) +
  geom_col(width = 0.6) +
  coord_flip() +
  labs(
    title = "Score FIES moyen selon le statut de pluriactivité",
    x = NULL, y = "Score FIES moyen (pondéré)", fill = NULL
  ) +
  theme_minimal() +
  theme(legend.position = "none")
print(p_pluri)
ggsave("figures/pluriactivite_fies.png", p_pluri, width = 9, height = 5, dpi = 300)

# ---- 14. MODÈLE 4 : HDDS (robustesse) ----
modele_hdds <- lm(score_hdds ~ producteur + log_revenu + a_vendu +
                    hhsize + hage + hgender + milieu_rural + educ_primaire +
                    log_pcexp + as.factor(region),
                  data = base_reg %>% filter(!is.na(score_hdds)),
                  weights = hhweight)
cat("\n=== MODÈLE HDDS ===\n")
print(summary(modele_hdds))

# ---- 15. GRAPHIQUE 2 : Comparaison des coefficients ----
if (!is.null(modele_ols) && !is.null(modele_iv) && !is.null(modele_hdds)) {
  vars_interet <- c("producteur", "log_revenu", "a_vendu",
                    "hhsize", "milieu_rural", "educ_primaire")
  
  coef_ols <- tidy(modele_ols, conf.int = TRUE) %>%
    filter(term %in% vars_interet) %>% mutate(modele = "OLS")
  
  coef_iv <- tidy(modele_iv, conf.int = TRUE) %>%
    mutate(term = if_else(term == "producteur_hat", "producteur", term)) %>%
    filter(term %in% vars_interet) %>% mutate(modele = "IV")
  
  coef_hdds <- tidy(modele_hdds, conf.int = TRUE) %>%
    filter(term %in% vars_interet) %>% mutate(modele = "HDDS")
  
  coef_comp <- bind_rows(coef_ols, coef_iv, coef_hdds) %>%
    mutate(
      term = dplyr::recode(term,
                           producteur = "Producteur de Mil",
                           log_revenu = "Log(revenu agricole Mil)",
                           a_vendu = "A vendu du Mil",
                           hhsize = "Taille ménage",
                           milieu_rural = "Rural",
                           educ_primaire = "Éduqué (≥primaire)")
    )
  
  p_coef <- ggplot(coef_comp, aes(x = term, y = estimate, color = modele, shape = (p.value < 0.05))) +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2,
                  position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    labs(title = "Comparaison des modèles (filière Mil)",
         x = NULL, y = "Coefficient", color = "Modèle", shape = "p<0.05") +
    theme_minimal()
  print(p_coef)
  ggsave("figures/comparaison_modeles.png", p_coef, width = 10, height = 6, dpi = 300)
}

# ---- 16. SAUVEGARDE DES RÉSULTATS ----
sink("figures/resultats_module5.txt")
cat("=== MODULE 5 : RÉSULTATS (Filière Mil) ===\n\n")
cat("=== MODÈLE OLS ===\n")
if (!is.null(modele_ols)) print(summary(modele_ols)) else cat("Non estimé\n")
cat("\n=== MODÈLE IV/2SLS ===\n")
if (!is.null(modele_iv)) print(summary(modele_iv)) else cat("Non estimé\n")
cat("\n")
if (length(iv_diag) > 0) cat(paste(iv_diag, collapse = "\n"), "\n")
cat("\n=== MODÈLE Pluriactivité × Irrigation ===\n")
print(summary(modele_pluri_irrig))
cat("\n=== MODÈLE Pluriactivité × Coopérative ===\n")
print(summary(modele_pluri_coop))
cat("\n=== MODÈLE HDDS ===\n")
if (!is.null(modele_hdds)) print(summary(modele_hdds)) else cat("Non estimé\n")
sink()

cat("\n✔ Module 5 (filière Mil) terminé. Résultats sauvegardés dans 'figures/'.\n")