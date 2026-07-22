# =============================================================================
# 00_load_data.R - Chargement centralisé des données EHCVM Niger 2021
# =============================================================================
library(haven)
library(dplyr)
install.packages("here")
library(here)

# --- Identification et géolocalisation (S00) ---
s00_CO <- read_dta(here("data", "s03_co_ner2021.dta")) 

s00 <- read_dta(here("data", "s00_me_ner2021.dta")) %>% 

  mutate(grappe = as.numeric(as.character(grappe))) %>%
  # Utiliser les colonnes GPS correctes
  select(grappe, menage, lat = GPS__Latitude, lon = GPS__Longitude) %>%
  # Garder une ligne par ménage (en cas de doublons)
  distinct(grappe, menage, .keep_all = TRUE)
# --- Bases Ménage ---
b2  <- read_dta(here("data", "ehcvm_conso_ner2021.dta"))      # Consommation / Welfare
b4  <- read_dta(here("data", "s07b_me_ner2021.dta"))          # Consommation alimentaire
b5  <- read_dta(here("data", "s16a_me_ner2021.dta"))          # Parcelles / Surfaces
b6  <- read_dta(here("data", "s16c_me_ner2021.dta"))          # Cultures / Rendements
b9  <- read_dta(here("data", "s16b_me_ner2021.dta"))          # Intrants
b16 <- read_dta(here("data", "s16d_me_ner2021.dta"))          # Commercialisation
s01 <- read_dta(here("data", "s01_me_ner2021.dta"))           # Démographie
s08 <- read_dta(here("data", "s08a_me_ner2021.dta"))          # FIES

# --- Bases Communautaires (Questionnaire grappe) ---
qc_s1 <- read_dta(here("data", "s01_co_ner2021.dta"))         # Général
qc_s2 <- read_dta(here("data", "s02_co_ner2021.dta"))         # Accès services (distances)
qc_s3 <- read_dta(here("data", "s03_co_ner2021.dta"))         # Agriculture / Coops / Irrigation
qc_s5 <- read_dta(here("data", "s04_co_ner2021.dta"))         # PRIX des marches locaux (QC-S5)
prix_communautaire= read_dta(here("data","ehcvm_prix_ner2021.dta"))
##Jointure de grappe dans prix_communautaires

# --- Poids et Welfare (pour régressions) ---
welfare <- read_dta(here("data", "ehcvm_welfare_ner2021.dta"))

# --- Nettoyage des types pour éviter les erreurs de jointure ---
convert_to_num <- function(df) {
  df %>% mutate(across(c(grappe, menage, vague), ~ as.numeric(as.character(.))))
}

b2  <- convert_to_num(b2)
b4  <- convert_to_num(b4)
b5  <- convert_to_num(b5)
b6  <- convert_to_num(b6)
b9  <- convert_to_num(b9)
b16 <- convert_to_num(b16)
s01 <- convert_to_num(s01)
s08 <- convert_to_num(s08)
welfare <- convert_to_num(welfare)

qc_s1 <- qc_s1 %>% mutate(grappe = as.numeric(as.character(grappe)))
qc_s2 <- qc_s2 %>% mutate(grappe = as.numeric(as.character(grappe)))
qc_s3 <- qc_s3 %>% mutate(grappe = as.numeric(as.character(grappe)))
qc_s5 <- qc_s5 %>% mutate(grappe = as.numeric(as.character(grappe)))
# --- Élevage (S17) ---
s17 <- read_dta(here("data", "s17_me_ner2021.dta")) %>%
  mutate(grappe = as.numeric(as.character(grappe)),
         menage = as.numeric(as.character(menage)))
# --- Uniformisation de la surface en hectares (Module 3) ---
if(!"surface_ha" %in% names(b5)){
  b5 <- b5 %>%
    mutate(surface_ha = case_when(
      as.numeric(s16aq09b) == 1 ~ as.numeric(s16aq09a),        # Hectare
      as.numeric(s16aq09b) == 2 ~ as.numeric(s16aq09a) / 100,  # Are
      TRUE ~ NA_real_
    ))
}
library(haven)
library(here)
