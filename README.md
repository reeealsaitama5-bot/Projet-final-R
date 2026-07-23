# Étude de la Filière du Mil et Sécurité Alimentaire au Niger
**Projet Final — Traitement Statistique avec R**  
**ENSAE Pierre Ndiaye — ISEP 2 — Année académique 2025–2026**

**Auteurs :** Thierno BOCOUM & Ndeye Khoudia DIOP  
**Superviseur :** M. Mouhamadou Hady Diallo, Ingénieur des Travaux Statistiques  
**Données :** Enquête Harmonisée sur les Conditions de Vie des Ménages (EHCVM) — Niger 2021

---

## Objectif du projet

Ce projet analyse la filière du **Mil** (*Pennisetum glaucum*) au Niger à partir des données EHCVM 2021, en couvrant l'ensemble de la chaîne : justification du choix du produit, profilage des ménages, rendements agricoles, commercialisation et prix, et impact sur la sécurité alimentaire.

---

## Structure du projet

```
FINALE/
│
├── README.md                        # Ce fichier
├── FINALE.Rproj                     # Projet RStudio (ouvrir en premier)
├── .Rhistory                        # Historique des commandes R
├── Rapport-final.log                # Journal de compilation du rapport
│
├── app.R                            # Application Shiny (dashboard interactif)
│
├── Module1.R                        # Script Module 1 : Choix du Mil
├── Module4.R                        # Script Module 4 : Commercialisation et prix
│
├── base_analyse_module2.rds         # Base fusionnée pour le Module 2
│
├── data/                            # Données brutes EHCVM Niger 2021
│   ├── s00_me_ner2021.dta           # Coordonnées GPS des grappes
│   ├── s01_me_ner2021.dta           # Caractéristiques sociodémographiques
│   ├── s07b_me_ner2021.dta          # Consommation alimentaire (7 jours)
│   ├── s08a_me_ner2021.dta          # Sécurité alimentaire (FIES)
│   ├── s16a_me_ner2021.dta          # Superficies des parcelles
│   ├── s16b_me_ner2021.dta          # Coûts des intrants agricoles
│   ├── s16c_me_ner2021.dta          # Cultures pratiquées et rendements
│   ├── s16d_me_ner2021.dta          # Utilisation de la production (ventes)
│   ├── s17_me_ner2021.dta           # Élevage (effectifs du cheptel)
│   ├── s01_co_ner2021.dta           # Questionnaire communautaire S1
│   ├── s02_co_ner2021.dta           # QC-S2 : Infrastructures et distances
│   ├── s03_co_ner2021.dta           # QC-S3 : Agriculture communautaire
│   ├── ehcvm_conso_ner2021.dta      # Agrégat de consommation
│   └── ehcvm_welfare_ner2021.dta    # Agrégat de bien-être (hhweight, pcexp)
│
├── Modules/                         # Scripts R par module analytique
│   ├── 00_load_data.R               # Chargement centralisé de toutes les bases
│   ├── Module2.R                    # Profilage des ménages (typologie)
│   ├── Module3.R                    # Rendements agricoles du Mil
│   ├── Module4.R                    # Commercialisation, prix et marges
│   └── Module5.R                    # Sécurité alimentaire et impact filière
│
├── figures/                         # Outputs graphiques et résultats
│   ├── SEN.jpg                      # Logo République du Sénégal
│   ├── ANSD.png                     # Logo ANSD
│   ├── ENSAE.png                    # Logo ENSAE
│   │
│   ├── — Module 1 —
│   ├── 1_frequence_consommation.png # Fréquence de consommation par produit
│   ├── 2_surface_agricole.png       # Surface agricole par culture
│   ├── 4_revenu_agricole.png        # Revenu agricole par culture
│   │
│   ├── — Module 2 —
│   ├── fies_score_par_groupe.png    # Score FIES par groupe de ménages
│   │
│   ├── — Module 3 —
│   ├── histogramme_rendement_mil.png # Distribution des rendements
│   ├── carnet_conversion_module3.csv # Carnet de conversion des unités locales
│   │
│   ├── — Module 4 —
│   ├── canaux_vente.png             # Canaux de commercialisation (pondéré)
│   ├── methodes_stockage.png        # Méthodes de stockage (pondéré)
│   ├── carte_marges.png             # Carte des marges par grappe
│   ├── prix_producteur_vs_marche.png # Comparaison prix producteur/marché
│   ├── regression_prix_producteur.txt # Résultats régression prix producteur
│   ├── model_prix.rds               # Modèle régression prix (objet R)
│   ├── prix_par_grappe.rds          # Base prix/marges par grappe (objet R)
│   ├── prix_par_grappe.csv          # Base prix/marges par grappe (CSV)
│   ├── marge_par_region.csv         # Marges commerciales par région (CSV)
│   │
│   └── — Module 5 —
│       ├── fies_distribution.png    # Distribution FIES par statut producteur
│       ├── comparaison_modeles.png  # Comparaison coefficients OLS/IV/HDDS
│       ├── coef_pluriactivite.png   # Effet de la pluriactivité sur le FIES
│       ├── model_iv.rds             # Modèle IV/2SLS (objet R)
│       └── resultats_module5.txt    # Résultats des 4 régressions Module 5
│
├── Rapport/                         # Rapport final (PDF et sources)
│   ├── Rapport-final.Rmd            # Source RMarkdown du rapport
│   ├── Rapport-final.pdf            # Rapport compilé (PDF)
│   └── presentation.tex            # Présentation Beamer (LaTeX)
│
├── App/                             # Application Shiny
│   └── (fichiers de l'application)
│
├── filiereNER/                      # Dossier de travail intermédiaire
│
├── rsconnect/                       # Configuration déploiement Shiny
│
└── Binome5/                         # Dossier partagé binôme
```

---

## Comment reproduire les analyses

### 1. Prérequis

```r
# Packages nécessaires
install.packages(c(
  "haven", "here", "dplyr", "ggplot2", "scales", "forcats",
  "tidyr", "broom", "knitr", "kableExtra", "maps", "sf"
))
```

### 2. Ordre d'exécution des scripts

```
1. Ouvrir FINALE.Rproj dans RStudio
2. Exécuter Modules/00_load_data.R   → charge toutes les bases
3. Exécuter Modules/Module1.R                → justification du choix du Mil
4. Exécuter Modules/Module2.R        → profilage des ménages
5. Exécuter Modules/Module3.R        → rendements agricoles
6. Exécuter Modules/Module4.R                → commercialisation et prix
7. Exécuter Modules/Module5.R        → sécurité alimentaire
8. Compiler Rapport/Rapport-final.Rmd → générer le PDF
```

### 3. Compiler le rapport

```r
rmarkdown::render("Rapport/Rapport-final.Rmd", output_format = "pdf_document")
```

---

## Description des modules

| Module | Script | Objectif principal |
|--------|--------|--------------------|
| **1** | `Modules/Module1.R` | Justifier empiriquement le choix du Mil comme produit stratégique |
| **2** | `Modules/Module2.R` | Profiler les ménages selon leur statut producteur/consommateur |
| **3** | `Modules/Module3.R` | Calculer les rendements du Mil et identifier leurs déterminants |
| **4** | `Modules/Module4.R` | Analyser la commercialisation, les prix et les marges commerciales |
| **5** | `Modules/Module5.R` | Tester l'impact de la filière sur la sécurité alimentaire (IV/2SLS) |

---

## Notes méthodologiques importantes

- **Pondération :** Toutes les statistiques et régressions sont pondérées par `hhweight`
- **Filtre Mil :** Code culture = 1 (S16C/S16D) ; code produit consommation = 7 (S07B)
- **Score FIES :** Construit sur 8 items (s08aq01–s08aq08) ; ménages avec < 6 réponses exclus
- **Winsorisation :** Appliquée aux prix producteurs (P1/P99) pour éliminer les valeurs aberrantes
- **Endogénéité :** Corrigée par IV/2SLS — instruments : irrigation (s03q17) + distance marché (QC-S2)
- **Décalage temporel :** FIES = 12 mois glissants ≠ production S16 = campagne agricole en cours

---

## Contact

**Thierno BOCOUM** | **Ndeye Khoudia DIOP**  
Élèves Ingénieurs Statisticiens Économistes — ENSAE Pierre Ndiaye  
Promotion ISEP 2 — Année académique 2025–2026
