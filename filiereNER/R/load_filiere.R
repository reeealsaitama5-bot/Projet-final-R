#' Charger les bases de données de l'EHCVM Niger
#'
#' @param data_dir Le chemin du dossier contenant les fichiers `.dta`.
#' Par défaut, `here("data")`.
#'
#' @return Une liste nommée contenant les 9 bases de données (b1 à b9).
#' @export
load_filiere <- function(data_dir = here("data")) {
  library(haven)
  library(here)

  liste_noms_fichiers <- c(
    "calorie_conversion_wa_2021.dta", # -> b1
    "ehcvm_conso_ner2021.dta",        # -> b2
    "ehcvm_prix_ner2021.dta",         # -> b3
    "s07b_me_ner2021.dta",            # -> b4
    "s16a_me_ner2021.dta",            # -> b5
    "s16c_me_ner2021.dta",            # -> b6
    "s16d_me_ner2021.dta",            # -> b7
    "s17_me_ner2021.dta",             # -> b8
    "s16b_me_ner2021.dta"             # -> b9
  )

  bases <- list()
  for (i in seq_along(liste_noms_fichiers)) {
    nom_fichier <- liste_noms_fichiers[i]
    chemin <- file.path(data_dir, nom_fichier)
    nom_objet <- paste0("b", i)

    if (file.exists(chemin)) {
      bases[[nom_objet]] <- read_dta(chemin)
    } else {
      warning("Fichier introuvable :", chemin)
    }
  }
  return(bases)
}
