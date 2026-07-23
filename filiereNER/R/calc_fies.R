#' Calculer le score FIES (Food Insecurity Experience Scale)
#'
#' @param data Le dataframe issu de la base S08 du Niger.
#' @return Un dataframe avec les colonnes `grappe`, `menage`, `fies_score`,
#' `fies_moderee` et `fies_severe`.
#' @export
calc_fies <- function(data) {
  library(dplyr)
  library(tidyr)

  fies_cols <- c("s08aq01", "s08aq02", "s08aq03", "s08aq04",
                 "s08aq05", "s08aq06", "s08aq07", "s08aq08")

  if (!all(fies_cols %in% names(data))) {
    stop("Erreur : Les colonnes FIES standard ne sont pas toutes présentes.")
  }

  convert_fies <- function(col) {
    vals_nums <- suppressWarnings(as.numeric(col))
    if (!all(is.na(vals_nums))) {
      return(ifelse(vals_nums == 1, 1, 0))
    } else {
      return(ifelse(col == "Oui" | col == "1", 1, 0))
    }
  }

  result <- data %>%
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

  return(result)
}
