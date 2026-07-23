#' Calculer le score de diversité alimentaire (HDDS)
#'
#' @param data Le dataframe issu de la base S7B (ex: b4).
#' @return Un dataframe avec les colonnes `grappe`, `menage` et `score_hdds`.
#' @export
calc_hdds <- function(data) {
  library(dplyr)

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

  result <- data %>%
    mutate(grappe = as.numeric(grappe), menage = as.numeric(menage)) %>%
    filter(as.numeric(s07bq02) == 1) %>%
    mutate(code_produit = as.numeric(s07bq01)) %>%
    left_join(groupe_map, by = "code_produit") %>%
    filter(!is.na(groupe)) %>%
    group_by(grappe, menage) %>%
    summarise(score_hdds = n_distinct(groupe), .groups = "drop")

  return(result)
}
