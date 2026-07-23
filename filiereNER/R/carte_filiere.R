#' Cartographier un indicateur de la filière par grappe
#'
#' Génère une carte (ggplot2) des rendements moyens ou des marges par grappe
#' en utilisant les coordonnées GPS (lat/lon).
#'
#' @param data_par_grappe Dataframe contenant les colonnes `lat`, `lon`, et l'indicateur.
#' @param indicateur Le nom de la colonne à cartographier (ex: `"rendement_moyen_grappe"`).
#' @param titre Le titre de la carte.
#'
#' @return Un objet ggplot.
#' @export
carte_filiere <- function(data_par_grappe, indicateur, titre = "Carte de la filière") {

  library(ggplot2)
  library(scales)

  # Vérification des colonnes
  if (!all(c("lat", "lon", indicateur) %in% names(data_par_grappe))) {
    stop("Erreur : Le dataframe doit contenir les colonnes 'lat', 'lon' et l'indicateur spécifié.")
  }

  p <- ggplot(data_par_grappe, aes(x = lon, y = lat, color = .data[[indicateur]])) +
    geom_point(size = 3, alpha = 0.6) +
    scale_color_viridis_c(option = "plasma", label = comma) +
    labs(title = titre, x = "Longitude", y = "Latitude", color = indicateur) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "bottom"
    )

  return(p)
}
