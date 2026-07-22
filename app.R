# =============================================================================
# app.R - Dashboard Étude de la Filière Mil - Niger
# Version corrigée avec bslib::value_box (pas de shinydashboard nécessaire)
# =============================================================================

library(shiny)
library(bslib)          # pour value_box
library(ggplot2)
library(dplyr)
library(tidyr)
library(haven)
library(here)
library(leaflet)
library(viridis)

# --- 0. Répertoire racine ---
root_dir <- if (requireNamespace("here", quietly = TRUE)) here::here() else getwd()

# --- 1. Chargement des données ---
load_data <- function() {
  data <- list()
  
  # Images pour l'onglet 1
  data$img_conso <- file.path(root_dir, "figures", "1_frequence_consommation.png")
  data$img_surface <- file.path(root_dir, "figures", "2_surface_agricole.png")
  data$img_revenu <- file.path(root_dir, "figures", "4_revenu_agricole.png")
  
  # Base Module 2
  base_path <- file.path(root_dir, "figures", "base_analyse_module2.rds")
  if (file.exists(base_path)) {
    df <- readRDS(base_path)
    df <- df %>% mutate(across(where(haven::is.labelled), haven::as_factor))
    # Quintiles de consommation (par tête)
    df <- df %>%
      mutate(quintile_consommation = ntile(conso_par_tete, 5))
    data$base <- df
  } else {
    data$base <- NULL
  }
  
  # Rendements Module 3
  rend_path <- file.path(root_dir, "figures", "rendement_par_grappe.csv")
  if (file.exists(rend_path)) {
    data$rendement <- read.csv(rend_path)
    # Tentative de récupération des coordonnées depuis prix si disponibles
    if (!("lat" %in% names(data$rendement)) | !("lon" %in% names(data$rendement))) {
      prix_path <- file.path(root_dir, "figures", "prix_par_grappe.rds")
      if (file.exists(prix_path)) {
        prix <- readRDS(prix_path)
        if ("lat" %in% names(prix) & "lon" %in% names(prix)) {
          data$rendement <- data$rendement %>%
            left_join(prix %>% select(grappe, lat, lon), by = "grappe")
        }
      }
    }
  } else {
    data$rendement <- NULL
  }
  
  # Prix Module 4
  prix_path <- file.path(root_dir, "figures", "prix_par_grappe.rds")
  if (file.exists(prix_path)) {
    data$prix <- readRDS(prix_path)
    if ("region" %in% names(data$prix) && haven::is.labelled(data$prix$region)) {
      data$prix$region <- haven::as_factor(data$prix$region)
    }
  } else {
    data$prix <- NULL
  }
  
  # Modèles pour l'onglet 5
  model_ols_path <- file.path(root_dir, "figures", "model_ols.rds")
  if (file.exists(model_ols_path)) {
    data$model_ols <- readRDS(model_ols_path)
  } else {
    data$model_ols <- NULL
  }
  
  model_iv_path <- file.path(root_dir, "figures", "model_iv.rds")
  if (file.exists(model_iv_path)) {
    data$model_iv <- readRDS(model_iv_path)
  } else {
    data$model_iv <- NULL
  }
  
  model_hdds_path <- file.path(root_dir, "figures", "model_hdds.rds")
  if (file.exists(model_hdds_path)) {
    data$model_hdds <- readRDS(model_hdds_path)
  } else {
    data$model_hdds <- NULL
  }
  
  return(data)
}

data <- load_data()

# --- 2. Interface utilisateur ---
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("Étude de la Filière du Mil - Niger (EHCVM 2021)"),
  tabsetPanel(
    # ---- Onglet 1 : Importance stratégique ----
    tabPanel(
      "1. Importance stratégique",
      fluidRow(
        column(4,
               h4("Fiche produit"),
               selectInput("produit_compare", "Comparer avec :",
                           choices = c("Mil", "Niébé", "Arachide", "Sorgho"), selected = "Mil")
        ),
        column(8,
               h4("Indicateurs clés"),
               fluidRow(
                 uiOutput("vbox_conso"),
                 uiOutput("vbox_surface"),
                 uiOutput("vbox_revenu")
               )
        )
      ),
      fluidRow(
        column(6, h4("Fréquence de consommation"), imageOutput("img_conso", height = "auto")),
        column(6, h4("Surface agricole"), imageOutput("img_surface", height = "auto"))
      ),
      fluidRow(
        column(6, h4("Revenu agricole"), imageOutput("img_revenu", height = "auto")),
        column(6, h4("Balance commerciale FAO"), 
               p("Données FAO : importations nettes de céréales (tonnes) - à compléter"))
      )
    ),
    # ---- Onglet 2 : Profil des ménages ----
    tabPanel(
      "2. Profil des ménages",
      fluidRow(
        column(3,
               h4("Filtres"),
               selectInput("region_profil", "Région",
                           choices = c("Toutes", if (!is.null(data$base)) levels(data$base$region) else NULL)),
               selectInput("milieu_profil", "Milieu",
                           choices = c("Tous", "Urbain", "Rural")),
               selectInput("quintile_profil", "Quintile de consommation",
                           choices = c("Tous", "1 (plus pauvre)", "2", "3", "4", "5 (plus riche)"))
        ),
        column(9,
               h4("Répartition des 4 groupes"),
               plotOutput("plot_profil_groupes"),
               h4("Statistiques comparatives"),
               tableOutput("table_profil_stats")
        )
      )
    ),
    # ---- Onglet 3 : Production et rendements ----
    tabPanel(
      "3. Production et rendements",
      fluidRow(
        column(3,
               h4("Paramètres"),
               sliderInput("winsor_pct", "Winsorisation (percentile)",
                           min = 0, max = 5, value = 1, step = 0.5),
               selectInput("culture_rend", "Culture",
                           choices = c("Mil", "Sorgho", "Niébé"), selected = "Mil"),
               checkboxInput("overlay_pluvio", "Superposer les pluies (CHIRPS)", value = FALSE)
        ),
        column(9,
               h4("Histogramme des rendements"),
               plotOutput("plot_rend_hist"),
               h4("Carte des rendements par grappe"),
               leafletOutput("map_rendement")
        )
      )
    ),
    # ---- Onglet 4 : Chaîne des prix ----
    tabPanel(
      "4. Chaîne des prix",
      fluidRow(
        column(3,
               h4("Filtres"),
               sliderInput("dist_marche", "Distance au marché (minutes)",
                           min = 0, max = 120, value = c(0, 60)),
               selectInput("type_acheteur", "Type d'acheteur",
                           choices = c("Tous", "Marché", "Particulier", "Coopérative", "Opérateur")),
               selectInput("saison", "Saison",
                           choices = c("Toutes", "Post-récolte", "Soudure", "Hivernage"))
        ),
        column(9,
               h4("Carte des prix producteurs"),
               leafletOutput("map_prix_prod"),
               h4("Gradient de marge"),
               plotOutput("plot_marge_gradient")
        )
      )
    ),
    # ---- Onglet 5 : Sécurité alimentaire ----
    tabPanel(
      "5. Sécurité alimentaire",
      fluidRow(
        column(3,
               h4("Options"),
               selectInput("outcome_choice", "Indicateur",
                           choices = c("FIES", "HDDS"), selected = "FIES"),
               selectInput("groupe_sa", "Catégorie de ménage",
                           choices = c("Tous", if (!is.null(data$base)) levels(data$base$groupe_typologie) else NULL)),
               checkboxInput("show_coef", "Afficher les coefficients des modèles", value = TRUE)
        ),
        column(9,
               h4("Score par groupe"),
               plotOutput("plot_sa_groupes"),
               h4("Carte de la sécurité alimentaire"),
               leafletOutput("map_sa"),
               conditionalPanel(
                 condition = "input.show_coef == true",
                 h4("Coefficients des modèles (OLS, IV, HDDS)"),
                 plotOutput("plot_coef_models")
               )
        )
      )
    )
  )
)

# --- 3. Serveur ---
server <- function(input, output, session) {
  
  # ---- Fonctions utilitaires ----
  render_image <- function(path) {
    renderImage({
      if (file.exists(path)) {
        list(src = path, contentType = "image/png", width = "100%")
      } else {
        tmpfile <- tempfile(fileext = ".png")
        png(tmpfile, width = 400, height = 300)
        plot(0, 0, type = "n", main = "Image non trouvée", axes = FALSE)
        text(0, 0, paste("Fichier introuvable :", basename(path)), cex = 0.8)
        dev.off()
        list(src = tmpfile, contentType = "image/png", width = "100%")
      }
    }, deleteFile = TRUE)
  }
  
  has_coords <- function(df) {
    if (is.null(df)) return(FALSE)
    if ("lat" %in% names(df) & "lon" %in% names(df)) {
      return(sum(!is.na(df$lat) & !is.na(df$lon)) > 0)
    }
    return(FALSE)
  }
  
  # ---- Onglet 1 ----
  output$img_conso <- render_image(data$img_conso)
  output$img_surface <- render_image(data$img_surface)
  output$img_revenu <- render_image(data$img_revenu)
  
  output$vbox_conso <- renderUI({
    value_box(
      title = "Ménages consommant le Mil",
      value = "79,2 %",
      showcase = icon("utensils"),
      theme_color = "primary"
    )
  })
  output$vbox_surface <- renderUI({
    value_box(
      title = "Superficie agricole occupée",
      value = "51,8 %",
      showcase = icon("tractor"),
      theme_color = "success"
    )
  })
  output$vbox_revenu <- renderUI({
    value_box(
      title = "Part du revenu agricole total",
      value = "14,5 %",
      showcase = icon("money-bill-wave"),
      theme_color = "warning"
    )
  })
  
  # ---- Onglet 2 ----
  filtered_base_profil <- reactive({
    df <- data$base
    if (is.null(df)) return(NULL)
    if (input$region_profil != "Toutes") df <- df %>% filter(region == input$region_profil)
    if (input$milieu_profil != "Tous") {
      milieu_code <- ifelse(input$milieu_profil == "Urbain", 1, 2)
      df <- df %>% filter(milieu == milieu_code)
    }
    if (input$quintile_profil != "Tous") {
      q <- as.numeric(gsub(" .*", "", input$quintile_profil))
      df <- df %>% filter(quintile_consommation == q)
    }
    df
  })
  
  output$plot_profil_groupes <- renderPlot({
    df <- filtered_base_profil()
    if (is.null(df)) { plot(0,0,type="n", main="Données manquantes"); return() }
    df %>% count(groupe_typologie) %>%
      ggplot(aes(x = groupe_typologie, y = n, fill = groupe_typologie)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Effectif des ménages par catégorie", x = NULL, y = "Nombre") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$table_profil_stats <- renderTable({
    df <- filtered_base_profil()
    if (is.null(df)) return(NULL)
    df %>%
      group_by(groupe_typologie) %>%
      summarise(
        Taille_moy = round(mean(taille_menage, na.rm = TRUE), 2),
        FIES_moy = round(mean(fies_score, na.rm = TRUE), 2),
        HDDS_moy = round(mean(hdds_score, na.rm = TRUE), 2),
        Conso_tete = round(mean(conso_par_tete, na.rm = TRUE), 0)
      )
  })
  
  # ---- Onglet 3 ----
  filtered_rendement <- reactive({
    rend <- data$rendement
    if (is.null(rend)) return(NULL)
    p_low <- input$winsor_pct / 100
    p_high <- 1 - p_low
    q_low <- quantile(rend$rendement_moyen_grappe, p_low, na.rm = TRUE)
    q_high <- quantile(rend$rendement_moyen_grappe, p_high, na.rm = TRUE)
    rend <- rend %>% filter(rendement_moyen_grappe >= q_low & rendement_moyen_grappe <= q_high)
    rend
  })
  
  output$plot_rend_hist <- renderPlot({
    rend <- filtered_rendement()
    if (is.null(rend)) { plot(0,0,type="n", main="Données de rendement manquantes"); return() }
    ggplot(rend, aes(x = rendement_moyen_grappe)) +
      geom_histogram(bins = 30, fill = "#28A745", alpha = 0.85) +
      labs(title = "Distribution des rendements moyens par grappe",
           x = "Rendement moyen (kg/ha)", y = "Nombre de grappes") +
      theme_minimal()
  })
  
  output$map_rendement <- renderLeaflet({
    rend <- filtered_rendement()
    if (is.null(rend) || !has_coords(rend)) {
      return(leaflet() %>% addTiles() %>% 
               setView(lng = 10, lat = 17, zoom = 5) %>%
               addControl("Coordonnées des grappes non disponibles", position = "bottomleft"))
    }
    pal <- colorNumeric(viridis(10), domain = rend$rendement_moyen_grappe)
    leaflet(rend) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~sqrt(rendement_moyen_grappe/1000),
        color = ~pal(rendement_moyen_grappe),
        fillOpacity = 0.8,
        popup = ~paste("Grappe", grappe, "<br>Rendement:", round(rendement_moyen_grappe, 0), "kg/ha")
      ) %>%
      addLegend(position = "bottomright", pal = pal, values = ~rendement_moyen_grappe,
                title = "Rendement (kg/ha)")
  })
  
  # ---- Onglet 4 ----
  filtered_prix <- reactive({
    prix <- data$prix
    if (is.null(prix)) return(NULL)
    if (!is.null(input$dist_marche)) {
      if ("temps_marche" %in% names(prix)) {
        prix <- prix %>% filter(temps_marche >= input$dist_marche[1] & temps_marche <= input$dist_marche[2])
      }
    }
    if (!is.null(input$type_acheteur) && input$type_acheteur != "Tous") {
      if ("type_acheteur" %in% names(prix)) {
        prix <- prix %>% filter(type_acheteur == input$type_acheteur)
      }
    }
    prix
  })
  
  output$map_prix_prod <- renderLeaflet({
    prix <- filtered_prix()
    if (is.null(prix) || !has_coords(prix)) {
      return(leaflet() %>% addTiles() %>% 
               setView(lng = 10, lat = 17, zoom = 5) %>%
               addControl("Coordonnées des grappes non disponibles", position = "bottomleft"))
    }
    pal <- colorNumeric("Reds", domain = prix$prix_prod_moy)
    leaflet(prix) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = 6,
        color = ~pal(prix_prod_moy),
        fillOpacity = 0.8,
        popup = ~paste("Grappe", grappe, "<br>Prix producteur:", round(prix_prod_moy, 0), "FCFA/kg")
      ) %>%
      addLegend(position = "bottomright", pal = pal, values = ~prix_prod_moy,
                title = "Prix (FCFA/kg)")
  })
  
  output$plot_marge_gradient <- renderPlot({
    prix <- filtered_prix()
    if (is.null(prix) || !("marge" %in% names(prix))) {
      plot(0,0,type="n", main="Données de marge indisponibles")
      return()
    }
    if ("temps_marche" %in% names(prix)) {
      ggplot(prix, aes(x = temps_marche, y = marge)) +
        geom_point(alpha = 0.5) +
        geom_smooth(method = "lm", se = TRUE, color = "red") +
        labs(title = "Gradient de marge en fonction de la distance au marché",
             x = "Temps d'accès au marché (minutes)", y = "Marge (FCFA/kg)") +
        theme_minimal()
    } else {
      ggplot(prix, aes(x = marge)) +
        geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) +
        labs(title = "Distribution de la marge commerciale",
             x = "Marge (FCFA/kg)", y = "Nombre de grappes") +
        theme_minimal()
    }
  })
  
  # ---- Onglet 5 ----
  filtered_sa <- reactive({
    df <- data$base
    if (is.null(df)) return(NULL)
    if (input$groupe_sa != "Tous") {
      df <- df %>% filter(groupe_typologie == input$groupe_sa)
    }
    df
  })
  
  output$plot_sa_groupes <- renderPlot({
    df <- filtered_sa()
    if (is.null(df)) { plot(0,0,type="n", main="Données manquantes"); return() }
    outcome <- ifelse(input$outcome_choice == "FIES", "fies_score", "score_hdds")
    df %>%
      group_by(groupe_typologie) %>%
      summarise(mean_outcome = mean(get(outcome), na.rm = TRUE)) %>%
      ggplot(aes(x = groupe_typologie, y = mean_outcome, fill = groupe_typologie)) +
      geom_col(show.legend = FALSE) +
      labs(title = paste("Score", input$outcome_choice, "moyen par catégorie"),
           x = NULL, y = paste("Score", input$outcome_choice)) +
      theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$map_sa <- renderLeaflet({
    df <- filtered_sa()
    if (is.null(df)) {
      return(leaflet() %>% addTiles() %>% setView(lng = 10, lat = 17, zoom = 5))
    }
    if ("region" %in% names(df)) {
      coords_reg <- data.frame(
        region = c("Agadez", "Diffa", "Dosso", "Maradi", "Niamey", "Tahoua", "Tillabéri", "Zinder"),
        lat = c(16.97, 13.32, 13.05, 13.50, 13.51, 14.89, 14.21, 13.78),
        lon = c(7.99, 12.61, 3.20, 7.10, 2.11, 5.27, 1.45, 8.99)
      )
      map_data <- df %>%
        group_by(region) %>%
        summarise(fies_moy = mean(fies_score, na.rm = TRUE)) %>%
        left_join(coords_reg, by = "region")
      pal <- colorNumeric("RdYlGn", domain = map_data$fies_moy, reverse = TRUE)
      leaflet(map_data) %>%
        addTiles() %>%
        addCircleMarkers(
          lng = ~lon, lat = ~lat,
          radius = ~sqrt(fies_moy)*3,
          color = ~pal(fies_moy),
          fillOpacity = 0.8,
          popup = ~paste(region, "<br>Score FIES moyen:", round(fies_moy, 2))
        ) %>%
        addLegend(position = "bottomright", pal = pal, values = ~fies_moy,
                  title = "FIES moyen")
    } else {
      leaflet() %>% addTiles() %>% setView(lng = 10, lat = 17, zoom = 5)
    }
  })
  
  output$plot_coef_models <- renderPlot({
    model_list <- list()
    if (!is.null(data$model_ols)) model_list$OLS <- data$model_ols
    if (!is.null(data$model_iv)) model_list$IV <- data$model_iv
    if (!is.null(data$model_hdds)) model_list$HDDS <- data$model_hdds
    
    if (length(model_list) == 0) {
      plot(0,0,type="n", main="Aucun modèle trouvé")
      return()
    }
    
    coef_df <- bind_rows(lapply(names(model_list), function(name) {
      mod <- model_list[[name]]
      coefs <- summary(mod)$coefficients
      coefs <- coefs[rownames(coefs) != "(Intercept)", , drop = FALSE]
      if (nrow(coefs) == 0) return(NULL)
      data.frame(
        Variable = rownames(coefs),
        Estimate = coefs[,1],
        StdError = coefs[,2],
        Model = name,
        row.names = NULL
      )
    }))
    
    if (is.null(coef_df) || nrow(coef_df) == 0) {
      plot(0,0,type="n", main="Aucun coefficient extrait")
      return()
    }
    
    ggplot(coef_df, aes(x = Variable, y = Estimate, color = Model)) +
      geom_point(position = position_dodge(width = 0.5)) +
      geom_errorbar(aes(ymin = Estimate - 1.96*StdError, ymax = Estimate + 1.96*StdError),
                    position = position_dodge(width = 0.5), width = 0.2) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      coord_flip() +
      labs(title = "Comparaison des coefficients des modèles",
           x = NULL, y = "Estimation (effet)") +
      theme_minimal()
  })
  
}

# --- 4. Lancement ---
shinyApp(ui, server)




rsconnect::deployApp(
  appDir = getwd(),
  account = "binome5",
  appFiles = c("Dashboard.R")  # ou appFile si c'est un seul fichier, mais dans la doc c'est appFiles
)