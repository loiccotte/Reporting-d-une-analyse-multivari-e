library(shiny)
library(leaflet)
library(dplyr)
library(bslib)
library(DT)
library(FactoMineR) # For PCA analysis
library(factoextra) # For PCA visualization
library(ggplot2) # factoextra uses ggplot2 under the hood
library(shinyjs) # For enabling/disabling UI elements
library(viridis) # For color scales, especially for regions
library(sf) # For spatial data manipulation
library(RColorBrewer) # For color palettes (though viridis is often preferred)

# Palette de couleurs par étage (avec noms numérotés)
etage_noms <- c(
  "Physiologique - 1" = "Physiologique",
  "Sécurité - 2" = "Sécurité",
  "Appartenance - 3" = "Appartenance",
  "Estime - 4" = "Estime",
  "Actualisation de soi - 5" = "Actualisation.de.soi",
  "Cognitif - 6" = "Cognitif"
)

etage_couleurs <- c(
  "Physiologique - 1" = "#e74c3c",
  "Sécurité - 2" = "#f39c12",
  "Appartenance - 3" = "#f1c40f",
  "Estime - 4" = "#2ecc71",
  "Actualisation de soi - 5" = "#3498db",
  "Cognitif - 6" = "#9b59b6"
)

# UI avec design moderne ----
ui <- fluidPage(
  useShinyjs(), # Initialize shinyjs
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#3498db",
    secondary = "#95a5a6",
    success = "#2ecc71",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c"
  ),
  
  # Custom CSS pour améliorer le visuel
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', 'Roboto', 'Helvetica Neue', Arial, sans-serif;
        background-color: #f0f2f5;
      }
      .main-title {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 25px;
        border-radius: 12px;
        margin-bottom: 25px;
        text-align: center;
        box-shadow: 0 6px 12px rgba(0,0,0,0.2);
        animation: fadeIn 1s ease-out;
      }
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(-20px); }
        to { opacity: 1; transform: translateY(0); }
      }
      .sidebar-panel {
        background: #ffffff;
        border-radius: 12px;
        padding: 25px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        border: 1px solid #e0e0e0;
      }
      .leaflet-container, .pca-plot-container {
        border-radius: 12px;
        box-shadow: 0 4px 10px rgba(0,0,0,0.15);
        overflow: hidden; /* Ensures borders are respected */
      }
      .control-section {
        background: #fdfdfd;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
        border: 1px solid #f0f0f0;
      }
      .control-section h4 {
        color: #34495e;
        margin-bottom: 15px;
        border-bottom: 1px solid #eee;
        padding-bottom: 10px;
      }
      .legend-box {
        background: #ffffff;
        padding: 15px;
        border-radius: 10px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.05);
        margin-top: 20px;
        border: 1px solid #f0f0f0;
      }
      .legend-item {
        display: flex;
        align-items: center;
        margin-bottom: 8px;
        font-size: 14px;
      }
      .legend-color {
        width: 22px;
        height: 22px;
        border-radius: 50%;
        margin-right: 12px;
        border: 2px solid white;
        box-shadow: 0 1px 3px rgba(0,0,0,0.2);
      }
      .tab-pane {
        padding-top: 15px;
      }
      .dataTables_wrapper .dataTables_filter {
        margin-bottom: 15px;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button.current, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
        background-color: #3498db !important;
        color: white !important;
        border-color: #3498db !important;
      }
      .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
        background-color: #ecf0f1 !important;
        color: #3498db !important;
        border-color: #ecf0f1 !important;
      }
      /* Custom styles for shinyjs disabled inputs */
      .shinyjs-disabled {
        opacity: 0.6;
        cursor: not-allowed;
      }
    "))
  ),
  
  div(class = "main-title",
      h1("🏛️ Carte des Communes - Pyramide de Maslow",
         style = "margin: 0; font-weight: bold;"),
      p("Visualisation des infrastructures selon les besoins fondamentaux",
        style = "margin: 5px 0 0 0; opacity: 0.9;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar-panel",
      
      # Conditional panels for controls based on selected tab
      conditionalPanel(
        condition = "input.main_tabs == '🗺️ Carte Interactive'",
        div(class = "control-section",
            h4("🎯 Contrôles", style = "color: #2c3e50; margin-bottom: 15px;"),
            
            selectInput("map_level",
                        label = div(icon("map"), "Niveau d'affichage :"),
                        choices = c("Commune", "Département" = "DEP", "Région"),
                        selected = "Commune"),
            
            selectInput("tri",
                        label = div(icon("sort"), "Trier par :"),
                        choices = c("Aucun", "Région", names(etage_noms)),
                        selected = "Aucun"),
            
            checkboxGroupInput("etages",
                               label = div(icon("layer-group"), "Étages à afficher :"),
                               choices = names(etage_noms),
                               selected = names(etage_noms)) # Default to all checked
        ),
        
        div(class = "control-section",
            h4("📊 Statistiques", style = "color: #2c3e50; margin-bottom: 15px;"),
            verbatimTextOutput("stats", placeholder = TRUE)
        ),
        
        # Légende (can be removed if Leaflet legend is sufficient)
        div(class = "legend-box",
            h5("🎨 Légende des Étages Maslow", style = "color: #2c3e50; margin-bottom: 10px;"),
            uiOutput("legend_content")
        )
      ),
      
      conditionalPanel(
        condition = "input.main_tabs == '📈 ACP'",
        div(class = "control-section",
            h4("⚙️ Paramètres ACP", style = "color: #2c3e50; margin-bottom: 15px;"),
            checkboxInput("pca_scale_unit", "Mettre à l'échelle (scale.unit = TRUE)", value = TRUE),
            numericInput("pca_ncp", "Nombre de dimensions (ncp)", value = 5, min = 2, max = 10),
            
            hr(),
            h5("Visualisation Individus", style = "color: #2c3e50; margin-bottom: 10px;"),
            selectInput("pca_ind_color", "Colorer les individus par :",
                        choices = c("Pas de couleur" = "none", "Cos2" = "cos2", "Contributions" = "contrib", "Région" = "Région", "Étage Dominant" = "etage_dominant"),
                        selected = "none"),
            checkboxInput("pca_ind_show_labels", "Afficher les étiquettes des individus", value = TRUE),
            checkboxInput("pca_ind_repel", "Éviter le chevauchement (individus)", value = TRUE),
            
            hr(),
            h5("Visualisation Variables", style = "color: #2c3e50; margin-bottom: 10px;"),
            selectInput("pca_var_color", "Colorer les variables par :",
                        choices = c("Pas de couleur" = "none", "Cos2" = "cos2", "Contributions" = "contrib"),
                        selected = "none"),
            checkboxInput("pca_var_show_labels", "Afficher les étiquettes des variables", value = TRUE),
            checkboxInput("pca_var_repel", "Éviter le chevauchement (variables)", value = TRUE),
            
            hr(),
            h5("Graphique Biplot (Individus et Variables)", style = "color: #2c3e50; margin-bottom: 10px;"),
            selectInput("pca_biplot_geom", "Géométrie :",
                        choices = c("Points et Texte" = "point_text",
                                    "Flèches et Texte" = "arrow_text",
                                    "Points seulement" = "point",
                                    "Texte seulement" = "text"),
                        selected = "arrow_text"),
            numericInput("pca_dim1", "Axe X (Dimension)", value = 1, min = 1),
            numericInput("pca_dim2", "Axe Y (Dimension)", value = 2, min = 1)
        )
      ),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabs", # Add an ID to the tabsetPanel
        type = "tabs",
        tabPanel("🗺️ Carte Interactive",
                 br(),
                 leafletOutput("carte", height = 650)),
        tabPanel("📋 Données",
                 br(),
                 DT::dataTableOutput("tableau")),
        tabPanel("📈 ACP",
                 br(),
                 h3("Analyse en Composantes Principales", style = "text-align: center; color: #2c3e50;"),
                 br(),
                 fluidRow(
                   column(6, div(class = "pca-plot-container", plotOutput("pca_ind_plot", height = "400px"))),
                   column(6, div(class = "pca-plot-container", plotOutput("pca_var_plot", height = "400px")))
                 ),
                 br(),
                 fluidRow(
                   column(12, div(class = "pca-plot-container", plotOutput("pca_biplot", height = "600px")))
                 ),
                 br(),
                 h4("Éboulis des valeurs propres (Scree Plot)", style = "text-align: center; color: #2c3e50;"),
                 div(class = "pca-plot-container", plotOutput("pca_eig_plot", height = "350px")),
                 br(),
                 h4("Contributions des variables aux dimensions", style = "text-align: center; color: #2c3e50;"),
                 div(class = "pca-plot-container", plotOutput("pca_contrib_var_plot", height = "350px"))
        )
      ),
      width = 9
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  # Chargement et nettoyage des données CommunesGeo.csv
  df_geo <- reactive({
    # Simulation de données si le fichier n'existe pas (keep this fallback)
    if (!file.exists("CommunesGeo.csv")) {
      warning("CommunesGeo.csv not found. Using simulated data.")
      set.seed(123)
      n <- 100
      data.frame(
        REG = sample(1:5, n, replace = TRUE),
        Région = sample(c("Auvergne-Rhône-Alpes", "Bretagne", "Nouvelle-Aquitaine", "Occitanie", "PACA"), n, replace = TRUE),
        DEP = sprintf("%02d", sample(c(1:10, 20, 21, 60:65, 90:95), n, replace = TRUE)),
        CODARR = sample(1:10, n, replace = TRUE),
        CODCAN = sample(1:20, n, replace = TRUE),
        CODCOM = paste0(sprintf("%02d", sample(1:95, n, replace = TRUE)), sprintf("%03d", sample(1:900, n, replace = TRUE))), # Changed COM to CODCOM
        COM = paste0(sprintf("%02d", sample(1:95, n, replace = TRUE)), sprintf("%03d", sample(1:900, n, replace = TRUE))), # Keep COM as original data
        Commune = paste("Commune", 1:n),
        PMUN = round(runif(n, 1000, 20000)),
        PCAP = round(runif(n, 10, 500)),
        PTOT = round(runif(n, 1010, 20500)),
        Physiologique = round(runif(n, 0, 100), 1),
        Sécurité = round(runif(n, 0, 100), 1),
        Appartenance = round(runif(n, 0, 100), 1),
        Estime = round(runif(n, 0, 100), 1),
        Actualisation.de.soi = round(runif(n, 0, 100), 1),
        Cognitif = round(runif(n, 0, 100), 1),
        latitude = runif(n, 42, 51),
        longitude = runif(n, -5, 8),
        stringsAsFactors = FALSE
      )
    } else {
      # CRITICAL CHANGE: Use sep=" " and quote='"' based on the provided CSV snippet.
      # This tells R that columns are space-separated and values/headers are quoted.
      df <- read.csv("CommunesGeo.csv", sep = " ", encoding = "UTF-8", header = TRUE,
                     quote = "\"", dec = ".", stringsAsFactors = FALSE)
      
      # Clean column names to be valid R names (e.g., remove spaces, hyphens)
      colnames(df) <- make.names(colnames(df), unique = TRUE) 
      
      # Convert relevant columns to numeric. Trim whitespace before conversion.
      # This loop is crucial for the "PMUN column not found or not numeric" warning.
      numeric_cols <- c("PMUN", "PCAP", "PTOT", "Physiologique", "Sécurité",
                        "Appartenance", "Estime", "Actualisation.de.soi", "Cognitif",
                        "latitude", "longitude")
      
      for (col in numeric_cols) {
        if (col %in% colnames(df)) {
          df[[col]] <- as.numeric(gsub(",", ".", trimws(as.character(df[[col]])))) 
          if(any(is.na(df[[col]]) & !is.na(df[[col]][!is.na(df[[col]])]))) {
            warning(paste0("NAs introduced by coercion in column '", col, "'. Check data format and decimal separator."))
          }
        } else {
          warning(paste0("Column '", col, "' not found in CommunesGeo.csv (after make.names). Please ensure correct column names or if the column exists after 'read.csv' processing."))
        }
      }
      
      # *** CRITICAL: Ensure CODCOM, DEP, Région are character for robust joining ***
      # Trim whitespace as data is quoted in CSV
      # Based on your new CSV snippet, the commune code is in "CODCOM"
      if ("CODCOM" %in% colnames(df)) df$CODCOM <- as.character(trimws(df$CODCOM)) 
      if ("DEP" %in% colnames(df)) df$DEP <- as.character(trimws(df$DEP)) 
      if ("Région" %in% colnames(df)) df$Région <- as.character(trimws(df$Région)) 
      
      # Debugging: Print structure and head of df after loading
      message("DEBUG: df_geo() loaded data structure:")
      print(str(df))
      message("DEBUG: df_geo() head of data:")
      print(head(df))
      
      df
    }
  })
  
  # Reactive for loading GeoJSON files (now loading pre-processed .rds files)
  regions_sf <- reactive({
    req(file.exists("regions_simplified.rds")) 
    readRDS("regions_simplified.rds") 
  })
  
  departements_sf <- reactive({
    req(file.exists("departements_simplified.rds"))
    readRDS("departements_simplified.rds") 
  })
  
  # Reactive expression to load communes data
  communes_sf <- reactive({
    file_path <- "communes-version-simplifiee.geojson"
    req(file.exists(file_path))
    
    communes_data <- sf::st_read(file_path, quiet = TRUE) # Lire le fichier GeoJSON
    
    # 1. Renommer les colonnes pour correspondre aux attentes du code de la carte
    # Utilisez `rename` pour changer 'Commune' en 'nom' et 'CODCOM' en 'code'
    if ("Commune" %in% colnames(communes_data) && "CODCOM" %in% colnames(communes_data)) {
      communes_data <- communes_data %>%
        rename(nom = Commune, code = CODCOM)
    } else {
      warning("DEBUG: 'Commune' or 'CODCOM' columns not found in GeoJSON. Check your GeoJSON structure.")
      # Si les colonnes ne sont pas là, essayez de trouver d'autres noms pertinents si possible
      # Ou gérer l'erreur pour éviter que l'app plante. Pour l'instant, on continue.
    }
    
    # 2. Calculer la colonne 'etage_dominant'
    # Identifiez les colonnes des scores des étages
    etage_score_cols_raw <- names(etage_noms) # Ex: "Physiologique - 1", "Sécurité - 2"
    # Convertissez les noms des étages en noms de colonnes valides pour R (ex: "Physiologique", "Sécurité")
    etage_score_cols_valid <- etage_noms[etage_score_cols_raw]
    
    # Assurez-vous que ces colonnes de scores existent dans le dataframe
    existing_score_cols <- intersect(etage_score_cols_valid, colnames(communes_data))
    
    if (length(existing_score_cols) == 0) {
      warning("DEBUG: No Maslow stage score columns found (e.g., 'Physiologique', 'Sécurité'). Cannot calculate 'etage_dominant'.")
      return(NULL) # Retourne NULL si aucune colonne de score n'est trouvée
    }
    
    # Calculer l'étage dominant pour chaque commune
    communes_data <- communes_data %>%
      rowwise() %>% # Applique la fonction ligne par ligne
      mutate(
        # Récupérer les scores des colonnes existantes pour la ligne actuelle
        current_scores = c_across(all_of(existing_score_cols)),
        # Trouver l'indice du score maximum
        max_score_idx = which.max(current_scores),
        # Récupérer le nom de la colonne du score maximum
        dominant_col_name = existing_score_cols[max_score_idx],
        # Retrouver le nom de l'étage original à partir de la correspondance etage_noms
        etage_dominant = names(etage_noms)[match(dominant_col_name, etage_noms)]
      ) %>%
      ungroup() # Annule rowwise()
    
    # Final check for critical columns (nom, code, etage_dominant, geometry)
    if (!all(c("nom", "code", "etage_dominant", "geometry") %in% colnames(communes_data))) {
      warning("DEBUG: Final check: Missing 'nom', 'code', 'etage_dominant', or 'geometry' after processing GeoJSON. Map might not display correctly for communes.")
      print(colnames(communes_data))
      return(NULL) # Return NULL if critical columns are still missing
    }
    
    return(communes_data)
  })
  data_processed <- reactive({
    df <- df_geo()
    
    # Filter based on population if PMUN column exists and is numeric (as per your request)
    # The filter is applied here, assuming df_geo() reads the raw data,
    # and this reactive then applies the 10k-20k population filter.
    if ("PMUN" %in% colnames(df) && is.numeric(df$PMUN)) {
      df <- df %>% filter(PMUN >= 10000 & PMUN <= 20000)
      if (nrow(df) == 0) {
        warning("No communes found after population filter (10k-20k).")
      }
    } else {
      warning("PMUN column not found or not numeric in df_geo(). Skipping population filter.")
    }
    
    etages_cols_base <- make.names(etage_noms)
    present_etage_cols <- intersect(etages_cols_base, colnames(df))
    
    if (length(present_etage_cols) > 0) {
      df$etage_dominant <- apply(df[, present_etage_cols, drop = FALSE], 1, function(row) {
        if (all(is.na(row)) || all(row == 0)) return("Aucun")
        idx <- which.max(row)
        original_name_map <- setNames(names(etage_noms), make.names(etage_noms))
        original_name_map[present_etage_cols[idx]]
      })
    } else {
      df$etage_dominant <- "Aucun"
    }
    
    df$couleur <- etage_couleurs[df$etage_dominant]
    df$couleur[is.na(df$couleur)] <- "#95a5a6"
    
    if (input$tri != "Aucun") {
      if (input$tri == "Région") {
        # Check if 'Région' column exists before arranging
        if("Région" %in% colnames(df)) {
          df <- df %>% arrange(.data[["Région"]])
        } else {
          warning("Column 'Région' not found for sorting.")
        }
      } else {
        tri_simple <- etage_noms[input$tri]
        col_tri <- make.names(tri_simple)
        if (col_tri %in% colnames(df)) {
          df <- df %>% arrange(desc(.data[[col_tri]]))
        }
      }
    }
    df
  })
  
  map_data_level <- reactive({
    df_maslow <- data_processed()
    
    # Early exit if df_maslow is empty after processing (e.g., after population filter)
    if (nrow(df_maslow) == 0) {
      message("DEBUG: df_maslow is empty after data_processed. Returning empty sf object for map.")
      return(st_sf(st_sfc(), crs = 4326)) 
    }
    
    level <- input$map_level
    etage_score_cols <- make.names(etage_noms)
    
    df_merged_sf <- NULL
    
    if (level == "Commune") {
      sf_data <- communes_sf()
      # *** CRITICAL RENAME AND JOIN ***
      # Rename the GeoJSON column 'INSEE_COM' to 'CODCOM' to match your CSV
      # Assuming commune-frmetdrom.geojson has 'INSEE_COM' as the INSEE code column
      # If your GeoJSON uses 'code', 'COM_CODE', etc., adjust 'INSEE_COM' below.
      if ("INSEE_COM" %in% colnames(sf_data)) {
        sf_data <- sf_data %>% rename(CODCOM = INSEE_COM)
      } else if ("code" %in% colnames(sf_data)) { # Fallback from previous common assumption
        sf_data <- sf_data %>% rename(CODCOM = code)
      } else {
        warning("Communes GeoJSON: Neither 'INSEE_COM' nor 'code' found for join. Please check GeoJSON columns.")
        return(st_sf(st_sfc(), crs = 4326)) # Exit if join key not found
      }
      df_merged_sf <- sf_data %>%
        inner_join(df_maslow, by = "CODCOM") 
      
    } else if (level == "DEP") {
      sf_data <- departements_sf()
      # *** CRITICAL RENAME AND JOIN ***
      # Assuming contour-des-departements.geojson has 'code' as the department code
      if (!"code" %in% colnames(sf_data)) {
        warning("Departements GeoJSON: 'code' column not found for join. Please check GeoJSON columns.")
        return(st_sf(st_sfc(), crs = 4326)) # Exit if join key not found
      }
      df_merged_sf <- sf_data %>%
        inner_join(df_maslow %>% group_by(DEP, Région) %>% 
                     summarise(across(all_of(etage_score_cols), ~sum(.x, na.rm = TRUE)), n_communes = n(), .groups = 'drop'), 
                   by = c("code" = "DEP"))
      
    } else { # level == "Région"
      sf_data <- regions_sf()
      # *** CRITICAL RENAME AND JOIN ***
      # Assuming regions.geojson has 'nom' as the region name
      if (!"nom" %in% colnames(sf_data)) {
        warning("Regions GeoJSON: 'nom' column not found for join. Please check GeoJSON columns.")
        return(st_sf(st_sfc(), crs = 4326)) # Exit if join key not found
      }
      df_grouped <- df_maslow %>%
        group_by(Région) %>%
        summarise(
          across(all_of(etage_score_cols), ~sum(.x, na.rm = TRUE)),
          n_communes = n(),
          .groups = 'drop'
        )
      df_merged_sf <- sf_data %>%
        inner_join(df_grouped, by = c("nom" = "Région"))
    }
    
    # Recalculate dominant stage and color for the merged entities
    if (!is.null(df_merged_sf) && inherits(df_merged_sf, "sf") && nrow(df_merged_sf) > 0) {
      existing_etage_cols <- intersect(etage_score_cols, colnames(df_merged_sf))
      
      if (length(existing_etage_cols) > 0) {
        df_merged_sf <- df_merged_sf %>%
          rowwise() %>%
          mutate(
            etage_dominant = {
              scores <- c_across(all_of(existing_etage_cols))
              if (all(is.na(scores)) || all(scores == 0)) "Aucun"
              else {
                idx <- which.max(scores)
                original_name_map <- setNames(names(etage_noms), make.names(etage_noms))
                original_name_map[existing_etage_cols[idx]]
              }
            },
            couleur = etage_couleurs[etage_dominant]
          ) %>%
          ungroup()
      } else {
        warning("No Maslow score columns found in merged SF data. Setting etage_dominant to 'Aucun'.")
        df_merged_sf$etage_dominant <- "Aucun"
      }
      df_merged_sf$couleur[is.na(df_merged_sf$couleur)] <- "#95a5a6"
      
      # Debugging: Print final merged sf object structure and head
      message("DEBUG: df_merged_sf (final map data) structure:")
      print(str(df_merged_sf))
      message("DEBUG: df_merged_sf (final map data) head:")
      print(head(df_merged_sf))
      
    } else {
      warning("No data merged for map level: ", level, ". Returning empty sf object.")
      message("DEBUG: Empty df_merged_sf at end of map_data_level.")
      df_merged_sf <- st_sf(st_sfc(), crs = 4326) # Return an empty sf object
    }
    
    return(df_merged_sf)
  })
  
  
  # Update legend (still useful for the sidebar)
  output$legend_content <- renderUI({
    etages_actifs <- input$etages
    legend_items <- lapply(etages_actifs, function(etage) {
      div(class = "legend-item",
          div(class = "legend-color",
              style = paste0("background-color: ", etage_couleurs[etage])),
          span(etage, style = "font-size: 14px;")
      )
    })
    tagList(legend_items)
  })
  
  # Statistiques
  output$stats <- renderText({
    df <- data_processed() 
    total_communes <- nrow(df)
    etage_counts <- table(df$etage_dominant)
    
    stats_text <- paste0(
      "📍 Total communes (après filtres population): ", total_communes, "\n\n",
      "🏆 Étage dominant (pour ces communes):\n"
    )
    
    for (etage in names(etage_counts)) {
      pct <- round(etage_counts[etage] / total_communes * 100, 1)
      stats_text <- paste0(stats_text, "• ", etage, ": ", etage_counts[etage],
                           " (", pct, "%)\n")
    }
    
    stats_text
  })
  
  # Carte (Choropleth map)
  output$carte <- renderLeaflet({
    df_map_sf <- map_data_level() # Get the prepared sf object
    
    # Handle empty/invalid sf object robustly
    if (is.null(df_map_sf) || !inherits(df_map_sf, "sf") || nrow(df_map_sf) == 0 || !("geometry" %in% colnames(df_map_sf))) {
      message("DEBUG: renderLeaflet received empty or invalid df_map_sf. Displaying fallback message.")
      return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>%
               setView(lng = 2.5, lat = 46.5, zoom = 6) %>%
               addLabelOnlyMarkers(lng = 2.5, lat = 46.5,
                                   label = "Aucune donnée cartographique à afficher pour la sélection actuelle. Vérifiez les fichiers GeoJSON ou les critères de filtre.",
                                   labelOptions = labelOptions(noHide = TRUE, direction = "top",
                                                               style = list("color" = "red", "font-weight" = "bold", "font-size" = "16px"))))
    }
    
    # Create a color palette based on the dominant stages
    color_domain <- sort(unique(df_map_sf$etage_dominant))
    pal_values <- intersect(color_domain, names(etage_couleurs))
    pal <- colorFactor(palette = etage_couleurs[pal_values], domain = pal_values,
                       na.color = "#95a5a6") # Fallback for NA colors (e.g., "Aucun")
    
    leaflet(df_map_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(etage_dominant),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = lapply(seq_len(nrow(df_map_sf)), function(i) {
          # Ensure label content is a plain character string
          item_name <- ""
          if (input$map_level == "Commune") {
            item_name <- if ("nom" %in% colnames(df_map_sf)) df_map_sf$nom[i] else df_map_sf$Commune[i]
          } else if (input$map_level == "DEP") {
            item_name <- if ("nom" %in% colnames(df_map_sf)) df_map_sf$nom[i] else df_map_sf$DEP[i]
          } else if (input$map_level == "Région") {
            item_name <- if ("nom" %in% colnames(df_map_sf)) df_map_sf$nom[i] else df_map_sf$Région[i]
          }
          
          n_communes_info <- if ("n_communes" %in% colnames(df_map_sf) && input$map_level != "Commune") {
            paste0("<br><span style='color: #888;'> (", df_map_sf$n_communes[i], " communes agrégées)</span>")
          } else ""
          
          region_name_info <- if ("Région" %in% colnames(df_map_sf)) {
            paste0("<br><span style='color: #555;'>Région: ", df_map_sf$Région[i], "</span>")
          } else ""
          
          texte <- paste0("<div style='font-family: Arial; font-size: 13px;'>",
                          "<b style='color: #2c3e50; font-size: 14px;'>🏛️ ", item_name, "</b>",
                          n_communes_info,
                          region_name_info, "<br><br>")
          
          scores_to_display <- names(etage_noms)
          for (score_etage in scores_to_display) {
            nom_simple <- etage_noms[score_etage]
            col <- make.names(nom_simple)
            if (col %in% colnames(df_map_sf)) {
              couleur_etage <- etage_couleurs[score_etage]
              texte <- paste0(texte,
                              "<div style='margin: 3px 0;'>",
                              "<span style='display: inline-block; width: 12px; height: 12px; background-color: ",
                              couleur_etage, "; border-radius: 50%; margin-right: 8px;'></span>",
                              "<b>", score_etage, "</b> : ", round(df_map_sf[[col]][i], 1), "</div>")
            }
          }
          
          texte <- paste0(texte, "<br><b style='color: #e74c3c;'>🏆 Étage dominant :</b> ",
                          df_map_sf$etage_dominant[i], "</div>")
          HTML(texte) # HTML() returns a single character string, which is what Leaflet expects
        }),
        labelOptions = labelOptions(direction = "auto",
                                    style = list("background-color" = "rgba(255,255,255,0.9)",
                                                 "border" = "1px solid #ccc",
                                                 "border-radius" = "5px",
                                                 "box-shadow" = "0 2px 4px rgba(0,0,0,0.2)"))
      ) %>%
      addLegend(pal = pal, values = color_domain, opacity = 0.7,
                title = "Étage Dominant", position = "bottomright") %>%
      setView(lng = 2.5, lat = 46.5, zoom = 6)
  })
  
  # Tableau de données
  output$tableau <- DT::renderDataTable({
    df <- data_processed()
    
    etage_original_names <- names(etage_noms)
    etage_make_names <- make.names(etage_noms)
    
    cols_to_select <- c("Commune", "Région", "DEP", "PMUN", etage_make_names, "etage_dominant") # Added PMUN here for table display
    
    cols_to_select_exist <- intersect(cols_to_select, colnames(df))
    
    df_display <- df %>%
      select(all_of(cols_to_select_exist))
    
    current_names <- colnames(df_display)
    new_names <- current_names
    for (i in seq_along(etage_original_names)) {
      col_actuelle <- etage_make_names[i] # Use make_names for checking against current_names
      etage_original <- names(etage_noms)[make.names(etage_noms) == col_actuelle]
      if (col_actuelle %in% current_names && length(etage_original) > 0) {
        names(df_display)[names(df_display) == col_actuelle] <- etage_original[1]
      }
    }
    
    DT::datatable(df_display,
                  options = list(pageLength = 15,
                                 scrollX = TRUE,
                                 dom = 'Bfrtip',
                                 language = list(
                                   url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
                                 )),
                  class = 'cell-border stripe hover',
                  caption = "Données des communes par étage de la pyramide de Maslow") %>%
      DT::formatStyle(columns = "etage_dominant",
                      backgroundColor = DT::styleEqual(names(etage_couleurs),
                                                       etage_couleurs))
  })
  
  
  # PCA Calculation (remains largely the same, but robust for data types)
  res_pca <- reactive({
    df <- data_processed() # Use data_processed which already has the population filter applied
    
    if (nrow(df) == 0) {
      warning("No data for PCA after population filter. Returning NULL.")
      return(NULL)
    }
    
    pca_data <- df %>%
      select(Physiologique, Sécurité, Appartenance, Estime, Actualisation.de.soi, Cognitif)
    
    # Ensure numeric conversion is robust (if not already handled by df_geo)
    pca_data <- lapply(pca_data, function(x) as.numeric(as.character(x))) %>% as.data.frame()
    
    # Handle NA values by imputing with the mean of the column
    for(col in names(pca_data)){
      if(any(is.na(pca_data[[col]])) && nrow(pca_data) > 0){
        pca_data[[col]][is.na(pca_data[[col]])] <- mean(pca_data[[col]], na.rm = TRUE)
      } else if (nrow(pca_data) == 0) { # This case should be caught by the nrow(df)==0 check above
        warning("No data for PCA after initial processing. PCA will not be performed.")
        return(NULL)
      }
    }
    
    # If all values are identical after imputation (e.g., all 0s), PCA can fail
    if(nrow(pca_data) > 1 && ncol(pca_data) > 1 && any(apply(pca_data, 2, var, na.rm=TRUE) == 0, na.rm = TRUE)) { 
      warning("Some PCA variables have zero variance after processing. PCA may fail or be degenerate.")
      # Remove zero-variance columns for PCA to proceed
      pca_data <- pca_data[, apply(pca_data, 2, var, na.rm=TRUE) != 0, drop = FALSE]
      if (ncol(pca_data) < 2) {
        warning("Less than 2 variables with variance remaining for PCA. Returning NULL.")
        return(NULL)
      }
    }
    
    # Ensure enough observations for PCA
    if(nrow(pca_data) < 2) {
      warning("Not enough observations for PCA after filtering. Returning NULL.")
      return(NULL)
    }
    
    res <- PCA(pca_data,
               scale.unit = input$pca_scale_unit,
               ncp = input$pca_ncp,
               graph = FALSE)
    
    # Attach original data for habillage (coloring by factors)
    processed_df_for_habillage <- df %>% # Use the already population-filtered df
      select(Commune, Région, DEP, etage_dominant)
    
    # Ensure row names align, critical for fviz_pca functions
    # Generate unique row names if original ones are not unique, to prevent issues with FactoMineR
    if (is.null(row.names(df)) || any(duplicated(row.names(df)))) {
      temp_row_names <- make.unique(df$Commune) # Try to use Commune name, make unique
      if (length(temp_row_names) != nrow(df)) { # Fallback if commune names are not sufficient to be unique
        temp_row_names <- as.character(1:nrow(df))
      }
      row.names(processed_df_for_habillage) <- temp_row_names
      row.names(pca_data) <- temp_row_names # Also set for pca_data if it's new
      res$call$X <- pca_data # Update the PCA result with correct row names
    } else {
      row.names(processed_df_for_habillage) <- row.names(df) 
    }
    
    res$call$X_original <- processed_df_for_habillage 
    
    res
  })
  
  # Dynamic UI controls for ACP (remains the same)
  observe({
    # Only disable/enable if shinyjs is initialized and working
    if (!is.null(shinyjs::hidden) && !is.null(input$pca_biplot_geom)) { # Basic check for shinyjs
      if (input$pca_biplot_geom == "text") {
        shinyjs::disable("pca_ind_repel")
        shinyjs::disable("pca_var_repel")
      } else {
        shinyjs::enable("pca_ind_repel")
        shinyjs::enable("pca_var_repel")
      }
      
      if (!input$pca_ind_show_labels) {
        shinyjs::disable("pca_ind_repel")
      } else {
        shinyjs::enable("pca_ind_repel")
      }
      if (!input$pca_var_show_labels) {
        shinyjs::disable("pca_var_repel")
      } else {
        shinyjs::enable("pca_var_repel")
      }
    }
  })
  
  
  # PCA Plots (remain largely the same, now use full_data for region/etage_dominant)
  
  output$pca_eig_plot <- renderPlot({
    res <- res_pca()
    if (is.null(res)) return(ggplot() + annotate("text", x=0.5, y=0.5, label="Pas assez de données pour l'ACP.") + theme_void())
    fviz_eig(res, addlabels = TRUE,
             main = "Éboulis des valeurs propres (Variance expliquée)") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(face = "bold", size = 12),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10))
  })
  
  output$pca_ind_plot <- renderPlot({
    res <- res_pca()
    if (is.null(res)) return(ggplot() + annotate("text", x=0.5, y=0.5, label="Pas assez de données pour l'ACP.") + theme_void())
    
    full_data <- res$call$X_original
    
    label_ind <- if (input$pca_ind_show_labels) "ind" else "none"
    geom_ind <- "point"
    
    p <- NULL
    if (input$pca_ind_color == "none") {
      p <- fviz_pca_ind(res,
                        axes = c(input$pca_dim1, input$pca_dim2),
                        geom.ind = geom_ind,
                        label = label_ind,
                        repel = input$pca_ind_repel,
                        title = "Graphique des individus")
    } else if (input$pca_ind_color %in% c("cos2", "contrib")) {
      p <- fviz_pca_ind(res,
                        axes = c(input$pca_dim1, input$pca_dim2),
                        col.ind = input$pca_ind_color,
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        geom.ind = geom_ind,
                        label = label_ind,
                        repel = input$pca_ind_repel,
                        title = paste("Graphique des individus (coloré par", input$pca_ind_color, ")"))
    } else if (input$pca_ind_color %in% c("Région", "etage_dominant")) {
      group_var <- if (input$pca_ind_color == "Région") as.factor(full_data$Région) else as.factor(full_data$etage_dominant)
      p <- fviz_pca_ind(res,
                        axes = c(input$pca_dim1, input$pca_dim2),
                        habillage = group_var,
                        addEllipses = TRUE,
                        ellipse.type = "convex",
                        legend.title = input$pca_ind_color,
                        geom.ind = geom_ind,
                        label = label_ind,
                        repel = input$pca_ind_repel,
                        title = paste("Graphique des individus (coloré par", input$pca_ind_color, ")")) +
        scale_color_viridis_d()
    }
    
    if (!is.null(p)) {
      p + theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              axis.title = element_text(face = "bold", size = 12),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10))
    }
  })
  
  output$pca_var_plot <- renderPlot({
    res <- res_pca()
    if (is.null(res)) return(ggplot() + annotate("text", x=0.5, y=0.5, label="Pas assez de données pour l'ACP.") + theme_void())
    
    label_var <- if (input$pca_var_show_labels) "var" else "none"
    geom_var <- "arrow"
    
    p <- NULL
    if (input$pca_var_color == "none") {
      p <- fviz_pca_var(res,
                        axes = c(input$pca_dim1, input$pca_dim2),
                        geom.var = geom_var,
                        label = label_var,
                        repel = input$pca_var_repel,
                        title = "Graphique des variables")
    } else {
      p <- fviz_pca_var(res,
                        axes = c(input$pca_dim1, input$pca_dim2),
                        col.var = input$pca_var_color,
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        geom.var = geom_var,
                        label = label_var,
                        repel = input$pca_var_repel,
                        title = paste("Graphique des variables (coloré par", input$pca_var_color, ")"))
    }
    
    if (!is.null(p)) {
      p + theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              axis.title = element_text(face = "bold", size = 12),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10))
    }
  })
  
  output$pca_biplot <- renderPlot({
    res <- res_pca()
    if (is.null(res)) return(ggplot() + annotate("text", x=0.5, y=0.5, label="Pas assez de données pour l'ACP.") + theme_void())
    
    full_data <- res$call$X_original
    
    label_ind <- if (input$pca_ind_show_labels) "ind" else "none"
    label_var <- if (input$pca_var_show_labels) "var" else "none"
    
    geom_ind_val <- "point"
    geom_var_val <- "arrow"
    
    if (input$pca_biplot_geom == "point_text") {
      geom_ind_val <- c("point", "text")
      geom_var_val <- c("point", "text")
    } else if (input$pca_biplot_geom == "arrow_text") {
      geom_ind_val <- c("point", "text")
      geom_var_val <- c("arrow", "text")
    } else if (input$pca_biplot_geom == "point") {
      geom_ind_val <- "point"
      geom_var_val <- "point"
    } else if (input$pca_biplot_geom == "text") {
      geom_ind_val <- "text"
      geom_var_val <- "text"
    }
    
    p <- NULL
    if (input$pca_ind_color %in% c("Région", "etage_dominant")) {
      group_var <- if (input$pca_ind_color == "Région") as.factor(full_data$Région) else as.factor(full_data$etage_dominant)
      
      p <- fviz_pca_biplot(res,
                           axes = c(input$pca_dim1, input$pca_dim2),
                           label = "all",
                           geom.ind = geom_ind_val,
                           geom.var = geom_var_val,
                           labels = list(ind = label_ind, var = label_var),
                           habillage = group_var,
                           addEllipses = TRUE,
                           ellipse.type = "convex",
                           legend.title = input$pca_ind_color,
                           repel = input$pca_ind_repel || input$pca_var_repel,
                           title = paste("Biplot ACP (Individus colorés par", input$pca_ind_color, ")")) +
        scale_color_viridis_d(aesthetics = "colour")
    } else {
      p <- fviz_pca_biplot(res,
                           axes = c(input$pca_dim1, input$pca_dim2),
                           label = "all",
                           geom.ind = geom_ind_val,
                           geom.var = geom_var_val,
                           labels = list(ind = label_ind, var = label_var),
                           col.ind = if (input$pca_ind_color != "none") input$pca_ind_color else "black",
                           col.var = if (input$pca_var_color != "none") input$pca_var_color else "black",
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = input$pca_ind_repel || input$pca_var_repel,
                           title = "Biplot ACP (Individus et Variables)")
    }
    
    if (!is.null(p)) {
      p + theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              axis.title = element_text(face = "bold", size = 12),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10))
    }
  })
  
  output$pca_contrib_var_plot <- renderPlot({
    res <- res_pca()
    if (is.null(res)) return(ggplot() + annotate("text", x=0.5, y=0.5, label="Pas assez de données pour l'ACP.") + theme_void())
    
    fviz_contrib(res, choice = "var", axes = c(input$pca_dim1, input$pca_dim2),
                 fill = "steelblue", color = "steelblue",
                 title = "Contributions des variables aux dimensions choisies") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            axis.title = element_text(face = "bold", size = 12),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10))
  })
}

# Lancer l'application
shinyApp(ui, server)