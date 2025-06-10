library(shiny)
library(leaflet)
library(dplyr)
library(bslib)
library(DT)

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
      .main-title {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
        text-align: center;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      .sidebar-panel {
        background: #f8f9fa;
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .leaflet-container {
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      
      .control-section {
        background: white;
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 15px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      
      .legend-box {
        background: white;
        padding: 10px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-top: 15px;
      }
      
      .legend-item {
        display: flex;
        align-items: center;
        margin-bottom: 5px;
      }
      
      .legend-color {
        width: 20px;
        height: 20px;
        border-radius: 50%;
        margin-right: 10px;
        border: 2px solid white;
        box-shadow: 0 1px 2px rgba(0,0,0,0.2);
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
      
      div(class = "control-section",
          h4("🎯 Contrôles", style = "color: #2c3e50; margin-bottom: 15px;"),
          
          selectInput("tri", 
                      label = div(icon("sort"), "Trier par :"),
                      choices = c("Aucun", names(etage_noms)),
                      selected = "Aucun"),
          
          checkboxGroupInput("etages", 
                             label = div(icon("layer-group"), "Étages à afficher :"),
                             choices = names(etage_noms),
                             selected = names(etage_noms)[1:3])
      ),
      
      div(class = "control-section",
          h4("📊 Statistiques", style = "color: #2c3e50; margin-bottom: 15px;"),
          verbatimTextOutput("stats", placeholder = TRUE)
      ),
      
      # Légende
      div(class = "legend-box",
          h5("🎨 Légende", style = "color: #2c3e50; margin-bottom: 10px;"),
          uiOutput("legend_content")
      ),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("🗺️ Carte Interactive", 
                 br(),
                 leafletOutput("carte", height = 650)),
        tabPanel("📋 Données", 
                 br(),
                 DT::dataTableOutput("tableau"))
      ),
      width = 9
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  # Chargement et nettoyage des données
  df_geo <- reactive({
    # Simulation de données si le fichier n'existe pas
    if (!file.exists("CommunesGeo.csv")) {
      set.seed(123)
      n <- 100
      data.frame(
        Commune = paste("Commune", 1:n),
        Physiologique = round(runif(n, 0, 100), 1),
        Sécurité = round(runif(n, 0, 100), 1),
        Appartenance = round(runif(n, 0, 100), 1),
        Estime = round(runif(n, 0, 100), 1),
        Actualisation.de.soi = round(runif(n, 0, 100), 1),
        Cognitif = round(runif(n, 0, 100), 1),
        latitude = runif(n, 42, 51),
        longitude = runif(n, -5, 8)
      )
    } else {
      read.csv("CommunesGeo.csv", sep = " ", encoding = "UTF-8")
    }
  })
  
  data_reactive <- reactive({
    df <- df_geo()
    colnames(df) <- make.names(colnames(df))
    
    # Récupération des noms simples d'étage
    etages_sel_simple <- etage_noms[input$etages]
    etages_sel_cols <- make.names(etages_sel_simple)
    
    # Identifier l'étage dominant
    df$etage_dominant <- apply(df[, etages_sel_cols, drop = FALSE], 1, function(row) {
      if (all(is.na(row)) || all(row == 0)) return("Aucun")
      idx <- which.max(row)
      input$etages[match(etages_sel_simple[idx], etages_sel_simple)]
    })
    
    # Ajouter la couleur correspondante
    df$couleur <- etage_couleurs[df$etage_dominant]
    df$couleur[is.na(df$couleur)] <- "#95a5a6"
    
    # Tri éventuel
    if (input$tri != "Aucun") {
      tri_simple <- etage_noms[input$tri]
      col_tri <- make.names(tri_simple)
      if (col_tri %in% colnames(df)) {
        df <- df %>% arrange(desc(.data[[col_tri]]))
      }
    }
    
    df
  })
  
  # Mise à jour de la légende
  output$legend_content <- renderUI({
    etages_actifs <- input$etages
    legend_items <- lapply(etages_actifs, function(etage) {
      div(class = "legend-item",
          div(class = "legend-color", 
              style = paste0("background-color: ", etage_couleurs[etage])),
          span(etage, style = "font-size: 12px;")
      )
    })
    tagList(legend_items)
  })
  
  # Statistiques
  output$stats <- renderText({
    df <- data_reactive()
    total_communes <- nrow(df)
    etage_counts <- table(df$etage_dominant)
    
    stats_text <- paste0(
      "📍 Total communes: ", total_communes, "\n\n",
      "🏆 Étage dominant:\n"
    )
    
    for (etage in names(etage_counts)) {
      pct <- round(etage_counts[etage] / total_communes * 100, 1)
      stats_text <- paste0(stats_text, "• ", etage, ": ", etage_counts[etage], 
                           " (", pct, "%)\n")
    }
    
    stats_text
  })
  
  # Carte
  output$carte <- renderLeaflet({
    df <- data_reactive()
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 8,
        fillColor = ~couleur,
        color = "white", 
        weight = 2,
        fillOpacity = 0.8,
        label = lapply(seq_len(nrow(df)), function(i) {
          commune <- df$Commune[i]
          scores <- input$etages
          texte <- paste0("<div style='font-family: Arial; font-size: 13px;'>",
                          "<b style='color: #2c3e50; font-size: 14px;'>🏛️ ", commune, "</b><br><br>")
          
          for (score_etage in scores) {
            nom_simple <- etage_noms[score_etage]
            col <- make.names(nom_simple)
            if (col %in% colnames(df)) {
              couleur_etage <- etage_couleurs[score_etage]
              texte <- paste0(texte, 
                              "<div style='margin: 3px 0;'>",
                              "<span style='display: inline-block; width: 12px; height: 12px; background-color: ", 
                              couleur_etage, "; border-radius: 50%; margin-right: 8px;'></span>",
                              "<b>", score_etage, "</b> : ", df[[col]][i], "</div>")
            }
          }
          
          texte <- paste0(texte, "<br><b style='color: #e74c3c;'>🏆 Étage dominant :</b> ", 
                          df$etage_dominant[i], "</div>")
          HTML(texte)
        }),
        labelOptions = labelOptions(direction = "auto", 
                                    style = list("background-color" = "rgba(255,255,255,0.9)",
                                                 "border" = "1px solid #ccc",
                                                 "border-radius" = "5px",
                                                 "box-shadow" = "0 2px 4px rgba(0,0,0,0.2)"))
      ) %>%
      setView(lng = 2.5, lat = 46.5, zoom = 6)
  })
  
  # Tableau de données
  output$tableau <- DT::renderDataTable({
    df <- data_reactive()
    
    # Sélection des colonnes à afficher
    etages_sel_simple <- etage_noms[input$etages]
    etages_sel_cols <- make.names(etages_sel_simple)
    
    # Vérification que les colonnes existent
    cols_existantes <- intersect(etages_sel_cols, colnames(df))
    
    if (length(cols_existantes) > 0) {
      df_display <- df %>%
        select(Commune, all_of(cols_existantes), etage_dominant)
      
      # Renommage des colonnes pour l'affichage
      for (i in seq_along(cols_existantes)) {
        col_actuelle <- cols_existantes[i]
        etage_original <- names(etage_noms)[make.names(etage_noms) == col_actuelle]
        if (length(etage_original) > 0) {
          names(df_display)[names(df_display) == col_actuelle] <- etage_original[1]
        }
      }
    } else {
      df_display <- df %>% select(Commune, etage_dominant)
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
}

# Lancer l'application
shinyApp(ui, server)