# 📦 Librairies nécessaires
library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(RColorBrewer)

# 🎨 Interface utilisateur moderne
ui <- page_navbar(
  title = "Pyramide de Maslow - Communes françaises",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C3E50",
    secondary = "#95A5A6",
    success = "#27AE60",
    info = "#3498DB",
    warning = "#F39C12",
    danger = "#E74C3C"
  ),
  
  # Onglet Carte
  nav_panel(
    title = "🗺️ Carte Interactive",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        
        # Titre de la sidebar
        h3("Paramètres", class = "text-primary"),
        
        # Sélecteur de niveau géographique
        selectInput("niveau",
                    "Niveau géographique :",
                    choices = c("Communes" = "communes",
                                "Départements" = "departements", 
                                "Régions" = "regions"),
                    selected = "communes"),
        
        hr(),
        
        # Légende des besoins avec style moderne
        h4("Besoins de Maslow", class = "text-primary"),
        div(class = "legend-container",
            div(class = "legend-item",
                span(class = "legend-color", style = "background-color: #66C2A5;"),
                "Physiologique - 1"
            ),
            div(class = "legend-item",
                span(class = "legend-color", style = "background-color: #FC8D62;"),
                "Sécurité - 2"
            ),
            div(class = "legend-item",
                span(class = "legend-color", style = "background-color: #8DA0CB;"),
                "Appartenance - 3"
            ),
            div(class = "legend-item",
                span(class = "legend-color", style = "background-color: #E78AC3;"),
                "Estime - 4"
            ),
            div(class = "legend-item",
                span(class = "legend-color", style = "background-color: #A6D854;"),
                "Actualisation de soi - 5"
            ),
            div(class = "legend-item",
                span(class = "legend-color", style = "background-color: #FFD92F;"),
                "Cognitif - 6"
            )
        ),
        
        hr(),
        
        # Informations
        div(class = "info-box",
            h5("ℹ️ Information"),
            p("Visualisation des besoins dominants selon la pyramide de Maslow pour les communes françaises de 10 à 20K habitants.")
        )
      ),
      
      # Contenu principal - Carte
      card(
        full_screen = TRUE,
        card_header("Carte des besoins dominants"),
        leafletOutput("carte", height = "700px")
      )
    )
  ),
  
  # Onglet Tableau de données
  nav_panel(
    title = "📊 Données",
    layout_columns(
      col_widths = 12,
      card(
        full_screen = TRUE,
        card_header(
          "Tableau des données",
          class = "bg-primary text-white"
        ),
        card_body(
          div(
            style = "margin-bottom: 15px;",
            selectInput("niveau_table",
                        "Niveau d'affichage :",
                        choices = c("Communes" = "communes",
                                    "Départements" = "departements", 
                                    "Régions" = "regions"),
                        selected = "communes",
                        width = "300px")
          ),
          DTOutput("tableau")
        )
      )
    )
  ),
  
  # CSS personnalisé
  tags$head(
    tags$style(HTML("
      .legend-container {
        margin-top: 10px;
      }
      .legend-item {
        display: flex;
        align-items: center;
        margin-bottom: 8px;
        padding: 5px;
        border-radius: 5px;
        background-color: #f8f9fa;
      }
      .legend-color {
        width: 20px;
        height: 20px;
        border-radius: 50%;
        margin-right: 10px;
        border: 2px solid #fff;
        box-shadow: 0 1px 3px rgba(0,0,0,0.3);
      }
      .info-box {
        background-color: #e3f2fd;
        border-left: 4px solid #2196f3;
        padding: 10px;
        border-radius: 5px;
        margin-top: 10px;
      }
      .info-box h5 {
        margin-top: 0;
        color: #1976d2;
      }
      .navbar-brand {
        font-weight: bold;
      }
    "))
  )
)

# 🖥️ Serveur
server <- function(input, output, session) {
  
  # 📊 Chargement et préparation des données
  donnees_reactives <- reactive({
    
    # Définir les noms de colonnes pour CommunesGeo.csv (avec ID ajouté)
    colonnes_maslow <- c(
      "ID", "REG", "Région", "DEP", "CODARR", "CODCAN", "CODCOM", "COM", "Commune", 
      "PMUN", "PCAP", "PTOT", "Physiologique", "Sécurité", "Appartenance", 
      "Estime", "Actualisation.de.soi", "Cognitif", "latitude", "longitude"
    )
    
    # Lecture du fichier CSV (délimiteur point-virgule selon l'erreur)
    communes_geo <- read_delim("CommunesGeo.csv", 
                               delim = ";", 
                               col_names = colonnes_maslow,
                               locale = locale(encoding = "UTF-8"),
                               skip = 1,
                               show_col_types = FALSE)
    
    # Création du code INSEE propre
    communes_geo <- communes_geo %>%
      mutate(
        DEP = str_pad(as.character(DEP), 2, pad = "0"),
        CODCOM = str_pad(as.character(CODCOM), 3, pad = "0"),
        INSEE_COM = paste0(DEP, CODCOM),
        INSEE_COM = str_replace_all(INSEE_COM, "[[:space:]]", ""),
        REG = as.character(REG),  # Conversion en caractère pour les jointures
        DEP = as.character(DEP)   # Conversion en caractère pour les jointures
      )
    
    # Chargement des données géographiques selon le niveau sélectionné
    if (input$niveau == "communes") {
      donnees_geo <- readRDS("communes_simplified.rds") %>%
        mutate(INSEE_COM = str_trim(as.character(INSEE_COM)),
               INSEE_COM = str_replace_all(INSEE_COM, "[[:space:]]", ""))
      
      # Jointure avec les données Maslow
      donnees_finales <- donnees_geo %>%
        left_join(communes_geo, by = "INSEE_COM")
      
      # Calcul du besoin dominant
      besoins <- c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation.de.soi", "Cognitif")
      
      donnees_finales <- donnees_finales %>%
        rowwise() %>%
        mutate(
          besoin_dominant = if (all(is.na(c_across(all_of(besoins))))) {
            NA_character_
          } else {
            besoins[which.max(c_across(all_of(besoins)))]
          }
        ) %>%
        ungroup()
      
    } else if (input$niveau == "departements") {
      donnees_geo <- readRDS("departements_simplified.rds")
      
      # Agrégation des données au niveau départemental
      agg_dep <- communes_geo %>%
        group_by(DEP) %>%
        summarise(
          Physiologique = mean(Physiologique, na.rm = TRUE),
          Sécurité = mean(Sécurité, na.rm = TRUE),
          Appartenance = mean(Appartenance, na.rm = TRUE),
          Estime = mean(Estime, na.rm = TRUE),
          Actualisation.de.soi = mean(`Actualisation.de.soi`, na.rm = TRUE),
          Cognitif = mean(Cognitif, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(DEP = as.character(DEP))  # S'assurer que c'est en caractère
      
      # Jointure (s'assurer que les types correspondent)
      donnees_geo <- donnees_geo %>%
        mutate(code = as.character(code))
      
      donnees_finales <- donnees_geo %>%
        left_join(agg_dep, by = c("code" = "DEP"))
      
      # Calcul du besoin dominant
      besoins <- c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation.de.soi", "Cognitif")
      
      donnees_finales <- donnees_finales %>%
        rowwise() %>%
        mutate(
          besoin_dominant = if (all(is.na(c_across(all_of(besoins))))) {
            NA_character_
          } else {
            besoins[which.max(c_across(all_of(besoins)))]
          }
        ) %>%
        ungroup()
      
    } else { # régions
      donnees_geo <- readRDS("regions_simplified.rds")
      
      # Agrégation des données au niveau régional
      agg_reg <- communes_geo %>%
        group_by(REG) %>%
        summarise(
          Physiologique = mean(Physiologique, na.rm = TRUE),
          Sécurité = mean(Sécurité, na.rm = TRUE),
          Appartenance = mean(Appartenance, na.rm = TRUE),
          Estime = mean(Estime, na.rm = TRUE),
          Actualisation.de.soi = mean(`Actualisation.de.soi`, na.rm = TRUE),
          Cognitif = mean(Cognitif, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(REG = as.character(REG))  # S'assurer que c'est en caractère
      
      # Jointure (s'assurer que les types correspondent)
      donnees_geo <- donnees_geo %>%
        mutate(code = as.character(code))
      
      donnees_finales <- donnees_geo %>%
        left_join(agg_reg, by = c("code" = "REG"))
      
      # Calcul du besoin dominant
      besoins <- c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation.de.soi", "Cognitif")
      
      donnees_finales <- donnees_finales %>%
        rowwise() %>%
        mutate(
          besoin_dominant = if (all(is.na(c_across(all_of(besoins))))) {
            NA_character_
          } else {
            besoins[which.max(c_across(all_of(besoins)))]
          }
        ) %>%
        ungroup()
    }
    
    return(donnees_finales)
  })
  
  # 📊 Données pour le tableau (réactive séparée)
  donnees_tableau <- reactive({
    
    # Définir les noms de colonnes pour CommunesGeo.csv (avec ID ajouté)
    colonnes_maslow <- c(
      "ID", "REG", "Région", "DEP", "CODARR", "CODCAN", "CODCOM", "COM", "Commune", 
      "PMUN", "PCAP", "PTOT", "Physiologique", "Sécurité", "Appartenance", 
      "Estime", "Actualisation.de.soi", "Cognitif", "latitude", "longitude"
    )
    
    # Lecture du fichier CSV
    communes_geo <- read_delim("CommunesGeo.csv", 
                               delim = ";", 
                               col_names = colonnes_maslow,
                               locale = locale(encoding = "UTF-8"),
                               skip = 1,
                               show_col_types = FALSE)
    
    # Création du code INSEE propre
    communes_geo <- communes_geo %>%
      mutate(
        DEP = str_pad(as.character(DEP), 2, pad = "0"),
        CODCOM = str_pad(as.character(CODCOM), 3, pad = "0"),
        INSEE_COM = paste0(DEP, CODCOM),
        INSEE_COM = str_replace_all(INSEE_COM, "[[:space:]]", ""),
        REG = as.character(REG),
        DEP = as.character(DEP)
      )
    
    # Calcul du besoin dominant
    besoins <- c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation.de.soi", "Cognitif")
    
    communes_geo <- communes_geo %>%
      rowwise() %>%
      mutate(
        besoin_dominant = if (all(is.na(c_across(all_of(besoins))))) {
          NA_character_
        } else {
          besoins[which.max(c_across(all_of(besoins)))]
        }
      ) %>%
      ungroup()
    
    # Préparation selon le niveau
    if (input$niveau_table == "communes") {
      tableau <- communes_geo %>%
        select(Commune, DEP, Région, Physiologique, Sécurité, Appartenance, 
               Estime, `Actualisation.de.soi`, Cognitif, besoin_dominant) %>%
        rename(
          `Commune` = Commune,
          `Département` = DEP,
          `Région` = Région,
          `Actualisation de soi` = `Actualisation.de.soi`,
          `Besoin dominant` = besoin_dominant
        )
    } else if (input$niveau_table == "departements") {
      tableau <- communes_geo %>%
        group_by(DEP, Région) %>%
        summarise(
          Physiologique = round(mean(Physiologique, na.rm = TRUE), 2),
          Sécurité = round(mean(Sécurité, na.rm = TRUE), 2),
          Appartenance = round(mean(Appartenance, na.rm = TRUE), 2),
          Estime = round(mean(Estime, na.rm = TRUE), 2),
          `Actualisation de soi` = round(mean(`Actualisation.de.soi`, na.rm = TRUE), 2),
          Cognitif = round(mean(Cognitif, na.rm = TRUE), 2),
          .groups = 'drop'
        ) %>%
        rowwise() %>%
        mutate(
          `Besoin dominant` = if (all(is.na(c_across(c(Physiologique, Sécurité, Appartenance, Estime, `Actualisation de soi`, Cognitif))))) {
            NA_character_
          } else {
            c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation de soi", "Cognitif")[which.max(c_across(c(Physiologique, Sécurité, Appartenance, Estime, `Actualisation de soi`, Cognitif)))]
          }
        ) %>%
        ungroup() %>%
        rename(`Département` = DEP)
    } else { # régions
      tableau <- communes_geo %>%
        group_by(REG, Région) %>%
        summarise(
          Physiologique = round(mean(Physiologique, na.rm = TRUE), 2),
          Sécurité = round(mean(Sécurité, na.rm = TRUE), 2),
          Appartenance = round(mean(Appartenance, na.rm = TRUE), 2),
          Estime = round(mean(Estime, na.rm = TRUE), 2),
          `Actualisation de soi` = round(mean(`Actualisation.de.soi`, na.rm = TRUE), 2),
          Cognitif = round(mean(Cognitif, na.rm = TRUE), 2),
          .groups = 'drop'
        ) %>%
        rowwise() %>%
        mutate(
          `Besoin dominant` = if (all(is.na(c_across(c(Physiologique, Sécurité, Appartenance, Estime, `Actualisation de soi`, Cognitif))))) {
            NA_character_
          } else {
            c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation de soi", "Cognitif")[which.max(c_across(c(Physiologique, Sécurité, Appartenance, Estime, `Actualisation de soi`, Cognitif)))]
          }
        ) %>%
        ungroup() %>%
        select(-REG)
    }
    
    return(tableau)
  })
  
  # 🗺️ Rendu de la carte
  output$carte <- renderLeaflet({
    
    donnees <- donnees_reactives()
    
    # Définition des besoins avec numérotation
    besoins_complets <- c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation.de.soi", "Cognitif")
    
    # Palette de couleurs
    pal <- colorFactor(
      palette = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"),
      domain = besoins_complets
    )
    
    # Création du label selon le niveau
    if (input$niveau == "communes") {
      label_text <- ~paste0(NOM, " — ", besoin_dominant)
    } else {
      label_text <- ~paste0(nom, " — ", besoin_dominant)
    }
    
    # Création de la carte
    leaflet(donnees) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(besoin_dominant),
        weight = 0.5,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        label = label_text,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~besoin_dominant,
        title = "Besoin dominant",
        opacity = 1,
        position = "bottomright"
      ) %>%
      setView(lng = 2.3, lat = 46.6, zoom = 6)
  })
  
  # 📊 Rendu du tableau
  output$tableau <- renderDT({
    datatable(
      donnees_tableau(),
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel'),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
        )
      ),
      extensions = 'Buttons',
      class = 'cell-border stripe hover',
      rownames = FALSE
    ) %>%
      formatStyle(
        'Besoin dominant',
        backgroundColor = styleEqual(
          c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation de soi", "Cognitif"),
          c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")
        ),
        color = 'white',
        fontWeight = 'bold'
      )
  })
}

# 🚀 Lancement de l'application
shinyApp(ui = ui, server = server)