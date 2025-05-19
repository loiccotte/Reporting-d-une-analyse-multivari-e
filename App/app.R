library(shiny)
library(leaflet)
library(dplyr)
library(bslib)

# Palette de couleurs par étage
etage_couleurs <- c(
  "Physiologique" = "#1f77b4",
  "Sécurité" = "#2ca02c",
  "Appartenance" = "#d62728",
  "Estime" = "#9467bd",
  "Actualisation.de.soi" = "#ff7f0e",
  "Cognitif" = "#17becf"
)

# UI ----
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  titlePanel("Carte des communes - Pyramide de Maslow"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("tri", "Trier par :", 
                  choices = c("Aucun", names(etage_couleurs))),
      checkboxGroupInput("etages", "Étages à afficher :", 
                         choices = names(etage_couleurs),
                         selected = names(etage_couleurs)[1]),
      width = 3
    ),
    
    mainPanel(
      leafletOutput("carte", height = 700)
    )
  )
)

# Server ----
server <- function(input, output, session) {
  
  # Chargement et nettoyage des données
  df_geo <- read.csv("CommunesGeo.csv", sep = " ", encoding = "UTF-8")
  colnames(df_geo) <- make.names(colnames(df_geo)) # nommage compatible
  
  data_reactive <- reactive({
    df <- df_geo
    
    # Étages sélectionnés
    etages_sel <- make.names(input$etages)
    
    # Identifier l'étage dominant
    df$etage_dominant <- apply(df[, etages_sel, drop = FALSE], 1, function(row) {
      idx <- which.max(row)
      input$etages[idx]
    })
    
    # Ajouter la couleur correspondante
    df$couleur <- etage_couleurs[df$etage_dominant]
    
    # Tri éventuel
    if (input$tri != "Aucun") {
      df <- df %>% arrange(desc(.data[[make.names(input$tri)]]))
    }
    
    df
  })
  
  output$carte <- renderLeaflet({
    df <- data_reactive()
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      clearMarkers() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        radius = 7,
        fillColor = ~couleur,
        color = "white", weight = 1,
        fillOpacity = 0.8,
        label = lapply(seq_len(nrow(df)), function(i) {
          commune <- df$Commune[i]
          scores <- input$etages
          texte <- paste0("<b>", commune, "</b><br>")
          for (score in scores) {
            col <- make.names(score)
            texte <- paste0(texte, score, " : ", df[[col]][i], "<br>")
          }
          texte <- paste0(texte, "<b>Ét. dominant :</b> ", df$etage_dominant[i])
          HTML(texte)
        }),
        labelOptions = labelOptions(direction = "auto")
      )
  })
}

# Lancer l'application
shinyApp(ui, server)
