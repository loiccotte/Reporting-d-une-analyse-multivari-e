# app.R
library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)

# --- Chargement / préparation des données
# Exemple de dataframe fourni par l'utilisateur
df=read.csv("Communes10-20K-Maslow.csv",sep=";")

# Charger le shapefile ou GeoJSON des communes (INSEE_COM à adapter)
communes_sf <- st_read("communes.geojson", quiet = TRUE)
# Jointure avec les données Maslow
communes_data <- communes_sf %>%
  left_join(df_maslow, by = c("INSEE_COM" = "COM"))

# --- UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "cerulean"),
  tags$head(
    # CSS personnalisé (optionnel)
    tags$link(rel = "stylesheet", href = "style.css")
  ),
  titlePanel("Carte des besoins selon Maslow par commune"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput(
        "niveau", "Sélectionnez un étage de la pyramide :",
        choices = c(
          "Physiologique", "Sécurité", "Appartenance",
          "Estime", "Actualisation.de.soi", "Cognitif"
        ),
        selected = "Physiologique"
      )
    ),
    
    mainPanel(
      width = 9,
      leafletOutput("map", height = "75vh")
    )
  )
)

# --- Serveur
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    req(input$niveau)
    
    # Palette adaptée aux valeurs sélectionnées
    vals <- communes_data[[input$niveau]]
    pal <- colorBin("YlOrRd", domain = vals, bins = 5, na.color = "transparent")
    
    leaflet(communes_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor   = ~pal(vals),
        weight      = 0.5,
        color       = "#444444",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 2, color = "#666", bringToFront = TRUE
        ),
        label = ~paste0(Commune, ": ", vals),
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      addLegend(
        pal = pal, values = vals, opacity = 0.8,
        title = input$niveau, position = "bottomright"
      )
  })
}

# --- Lancement de l'application
shinyApp(ui, server)
