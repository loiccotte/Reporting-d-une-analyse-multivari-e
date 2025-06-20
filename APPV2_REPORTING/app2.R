library(shiny)
library(bslib)
library(leaflet)
library(DT)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(plotly)


# 🎨 Thème UI ultra-moderne
modern_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#1e3a8a",
  secondary = "#64748b",
  success = "#059669",
  info = "#0284c7",
  warning = "#d97706",
  danger = "#dc2626",
  dark = "#0f172a",
  light = "#f8fafc",
  base_font = font_google("Inter"),
  heading_font = font_google("Poppins", wght = c(400, 600, 700)),
  code_font = font_google("JetBrains Mono")
)

# 🖼️ UI ultra-moderne avec animations et effets
ui <- page_navbar(
  title = tags$div(
    class = "navbar-brand-custom",
    tags$div(
      class = "brand-icon",
      icon("mountain", class = "fa-2x")
    ),
    tags$div(
      class = "brand-text",
      tags$h4("Pyramide de Maslow", class = "mb-0"),
      tags$small("Analyse des communes françaises", class = "text-muted")
    )
  ),
  theme = modern_theme,
  fillable = TRUE,
  id = "main_navbar",
  
  # 🗺️ ONGLET CARTE - Design premium
  nav_panel(
    title = tags$span(
      icon("map-location-dot", class = "me-2"),
      "Carte Interactive"
    ),
    value = "carte_tab",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        class = "sidebar-modern",
        
        # Header du sidebar avec gradient
        tags$div(
          class = "sidebar-header",
          tags$h3(
            icon("sliders", class = "me-2"),
            "Paramètres",
            class = "sidebar-title"
          )
        ),
        
        # Section niveau géographique
        tags$div(
          class = "control-section",
          tags$div(
            class = "section-title",
            icon("globe", class = "me-2"),
            "Niveau géographique"
          ),
          selectInput(
            "niveau", 
            NULL,
            choices = list(
              "🏘️ Communes" = "communes",
              "🏛️ Départements" = "departements", 
              "🌍 Régions" = "regions"
            ),
            selected = "communes"
          )
        ),
        
        tags$hr(class = "section-divider"),
        
        # Section légende Maslow avec design cards
        tags$div(
          class = "control-section",
          tags$div(
            class = "section-title",
            icon("layer-group", class = "me-2"),
            "Hiérarchie des besoins"
          ),
          tags$div(
            class = "maslow-legend",
            lapply(1:6, function(i) {
              couleurs <- c("#10b981", "#f59e0b", "#8b5cf6", "#ef4444", "#06b6d4", "#84cc16")
              noms <- c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation", "Cognitif")
              icones <- c("utensils", "shield-alt", "users", "trophy", "star", "brain")
              
              tags$div(
                class = "maslow-item",
                tags$div(
                  class = "maslow-level",
                  style = paste0("background: linear-gradient(135deg, ", couleurs[i], ", ", couleurs[i], "aa);"),
                  tags$i(class = paste("fas fa", icones[i]))
                ),
                tags$div(
                  class = "maslow-info",
                  tags$strong(paste("Niveau", i)),
                  tags$br(),
                  tags$span(noms[i], class = "maslow-name")
                )
              )
            })
          )
        ),
        
        tags$hr(class = "section-divider"),
        
        # Info box moderne
        tags$div(
          class = "info-card",
          tags$div(
            class = "info-header",
            icon("info-circle", class = "me-2"),
            "À propos"
          ),
          tags$p(
            "Cette visualisation présente les besoins dominants selon la pyramide de Maslow pour les territoires français.",
            class = "info-text"
          ),
          tags$div(
            class = "stats-mini",
            tags$div(class = "stat-item", 
                     tags$strong("35,000+"), 
                     tags$br(), 
                     tags$small("Communes")),
            tags$div(class = "stat-item", 
                     tags$strong("101"), 
                     tags$br(), 
                     tags$small("Départements")),
            tags$div(class = "stat-item", 
                     tags$strong("13"), 
                     tags$br(), 
                     tags$small("Régions"))
          )
        )
      ),
      
      # Zone principale avec carte
      tags$div(
        class = "main-content",
        card(
          full_screen = TRUE,
          class = "map-card",
          card_header(
            class = "card-header-modern",
            tags$div(
              class = "d-flex align-items-center",
              icon("map", class = "me-2 text-white"),
              tags$div(
                tags$h4("Cartographie des besoins dominants", class = "mb-0 text-white"),
                tags$small("Cliquez sur les territoires pour plus d'informations", class = "text-white-50")
              )
            )
          ),
          card_body(
            class = "p-0",
            leafletOutput("carte", height = "750px")
          )
        )
      )
    )
  ),
  
  # 📊 ONGLET TABLEAU - Design data-focused
  nav_panel(
    title = tags$span(
      icon("table", class = "me-2"),
      "Données"
    ),
    value = "tableau_tab",
    
    tags$div(
      class = "page-container",
      
      # Header de page
      tags$div(
        class = "page-header",
        tags$h2(
          icon("database", class = "me-3"),
          "Exploration des données",
          class = "page-title"
        ),
        tags$p("Analysez en détail les scores de chaque territoire", class = "page-subtitle")
      ),
      
      # Controls et tableau
      card(
        class = "data-card",
        card_header(
          class = "card-header-modern",
          tags$div(
            class = "d-flex justify-content-between align-items-center",
            tags$div(
              icon("filter", class = "me-2"),
              "Filtres et données"
            ),
            tags$div(
              class = "level-selector",
              selectInput(
                "niveau_table", 
                NULL,
                choices = list(
                  "🏘️ Communes" = "communes",
                  "🏛️ Départements" = "departements", 
                  "🌍 Régions" = "regions"
                ),
                selected = "communes",
                width = "200px"
              )
            )
          )
        ),
        card_body(
          DTOutput("tableau", height = "600px")
        )
      )
    )
  ),
  
  # 📈 ONGLET ACP - Design analytique
  nav_panel(
    title = tags$span(
      icon("chart-line", class = "me-2"),
      "ACP"
    ),
    value = "acp_tab",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        class = "sidebar-modern sidebar-analytics",
        
        tags$div(
          class = "sidebar-header",
          tags$h3(
            icon("microscope", class = "me-2"),
            "Configuration ACP",
            class = "sidebar-title"
          )
        ),
        
        # Paramètres ACP
        tags$div(
          class = "control-section",
          tags$div(
            class = "section-title",
            icon("cogs", class = "me-2"),
            "Paramètres d'analyse"
          ),
          
          selectInput(
            "niveau_acp", 
            "Niveau d'agrégation :",
            choices = list(
              "🏘️ Communes" = "communes",
              "🏛️ Départements" = "departements", 
              "🌍 Régions" = "regions"
            ),
            selected = "communes"
          ),
          
          tags$div(
            class = "custom-checkbox",
            checkboxInput("exclure_paris", "Exclure Paris de l'analyse", value = FALSE)
          ),
          
          selectInput("type_viz", "Type de visualisation :",
                      choices = c(
                        "ACP Individus (Dim 1-2)" = "ind_12",
                        "ACP Individus (Dim 1-3)" = "ind_13",
                        "Cercle des corrélations (1-2)" = "var_circle",
                        "Cercle des corrélations (1-3)" = "var_circle13",
                        "Contribution des dimensions" = "eigenvalues"),
                      selected = "ind_12"),
          
        ),
        
        # Case pour afficher ou non les labels (communes, départements, régions)
        conditionalPanel(
          condition = "input.type_viz.includes('ind_')",
          tags$div(
            class = "custom-checkbox",
            checkboxInput("show_labels", "Afficher les étiquettes", value = TRUE)
          )
        ),
        
        
        
        # Statistiques des clusters
        tags$div(
          class = "control-section",
          tags$div(
            class = "section-title",
            icon("chart-pie", class = "me-2"),
            "Statistiques clusters"
          ),
          tags$div(
            class = "stats-output",
            verbatimTextOutput("stats_clusters")
          )
        )
      ),
      
      # Zone principale ACP
      tags$div(
        class = "main-content",
        layout_columns(
          col_widths = c(8, 4),
          
          # Graphique principal
          card(
            full_screen = TRUE,
            class = "chart-card",
            card_header(
              class = "card-header-modern",
              tags$div(
                icon("chart-area", class = "me-2 text-primary"),
                tags$h4("Analyse en Composantes Principales", class = "mb-0"),
                tags$small("Exploration multidimensionnelle des données", class = "text-muted")
              )
            ),
            card_body(
              uiOutput("acp_plot_ui")
              
              
            )
          ),
          
         
        )
      )
    )
  ),
  
  # 🎨 CSS MODERNE ET PROFESSIONNEL
  tags$head(
    tags$style(HTML("
      /* Variables CSS pour cohérence */
      :root {
        --primary-gradient: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        --success-gradient: linear-gradient(135deg, #11998e 0%, #38ef7d 100%);
        --warning-gradient: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
        --shadow-soft: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
        --shadow-medium: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05);
        --shadow-large: 0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04);
        --border-radius: 12px;
        --transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      /* Body et layout général */
      body {
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
      }
      
      # Navigation moderne
      .navbar {
        background: rgba(255, 255, 255, 0.95) !important;
        backdrop-filter: blur(20px);
        border-bottom: 1px solid rgba(255, 255, 255, 0.2);
        box-shadow: var(--shadow-soft);
        padding: 10px 0 !important;
      }
      
      .navbar-nav {
        align-items: center;
      }
      
      .nav-link {
        transition: var(--transition) !important;
        font-weight: 500 !important;
        padding: 8px 16px !important;
        border-radius: 8px !important;
        margin: 0 4px !important;
      }
      
      .nav-link:hover {
        transform: translateY(-1px);
        background: rgba(99, 102, 241, 0.1) !important;
      }
      
      .nav-link.active {
        background: var(--primary-gradient) !important;
        color: white !important;
      }
      
      .navbar-brand-custom {
        display: flex;
        align-items: center;
        gap: 15px;
        padding: 8px 0;
      }
      
      .brand-icon {
        width: 50px;
        height: 50px;
        background: var(--primary-gradient);
        border-radius: 12px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: white;
        box-shadow: var(--shadow-medium);
      }
      
      .brand-text h4 {
        color: #1e293b;
        font-weight: 700;
        margin: 0;
      }
      
      .brand-text small {
        color: #64748b;
        font-weight: 500;
      }
      
      /* Sidebar moderne */
      .sidebar-modern {
        background: rgba(255, 255, 255, 0.95) !important;
        backdrop-filter: blur(20px);
        border-radius: var(--border-radius);
        border: 1px solid rgba(255, 255, 255, 0.2);
        box-shadow: var(--shadow-large);
        margin: 20px;
        height: calc(100vh - 140px);
        overflow-y: auto;
      }
      
      .sidebar-header {
        background: var(--primary-gradient);
        margin: -16px -16px 20px -16px;
        padding: 20px;
        border-radius: var(--border-radius) var(--border-radius) 0 0;
      }
      
      .sidebar-title {
        color: white;
        margin: 0;
        font-weight: 600;
        font-size: 1.2rem;
      }
      
      /* Sections de contrôles */
      .control-section {
        margin-bottom: 25px;
      }
      
      .section-title {
        font-weight: 600;
        color: #374151;
        margin-bottom: 15px;
        padding: 8px 12px;
        background: rgba(99, 102, 241, 0.1);
        border-radius: 8px;
        font-size: 0.9rem;
      }
      
      .section-divider {
        border: none;
        height: 1px;
        background: linear-gradient(90deg, transparent, #e5e7eb, transparent);
        margin: 20px 0;
      }
      
      /* Légende Maslow moderne */
      .maslow-legend {
        display: flex;
        flex-direction: column;
        gap: 12px;
      }
      
      .maslow-item {
        display: flex;
        align-items: center;
        gap: 15px;
        padding: 12px;
        background: rgba(255, 255, 255, 0.7);
        border-radius: 10px;
        border: 1px solid rgba(255, 255, 255, 0.3);
        transition: var(--transition);
        cursor: pointer;
      }
      
      .maslow-item:hover {
        transform: translateY(-2px);
        box-shadow: var(--shadow-medium);
        background: rgba(255, 255, 255, 0.9);
      }
      
      .maslow-level {
        width: 45px;
        height: 45px;
        border-radius: 12px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: white;
        font-size: 1.2rem;
        box-shadow: var(--shadow-soft);
      }
      
      .maslow-info strong {
        color: #374151;
        font-size: 0.85rem;
        font-weight: 600;
      }
      
      .maslow-name {
        color: #6b7280;
        font-size: 0.8rem;
        font-weight: 500;
      }
      
      /* Cards modernes */
      .card {
        border: none !important;
        border-radius: var(--border-radius) !important;
        box-shadow: var(--shadow-medium) !important;
        background: rgba(255, 255, 255, 0.95) !important;
        backdrop-filter: blur(20px);
        transition: var(--transition);
      }
      
      .card:hover {
        transform: translateY(-2px);
        box-shadow: var(--shadow-large) !important;
      }
      
      .card-header-modern {
        background: var(--primary-gradient) !important;
        color: white !important;
        border: none !important;
        border-radius: var(--border-radius) var(--border-radius) 0 0 !important;
        padding: 20px !important;
      }
      
      .card-header-modern h4 {
        font-weight: 600;
        font-size: 1.1rem;
      }
      
      .card-header-modern small {
        opacity: 0.9;
      }
      
      /* Page headers */
      .page-container {
        padding: 30px;
      }
      
      .page-header {
        margin-bottom: 30px;
        text-align: center;
      }
      
      .page-title {
        color: #1e293b;
        font-weight: 700;
        margin-bottom: 10px;
      }
      
      .page-subtitle {
        color: #64748b;
        font-size: 1.1rem;
        font-weight: 500;
      }
      
      /* Info card moderne */
      .info-card {
        background: linear-gradient(135deg, rgba(59, 130, 246, 0.1), rgba(139, 92, 246, 0.1));
        border: 1px solid rgba(59, 130, 246, 0.2);
        border-radius: var(--border-radius);
        padding: 20px;
        margin-top: 20px;
      }
      
      .info-header {
        font-weight: 600;
        color: #1e40af;
        margin-bottom: 12px;
        font-size: 1rem;
      }
      
      .info-text {
        color: #475569;
        line-height: 1.6;
        margin-bottom: 15px;
      }
      
      .stats-mini {
        display: flex;
        justify-content: space-between;
        gap: 15px;
      }
      
      .stat-item {
        text-align: center;
        flex: 1;
        padding: 8px;
        background: rgba(255, 255, 255, 0.5);
        border-radius: 8px;
      }
      
      .stat-item strong {
        color: #1e40af;
        font-size: 1.1rem;
      }
      
      /* Inputs modernes */
      .form-select, .form-control {
        border: 2px solid rgba(229, 231, 235, 0.8) !important;
        border-radius: 10px !important;
        padding: 12px 16px !important;
        background: rgba(255, 255, 255, 0.9) !important;
        transition: var(--transition) !important;
        font-weight: 500;
      }
      
      .form-select:focus, .form-control:focus {
        border-color: #6366f1 !important;
        box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.1) !important;
        transform: translateY(-1px);
      }
      
      /* Checkbox personnalisé */
      .custom-checkbox {
        margin: 15px 0;
      }
      
      .form-check-input:checked {
        background-color: #6366f1 !important;
        border-color: #6366f1 !important;
      }
      
      /* Stats output */
      .stats-output pre {
        background: rgba(15, 23, 42, 0.05) !important;
        border: 1px solid rgba(226, 232, 240, 0.5) !important;
        border-radius: 10px !important;
        padding: 15px !important;
        font-size: 0.85rem;
        color: #374151;
        line-height: 1.5;
      }
      
      /* DataTable moderne */
      .dataTables_wrapper {
        font-family: 'Inter', sans-serif !important;
        padding: 20px;
      }
      
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter {
        margin-bottom: 20px;
      }
      
      .dataTables_wrapper .dataTables_length select,
      .dataTables_wrapper .dataTables_filter input {
        border: 2px solid rgba(229, 231, 235, 0.8) !important;
        border-radius: 8px !important;
        padding: 8px 12px !important;
        font-weight: 500;
        background: rgba(255, 255, 255, 0.9) !important;
      }
      
      .dataTables_wrapper .dataTables_length label,
      .dataTables_wrapper .dataTables_filter label {
        font-weight: 500;
        color: #374151;
      }
      
      table.dataTable {
        border-radius: var(--border-radius) !important;
        overflow: hidden;
        box-shadow: var(--shadow-soft);
      }
      
      table.dataTable thead th {
        background: var(--primary-gradient) !important;
        color: white !important;
        border: none !important;
        font-weight: 600;
        padding: 15px 12px !important;
        text-align: center;
      }
      
      table.dataTable tbody td {
        padding: 12px !important;
        vertical-align: middle;
        border-bottom: 1px solid rgba(229, 231, 235, 0.5) !important;
      }
      
      table.dataTable tbody tr:hover {
        background: rgba(99, 102, 241, 0.05) !important;
      }
      
      table.dataTable tbody tr:nth-child(even) {
        background: rgba(248, 250, 252, 0.5);
      }
      
      /* Animations et transitions */
      .nav-item {
        margin: 0 2px;
      }
      
      /* Responsive */
      @media (max-width: 768px) {
        .navbar-brand-custom {
          flex-direction: column;
          gap: 8px;
        }
        
        .brand-icon {
          width: 40px;
          height: 40px;
        }
        
        .sidebar-modern {
          margin: 10px;
          height: auto;
        }
        
        .page-container {
          padding: 15px;
        }
        
        .stats-mini {
          flex-direction: column;
          gap: 10px;
        }
      }
      
      /* Scrollbar personnalisée */
      ::-webkit-scrollbar {
        width: 8px;
      }
      
      ::-webkit-scrollbar-track {
        background: rgba(241, 245, 249, 0.5);
        border-radius: 4px;
      }
      
      ::-webkit-scrollbar-thumb {
        background: linear-gradient(135deg, #6366f1, #8b5cf6);
        border-radius: 4px;
      }
      
      ::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(135deg, #4f46e5, #7c3aed);
      }
    "))
  )
)


# 🖥️ Serveur
server <- function(input, output, session) {
  # 🔧 Fonctions utilitaires
  load_data <- function() {
    colonnes <- c("ID", "REG", "Région", "DEP", "CODARR", "CODCAN", "CODCOM", "COM", "Commune", 
                  "PMUN", "PCAP", "PTOT", "Physiologique", "Sécurité", "Appartenance", 
                  "Estime", "Actualisation.de.soi", "Cognitif", "latitude", "longitude")
    
    read_delim("CommunesGeo.csv", delim = ";", col_names = colonnes,
               locale = locale(encoding = "UTF-8"), skip = 1, show_col_types = FALSE) %>%
      mutate(
        DEP = str_pad(as.character(DEP), 2, pad = "0"),
        CODCOM = str_pad(as.character(CODCOM), 3, pad = "0"),
        INSEE_COM = paste0(DEP, CODCOM),
        INSEE_COM = str_replace_all(INSEE_COM, "[[:space:]]", ""),
        REG = as.character(REG), DEP = as.character(DEP)
      )
  }
  
  calc_dominant_need <- function(data) {
    besoins <- c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation.de.soi", "Cognitif")
    data %>%
      rowwise() %>%
      mutate(besoin_dominant = if (all(is.na(c_across(all_of(besoins))))) NA_character_ 
             else besoins[which.max(c_across(all_of(besoins)))]) %>%
      ungroup()
  }
  
  aggregate_data <- function(data, level, exclure_paris = FALSE) {
    if (exclure_paris) {
      data <- data %>% filter(!str_detect(Commune, "Paris"))
    }
    
    if (level == "communes") return(data)
    
    group_var <- if (level == "departements") "DEP" else "REG"
    
    data %>%
      group_by(!!sym(group_var)) %>%
      summarise(
        across(c(Physiologique, `Sécurité`, Appartenance, Estime, `Actualisation.de.soi`, Cognitif, PTOT), 
               ~ mean(.x, na.rm = TRUE)),
        .groups = 'drop'
      ) %>%
      rename(!!sym(if (level == "departements") "code" else "code") := !!sym(group_var))
  }
  
  # Données de base
  base_data <- reactive({ load_data() })
  
  # Données pour la carte
  donnees_reactives <- reactive({
    communes_geo <- base_data()
    
    if (input$niveau == "communes") {
      donnees_geo <- readRDS("communes_simplified.rds") %>%
        mutate(INSEE_COM = str_trim(as.character(INSEE_COM)),
               INSEE_COM = str_replace_all(INSEE_COM, "[[:space:]]", ""))
      
      donnees_finales <- donnees_geo %>%
        left_join(communes_geo, by = "INSEE_COM") %>%
        calc_dominant_need()
    } else {
      geo_file <- paste0(input$niveau, "_simplified.rds")
      donnees_geo <- readRDS(geo_file) %>% mutate(code = as.character(code))
      
      agg_data <- aggregate_data(communes_geo, input$niveau)
      donnees_finales <- donnees_geo %>%
        left_join(agg_data, by = "code") %>%
        calc_dominant_need()
    }
    
    return(donnees_finales)
  })
  
  # Données pour le tableau
  donnees_tableau <- reactive({
    communes_geo <- base_data() %>% calc_dominant_need()
    
    if (input$niveau_table == "communes") {
      communes_geo %>%
        select(Commune, DEP, Région, Physiologique, `Sécurité`, Appartenance, 
               Estime, `Actualisation.de.soi`, Cognitif, besoin_dominant) %>%
        rename(`Département` = DEP, `Actualisation de soi` = `Actualisation.de.soi`, 
               `Besoin dominant` = besoin_dominant)
    } else {
      group_var <- if (input$niveau_table == "departements") c("DEP", "Région") else c("REG", "Région")
      
      communes_geo %>%
        group_by(across(all_of(group_var))) %>%
        summarise(across(c(Physiologique, `Sécurité`, Appartenance, Estime, `Actualisation.de.soi`, Cognitif), 
                         ~ round(mean(.x, na.rm = TRUE), 2)), .groups = 'drop') %>%
        calc_dominant_need() %>%
        rename(`Actualisation de soi` = `Actualisation.de.soi`, `Besoin dominant` = besoin_dominant)
    }
  })
  
  # Données pour l'ACP
  donnees_acp <- reactive({
    donnees <- base_data()
    
    # Normalisation des noms de colonnes pour l'ACP
    donnees <- donnees %>%
      rename(
        Securite = `Sécurité`, 
        Actualisationdesoi = `Actualisation.de.soi`
      )
    
    # Filtrage Paris si demandé
    if (input$exclure_paris) {
      donnees <- donnees %>% filter(!str_detect(Commune, "Paris"))
    }
    
    # Agrégation selon le niveau
    if (input$niveau_acp == "departements") {
      donnees <- donnees %>%
        group_by(DEP) %>%
        summarise(
          across(c(Physiologique, Securite, Appartenance, Estime, Actualisationdesoi, Cognitif, PTOT), 
                 ~ mean(.x, na.rm = TRUE)),
          .groups = 'drop'
        ) %>%
        mutate(code = DEP)
    } else if (input$niveau_acp == "regions") {
      donnees <- donnees %>%
        group_by(REG, Région) %>%  # Conserver le nom
        summarise(
          across(c(Physiologique, Securite, Appartenance, Estime, Actualisationdesoi, Cognitif, PTOT), 
                 ~ mean(.x, na.rm = TRUE)),
          .groups = 'drop'
        ) %>%
        mutate(code = REG)
      
    }
    
    # Calcul des proportions
    donnees_prop <- donnees %>%
      mutate(
        total_maslow = Physiologique + Securite + Appartenance + Estime + Actualisationdesoi + Cognitif,
        across(c(Physiologique, Securite, Appartenance, Estime, Actualisationdesoi, Cognitif), 
               ~ .x / total_maslow, .names = "prop_{.col}")
      )
    
    # Préparation des données pour l'ACP
    maslow_data <- donnees_prop %>%
      select(starts_with("prop_")) %>%
      rename_with(~ str_remove(.x, "prop_")) %>%
      rename(
        `actualisation de soi` = Actualisationdesoi, 
        sécurité = Securite, 
        physiologique = Physiologique, 
        appartenance = Appartenance, 
        estime = Estime, 
        cognitif = Cognitif
      ) %>%
      filter(complete.cases(.))
    
    # Vérification qu'il y a assez de données
    if(nrow(maslow_data) < 3) {
      return(NULL)
    }
    
    # ACP
    res_acp <- PCA(maslow_data, graph = FALSE)
    res_hcpc <- HCPC(res_acp, graph = FALSE)
    
    donnees_finales <- cbind(donnees_prop[complete.cases(maslow_data), ], 
                             cluster = as.factor(res_hcpc$data.clust$clust))
    
    list(donnees = donnees_finales, acp = res_acp, hcpc = res_hcpc)
  })
  
  # Rendu carte
  output$carte <- renderLeaflet({
    donnees <- donnees_reactives()
    besoins <- c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation.de.soi", "Cognitif")
    pal <- colorFactor(palette = c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"), 
                       domain = besoins)
    
    label_text <- if (input$niveau == "communes") ~paste0(NOM, " — ", besoin_dominant) 
    else ~paste0(nom, " — ", besoin_dominant)
    
    leaflet(donnees) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = ~pal(besoin_dominant), weight = 0.5, opacity = 1, color = "white",
                  dashArray = "3", fillOpacity = 0.8, label = label_text,
                  highlightOptions = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = ~besoin_dominant, title = "Besoin dominant", opacity = 1, position = "bottomright") %>%
      setView(lng = 2.3, lat = 46.6, zoom = 6)
  })
  
  # Rendu tableau
  output$tableau <- renderDT({
    datatable(donnees_tableau(), options = list(pageLength = 25, scrollX = TRUE, dom = 'Bfrtip',
                                                buttons = c('copy', 'csv', 'excel'),
                                                language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')),
              extensions = 'Buttons', class = 'cell-border stripe hover', rownames = FALSE) %>%
      formatStyle('Besoin dominant', 
                  backgroundColor = styleEqual(c("Physiologique", "Sécurité", "Appartenance", "Estime", "Actualisation de soi", "Cognitif"),
                                               c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F")),
                  color = 'white', fontWeight = 'bold')
  })
  
  
  
  output$acp_plot_ui <- renderUI({
    if (grepl("^ind_", input$type_viz)) {
      plotlyOutput("plot_acp_plotly", height = "650px")
    } else {
      plotOutput("plot_acp_static", height = "650px")
    }
  })
  output$plot_acp_plotly <- renderPlotly({
    acp_data <- donnees_acp()
    if (is.null(acp_data)) return(NULL)
    
    labels <- NULL
    if (input$niveau_acp == "departements") labels <- acp_data$donnees$DEP
    if (input$niveau_acp == "regions") labels <- acp_data$donnees$Région
    
    plot_base <- switch(input$type_viz,
                        "ind_12" = fviz_pca_ind(acp_data$acp, axes = c(1, 2), col.ind = acp_data$donnees$cluster,
                                                label = "none", geom = "point", repel = TRUE) +
                          ggtitle("ACP - Dimension 1 et 2") + theme_minimal(),
                        "ind_13" = fviz_pca_ind(acp_data$acp, axes = c(1, 3), col.ind = acp_data$donnees$cluster,
                                                label = "none", geom = "point", repel = TRUE) +
                          ggtitle("ACP - Dimension 1 et 3") + theme_minimal(),
                        "ind_23" = fviz_pca_ind(acp_data$acp, axes = c(2, 3), col.ind = acp_data$donnees$cluster,
                                                label = "none", geom = "point", repel = TRUE) +
                          ggtitle("ACP - Dimension 2 et 3") + theme_minimal()
    )
    
    if (!is.null(labels) && isTRUE(input$show_labels)) {
      coords <- as.data.frame(acp_data$acp$ind$coord)
      axes <- switch(input$type_viz,
                     "ind_12" = c("Dim.1", "Dim.2"),
                     "ind_13" = c("Dim.1", "Dim.3"),
                     "ind_23" = c("Dim.2", "Dim.3"))
      coords$label <- labels
      plot_base <- plot_base +
        geom_text(data = coords, aes_string(x = axes[1], y = axes[2], label = "label"),
                  vjust = -0.5, size = 3, inherit.aes = FALSE)
    }
    
    
    ggplotly(plot_base)
  })
  output$plot_acp_static <- renderPlot({
    acp_data <- donnees_acp()
    if (is.null(acp_data)) {
      plot.new()
      text(0.5, 0.5, "Données insuffisantes", cex = 1.5)
      return()
    }
    
    switch(input$type_viz,
           "var_circle" = fviz_pca_var(acp_data$acp, axes = c(1, 2), col.var = "contrib",
                                       gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) +
             ggtitle("Cercle des corrélations - Dim 1 et 2") + theme_minimal(),
           "var_circle13" = fviz_pca_var(acp_data$acp, axes = c(1, 3), col.var = "contrib",
                                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) +
             ggtitle("Cercle des corrélations - Dim 1 et 3") + theme_minimal(),
           "eigenvalues" = fviz_eig(acp_data$acp, addlabels = TRUE, barfill = "#0073C2FF",
                                    barcolor = "#0073C2FF", linecolor = "black") +
             ggtitle("Contribution des dimensions") + theme_minimal()
    )
  })
  
  
  # Statistiques clusters
  output$stats_clusters <- renderText({
    acp_data <- donnees_acp()
    
    if(is.null(acp_data)) {
      return("Données insuffisantes pour l'analyse")
    }
    
    moyennes_pop <- acp_data$donnees %>%
      group_by(cluster) %>%
      summarise(effectif = n(), 
                moyenne_PTOT = round(mean(PTOT, na.rm = TRUE), 0),
                mediane_PTOT = round(median(PTOT, na.rm = TRUE), 0), 
                .groups = 'drop')
    
    output_text <- paste("Clusters:", max(as.numeric(as.character(acp_data$donnees$cluster))), "\n\n")
    for (i in 1:nrow(moyennes_pop)) {
      output_text <- paste0(output_text, "Cluster ", moyennes_pop$cluster[i], ":\n",
                            "  Effectif: ", moyennes_pop$effectif[i], "\n",
                            "  Pop. moy: ", format(moyennes_pop$moyenne_PTOT[i], big.mark = " "), "\n\n")
    }
    output_text
  })
  
  
}

# 🚀 Lancement
shinyApp(ui = ui, server = server)