library(shiny)
library(leaflet)
library(sf)
library(rnaturalearth)
library(dbscan)
library(cluster)
library(ggplot2)



# Chargement des données depuis le fichier CSV
data <- read.csv("ACLED-Western_Africa.csv")

# Clusterisation des données
clusters <- dbscan(data[, c("latitude", "longitude")], eps = 0.1, minPts = 5)
data$cluster <- as.factor(clusters$cluster)

# Vérification de l'existence de la variable 'annee'
if (!"annee" %in% colnames(data)) {
  stop("La variable 'annee' n'est pas présente dans les données.")
}

# UI
library(shiny)
library(shinydashboard)
library(leaflet)

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "I.G.I.T Map"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Carte du monde par évènements", icon = icon("globe-americas"), tabName = "map_tab"),
      menuItem("Filtrage des événements", icon = icon("filter"), tabName = "filter_tab")
    )
  ),
  dashboardBody(
    # Thème du tableau de bord
    skin = "blue",
    
    tabItems(
      # Onglet - Carte du monde par évènements
      tabItem(
        tabName = "map_tab",
        fluidRow(
          box(
            title = "Choisir un pays",
            checkboxGroupInput("event_filter", "Choisir un pays :", choices = unique(data$pays), selected = unique(data$pays)),
            br(),
            actionButton("filter_button", "Filtrer", icon = icon("search"))
          ),
          box(
            title = "Carte du monde",
            leafletOutput("map")
          )
        )
      ),
      
      # Onglet - Filtrage des événements
      tabItem(
        tabName = "filter_tab",
        fluidRow(
          box(
            title = "Filtrer les événements",
            selectInput("country_filter", "Choisir un pays :", choices = unique(data$pays)),
            br(),
            selectInput("event_type_filter", "Choisir un type d'événement :", choices = unique(data$type)),
            br(),
            selectInput("year_filter", "Choisir une année :",choices = unique(data$annee))
          ),
          box(
            title = "Carte Filtrée",
            leafletOutput("filtered_map")
          )
        )
      )
    )
  )
)



# Server
server <- function(input, output, session) {
  
  # Carte de l'Afrique de l'Ouest
  # Carte par pays
  output$map <- renderLeaflet({
    filtered <- subset(data, pays %in% input$event_filter)
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      )
  })
  
  # Filtrage des événements
  filteredData <- reactive({
    subset(data, pays == input$country_filter & type == input$event_type_filter & annee == input$year_filter)
  })
  
  # Carte filtrée
  output$filtered_map <- renderLeaflet({
    filtered <- filteredData()
    
    leaflet(filtered) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~cluster,
        opacity = 0.8,
        fillOpacity = 0.8,
        label = ~paste("Pays :", pays, "<br>Type :", type, "<br>Année :", annee),
        clusterOptions = markerClusterOptions()
      )
  })
   
     
  
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)



















