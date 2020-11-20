# Check packages are installed, probably not necessary here but just in case!

if(!require("rgdal")) install.packages("sp", dependencies = TRUE)
if(!require("shiny")) install.packages("shiny", dependencies = TRUE)
if(!require("shinyWidgets")) install.packages("shinyWidgets", dependencies = TRUE)
if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("leaflet")) install.packages("leaflet", dependencies = TRUE)
if(!require("sp")) install.packages("sp", dependencies = TRUE)

# Load packages

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sp)

# Load in csv of data and predictions

# Transform Easting/Northings into Lat/Longs as Leaflet will want them that way

ofsted_data <- read_csv(
    "data/Inspection Outcome Map Data - Logistic Regression.csv") %>% 
    rename(lat = Northing, long = Easting)


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("phase", "Select school phase(s)", 
                        unique(ofsted_data$Ofstedphase), multiple = TRUE),
            selectInput("outcome", "Select current ofsted outcome group", 
                        unique(ofsted_map$prev_good), multiple = TRUE),
            numericInput("poorschools", "Enter how many at risk schools to display", value = 20)
        ),
        mainPanel(
            leafletOutput("schoolmap"),
            tableOutput("datatable")
        )
    )
)

server <- function(input, output, session) {
    
    map_data <- reactive({
        
        temp_data <- ofsted_data
        
        if (isTruthy(input$phase)) {
            temp_data <- temp_data %>%
                filter(Ofstedphase %in% input$phase)
        }
        
        if (isTruthy(input$outcome)) {
            temp_data <- temp_data %>%
                filter(prev_good %in% input$outcome)
        }
        
        if (isTruthy(input$poorschools)) {
            temp_data <- temp_data %>%
                arrange(desc(bad_out_chance)) %>%
                slice(1:input$poorschools)
        }
        
        school_location <- temp_data %>%
            select(long, lat)
        
        school_data <- temp_data %>%
            select(-lat, -long)
        
        ofsted_map <- SpatialPointsDataFrame(
            coords = school_location,
            data = school_data,
            proj4string = CRS("+init=epsg:27700")) %>%
            spTransform(CRS("+init=epsg:4326")
            )
    })
    
    output$schoolmap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addMarkers(
                data = map_data()
                # lng = map_table()$long,
                # lat = map_table()$lat
            )
        
    })
    
    output$datatable <- renderTable(map_data())
}

shinyApp(ui, server)
