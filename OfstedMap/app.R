# Check packages are installed, probably not necessary here but just in case!

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

ofsted_data <- read_csv(
    "data/Inspection Outcome Map Data - Logistic Regression.csv"
    )

# Transform Easting/Northings into Lat/Longs as Leaflet will want them that way

school_location <- ofsted_data %>%
    select(Easting, Northing) %>%
    rename(lat = Northing, long = Easting)

school_data <- ofsted_data %>%
    select(-Easting, -Northing)

ofsted_map <- SpatialPointsDataFrame(
    coords = school_location,
    data = school_data,
    proj4string = CRS("+init=epsg:27700")
) %>%
    spTransform(CRS("+init=epsg:4326"))


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("phase", "Select school phase(s)", ofsted_map$Ofstedphase, multiple = TRUE),
            selectInput("outcome", "Select current ofsted outcome group", ofsted_map$prev_good, multiple = TRUE),
            numericInput("poorschools", "Enter how many at risk schools to display", value = 20)
        ),
        mainPanel(
            leafletOutput("schoolmap"),
            tableOutput("datatable")
        )
    )
)

server <- function(input, output, session) {
    
    map_table <- reactive({
        temp_map <- ofsted_map
        
        if (isTruthy(input$phase))
            {temp_map <- temp_map %>%
            filter(temp_map$Ofstedphase %in% input$phase)}
        
        if (isTruthy(input$outcome))
            {temp_map <- temp_map %>%
            filter(temp_map$prev_good %in% input$outcome)}
        
        if (isTruthy(input$poorschools))
            {temp_map <- temp_map %>%
            arrange(desc(temp_map$bad_out_chance)) %>%
            slice(1:input$poorschools)}
        
    })
    
    map <- reactive({
        leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addMarkers(
        data = getMapData(map_table())#,
        # lng = map_table()$long,
        # lat = map_table()$lat
    )
    })
    # 
    output$schoolmap <- renderLeaflet(map())
    output$datatable <- renderTable(map_table())
    
    # output$datatable <- renderTable(view(ofsted_map))
    
}

shinyApp(ui, server)