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
    rename(lat = Northing, long = Easting) %>%
    mutate(
        URN = as.integer(URN),
        LAESTAB = as.integer(LAESTAB),
        Estab = as.integer(LAESTAB - 9370000),
        Publicationdate = as.integer(Publicationdate),
        Previouspublicationdate = as.integer(Previouspublicationdate),
        Overalleffectiveness = as.integer(Overalleffectiveness),
        Previousfullinspectionoveralleffectiveness = as.integer(Previousfullinspectionoveralleffectiveness),
        Current_OE = ifelse(Overalleffectiveness == 1, "Outstanding", 
                            ifelse(Overalleffectiveness == 2, "Good",
                                   ifelse(Overalleffectiveness == 3, "Requires Improvement",
                                          "Inadequate"))),
        Previous_OE = ifelse(Previousfullinspectionoveralleffectiveness == 1, "Outstanding",
                             ifelse(Previousfullinspectionoveralleffectiveness == 2, "Good",
                                    ifelse(Previousfullinspectionoveralleffectiveness == 3, "Requires Improvement",
                                           "Inadequate"))),
        Academy = ifelse(Academy == 1, "Yes", "No"),
        LACode = as.integer(LACode),
        daysbetween = as.integer(daysbetween),
        IDACI = as.integer(IDACI),
        num_pupils = as.integer(num_pupils),
        bad_out_chance = bad_out_chance*100,
        good_out_chance = good_out_chance*100) %>%
    mutate(
        Pub_date = format(as.Date(Publicationdate, origin = "1970-01-01"),"%d/%m/%Y"),
        Prev_Pub_date = format(as.Date(Previouspublicationdate, origin = "1970-01-01"),"%d/%m/%Y"),
        dayssince = as.integer(Sys.Date() - as.Date(Publicationdate, origin = "1970-01-01"))
    )


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("phase", "Select school phase(s)", 
                        unique(ofsted_data$Ofstedphase), multiple = TRUE),
            selectInput("outcome", "Select current ofsted outcome group", 
                        unique(ofsted_data$prev_good), multiple = TRUE),
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
    
    observeEvent(input$schoolmap_marker_click,
                 {
                     cat(str(input$schoolmap_marker_click))
                     loc <- input$schoolmap_marker_click

                     school_id <- input$schoolmap_marker_click$id

                     school_details <- ofsted_data %>%
                         filter(LAESTAB == school_id) %>%
                         slice(1)

                     leafletProxy("schoolmap") %>%
                         addPopups(loc$lng, loc$lat, paste0("School name: ", school_details$Schoolname,
                                                            "<br>DfE Number: ", school_details$LAESTAB,
                                                            "<br>Is the school an academy? ", school_details$Academy,
                                                            "<br>Latest inspection outcome: ", school_details$Current_OE,
                                                            "<br>Most recent full inspection date: ", school_details$Pub_date,
                                                            "<br>Days since last inspection: ", school_details$dayssince,
                                                            "<br>Chance of getting a less than good outcome at next inspection: ", school_details$bad_out_chance,"%"
                                                            ))
                 }
                 )
        
    output$schoolmap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addMarkers(data = map_data(),
                       layerId = map_data()@data$LAESTAB
                       )
            
        })
        
        

    
    output$datatable <- renderTable(map_data())
}

shinyApp(ui, server)
