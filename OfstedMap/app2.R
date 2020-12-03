#This version of the app moves the mpa to below the filters and alters the table to only display the important fields
#The popups now seem to be unneeded as all data is in the table

# Check packages are installed, probably not necessary here but just in case!

if(!require("rgdal")) install.packages("sp", dependencies = TRUE)
if(!require("shiny")) install.packages("shiny", dependencies = TRUE)
if(!require("shinyWidgets")) install.packages("shinyWidgets", dependencies = TRUE)
if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("leaflet")) install.packages("leaflet", dependencies = TRUE)
if(!require("sp")) install.packages("sp", dependencies = TRUE)
if(!require("DT")) install.packages("DT", dependencies = TRUE)

# Load packages

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sp)
library(DT)

# Load in csv of data and predictions

# Transform Easting/Northings into Lat/Longs as Leaflet will want them that way

ofsted_data <- read_csv(
    "data/Inspection Outcome Map Data - Logistic Regression.csv") %>% 
    rename(lat = Northing, long = Easting) %>%
    mutate(
        URN = as.integer(URN),
        LAESTAB = as.integer(LAESTAB),
        `DfE number` = as.integer(LAESTAB - 9370000),
        `School Name` = Schoolname,
        Phase = Ofstedphase,
        `Type of Education` = Typeofeducation,
        Publicationdate = as.integer(Publicationdate),
        Previouspublicationdate = as.integer(Previouspublicationdate),
        Overalleffectiveness = as.integer(Overalleffectiveness),
        Previousfullinspectionoveralleffectiveness = as.integer(Previousfullinspectionoveralleffectiveness),
        `Current Overall Effectiveness` = ifelse(Overalleffectiveness == 1, "Outstanding", 
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
        `Chance of less than good outcome` = bad_out_chance*100,
        good_out_chance = good_out_chance*100) %>%
    mutate(
        `Inspection Published` = format(as.Date(Publicationdate, origin = "1970-01-01"),"%d/%m/%Y"),
        Prev_Pub_date = format(as.Date(Previouspublicationdate, origin = "1970-01-01"),"%d/%m/%Y"),
        `Days since last full inspection` = as.integer(Sys.Date() - as.Date(Publicationdate, origin = "1970-01-01"))
    )


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("phase", "Select school phase(s)", 
                        unique(ofsted_data$Phase), multiple = TRUE),
            selectInput("outcome", "Select current ofsted outcome group", 
                        unique(ofsted_data$prev_good), multiple = TRUE),
            numericInput("poorschools", "Enter how many at risk schools to display", value = 20),
            leafletOutput("schoolmap", width = "100%", height = 700)
        ),
        mainPanel(
            tableOutput("datatable")
        )
    )
)

server <- function(input, output, session) {
    
    map_data <- reactive({
        
        temp_data <- ofsted_data
        
        if (isTruthy(input$phase)) {
            temp_data <- temp_data %>%
                filter(Phase %in% input$phase)
        }
        
        if (isTruthy(input$outcome)) {
            temp_data <- temp_data %>%
                filter(prev_good %in% input$outcome)
        }
        
        if (isTruthy(input$poorschools)) {
            temp_data <- temp_data %>%
                arrange(desc(`Chance of less than good outcome`)) %>%
                slice(1:input$poorschools)
        }
        
        school_location <- temp_data %>%
            select(long, lat)
        
        school_data <- temp_data %>%
            select(
                URN, `DfE number`, `School Name`, Phase, `Type of Education`, Academy, IDACI, `Chance of less than good outcome`, 
                `Current Overall Effectiveness`, `Inspection Published`, `Days since last full inspection`
                )
        
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
                         filter(URN == school_id) %>%
                         slice(1)

                     leafletProxy("schoolmap") %>%
                         addPopups(loc$lng, loc$lat, paste0("School name: ", school_details$`School Name`,
                                                            "<br>DfE Number: ", school_details$`DfE number`,
                                                            "<br>Is the school an academy? ", school_details$Academy,
                                                            "<br>Latest inspection outcome: ", school_details$`Current Overall Effectiveness`,
                                                            "<br>Most recent full inspection date: ", school_details$`Inspection Published`,
                                                            "<br>Days since last inspection: ", school_details$`Days since last full inspection`,
                                                            "<br>Chance of getting a less than good outcome at next inspection: ", round(school_details$`Chance of less than good outcome`, 2),"%"
                                                            ))
                 }
                 )
        
    output$schoolmap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addMarkers(data = map_data(),
                       layerId = map_data()@data$URN
            )
        })
    
    output$datatable <- renderTable(map_data()@data)
}

shinyApp(ui, server)
