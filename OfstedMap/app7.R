#Ok so made quite a bit of progress with the previous version, this one will focus on changing the map markers to be coloured according to a
#5% grouping band based on the risk. Next version will focus on interactivity between the map and table

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
        DfE_number = as.integer(LAESTAB - 9370000),
        Ofstedphase = as.factor(Ofstedphase),
        Typeofeducation = as.factor(Typeofeducation),
        Publicationdate = as.integer(Publicationdate),
        Previouspublicationdate = as.integer(Previouspublicationdate),
        Overalleffectiveness = as.integer(Overalleffectiveness),
        Previousfullinspectionoveralleffectiveness = as.integer(Previousfullinspectionoveralleffectiveness),
        Current_OE = as.factor(ifelse(Overalleffectiveness == 1, "Outstanding", 
                                      ifelse(Overalleffectiveness == 2, "Good",
                                             ifelse(Overalleffectiveness == 3, "Requires Improvement",
                                                    "Inadequate")))),
        Previous_OE = ifelse(Previousfullinspectionoveralleffectiveness == 1, "Outstanding",
                             ifelse(Previousfullinspectionoveralleffectiveness == 2, "Good",
                                    ifelse(Previousfullinspectionoveralleffectiveness == 3, "Requires Improvement",
                                           "Inadequate"))),
        Academy = as.factor(ifelse(Academy == 1, "Yes", "No")),
        LACode = as.integer(LACode),
        daysbetween = as.integer(daysbetween),
        IDACI = as.integer(IDACI),
        num_pupils = as.integer(num_pupils),
        bad_out_chance = round(bad_out_chance*100,2),
        good_out_chance = good_out_chance*100) %>%
    mutate(chance_category = case_when(
        bad_out_chance <= 5 ~ "0-5",
        bad_out_chance <= 10 ~ "5-10",
        bad_out_chance <= 15 ~ "10-15",
        bad_out_chance <= 20 ~ "15-20",
        bad_out_chance <= 25 ~ "20-25",
        bad_out_chance <= 30 ~ "25-30",
        bad_out_chance <= 35 ~ "30-35",
        bad_out_chance <= 40 ~ "35-40",
        bad_out_chance <= 45 ~ "40-45",
        bad_out_chance > 45 ~ "45+")) %>% 
    mutate(
        Pub_date = format(as.Date(Publicationdate, origin = "1970-01-01"),"%d/%m/%Y"),
        Prev_Pub_date = format(as.Date(Previouspublicationdate, origin = "1970-01-01"),"%d/%m/%Y"),
        days_since = as.integer(Sys.Date() - as.Date(Publicationdate, origin = "1970-01-01"))
    ) %>%
    arrange(desc(bad_out_chance))

factpal <- colorFactor(c("#ffe6e6","#ffcccc","#ffb3b3","#ff9999","#ff8080","#ff6666","#ff4d4d","#ff3333","#ff1a1a","#ff0000"), 
                       ofsted_data$chance_category)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            numericInput("poorschools", "Enter how many at risk schools to display (default is maximum)", value = nrow(ofsted_data)),
            leafletOutput("schoolmap", width = "100%", height = 850)
        ),
        mainPanel(
            DTOutput("datatable")
        )
    )
)

server <- function(input, output, session) {
    
    map_data <- reactive({
        
        temp_data <- ofsted_data
        
        if (isTruthy(input$poorschools)) {
            temp_data <- temp_data %>%
                arrange(desc(bad_out_chance)) %>%
                slice(1:input$poorschools)
        }
        
        school_location <- temp_data %>%
            select(long, lat)
        
        school_data <- temp_data %>%
            select(
                URN, DfE_number, Schoolname, Ofstedphase, Typeofeducation, Academy, IDACI, bad_out_chance, 
                Current_OE, Pub_date, days_since, chance_category
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
                     loc <- input$schoolmap_marker_click
                     
                     school_id <- input$schoolmap_marker_click$id
                     
                     school_details <- ofsted_data %>%
                         filter(URN == school_id) %>%
                         slice(1)
                     
                     leafletProxy("schoolmap") %>%
                         addPopups(loc$lng, loc$lat, paste0("<b>School name:</b> ", school_details$Schoolname,
                                                            "<br><b>DfE Number:</b> ", school_details$DfE_number
                         ))
                 }
    )
    
    output$schoolmap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addCircleMarkers(data = map_data(),
                             radius = 5,
                             layerId = map_data()@data$URN,
                             opacity = 1,
                             color = ~ factpal(chance_category),
                             fillColor = ~ factpal(chance_category),
                             fillOpacity = 1
            )
    })
    
    output$datatable <- renderDT(select(map_data()@data, -chance_category),
                                 class = "cell-border stripe",
                                 filter = "top",
                                 colnames = c("URN", "DfE number", "School name", "School Phase", "Type of Education", "Academy", "IDACI Quintile", "Chance of less than good outcome", "Current Overall Effectiveness", "Inspection Published", "Days since last full inspection"),
                                 rownames = FALSE,
                                 options = list(sDom  = '<"top">lrt<"bottom">ip'),
                                 selection = "single"
    )
}

shinyApp(ui, server)