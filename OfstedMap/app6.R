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
if(!require("RColorBrewer")) install.packages("RColorBrewer", dependencies = TRUE)
if(!require("crosstalk")) install.packages("crosstalk", dependencies = TRUE)

# Load packages

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sp)
library(DT)
library(RColorBrewer)
library(crosstalk)

# Load in csv of data and predictions and mutate it to have the right data types and form

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
    mutate(
        Pub_date = format(as.Date(Publicationdate, origin = "1970-01-01"),"%d/%m/%Y"),
        Prev_Pub_date = format(as.Date(Previouspublicationdate, origin = "1970-01-01"),"%d/%m/%Y"),
        days_since = as.integer(Sys.Date() - as.Date(Publicationdate, origin = "1970-01-01"))
    ) %>%
    arrange(desc(bad_out_chance))

# Add in a group of bins to band data. Each bin will be a different colour when mapped.

ofsted_data <- ofsted_data %>%
    mutate(colour_bins = cut(bad_out_chance, c(0,5,10,15,20,25,30,35,40,45,50),
        labels = c('0-5','5-10','10-15','15-20','20-25','25-30','30-35','35-40','40-45','45-50'))) #%>%
    #mutate(point_colour = colorFactor(palette = 'RdYlGn', colour_bins))


#Create ui, a side bar on the left with a numeric input and a map below it, then a data table on the right

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            numericInput("poorschools", "Enter how many at risk schools to display", value = 200),
            leafletOutput("schoolmap", width = "100%", height = 850)
        ),
        mainPanel(
            DTOutput("datatable")
        )
    )
)

#Create server function

server <- function(input, output, session) {

#First thing to do is create a reactive dataset that will be used to plot data on the map and populate the table    
    
    map_data <- reactive({
        
        temp_data <- ofsted_data
        
        #This first section relates to the numeric input and controls how many schools are displayed on the map and in the table
        
        if (isTruthy(input$poorschools)) {
            temp_data <- temp_data %>%
                arrange(desc(bad_out_chance)) %>%
                slice(1:input$poorschools)
        }
        
        #More statements could be added here for additional filters but the table is a better way to filter
        
        #Create a dataset of the location data
        
        school_location <- temp_data %>%
            select(long, lat)
        
        #Create a dataset of the non-location data, this will be what is displayed or used from now on
        
        school_data <- temp_data %>%
            select(
                URN, DfE_number, Schoolname, Ofstedphase, Typeofeducation, Academy, IDACI, bad_out_chance, 
                Current_OE, Pub_date, days_since, colour_bins
                )
        
        #Create a spatial data frame, this then has to be transformed from BNG format to Lat/Long format
        
        ofsted_map <- SpatialPointsDataFrame(
            coords = school_location,
            data = school_data,
            proj4string = CRS("+init=epsg:27700")) %>%
            spTransform(CRS("+init=epsg:4326")
            )
    })
    
    shared_map_data <- SharedData$new(map_data)
    
    #This next segment looks to see if the map is clicked on and if it a school that is clicked on will display a popup box stating the 
    #school name and dfe number
    
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
   
    #This next section controls the map, additional sections could be added to show border lines for instance.
    #The current plan is to show circles for each school colour coded dependent on their risk band
    
    output$schoolmap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            addCircleMarkers(data = map_data(),
                       radius = 5,
                       layerId = map_data()@data$URN,
                       opacity = 1,
                       color = ~colorFactor(palette = 'RdYlGn', map_data()@data$colour_bins),
                       fillColor = ~colorFactor(palette = 'RdYlGn', map_data()@data$colour_bins),
                       fillOpacity = 0.5
            )
        })
    
    #This section draws the data table. This is an interactive table with filters at the top
    
    output$datatable <- renderDT(select(map_data()@data, -colour_bins),
                                 class = "cell-border stripe",
                                 filter = "top",
                                 colnames = c("URN", "DfE number", "School name", "School Phase", "Type of Education", "Academy", "IDACI Quintile", "Chance of less than good outcome", "Current Overall Effectiveness", "Inspection Published", "Days since last full inspection"),
                                 rownames = FALSE,
                                 options = list(sDom  = '<"top">lrt<"bottom">ip'),
                                 selection = "single"
                                 )
}

shinyApp(ui, server)
