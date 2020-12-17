#Ofsted Outcome Predictor App

#This is the nearly final (potentially final) version of this app
#The idea is to take the provided file that has already undergone modelling and then spit out an interactive map and table
#The map will have popups for each school and the markers for each school will be colour coded according to risk
#Comments will be brief but are designed to give you (and me) an idea what each section is trying to achieve!



# Check packages are installed, probably not necessary here but just in case!

if(!require("rgdal")) install.packages("sp", dependencies = TRUE)
if(!require("shiny")) install.packages("shiny", dependencies = TRUE)
if(!require("shinyWidgets")) install.packages("shinyWidgets", dependencies = TRUE)
if(!require("tidyverse")) install.packages("tidyverse", dependencies = TRUE)
if(!require("leaflet")) install.packages("leaflet", dependencies = TRUE)
if(!require("sp")) install.packages("sp", dependencies = TRUE)
if(!require("DT")) install.packages("DT", dependencies = TRUE)
if(!require("crosstalk")) install.packages("DT", dependencies = TRUE)

# Load packages

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(sp)
library(DT)
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
    mutate(chance_category = case_when(
        bad_out_chance <= 5 ~ "00-05",
        bad_out_chance <= 10 ~ "05-10",
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

#Now create a colour factor set with one shade of red for each of the chance_category groups

factpal <- colorFactor(c("#ffe6e6","#ffcccc","#ffb3b3","#ff9999","#ff8080","#ff6666","#ff4d4d","#ff3333","#ff1a1a","#ff0000"), 
                       ofsted_data$chance_category)

#Create ui, a side bar on the left with a numeric input and a map below it, then a data table on the right

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

#Create server function

server <- function(input, output, session) {

#First thing to do is create a reactive dataset that will be used to plot data on the map and populate the table     
        
    map_data <- reactive({
        
        temp_data <- ofsted_data
        
        #This first section relates to the numeric input and controls how many schools are displayed on the map and in the table, the default
        #value starts set to the maximum number of schools, but can be changed. This bit of code deals with the change.
        
        if (isTruthy(input$poorschools)) {
            temp_data <- temp_data %>%
                arrange(desc(bad_out_chance)) %>%
                slice(1:input$poorschools)
        }
        
        #Additional conditions and filters could be added to the ui and controls entered here. However the filtering on the table should be
        #sufficient for now.
        
        #Create a dataset of the location data
        
        school_location <- temp_data %>%
            select(long, lat)
        
        #Create a dataset of the non-location data, this will be what is displayed or used from now on
        
        school_data <- temp_data %>%
            select(
                URN, DfE_number, Schoolname, Ofstedphase, Typeofeducation, Academy, IDACI, bad_out_chance, 
                Current_OE, Pub_date, days_since, chance_category
            )
        
        #Create a spatial data frame, this then has to be transformed from BNG format to Lat/Long format
        
        ofsted_map <- SpatialPointsDataFrame(
            coords = school_location,
            data = school_data,
            proj4string = CRS("+init=epsg:27700")) %>%
            spTransform(CRS("+init=epsg:4326")
            )
        
        # convert back to a normal dataframe

        cbind(ofsted_map@coords, school_data)
    })
    
    #Turn the newly created reactive map_data into a Shared Dataframe
    
    shared_map <- SharedData$new(map_data)
    
    #It is a requirement that popups are displayed when a school is selected so that you can see which school it is
    #Ideally this will also move to the related row in the data table but this is not functional currently
    
    observeEvent(input$schoolmap_marker_click,
                 {
                     #When the map is clicked it produces the above event. This event contains 4 data items these are saved to a variable
                     
                     loc <- input$schoolmap_marker_click
                     
                     #When drawing the map a layer id is created. This saves the layer id aspect of the event to a variable

                     school_id <- input$schoolmap_marker_click$id
                     
                     #Next we filter down the original data set to the row(s) corresponding to the id. There should only be one row but a 
                     #slice is used just in case!

                     school_details <- ofsted_data %>%
                         filter(URN == school_id) %>%
                         slice(1)
                     
                     #Then we draw the popups onto the map in the right locations

                     leafletProxy("schoolmap") %>%
                         addPopups(loc$lng, loc$lat, paste0("<b>School name:</b> ", school_details$Schoolname,
                                                            "<br><b>DfE Number:</b> ", school_details$DfE_number
                         ))
                 }
    )
    
    #This section draws the map
    
    output$schoolmap <- renderLeaflet({
        leaflet() %>%
            
            #The WCC firewall blocks OpenStreetMap, which is extremely helpful
            
            addProviderTiles(providers$OpenStreetMap) %>%
            
            #A couple of different markers were used in building this app, circles were the only one which allowed large range of colours to
            #be used, in particular hex codes. This bit of code creates the markers, sets the colour and opacity of them according to their
            #chance category and creates the layer id based on the schools URN
            
            addCircleMarkers(data = shared_map,
                             radius = 5,
                             layerId = shared_map$origData()$URN,
                             opacity = 0.75,
                             color = ~ factpal(chance_category),
                             fillColor = ~ factpal(chance_category),
                             fillOpacity = 0.9
            )
    })
    
    #Finally the data table is produced. Again this feature was experimented with to find the best set of options to use for the app. The
    #output could still be improved I think but it will suffice for the current needs. The first thing that I do is remove the 
    #chance_category from the dataset, we don't want that displayed in the table. The table has a filter assigned at the top and a more
    #helpful set of field names than is present in the dataset. Some of the default options of the table have been removed. The table has 
    #been curated to fit nicely on the page.
    
    #Ideally selecting a row in the table should cause the map to focus into that particular school on the map but this is not currently
    #working.
    
    output$datatable <- renderDT(shared_map,
                                 class = "cell-border stripe",
                                 filter = "top",
                                 colnames = c("URN", "DfE number", "School name", "School Phase", "Type of Education", "Academy", "IDACI Quintile", "Chance of less than good outcome", "Current Overall Effectiveness", "Inspection Published", "Days since last full inspection"),
                                 rownames = FALSE,
                                 options = list(sDom  = '<"top">rt<"bottom">ip'),
                                 selection = "single",
                                 server = FALSE
    )
    
}

#Finally call the app and get to using the data

shinyApp(ui, server)