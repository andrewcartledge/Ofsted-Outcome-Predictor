library(shiny)
library(shinyWidgets)
library(tidyverse)

ofsted_map <- read_csv(
    "data/Inspection Outcome Map Data - Logistic Regression.csv"
)

sec <- "Secondary"

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("phase", "Select school phase(s)", c("All", ofsted_map$Ofstedphase), selected = "All", multiple = TRUE),
            selectInput("outcome", "Select current ofsted outcome group", c("Both", ofsted_map$prev_good), selected = "Both", multiple = TRUE),
            sliderInput("poorschools", "Select how many at risk schools to display", min = 0, max = 200, value = 20)
            # selectInput("testing", "Test filtering", choices = c("ofsted_map$Ofstedphase == sec", "ofsted_map$Academy == 1", 
                                                                 # "ofsted_map$bad_out_chance <= 10"), selected = "ofsted_map$Ofstedphase == sec")
        ),
        mainPanel(
            textOutput("testing"),
            tableOutput("datatable")
        )
    )
)

server <- function(input, output) {
    output$testing <- renderText({input$phase})
    
    # filter_string <- reactive({input$testing})

    map_table <- reactive({
        if ("All" %in% input$phase)
        {filter(ofsted_map, ofsted_map$Ofstedphase %like% "%%")}
        else
        {filter(ofsted_map, ofsted_map$Ofstedphase %in% input$phase)}
    })
        
    # map_table <- reactive(ofsted_map %>%
    #         filter(ofsted_map$Ofstedphase == input$phase)
    # )
    
    output$datatable <- renderTable(map_table())
    
}

shinyApp(ui, server)