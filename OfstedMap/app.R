library(shiny)
library(shinyWidgets)
library(tidyverse)

ofsted_map <- read_csv(
    "data/Inspection Outcome Map Data - Logistic Regression.csv"
    )

phase_list <- unique(ofsted_map$Ofstedphase)
outcome_list <- unique(ofsted_map$prev_good)
max_schools <-nrow(ofsted_map)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            selectInput("phase", "Select school phase(s)", ofsted_map$Ofstedphase, selected = phase_list, multiple = TRUE),
            selectInput("outcome", "Select current ofsted outcome group", ofsted_map$prev_good, selected = outcome_list, multiple = TRUE),
            sliderInput("poorschools", "Select how many at risk schools to display", min = 0, max = max_schools, value = 20)
        ),
        mainPanel(
            textOutput("test"),
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
        
    })
    
    sliderupdate <- reactive({
        max_schools = nrow(map_table)
        updateSliderInput(session, "poorschools", max = max_schools)
    })

    output$test <- renderText(max_schools)
    output$datatable <- renderTable(map_table())
    
}

shinyApp(ui, server)