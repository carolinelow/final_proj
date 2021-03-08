#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(countrycode)
library(knitr)
library(stringr)
library(rsconnect)
library(maps)
library(dplyr)
source("facility.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Number of Mental Health Facilities in Each State"),
    sidebarLayout(
        sidebarPanel(
            selectInput("facilities",
                        label = h3("Number of Mental Health Facilities in Each State"),
                                   c("Total" = "total",
                                     "Psychiatric Hospital" = "psych",
                                     "Separate inpatient psychiatric unit of a general hospital" = "separate",
                                     "Residential treatment center for children" = "res_child",
                                     "Residential treatment center for adults" = "res_adult",
                                     "Other type of residential treatment facility" = "other_res",
                                     "Veterans Administration medical center (VAMC)" = "vet",
                                     "Community mental health center (CMHC)" = "com",
                                     "Partial hospitalization/day treatment facility" = "partial",
                                     "Outpatient mental health facility" = "outpatient",
                                     "Multi-setting mental health facility" = "multi",
                                     "Other" = "other"))
        ),
        mainPanel(
            plotlyOutput("facility_map")
    ),
),
)
server <- function(input, output) {
    facilities_input <- reactive({
        if (input$facilities == "total") {
            dataset <- state_total
        }
        if (input$facilities == "psych") {
            dataset <- state_psych
        }
        if (input$facilities == "separate") {
            dataset <- state_separate
        }
        if (input$facilities == "res_child") {
            dataset <- state_res_child
        }
        if (input$facilities == "res_adult") {
            dataset <- state_res_adult
        }
        if (input$facilities == "other_res") {
            dataset <- state_other_res
        }
        if (input$facilities == "vet") {
            dataset <- state_vet
        }
        if (input$facilities == "com") {
            dataset <- state_com
        }
        if (input$facilities == "partial") {
            dataset <- state_partial
        }
        if (input$facilities == "outpatient") {
            dataset <- state_outpatient
        }
        if (input$facilities == "multi") {
            dataset <- state_multi
        }
        else if (input$facilities == "other") {
            dataset <- state_other
        }
        return(dataset)
    })
    output$facility_map <- renderPlotly({
        {facilities_input() %>%
                ggplot() +
                geom_polygon(
                    mapping = aes(x = long, y = lat, group = group, fill = number),
                    color = "gray", size = 0.3
                ) +
                coord_map() +
                scale_fill_continuous(limits = c(0, max(facilities_input()$number)),
                                      na.value = "white", low = "slateblue4", high = "turquoise") +
                labs(title = "Number of Mental Health Facilities in Each State, 2018") +
                labs(fill = "Number of Facilities") +
                blank_theme
    }
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
