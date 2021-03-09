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
                        label = h3("Type of Facility"),
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
                                     "Other" = "other")),
            selectInput("state",
                        label = h3("Select a State"),
                        c("Alabama (AL)" = "al", "Alaska (AK)" = "ak", 
                        "Arizona (AZ)" = "az", "Arkansas (AR)" = "ar", 
                        "California (CA)" = "ca", "Colorado (CO)" = "co",
                        "Connecticut (CT)" = "ct", "Delaware (DE)" = "de",
                        "Florida (FL)" = "fl", "Georgia (GA)" = "ga",
                        "Hawaii (HI)" = "hi", "Idaho (ID)" = "id",
                        "Illinois (IL)" = "il", "Indiana (IN)" = "in",
                        "Iowa (IA)" = "ia", "Kansas (KS)" = "ks",
                        "Kentucky (KY)" = "ky", "Louisiana (LA)" = "la",
                        "Maine (ME)" = "me", "Maryland (MD)" = "md",
                        "Massachusetts (MA)" = "ma", "Michigan (MI)" = "mi",
                        "Minnesota (MN)" = "mn", "Mississippi (MS)" = "ms",
                        "Missouri (MO)" = "mo", "Montana (MT)" = "mt",
                        "Nebraska (NE)" = "ne", "Nevada (NV)" = "nv",
                        "New Hampshire (NH)" = "nh", "New Jersey (NJ)" = "nj",
                        "New Mexico (NM)" = "nm", "New York (NY)" = "ny",
                        "North Carolina (NC)" = "nc",
                         "North Dakota (ND)" = "nd",
                        "Ohio (OH)" = "oh", "Oklahoma (OK)" = "ok",
                        "Oregon (OR)" = "or", "Pennsylvania (PA)" = "pa",
                        "Rhode Island (RI)" = "ri",
                         "South Carolina (SC)" = "sc",
                        "South Dakota (SD)" = "sd", "Tennessee (TN)" = "tn",
                        "Texas (TX)" = "tx", "Utah (UT)" = "ut",
                        "Vermont (VT)" = "vt", "Virginia (VA)" = "va",
                        "Washington (WA)" = "wa", "West Virginia (WV)" = "wv",
                        "Wisconsin (WI)" = "wi", "Wyoming (WY)" = "wy"
                        ))), 
        mainPanel(
            plotlyOutput("facility_map"),
            plotlyOutput("state_graph")
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
                    mapping = aes(x = long, y = lat, group = group, 
                                  LST = LST, fill = number,
                                  text = paste("State:", LST,
                                               "<br>Number of Mental Health Facilities:", number)),
                    color = "gray", size = 0.3
                ) +
                coord_map() +
                scale_fill_continuous(limits = c(0, max(facilities_input()$number)),
                                      na.value = "white", low = "slateblue4", high = "turquoise") +
                labs(title = "Number of Mental Health Facilities in Each State, 2018") +
                labs(fill = "Number of Facilities") +
                blank_theme
    } %>%
            ggplotly(tooltip = "text")
        })
    state_input <- reactive({
        if (input$state == "al") {
            data <- al
        }
        if (input$state == "ak") {
            data <- ak
        }
        if (input$state == "az") {
            data <- az
        }
        if (input$state == "ar") {
            data <- ar
        }
        if (input$state == "ca") {
            data <- ca
        }
        if (input$state == "co") {
            data <- co
        }
        else if (input$state == "wy") {
            data <- wy
        }
        return(data)
    })
    output$state_graph <- renderPlotly({
        {state_input() %>%
                ggplot(aes(x = key, y = value, fill = key)) +
                geom_bar(stat = "identity", width = 0.5)
                }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
