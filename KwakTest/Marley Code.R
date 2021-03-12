library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rgdal)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(ggthemes)
library(DT)

Litterfall <-
    read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/Litterfall.csv") %>%
    mutate(Treatment = paste(Treatment))

Litterfall <- Litterfall%>%mutate(Treatment = paste(Treatment))
LitterTable <- Litterfall %>% select(Year, Season, Site, Stand, Plot, Treatment, whole.mass) %>%
    rename("Mass" = 7)

ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Dashboard"),
    dashboardSidebar(sidebarMenu(
        menuItem("Home Page", tabName = "Home_Page", icon = icon("home")),
        menuItem("Litterfall", tabName = "Litterfall", icon = icon("leaf")),
        menuItem("Litterfall Data", tabName = "Litterfall_Data", icon = icon("table"))
    )),
    #Tab Names
    dashboardBody(tabItems(
        tabItem(tabName = "Home_Page",
                h1("Home Page, desciption of app and how to use will be placed here")),
        tabItem(tabName = "Litterfall",
                h1("Litterfall"),
                #Input for Year 
                box(width = 3, sliderInput("Year", label = em("Date Range:",
                                                              style = "text-align:center;color black;font-size:100%"),
                                           min = min(Litterfall$Year),
                                           max = max(Litterfall$Year),
                                           value = c(min(Litterfall$Year), max(Litterfall$Year)),
                                           sep = "",
                                           step = 1)),
                #Input for Treatment
                box(width = 3, selectInput("Treatment", label = em("Select Treatment:",
                                                                   style = "text-align:center;color black;font-size:100%"),
                                           unique(Litterfall$Treatment), multiple = TRUE, selected = c("N", "P", "NP", "Ca", "C"))),
                #Input for Stand
                box(width = 3, selectizeInput("Stand", label = em("Select Stand:",
                                                                  style = "text-align:center;color black;font-size:100%"),
                                              choices = unique(Litterfall$Stand), multiple = TRUE, selected = "C1")),
                
                #Input Time Series
                box(plotOutput("timeseries_plot"), width = 12),
                #Input Box Plot
                box(plotOutput("litterfall_box"), width = 12)),
        
        #Input Data Table
        tabItem(tabName = "Litterfall_Data",
                h1("Litterfall Data"),
                DT:: dataTableOutput("litterfalltable"),
        )
    ))
)

server <- function(input, output) {
    
    
    #Litterfall time series plot
    output$timeseries_plot <- renderPlot({
        min <- input$Year[1]
        max <- input$Year[2]
        Treatmentselection <- input$Treatment
        Standselection <- input$Stand
        
        
        Litterfall %>%
            filter(Year >= min & Year <= max) %>%
            filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
            mutate(Year = as.factor(Year)) %>%
            ggplot(aes(x = Year, y = whole.mass)) +
            geom_boxplot(aes(x = Year, y = whole.mass, color = Treatment), position = position_dodge(0.8), lwd = 1)+
            geom_dotplot(aes(x = Year, y = whole.mass, color = Treatment), position = position_dodge(0.8), 
                         binaxis = "y", binwidth = 3)+
            theme_bw() +
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
            labs(title ="Time Series: Litterfall Mass vs. Time",
                 x = "Year",
                 y = "Mass (g litter /m2)") +
            facet_wrap(facets = "Stand", ncol = 4)
    })
    
    #Litterfall boxplot output
    output$litterfall_box <- renderPlot({
        min <- input$Year[1]
        max <- input$Year[2]
        Treatmentselection <- input$Treatment
        Standselection <- input$Stand
        
        Litterfall %>%
            filter(Year >= min & Year <= max) %>%
            filter(Stand %in% Standselection & Treatment %in% Treatmentselection) %>%
            ggplot(aes(x=Treatment, y=whole.mass, fill = Treatment)) +
            geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                         outlier.size = 5, lwd = 1)+
            geom_line()+
            theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
            theme_bw() +
            labs(title ="Boxplot: Litterfall Mass vs. Treatment Type",
                 x = "Treatment",
                 y = "Mass (g litter /m2)") +
            facet_wrap(facets = "Stand", ncol = 4)
        
    })
    #Litterfall data table output
    output$litterfalltable = DT::renderDataTable(
        LitterTable, options = list(pageLength = 20)
        
    )
    
}

shinyApp(ui, server)