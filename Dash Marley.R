library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(rgdal)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(DT)
library(plotly)
library(RColorBrewer)

Litterfall <- read_csv("Data/Litterfall.csv")


LitterTable <- Litterfall %>% select(Year, Season, Site, Stand, Plot, Treatment, whole.mass) %>%
  rename("Mass" = 7)

Species <- c("ASH", "BASP", "BASS", "BE", "HB", "OAK", "PC", "RM", "SM", "STM", "WB","YB", "QASP", "GB", "MM", "RO", "SEEDS", "TWIGS", "ASP", "VIB", "UNK", "NL")
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home Page", tabName = "Home_Page", icon = icon("home")),
    menuItem("Litterfall Time Series", tabName = "Litterfall", icon = icon("leaf")),
    menuItem("Litterfall Data Table", tabName = "Litterfall_Data", icon = icon("table")),
    menuItem("Litterfall Species", tabName = "Litterfall_Species", icon = icon("book"))
  )),
#Tab Names
  dashboardBody(tabItems(
#Input Homepage
    tabItem(tabName = "Home_Page",
            h1("Home Page, desciption of app and how to use will be placed here")),
#Input Litterfall timeseries
    tabItem(tabName = "Litterfall",
            h1("Litterfall Time Series Plots"),
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
            
            box(width = 3, selectizeInput("Plot", label = em("Select Plot:",
                                                              style = "text-align:center;color black;font-size:100%"),
                                          choices = unique(Litterfall$Plot), multiple = TRUE, selected = c("1", "2", "3", "4", "5", "7"))),
           #Input Time Series Plot
            box(plotOutput("timeseries_plot"), width = 12),
           #Input Box Plot
            box(plotlyOutput("litterfall_box"), width = 12)),
    
#Input Species Visualization
    tabItem(tabName = "Litterfall_Species",
            h1("Litterfall Data: Species Visualization"),
          #Input Year
            box(width = 4, sliderInput("Species_Year", label = em("Date Range:",
                                                          style = "text-align:center;color black;font-size:100%"),
                                       min = min(Litterfall$Year),
                                       max = max(Litterfall$Year),
                                       value = c(min(Litterfall$Year), max(Litterfall$Year)),
                                       sep = "",
                                       step = 1)),
           #Input for Treatment
            box(width = 4, selectInput("Species_Treatment", label = em("Select Treatment:",
                                                               style = "text-align:center;color black;font-size:100%"),
                                       choices = unique(Litterfall$Treatment), multiple = TRUE, selected = c("N", "P", "NP", "Ca", "C"))),
            
          #Input for Species
            box(width = 4, selectInput("Species", label = em("Select Species:",
                                                               style = "text-align:center;color black;font-size:100%"),
                                        choices = c("ASH", "BASP", "BASS", "BE", "HB", "OAK", "PC", "RM", "SM",
                                                    "STM", "WB","YB", "QASP", "GB", "MM", "RO", "SEEDS",
                                                    "TWIGS", "ASP", "VIB", "UNK", "NL"), multiple = FALSE, selected = "ASH")),
          #Input Boxplot
            box(plotlyOutput("litter_species_boxplot"), width = 12),
          
          #Input Species Key
box(width = 12, helpText("Species Key:
                                    ASH: White Ash,
                                    BASP: Bigtooth Aspen,
                                    BASS: Basswood,
                                    BE: American Beech,
                                    HB: Hobblebush,
                                    OAK: Northern Red Oak,
                                    PC: Pin Cherry,
                                    RM: Red Maple,
                                    SM: Sugar Maple,
                                    STM: Striped Maple,
                                    WB: White Birch,
                                    YB: Yellow Birch,
                                    QASP: Quaking Aspen,
                                    GB: Gray Birch,
                                    MM: Mountain Maple,
                                    RO: Northern Red Oak
                                    SEEDS: Seeds,
                                    TWIGS: Twigs, 
                                    ASP: Bigtooth Aspen,
                                    VIB: Viburnum Lantanoides,
                                    UNK: Unknown,
                                    NL: Non Leaf"))),
            
#Input Data Table
    tabItem(tabName = "Litterfall_Data",
            h1("Litterfall Data Table"),
            DT:: dataTableOutput("litterfalltable"))
    
  ))
)


server <- function(input, output) {
  
  
#Litterfall time series plot
  output$timeseries_plot <- renderPlot({
    min <- input$Year[1]
    max <- input$Year[2]
    Treatmentselection <- input$Treatment
    Standselection <- input$Stand
    Plotselection <- input$Plot
    
    Litterfall %>%
      filter(Year >= min & Year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection & Plot %in% Plotselection) %>%
      mutate(Year = as.factor(Year)) %>%
      ggplot(aes(x = Year, y = whole.mass)) +
      geom_boxplot(aes(x = Year, y = whole.mass, color = Treatment), position = position_dodge(0.8), lwd = 1)+
      geom_dotplot(aes(x = Year, y = whole.mass, color = Treatment), position = position_dodge(0.8), 
                   binaxis = "y", binwidth = 3)+
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme_bw() +
      labs(title ="Time Series: Litterfall Mass vs. Time",
           x = "Year",
           y = "Mass (g litter /m2)") +
      facet_wrap(facets = "Stand", ncol = 4) 
      
  })
  
#Litterfall boxplot output
  output$litterfall_box <- renderPlotly({
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
      scale_fill_brewer(palette = "Set1") +
      labs(title ="Boxplot: Litterfall Mass vs. Treatment Type",
           x = "Treatment",
           y = "Mass (g litter /m2)") +
      facet_wrap(facets = "Stand", ncol = 4)
    
  })
  
#Litterfall Species Plot Output
  output$litter_species_boxplot <- renderPlotly({
    min <- input$Species_Year[1]
    max <- input$Species_Year[2]
    Treatmentselection <- input$Species_Treatment
    Speciesselection <- input$Species
    
    Litterfall %>%
      filter(Year >= min & Year <= max) %>%
      filter(Treatment %in% Treatmentselection & Species %in% Speciesselection) %>%
      mutate(Year = as.factor(Year)) %>%
      ggplot(aes(x=Treatment, y=whole.mass, fill = Treatment)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 5, lwd = 1)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      theme_bw() +
      scale_fill_brewer(palette = "Set1") +
      labs(title ="Boxplot: Litterfall Mass vs. Treatment Type",
           x = "Treatment",
           y = "Mass (g litter /m2)")

    
  })
  
#Litterfall data table output
  output$litterfalltable = DT::renderDataTable(
    LitterTable, options = list(pageLength = 20)
    
  )
  
}


shinyApp(ui, server)