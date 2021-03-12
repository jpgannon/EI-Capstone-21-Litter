library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rgdal)
library(tidyr)
library(ggplot2)
library(lubridate)
library(DT)
library(tidyverse)
library(ggthemes)
library(readr)

Litterfall <-
  read_csv("Data/Litterfall.csv")
SoilRespiration <-
  read_csv("Data/SoilResp.csv")
StandLocations <-
  read_csv("Data/StandLocations.csv")
lat_long <-
  read_csv("Data/lat_long.csv")

#Filter soil resp data and converted to correct date format 
CleanSoilResp <- SoilRespiration %>% select(date, stand, flux, temperature, treatment) %>%
    mutate(date = mdy(date))

Litterfall <- Litterfall%>%mutate(Treatment = paste(Treatment))
LitterTable <- Litterfall %>% select(Year, Season, Site, Stand, Plot, Treatment, whole.mass) %>%
  rename("Mass" = 7)

lat_long <- lat_long%>% mutate(popup_info = paste("Stand:",Site))

colors <- c("green", "blue")
pal <- colorFactor(colors, lat_long$Site)


#Dashboard setup
ui <- dashboardPage(
    skin = "red",
    dashboardHeader(title = "Dashboard"),
    dashboardSidebar(sidebarMenu(
        menuItem("Home Page", tabName = "Home_Page", icon = icon("home")),
        menuItem("Map", tabName = "Map", icon = icon("globe")),
        menuItem("Litterfall", icon = icon("globe"), startExpanded = TRUE,
                 menuSubItem("Litterfall Charts",
                             tabName = "Litterfall",
                             icon = icon("bar-chart-o")),
                 menuSubItem("Litterfall Data",
                             tabName = "Litterfall_Data",
                             icon = icon("book"))),
        menuItem("Soil Respiration", icon = icon("tint"), startExpanded = TRUE,
                 menuSubItem("Soil Respiration Charts",
                             tabName = "Soil_Respiration",
                             icon = icon("bar-chart-o")),
                 menuSubItem("Soil Respiration Data",
                             tabName = "SoilRespiration_Data",
                             icon = icon("book")))
    )),
    dashboardBody(tabItems(
        tabItem(tabName = "Home_Page",
                h1("Home Page, desciption of app and
               how to use will be placed here")),
        tabItem(tabName = "Map",
                h1("Interactive map of tree stands under study"),
                #User input for date range
                box(width = 12, dateRangeInput("MapDate", "Date Range:",
                                               start = "2008-07-01",
                                               end = "2020-07-25",
                                               min = "2008-07-01",
                                               max = "2020-07-25"),
                ),
                #User input for stand type 
                box(width = 6, selectInput("Site", "Select Stand :",
                                           c("HBO", "HBM", "JBO", "C9", "C6",
                                             "HB", "JB", "C1", "C2", "C3",   
                                             "C4", "C5", "C7", "C8", "W5", 
                                             "JBM", "HBCa"),
                                           selectize = TRUE, multiple = TRUE, selected = "HBO"),
                ),
                #User input for treatment type             
                box(width = 6, selectInput("MapTreatment", "Select Treatment:",
                                           c("P", "N", "NP", "C", "Ca"),
                                           selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
                ),
                #New Map Code
                fluidRow(box(width = 12, leafletOutput("StandMap")))),
        #Name and layout of Litterfall tab
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
        ),
        #Name and layout of soil respiration tab
        tabItem(tabName = "Soil_Respiration",
                h1("Soil Respiration"),
                
                #User input for date range
                box(width = 12, dateRangeInput("date", "Date Range:",
                                               start = "2008-07-01",
                                               end = "2020-07-25",
                                               min = "2008-07-01",
                                               max = "2020-07-25"),
                ),
                
                #User input for stand type 
                box(width = 6, selectInput("stand", "Select Stand :",
                                           c("HBO", "HBM", "JBO", "C9", "C6",
                                             "HB", "JB", "C1", "C2", "C3",   
                                             "C4", "C5", "C7", "C8", "W5", 
                                             "JBM", "HBCa"),
                                           selectize = TRUE, multiple = TRUE, selected = "C1"),
                ),
                
                #User input for treatment type             
                box(width = 6, selectInput("treatment", "Select Treatment:",
                                           c("P", "N", "NP", "C", "Ca"),
                                           selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
                ),
                
                #Making the TS plot
                box(plotOutput("flux_ts_plot"), width = 12),
                
                #Making the boxplot analysis
                box(plotOutput("soil_boxplot"), width = 12),
        ),
        
        #Making tab for table to explore soil respiration data
        tabItem(tabName = "SoilRespiration_Data",
                h1("SoilRespiration Data"),
                DT:: dataTableOutput("soilresptable")
        )
    ))
)

server <- function(input, output) {
    #Map Server Code
    output$StandMap <- renderLeaflet({
        
        StandSelect <- input$Site
        
        lat_long <- lat_long %>%
            filter(Site %in% StandSelect)
        
        leaflet()%>% 
            addTiles()%>% 
            addCircleMarkers(data = lat_long,
                             lat = ~Lat, 
                             lng = ~Long, 
                             radius = 3,
                             popup = ~popup_info,
                             color = ~pal(Site))
    })
    
    output$flux_ts_plot <- renderPlot({
        startdate <- input$date[1]
        enddate <- input$date[2]
        standselection <- input$stand
        treatmentselection <- input$treatment
        
        CleanSoilResp %>%
            filter(date >= startdate & date <= enddate)%>%
            filter(stand %in% standselection & treatment %in% treatmentselection)%>%
            mutate(date = as.factor(date))%>%
            ggplot(aes(x = date, y = flux))+
            geom_boxplot(aes(x = date, y = flux, color = treatment), position = position_dodge(0.8))+
            geom_dotplot(aes(x = date, y = flux, fill = treatment), position = position_dodge(0.8), 
                         binaxis = "y")+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
            labs(title = "Soil Respiration Flux", 
                 x = "Date", 
                 y = "CO2 efflux per unit area (μg CO2/m2/s)")+
            facet_wrap(facets = "stand", ncol = 4)
        
        
    })
    
    output$soil_boxplot <- renderPlot({
        startdate <- input$date[1]
        enddate <- input$date[2]
        standselection <- input$stand
        treatmentselection <- input$treatment
        
        CleanSoilResp %>%
            filter(date >= startdate & date <= enddate)%>%
            filter(stand %in% standselection & treatment %in% treatmentselection)%>%
            ggplot(aes(x = treatment, y = flux, fill = treatment))+
            geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                         outlier.size = 5, lwd = 1.5)+
            geom_line()+
            theme(axis.text.x = element_text(angle = 60, hjust = 1))+
            theme_bw()+
            labs(title = "Soil Respiration Flux", 
                 x = "Treatment", 
                 y = "CO2 efflux per unit area (μg CO2/m2/s)")+
            facet_wrap(facets = "stand", ncol = 4)
    })
    
    output$soilresptable = DT::renderDataTable({
        SoilRespiration
    })
    
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
