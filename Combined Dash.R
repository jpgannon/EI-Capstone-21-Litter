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

#Read in data
Litterfall <-
  read_csv("Data/Litterfall.csv")
SoilRespiration <-
  read_csv("Data/SoilResp.csv")
StandLocations <-
  read_csv("Data/StandLocations.csv")
lat_long <-
  read_csv("Data/lat_long.csv")
LitterBasket_Coord <-
  read_csv("Data/LitterBasket_Coord.csv")
Subplot_Coord <-
  read_csv("Data/Subplot_Coord.csv")

#converting UTM to Lat Long
LitterUTM <- select(LitterBasket_Coord, stand, plot, basket, stake1_utm_x, stake1_utm_y)
utm <- SpatialPoints(LitterUTM[c("stake1_utm_x", "stake1_utm_y")], proj4string=CRS("+proj=utm +zone=19T +datum=WGS84"))
Lat_Long_Lit <- spTransform(utm, CRS("+proj=longlat +datum=WGS84"))
Lat_Long_Lit <- as.data.frame(Lat_Long_Lit)
write_csv(Lat_Long_Lit, "Lat_Long_Lit")
Litterfall_Lat_Long <- LitterUTM %>% 
  mutate(Lat = Lat_Long_Lit$stake1_utm_y) %>% 
  mutate(Long = Lat_Long_Lit$stake1_utm_x)

#renaming data to merge tables
lat_long <- lat_long %>%
  rename(Stand = Site, Basket = Stake)
Litterfall_Lat_Long <- Litterfall_Lat_Long %>% 
  rename(Stand = stand, Plot = plot, Basket = basket)

#Merge Litterset to include Lat and Long, also adding pop up info to litter merge
LitterMerge <-merge(Litterfall, lat_long, by = c('Stand', 'Plot', 'Basket'), all = TRUE)

LitterMerge <- LitterMerge%>% 
  mutate(popup_info = paste("Stand:", Stand, "<br/>",
                            "Plot:", Plot, "<br/>",
                            "Basket:", Basket, "<br/>",
                            "Year:", Year, "<br/>",
                            "Treatment:", Treatment, "<br/>",
                            "Mass per Unit Area", mass.per.unit.area))

#Filter soil resp data and converted to correct date format 
CleanSoilResp <- SoilRespiration %>% select(date, stand, flux, temperature, treatment) %>%
    mutate(date = mdy(date))

#Clean up Litterfall Data and create LitterTable
Litterfall <- Litterfall%>%mutate(Treatment = paste(Treatment))
LitterTable <- Litterfall %>% select(Year, Season, Site, Stand, Plot, Treatment, whole.mass) %>%
  rename("Mass" = 7)

#Create color palet used for interactive map
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
        #Creates homepage tab
        tabItem(tabName = "Home_Page",
                h1("Home Page, desciption of app and
               how to use will be placed here")),
        #Creates interactive map tab with basic functions
        tabItem(tabName = "Map",
                h1("Interactive map of tree stands under study"),
                h1("Interactive map of tree stands under study"),
                #User input for stand type
                #"HBCa""W5", "HB", "JB" not working
                box(width = 6, selectInput("Site", "Select Stand :",
                                           c( "C1", "C2", "C3", "C4", 
                                              "C5", "C6", "C7", "C8",
                                              "C9", "HBO", "HBM", "JBO", 
                                              "JBM"),
                                           selectize = TRUE, multiple = TRUE, selected = "C1"),
                ),
                #User input for treatment type             
                box(width = 6, selectInput("MapTreatment", "Select Treatment:",
                                           c("P", "N", "NP", "C", "Ca"),
                                           selectize = TRUE, multiple = TRUE, selected = "P"),
                ),
                #User input for date range
                box(width = 12, sliderInput("MapDate", "Year Range:",
                                            min = min(Litterfall$Year),
                                            max = max(Litterfall$Year),
                                            value = 2020,
                                            sep = "")
                ),
                #Creates map of stands using leaflet
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
  #Uses sites from input above to create cirlce markers for each specific Stands on interactive map
  output$StandMap <- renderLeaflet({
    
    StandSelect <- input$Site
    TreatmentSelect <- input$MapTreatment
    YearSelect <- input$MapDate
    
    LitterMerge1 <- LitterMerge %>%
      filter(Stand %in% StandSelect) %>% 
      filter(Treatment %in% TreatmentSelect) %>% 
      filter(Year %in% YearSelect)
    
    leaflet()%>% 
      addTiles()%>% 
      addCircleMarkers(data = LitterMerge1,
                       lat = ~Lat, 
                       lng = ~Long, 
                       radius = 3,
                       popup = ~popup_info,
                       color = ~pal(Stand))
  })
    #Soil Respiration time series GGPlot based on user input
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
    #Soil respirations Boxplots using GGPlot based on user input
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
    
    #Data table of soil Respiration 
    output$soilresptable = DT::renderDataTable({
        SoilRespiration
    })
    
    #Litterfall time series GGPlot based on user input above
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
    
    #Litterfall Boxplots using GGPlot based on user input above
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
