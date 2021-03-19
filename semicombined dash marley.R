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
library(plotly)

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
pal <- colorFactor(colors, lat_long$Stand)

#Grouping Data and creating tree species dataframe
Species <- c("ASH", "BASP", "BASS", "BE", "HB", "OAK", "PC", "RM", "SM", "STM", "WB","YB", 
             "QASP", "GB", "MM", "RO", "SEEDS", "TWIGS", "ASP", "VIB", "UNK", "NL")

GroupedLitter <- LitterTable %>% group_by(Year, Stand, Treatment, Plot) %>%
  summarize(Mass = mean(Mass))

GroupedSoilData <- CleanSoilResp %>%
  group_by(date, stand, treatment) %>%
  summarize(flux = mean(flux), temperature = mean(temperature))


#Dashboard setup
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home Page", tabName = "Home_Page", icon = icon("home")),
    menuItem("Map", tabName = "Map", icon = icon("globe")),
    menuItem("Litterfall", icon = icon("globe"), startExpanded = TRUE,
             menuSubItem("Litterfall Charts",
                         tabName = "Litterfall",
                         icon = icon("bar-chart-o")),
             menuSubItem("Litterfall Chart Average",
                         tabName = "Litterfall_Grouped",
                         icon = icon("bar-chart-o")),
             menuSubItem("Litterfall Species",
                         tabName = "Litterfall_Species",
                         icon = icon("bar-chart-o"))),
    menuItem("Soil Respiration", icon = icon("tint"), startExpanded = TRUE,
             menuSubItem("Soil Respiration Flux",
                         tabName = "Soil_Respiration",
                         icon = icon("bar-chart-o")),
             menuSubItem("Soil Respiration Flux Average",
                         tabName = "SoilRespiration_FluxMean",
                         icon = icon("bar-chart-o")),
             menuSubItem("Soil Respiration Temp",
                         tabName = "SoilRespiration_Temp",
                         icon = icon("bar-chart-o")),
             menuSubItem("Soil Respiration Temp Average",
                         tabName = "SoilRespiration_TMean",
                         icon = icon("bar-chart-o"))),
    menuItem("Bivariate Analysis", icon = icon("globe"), startExpanded = TRUE,
             menuSubItem("Litterfall Bivariate",
                         tabName = "Litterfall_BiVar",
                         icon = icon("bar-chart-o")),
             menuSubItem("Soil Respiration Bivariate",
                         tabName = "SoilRespiration_BiVar",
                         icon = icon("bar-chart-o"))),
    menuItem("Explore Data", icon = icon("tint"), startExpanded = TRUE,
             menuSubItem("Litterfall Data",
                         tabName = "Litterfall_Data",
                         icon = icon("book")),
             menuSubItem("Soil Respiration Data",
                         tabName = "SoilRespiration_Data",
                         icon = icon("book")))
  )),
  dashboardBody(tabItems(
    tabItem(tabName = "Home_Page",
            h1("User Guide: Description of Tabs"),
            box(width=12, helpText("Interactive Map Tab: map that allows you to visualize 
                                       data by plotting circle markers on a world map. 
                                       Markers will show up based on user input in the 
                                       Stand, Treatment, and year boxes, or a combination.  
                                       In addition, if you click on the points placed on 
                                       the map you will see additional pop-up information. ")
            ),
            box(width=12, helpText("Bivariate Plot Tab: Interactive bivariate plots for both Litterfall and Soil 
                                       Respiration datasets. User can select variables of interest 
                                       (for their desired dataset) and ultimately visualize the 
                                       relationship in a scatter plot. The user is able to select 
                                       other parameters that can narrow their plot input.")
            ),
            box(width = 12, helpText("Litterfall Tab: Within the litterfall tab, the 
                                         user can choose which way they want to visualize 
                                         this data. In the first tab the user can visualize
                                         the litterfall mass over time through a time series
                                         plot and boxplot. The user can choose the date range,
                                         which treatment types they want to visualize, the 
                                         stand and plot. The next tab is a data table showing
                                         the litterfall data. The third tab includes a boxplot
                                         that lets the user visualize the affect of treatment
                                         type on the litterfall mass of individual tree species
                                         over time.")
            ),
            box(width = 12, helpText("Soil Respiration Tab: User has options to view either
                                         all soil respiration data or grouped soil respiration data.
                                         “Soil Respiration Flux” and “Soil Respiration Temp” contain
                                         all data and user is able to filter by date range, stand,
                                         and treatment for time series and boxplot visualization.
                                         “Soil Respiration Flux Average” and “Soil Respiration Temp
                                         Average” contain grouped data. The purpose of grouped data
                                         is to represent the mean measurement per day. The user is
                                         able to filter by date range, stand, and treatment for grouped
                                         data time series and boxplot visualization.")
            ),
            box(width = 12, helpText("Explore Data Tab: User has the ability to analyze and view
                                         cleaned data from the Litterfall and Soil Respiration datasets."))
            
    ),
    #Creates interactive map tab with basic functions
    tabItem(tabName = "Map",
            h1("Interactive Map of Tree Stands Under Study"),
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
            h1("Litterfall Charts"),
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
                                       unique(Litterfall$Treatment), multiple = TRUE, selected = c("N", "P"))),
            #Input for Stand
            box(width = 3, selectizeInput("Stand", label = em("Select Stand:",
                                                              style = "text-align:center;color black;font-size:100%"),
                                          choices = unique(Litterfall$Stand), multiple = TRUE, selected = "C1")),
            
            #Input Time Series
            box(plotOutput("timeseries_plot"), width = 12),
            #Input Box Plot
            box(plotOutput("litterfall_box"), width = 12)),
    
    #Input Species Visualization
    tabItem(tabName = "Litterfall_Species",
            h1("Litterfall Species Charts"),
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
                                       choices = unique(Litterfall$Treatment), multiple = TRUE, selected = c("N", "P"))),
            
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
    #Input Grouped Plots
    tabItem(tabName = "Litterfall_Grouped",
            h1("Litterfall Grouped Plots"),
            #Input for Year 
            box(width = 3, sliderInput("Grouped_Year", label = em("Date Range:",
                                                                  style = "text-align:center;color black;font-size:100%"),
                                       min = min(Litterfall$Year),
                                       max = max(Litterfall$Year),
                                       value = c(min(Litterfall$Year), max(Litterfall$Year)),
                                       sep = "",
                                       step = 1)),
            #Input for Treatment
            box(width = 3, selectInput("Grouped_Treatment", label = em("Select Treatment:",
                                                                       style = "text-align:center;color black;font-size:100%"),
                                       unique(Litterfall$Treatment), multiple = TRUE, selected = c("N", "P"))),
            #Input for Stand
            box(width = 3, selectizeInput("Grouped_Stand", label = em("Select Stand:",
                                                                      style = "text-align:center;color black;font-size:100%"),
                                          choices = unique(Litterfall$Stand), multiple = TRUE, selected = "C1")),
            
            box(width = 3, selectizeInput("Grouped_Plot", label = em("Select Plot:",
                                                                     style = "text-align:center;color black;font-size:100%"),
                                          choices = unique(Litterfall$Plot), multiple = TRUE, selected = c("1", "2", "3", "4", "5", "7"))),
            #Input Time Series Plot
            box(plotOutput("grouped_timeseries_plot"), width = 12),
            #Input Box Plot
            box(plotlyOutput("grouped_litter_box"), width = 12)),
    
    #Input Data Table
    tabItem(tabName = "Litterfall_Data",
            h1("Litterfall Data"),
            DT:: dataTableOutput("litterfalltable"),
    ),
    #Name and layout of soil respiration tab
    tabItem(tabName = "Soil_Respiration",
            h1("Soil Respiration Flux"),
            
            #User input for date range
            box(width = 12, dateRangeInput("date", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-07-25",
                                           min = "2008-07-01",
                                           max = "2020-07-25"),
            ),
            
            #User input for stand type 
            box(width = 6, selectInput("stand", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
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
            box(plotlyOutput("soil_boxplot"), width = 12),
    ),
    
    #Name and layout of Temp soil respiration tab
    tabItem(tabName = "SoilRespiration_Temp",
            h1("Soil Respiration Temperature"),
            
            #User input for date range
            box(width = 12, dateRangeInput("temp_date", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-07-25",
                                           min = "2008-07-01",
                                           max = "2020-07-25"),
            ),
            
            #User input for stand type 
            box(width = 6, selectInput("temp_stand", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
                                         "JBM", "HBCa"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            
            #User input for treatment type             
            box(width = 6, selectInput("temp_treatment", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
            ),
            #Making the TS plot
            box(plotOutput("temp_ts_plot"), width = 12),
            
            #Making the boxplot analysis
            box(plotlyOutput("temp_boxplot"), width = 12),
    ),
    
    tabItem(tabName = "SoilRespiration_FluxMean",
            h1("Average Soil Respiration Flux"),
            
            box(width = 12, dateRangeInput("line_date", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-07-25",
                                           min = "2008-07-01",
                                           max = "2020-07-25"),
            ),
            
            #User input for stand type 
            box(width = 6, selectInput("line_stand", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
                                         "JBM", "HBCa"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            
            #User input for treatment type             
            box(width = 6, selectInput("line_treatment", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
            ),
            #Making the soil resp TS plot
            box(plotOutput("line_ts_plot"), width = 12),
            
            
            
            box(plotlyOutput("line_boxplot"), width = 12)
    ),
    
    tabItem(tabName = "SoilRespiration_TMean",
            h1("Soil Respiration Temperature Average"),
            
            box(width = 12, dateRangeInput("TMean_date", "Date Range:",
                                           start = "2008-07-01",
                                           end = "2020-07-25",
                                           min = "2008-07-01",
                                           max = "2020-07-25"),
            ),
            
            #User input for stand type 
            box(width = 6, selectInput("TMean_stand", "Select Stand :",
                                       c("HBO", "HBM", "JBO", "HB", "JB", 
                                         "C1", "C2", "C3", "C4", "C5", 
                                         "C6", "C7", "C8", "C9","W5", 
                                         "JBM", "HBCa"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            
            #User input for treatment type             
            box(width = 6, selectInput("TMean_treatment", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected =c("N", "P")),
            ),
            #Making the soil resp TS plot
            box(plotOutput("TMean_ts_plot"), width = 12),
            
            
            
            box(plotlyOutput("TMean_boxplot"), width = 12)
    ), 
    #Making tab for table to explore soil respiration data
    tabItem(tabName = "SoilRespiration_Data",
            h1("Soil Respiration Data"),
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
  
  #Flux timeseries plot based on user input above
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
  
  output$soil_boxplot <- renderPlotly({
    startdate <- input$date[1]
    enddate <- input$date[2]
    standselection <- input$stand
    treatmentselection <- input$treatment
    
    CleanSoilResp %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = flux, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 1)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_bw()+
      labs(title = "Soil Respiration Flux", 
           x = "Treatment", 
           y = "CO2 efflux per unit area (μg CO2/m2/s)")+
      facet_wrap(facets = "stand", ncol = 4)
  })
  
  output$soilresptable = DT::renderDataTable({
    CleanSoilResp
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
      geom_dotplot(aes(x = Year, y = whole.mass, fill = Treatment), position = position_dodge(0.8), 
                   binaxis = "y")+
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
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
                   outlier.size = 4, lwd = 1)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5,  hjust = 0)) +
      theme_bw() +
      labs(title ="Boxplot: Litterfall Mass vs. Treatment Type",
           x = "Treatment",
           y = "Mass (g litter /m2)")
    
  })
  #Litterfall Grouped Time series plot
  output$grouped_timeseries_plot <- renderPlot({
    min <- input$Grouped_Year[1]
    max <- input$Grouped_Year[2]
    Treatmentselection <- input$Grouped_Treatment
    Standselection <- input$Grouped_Stand
    Plotselection <- input$Grouped_Plot
    
    GroupedLitter %>%
      filter(Year >= min & Year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection & Plot %in% Plotselection) %>%
      mutate(Year = as.factor(Year)) %>%
      ggplot(aes(x = Year, y = Mass)) +
      geom_point(aes(x = Year, y = Mass, group = Treatment, color = Treatment)) + 
      geom_line(aes(x = Year, y = Mass, color = Treatment, group = Treatment)) + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
      labs(title ="Grouped Time Series: Litterfall Mass vs. Time",
           x = "Year",
           y = "Mass (g litter /m2)") +
      facet_wrap(facets = "Stand", ncol = 4)
    
  })
  
  #Grouped Litterfall boxplot output
  output$grouped_litter_box<- renderPlotly({
    min <- input$Grouped_Year[1]
    max <- input$Grouped_Year[2]
    Treatmentselection <- input$Grouped_Treatment
    Standselection <- input$Grouped_Stand
    Plotselection <- input$Grouped_Plot
    
    GroupedLitter %>%
      filter(Year >= min & Year <= max) %>%
      filter(Stand %in% Standselection & Treatment %in% Treatmentselection & Plot %in% Plotselection) %>%
      ggplot(aes(x = Treatment, y = Mass, fill = Treatment)) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 1)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0)) +
      theme_bw() +
      labs(title ="Boxplot: Average Litterfall Mass vs. Treatment Type",
           x = "Treatment",
           y = "Mass (g litter /m2)") +
      facet_wrap(facets = "Stand", ncol = 4)
  })
  
  #Litterfall data table output
  output$litterfalltable = DT::renderDataTable(
    LitterTable, options = list(pageLength = 20)
    
  )
  output$temp_ts_plot <- renderPlot({
    startdate <- input$temp_date[1]
    enddate <- input$temp_date[2]
    standselection <- input$temp_stand
    treatmentselection <- input$temp_treatment
    
    CleanSoilResp %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      mutate(date = as.factor(date))%>%
      ggplot(aes(x = date, y = temperature))+
      geom_boxplot(aes(x = date, y = temperature, color = treatment), position = position_dodge(0.8))+
      geom_dotplot(aes(x = date, y = temperature, fill = treatment), position = position_dodge(0.8), 
                   binaxis = "y")+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
      labs(title = "Soil Respiration Temperature", 
           x = "Date", 
           y = "Temperature")+
      facet_wrap(facets = "stand", ncol = 4)
    
  })
  
  output$temp_boxplot <- renderPlotly({
    startdate <- input$date[1]
    enddate <- input$date[2]
    standselection <- input$stand
    treatmentselection <- input$treatment
    
    CleanSoilResp %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = temperature, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 1)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_bw()+
      labs(title = "Soil Respiration Flux", 
           x = "Treatment", 
           y = "Temperature")+
      facet_wrap(facets = "stand", ncol = 4)
    
  })
  
  output$line_ts_plot <- renderPlot({
    startdate <- input$line_date[1]
    enddate <- input$line_date[2]
    standselection <- input$line_stand
    treatmentselection <- input$line_treatment
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      mutate(date = as.factor(date))%>%
      ggplot(aes(x = date, y = flux))+
      geom_point(aes(x = date, y = flux, group = treatment, color = treatment))+
      geom_path(aes(x = date, y = flux, group = treatment, color = treatment))+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
      labs(title = "Average Daily Soil Respiration Flux", 
           x = "Date", 
           y = "Average CO2 efflux per unit area (μg CO2/m2/s)")+
      facet_wrap(facets = "stand", ncol = 4)
    
  })
  
  output$line_boxplot <- renderPlotly({
    startdate <- input$line_date[1]
    enddate <- input$line_date[2]
    standselection <- input$line_stand
    treatmentselection <- input$line_treatment
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = temperature, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 1)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      theme_bw()+
      labs(title = "Average Soil Respiration Flux", 
           x = "Treatment", 
           y = "Average Soil Respiration Flux")+
      facet_wrap(facets = "stand", ncol = 4)
    
  })
  output$TMean_ts_plot <- renderPlot({
    startdate <- input$TMean_date[1]
    enddate <- input$TMean_date[2]
    standselection <- input$TMean_stand
    treatmentselection <- input$TMean_treatment
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      mutate(date = as.factor(date))%>%
      ggplot(aes(x = date, y = flux))+
      geom_point(aes(x = date, y = flux, group = treatment, color = treatment))+
      geom_line(aes(x = date, y = flux, group = treatment, color = treatment))+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))+
      labs(title = "Average Daily Soil Respiration Temperature", 
           x = "Date", 
           y = "Average Temperature")+
      facet_wrap(facets = "stand", ncol = 4)
    
    
  })
  
  output$TMean_boxplot <- renderPlotly({
    startdate <- input$TMean_date[1]
    enddate <- input$TMean_date[2]
    standselection <- input$TMean_stand
    treatmentselection <- input$TMean_treatment
    
    GroupedSoilData %>%
      filter(date >= startdate & date <= enddate)%>%
      filter(stand %in% standselection & treatment %in% treatmentselection)%>%
      ggplot(aes(x = treatment, y = temperature, fill = treatment))+
      geom_boxplot(outlier.colour = "red", outlier.shape = 4,
                   outlier.size = 4, lwd = 1)+
      geom_line()+
      theme(axis.text.x = element_text(angle = 90, hjust = 0))+
      theme_bw()+
      labs(title = "Average Soil Respiration Temperature", 
           x = "Treatment", 
           y = "Average Soil Respiration Temperature")+
      facet_wrap(facets = "stand", ncol = 4)
    
  })
}


shinyApp(ui, server)
