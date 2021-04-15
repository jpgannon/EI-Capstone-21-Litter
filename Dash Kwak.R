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

#Renaming Colunms
lat_long <- lat_long %>%
  rename(Stand = Site, Basket = Stake)
Litterfall_Lat_Long <- Litterfall_Lat_Long %>% 
  rename(Stand = stand, Plot = plot, Basket = basket)

#Merge Datasets
LitterMerge <-merge(Litterfall, lat_long, by = c('Stand', 'Plot', 'Basket'), all = TRUE)

#Create Popup info
LitterMerge <- LitterMerge%>% 
  mutate(popup_info = paste("Stand:", Stand, "<br/>",
                            "Plot:", Plot, "<br/>",
                            "Basket:", Basket, "<br/>",
                            "Treatment:", Treatment, "<br/>",
                            "Whole Mass", whole.mass))


LitterMerge <-  LitterMerge %>% filter(!is.na(whole.mass))

#Group Data Set
GroupedLitterMerge <- LitterMerge %>%
  group_by(Year, Stand, Treatment, Plot, Basket, popup_info) %>%
  summarize(WholeMass = mean(whole.mass), Lat = mean(Lat), Long = mean(Long)) %>% 
  filter(!is.na(Lat)) %>% 
  filter(!is.na(WholeMass)) %>% 
  ungroup()

#Create Popup info
GroupedLitterMerge <- GroupedLitterMerge%>% 
  mutate(popup_info = paste("Stand:", Stand, "<br/>",
                            "Plot:", Plot, "<br/>",
                            "Basket:", Basket, "<br/>",
                            "Year:", Year, "<br/>",
                            "Treatment:", Treatment, "<br/>",
                            "Average Whole Mass", WholeMass))

#Create Color Palletes
colors <- c("white", "yellow", "orange", "red")
colors2 <- c("Blues")

pal <- colorNumeric(colors2, GroupedLitterMerge$WholeMass)

#Dashboard setup
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home Page", tabName = "Home_Page", icon = icon("home")),
    menuItem("Map", tabName = "Map", icon = icon("globe")),
    menuItem("Litterfall", tabName = "Litterfall", icon = icon("leaf")),
    menuItem("Soil Respiration", tabName = "Soil_Respiration", icon = icon("tint"))
  )),
  dashboardBody(tabItems(
    #Creates homepage tab
    tabItem(tabName = "Home_Page",
            h1("Home Page, desciption of app and
               how to use will be placed here")),
    #Creates interactive map tab with basic functions
    tabItem(tabName = "Map",
            h1("Interactive map of tree stands under study"),
            #User input for stand type
            #"HBCa""W5", "HB", "JB" not working
            box(width = 6, selectInput("MapSite", "Select Stand :",
                                       c( "C1", "C2", "C3", "C4", 
                                         "C5", "C6", "C7", "C8",
                                         "C9", "HBO", "HBM", "JBO", 
                                         "JBM"),
                                       selectize = TRUE, multiple = TRUE, selected = "C1"),
            ),
            #User input for treatment type             
            box(width = 3, selectInput("MapTreatment", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected = "P"),
            ),
            #Average of whole plot
            box(width = 3, title = "Plot Average:",
                checkboxInput('average', 'Average per Plot')),
            #User input for date range
            box(width = 12, sliderInput("MapYear", "Year:",
                        min = 2011, 
                        max = 2020,
                        value = 2015, 
                        sep = "")
            ),
            #Creates Box for world Map
            fluidRow(box(width = 12, leafletOutput("StandMap")))),
    #Name and layout of Litterfall tab
    tabItem(tabName = "Litterfall",
            h1("Litterfall")),
    #Name and layout of soil respiration tab
    tabItem(tabName = "Soil_Respiration",
            h1("Soil Respiration"),
            )
    ))
  )

server <- function(input, output) {
#Stand Selection Server Code
  output$StandMap <- renderLeaflet({
    
    StandSelect <- input$MapSite
    TreatmentSelect <- input$MapTreatment
    YearSelect <- input$MapYear
    #min <- input$MapYear[1]
    #max <- input$MapYear[2]
    
    LitterAverage <- GroupedLitterMerge %>% 
      filter(Stand %in% StandSelect) %>% 
      filter(Treatment %in% TreatmentSelect) %>%
      filter(Year %in% YearSelect) %>% 
      filter(!is.na(WholeMass)) %>%
      mutate(popup_info = paste("Stand:", Stand, "<br/>",
                                "Plot:", Plot, "<br/>",
                                "Basket:", Basket, "<br/>",
                                "Year:", Year, "<br/>",
                                "Treatment:", Treatment, "<br/>",
                                "Average Whole Mass", mean(WholeMass))) %>% 
      group_by(Treatment)
      
    
    GroupedLitterMerge <- GroupedLitterMerge %>%
      filter(Stand %in% StandSelect) %>% 
      filter(Treatment %in% TreatmentSelect) %>% 
      #filter(Year >= min & Year <= max) %>% 
      filter(Year %in% YearSelect) %>% 
      filter(!is.na(WholeMass))
    
    if (input$average)
    leaflet()%>% 
      addTiles()%>%
      addCircleMarkers(data = LitterAverage,
                       lat = ~mean(Lat), 
                       lng = ~mean(Long),
                       radius = 3,
                       popup = ~popup_info,
                       color = ~pal(WholeMass)
                 ) %>% 
      addLegend(position = "bottomright",
                pal = pal,
                values = LitterMerge$whole.mass,
                bins = 6,
                title = "Average Whole Mass per Year",
                opacity = 1)
    else
      leaflet()%>% 
      addTiles()%>%
      addCircleMarkers(data = GroupedLitterMerge,
                       lat = ~Lat, 
                       lng = ~Long,
                       radius = 3,
                       popup = ~popup_info,
                       color = ~pal(WholeMass)
      ) %>% 
      addLegend(position = "bottomright",
                pal = pal,
                values = LitterMerge$whole.mass,
                bins = 6,
                title = "Average Whole Mass per Year",
                opacity = 1)
      
    })
}


shinyApp(ui, server)