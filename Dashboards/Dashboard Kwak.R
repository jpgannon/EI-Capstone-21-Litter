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

#library(sf)
#library(tigris)


Litterfall <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/Litterfall.csv")
SoilRespiration <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/SoilResp.csv")
StandLocations <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/StandLocations.csv")
lat_long <-
  read_csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/Directory 2/EI-Capstone-21-Litter/Data/lat_long.csv")

#Filter soil resp data and converted to correct date format 
CleanSoilResp <- SoilRespiration %>% select(date, stand, flux, temperature, treatment) %>%
  mutate(date = mdy(date))

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
    menuItem("Litterfall", tabName = "Litterfall", icon = icon("leaf")),
    menuItem("Soil Respiration", tabName = "Soil_Respiration", icon = icon("tint"))
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
            fluidRow(box(width = 12, leafletOutput("StandMap"),
                         actionButton("Stands", "Stands")))),
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
}


shinyApp(ui, server)