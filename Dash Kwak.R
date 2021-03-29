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
                            "Year:", Year, "<br/>",
                            "Treatment:", Treatment, "<br/>",
                            "Whole Mass", whole.mass))

#Group Data Set
GroupedLitterMerge <- LitterMerge %>%
  group_by(Year, Stand, Treatment, Plot, Basket, popup_info) %>%
  summarize(WholeMass = mean(whole.mass), Lat = mean(Lat), Long = mean(Long)) %>% 
  filter(!is.na(Lat))
  #ungroup()

#Create Popup info
GroupedLitterMerge <- GroupedLitterMerge%>% 
  mutate(popup_info = paste("Stand:", Stand, "<br/>",
                            "Plot:", Plot, "<br/>",
                            "Basket:", Basket, "<br/>",
                            "Year:", Year, "<br/>",
                            "Treatment:", Treatment, "<br/>",
                            "Average Whole Mass", WholeMass))

#Create Color Palletes
colors <- c("green", "blue", "red")
colors2 <- c("darkorange",
             "darkorange1",
             "darkorange2",
             "darkorange3",
             "darkorange4")

pal <- colorFactor(colors, LitterMerge$Treatment)
pal2 <- colorFactor(colors2, LitterMerge$mass.per.unit.area)


# create awesome icons
#litter_icons <- iconList(
 # Phos = makeIcon("https://fontawesome.com/icons/square?style=solid"),
  #Carb = makeIcon("https://fontawesome.com/icons/circle?style=solid"),
  #Nitro = makeIcon("https://fontawesome.com/icons/star?style=solid"),
  #Both = makeIcon("https://fontawesome.com/icons/certificate?style=solid"))



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
            box(width = 6, selectInput("MapTreatment", "Select Treatment:",
                                       c("P", "N", "NP", "C", "Ca"),
                                       selectize = TRUE, multiple = TRUE, selected = "P"),
            ),
            #User input for date range
            box(width = 12, sliderInput("MapYear", "Year Range:",
                        min = 2011, 
                        max = 2020,
                        value = c(2011, 2020), 
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
    min <- input$MapYear[1]
    max <- input$MapYear[2]
    
    GroupedLitterMerge <- GroupedLitterMerge %>%
      filter(Stand %in% StandSelect) %>% 
      filter(Treatment %in% TreatmentSelect) %>% 
      filter(Year >= min & Year <= max)
    
    leaflet()%>% 
      addTiles()%>% 
      addCircleMarkers(data = GroupedLitterMerge,
                       lat = ~Lat, 
                       lng = ~Long,
                       #icon = ~litter_icons,
                       radius = 3,
                       popup = ~popup_info,
                       color = ~pal(Treatment))
    })
}


shinyApp(ui, server)