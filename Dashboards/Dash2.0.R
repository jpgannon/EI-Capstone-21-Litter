library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rgdal)
library(tidyr)
library(ggplot2)


Litterfall <-
    read.csv("~/SENIOR_YEAR/2021_SPRING/Capstone/EI_Capstone_S21/Litter_and_Respiration/Litterfall.csv")
SoilRespiration <-
    read.csv("~/SENIOR_YEAR/2021_SPRING/Capstone/EI_Capstone_S21/Litter_and_Respiration/SoilResp.csv")
StandLocations <-
    read.csv("~/SENIOR_YEAR/2021_SPRING/Capstone/EI_Capstone_S21/Litter_and_Respiration/StandLocations.csv")

#Filter soil resp data and converted to correct date format 
CleanSoilResp <- SoilRespiration %>% select(date, stand, flux, treatment) %>%
    mutate(date = mdy(date))


#CleanLit <- select()

#FixedLocations <-



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
                h1("Home Page, desciption of app and how to use will be placed here")),
        tabItem(tabName = "Map",
                box(width = 8, sliderInput("Year", "Year:", 
                                           min = min(Litterfall$Year),
                                           max = max(Litterfall$Year),
                                           value = c(min(Litterfall$Year), max(Litterfall$Year)),
                                           sep = "",
                                           step = 1)),
                box(width = 4, selectInput("Stand", "Stand:", StandLocations$Site)),
                fluidRow(box(width = 12, leaflet()%>% addTiles())),
                h1("Map")),
        tabItem(tabName = "Litterfall",
                box(
                    #plot code goes here
                ),
                h1("Litterfall")),
        tabItem(tabName = "Soil_Respiration",
                h1("Soil Respiration"),
                box(width = 12, dateRangeInput("date", "Date Range:",
                                                    start = "2008-07-01",
                                                    end = "2020-07-25",
                                                    min = "2008-07-01",
                                                    max = "2020-07-25")
                    ),
                box(plotOutput("flux_ts_plot"), width = 12),
                )
    ))
)

server <- function(input, output) {
    output$flux_ts_plot <- renderPlot({
        startdate <- input$date[1]
        enddate <- input$date[2]
        
        CleanSoilResp %>%
            filter(date >= startdate & date <= enddate)%>%
        ggplot(aes(x = date, y = flux))+
            geom_line(color = "#00AFBB", size = 1)+
            labs(title = "Soil Respiration Flux", 
                 x = "Date", 
                 y = "CO2 efflux per unit area (μg CO2/m2/s)")+
            geom_smooth(method = "lm")
    })
    
}


shinyApp(ui, server)