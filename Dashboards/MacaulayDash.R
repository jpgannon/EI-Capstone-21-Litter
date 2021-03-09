library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(rgdal)
library(tidyr)
library(ggplot2)
library(lubridate)


Litterfall <-
    read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/Litterfall.csv")
SoilRespiration <-
    read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/SoilResp.csv")
StandLocations <-
    read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/StandLocations.csv")
lat_long <-
    read.csv("Z:/Virginia Tech School Work/Current Classes/Capstone/Project directory/EI Capstone/Litter_and_Respiration/lat_long.csv")

#Filter soil resp data and converted to correct date format 
CleanSoilResp <- SoilRespiration %>% select(date, stand, flux, temperature, treatment) %>%
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
                )
    ))
)

server <- function(input, output) {
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
            geom_dotplot(aes(x = date, y = flux, color = treatment), position = position_dodge(0.8), 
                         binaxis = "y")+
            theme(axis.text.x = element_text(angle = 60, hjust = 1))+
            theme_bw()+
            labs(title = "Soil Respiration Flux", 
                 x = "Date", 
                 y = "CO2 efflux per unit area (μg CO2/m2/s)")+
            facet_wrap(facets = "stand", ncol = 4)
        #Hello 
        
        
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
}


shinyApp(ui, server)