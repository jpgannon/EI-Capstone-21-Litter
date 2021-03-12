
library(tidyverse)
library(lubridate)
library(readr)
library(shiny)
#Import Soil Data

soil_resp_data <- read_csv("Litter_and_Respiration/soil.resp.va.tech.1.29.21.csv")


# Import Litter Data 
litter_data <- read_csv("Litter_and_Respiration/litter.va.tech.1.29.21.csv")



#Convert soil respiration data to long so we can filter by by stand or treatment



# convert to long and #Convert date to "month, day, year" form
soil_resp_data2 <- soil_resp_data %>% select(date, stand, flux, treatment) %>%
  pivot_longer(cols = c("flux")) %>%
  mutate(date = mdy(date))


#Build the Shiny App

#Define UI
ui <- fluidPage(
  
  #Application title
  titlePanel("Soil Respiration Flux in MELNHE"),
  
  #Sidebar to choose dates and parameters in histogram
  sidebarLayout(
    sidebarPanel(
      
      
    #Referenced below in with "input$parameter"
      selectInput("parameter", "Plot Flux",
                  choices = unique(soil_resp_data2$name),
                  selected = "Flux"),
    
    #Referenced below with input$start_date
      dateRangeInput("start_date", "Start Date:",
                     start = "2008-07-01",
                     end = "2020-07-25",
                     min = "2008-07-01",
                     max = "2020-07-25"),
    ),
    
    #Show plot of the generated distribution
    mainPanel(
      #Plots output$fluxplot in server
      plotOutput("fluxplot")
    )
  )
)


# DEFINE THE SERVER!
server <- function(input, output) {
  
  
  #The code between the ({}) below creates the plot
  #inputs assigned above are referenced with input$"their name"
  output$fluxplot <- renderPlot({
    startdate <- input$start_date[1]
    enddate <- input$start_date[2]
    
  soil_resp_data2 %>%
    filter(Date >= startdate & Date <= enddate) %>%
    filter(name == input$parameter) %>%
    ggplot(aes(x = date, y = value))+
    geom_line()+
    theme_classic()+
    ylab(input$parameter)
  })
}

#Run the application
shinyApp(ui = ui, server = server)

ui <- fluidPage(
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
