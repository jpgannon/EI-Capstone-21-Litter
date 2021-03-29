library(ggplot2)

SoilRespiration <-
  read_csv("Data/SoilResp.csv")

#Filter soil resp data and converted to correct date format 
CleanSoilResp <- SoilRespiration %>% select(date, stand, flux, temperature, treatment) %>%
  mutate(date = mdy(date)) %>%
  filter(!is.na(date))

dataset1 <- CleanSoilResp

ui <- fluidPage(
  
  title = "Soil Respiration Plot Explorer",
  
  plotOutput('plot1'),
  
  hr(),
  
  fluidRow(
    column(2,
           h4("Soil Respiration Plot Explorer"),
           sliderInput("date", "Date:",
                       min = min(dataset1$date), 
                       max = max(dataset1$date),
                       value = c(min(dataset1$date), max(dataset1$date)),
                       sep = "/",
           ),
           br(),
           checkboxInput('jitter', 'Jitter'),
           checkboxInput('smooth', 'Smooth')
    ),
    column(3, offset = 1,
           selectInput('x', 'X', names(dataset1)),
           selectInput('y', 'Y', names(dataset1), names(dataset1)[[2]]),
           selectInput('color', 'Color', c('None', names(dataset1)))
           
           
    )
  )
)

server <- function(input, output) {
  
  dataset1 <- reactive({
    CleanSoilResp[sample(nrow(CleanSoilResp), ),]
  })
  
  output$plot1 <- renderPlot({
    
    p <- ggplot(dataset1(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    
    if (input$jitter)
      p <- p + geom_jitter()
    if (input$smooth)
      p <- p + geom_smooth()
    
    print(p)
    
  })
  
}

shinyApp(ui, server)
