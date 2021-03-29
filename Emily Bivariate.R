library(ggplot2)

Litterfall <-
  read_csv("Data/Litterfall.csv")

dataset <- Litterfall

ui <- fluidPage(
  
  title = "Litterfall Plot Explorer",
  
  plotOutput('plot'),
  
  hr(),
  
  fluidRow(
    column(2,
           h4("Litterfall Plot Explorer"),
           sliderInput("Year", "Year Range:",
                       min = min(dataset$Year), 
                       max = max(dataset$Year),
                       value = c(min(dataset$Year), max(dataset$Year)), 
                       step = 1, 
                       sep = ""
                       ),
           br(),
           checkboxInput('jitter', 'Jitter'),
           checkboxInput('smooth', 'Smooth')
    ),
    column(3, offset = 1,
           selectInput('x', 'X', names(dataset)),
           selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
           selectInput('color', 'Color', c('None', names(dataset)))
    
   
    )
  )
)

server <- function(input, output) {
  
  dataset <- reactive({
    dataset <- as.data.frame(Litterfall[sample(nrow(Litterfall), input$Year),])
  })
  
  output$plot <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
    
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



