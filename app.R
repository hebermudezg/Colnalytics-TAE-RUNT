server <- function(input, output) {
  require(ggplot2)
  library(readxl)
 # library(highcharter)
  library(dplyr)
  library(tidyr)
  library(shiny)
  library(DT)
  library(sqldf)
  library(xts)
  library(lubridate)
  
  
  d <- reactive({
  
  predicciones <- read_excel("predicciones.xlsx", 
                               col_types = c("date", "text", "text", 
                                             "text", "text", "numeric"))
    
    
  #predicciones <- data.frame(predicciones)
  
  #a <- predicciones[predicciones$Fecha >= "2012-01-01" & predicciones$Fecha <= "2012-01-05",]
  predicciones[predicciones$Fecha >= input$fecha1 & predicciones$Fecha <=input$fecha2,]
  })
  
  
  
  # Generate numeber total of cars

  output$suma <- renderText({
   sum(d()$Unidades)

  })
  
  
  
  
  # Generate an HTML table view of the head of the data ----
  output$table <- renderTable({
  head(d()[,-c(1)], input$n)
  })
  
  
  
  # Generate plot

  
  output$plot <- renderPlot({
    ggplot(d(), aes(x=Fecha , y=Unidades)) + geom_line(colour = 'orange')
  })
  
}




#..................................modelo html...........................................

shinyApp(ui = htmlTemplate("www/index.html"), server)


