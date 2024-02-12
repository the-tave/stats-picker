library(shiny)
library(dplyr)

ui <- fluidPage(
      selectInput("dataset", "Choose a dataset", c("mtcars", "sleep")),
      checkboxGroupInput("columns", "Select the columns to show", inline = TRUE),
      tableOutput("data_table")
  )


server <- function(input, output, session) {
  
  mydata <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  output$data_table <- renderTable(mydata())
}

shinyApp(ui, server)