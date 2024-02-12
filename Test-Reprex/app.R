#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(shiny)
library(dplyr)
library(ggplot2)


# Variables interested in selecting
my_vars <- c("cyl", "gear", "carb")



# UI
ui <- fluidPage(
  
  # Title
  titlePanel("Reprex"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      selectInput("sel_1",
                  "Variable 1",
                  choices  = my_vars,
                  selected = my_vars[[1]],
                  multiple = FALSE
      ),
      selectInput("sel_2",
                  "Variable 1 choices",
                  choices  = unique(mtcars[[ my_vars[[1]] ]]),
                  multiple = FALSE
      )
    ), # sidebarPanel close
    
    # Plot
    mainPanel(
      plotOutput("plot_out"),
      verbatimTextOutput("info")
    )  # mainPanel close
  )    # sidebarLayout close
)      # UI close


server <- function(input, output, session) {
  
  # Make drop-down choice of sel_2 dependent upon user input of sel_1
  observeEvent(input$sel_1, {
    updateSelectInput(session,
                      "sel_2",
                      choices = sort(unique(mtcars[[input$sel_1]]))
    )
  })
  
  output$plot_out <- renderPlot({
    
    # Assign inputs
    sel_1 <- input$sel_1
    sel_2 <- input$sel_2
    
    # Data to plot
    my_data <- mtcars %>% 
      filter(.data[[sel_1]] == sel_2)
    
    # Plot
    ggplot(my_data, aes(x = factor(.data[[sel_1]]), y = hp)) + geom_point()
    
  })
  
  gnarf <- renderPrint({
    bla <-  t.test(iris$Sepal.Length, iris$Sepal.Width)
    # res <- data.frame(bla)
    # res
    bla
  })
  
  output$info <- gnarf
}

# Run the application 
shinyApp(ui = ui, server = server)
