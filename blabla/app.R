#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),#
            selectInput("scale",
                        "Welches Skalenniveau hat Variable 1?",
                        choices = c("intervall", "ordinal", "nominal")) # defaults to first value of choices
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tableOutput("table"),
          #4 Make the final row bold using tags$style
          tags$style(type="text/css", "td:first-child {font-weight:bold;}"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table <- renderTable(xtable::xtable(table(nom,nom)), rownames = T) #
    # iris %>% group_by(Species) %>% summarize(Mean = mean(Sepal.Length)))

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        switch(input$scale, #
               "intervall" = hist(x, breaks = bins, col = 'darkgray', border = 'white',
                                  xlab = 'Waiting time to next eruption (in mins)',
                                  main = 'Histogram of waiting times'),
               "ordinal" = hist(x, breaks = bins, col = 'darkred', border = 'white',
                                xlab = 'Waiting time to next eruption (in mins)',
                                main = 'Histogram of waiting times'),
               "nominal" = hist(x, breaks = bins, col = 'darkgreen', border = 'white',
                                xlab = 'Waiting time to next eruption (in mins)',
                                main = 'Histogram of waiting times')
        )
        # draw the histogram with the specified number of bins
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
