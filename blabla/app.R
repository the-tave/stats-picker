#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

set.seed(123)
num <- rnorm(10, mean = 3, sd = 1) |> round(2)
ord <- factor(c("doof", "doof", "ok","ok","ok", "super", "super", "super", "super", "super"), 
              order = TRUE, levels = c("doof", "ok", "super")) 
nom <- c("bla", "bli", "blub", "bla", "blub", "blub", "bla", "bli", "blub", "blub")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Geysier Daten"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Anzahl der bins:",
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
                                  xlab = 'Wartezeit bis zum Ausbruch (in mins)',
                                  main = 'Histogram der Wartezeit'),
               "ordinal" = hist(x, breaks = bins, col = 'darkred', border = 'white',
                                xlab = 'Wartezeit bis zum Ausbruch (in mins)',
                                main = 'Histogram der Wartezeit'),
               "nominal" = hist(x, breaks = bins, col = 'darkgreen', border = 'white',
                                xlab = 'Wartezeit bis zum Ausbruch (in mins)',
                                main = 'Histogram der Wartezeit')
        )
        # draw the histogram with the specified number of bins
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
