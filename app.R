#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

library(knitr)

# Define some default variables
set.seed(666)
num <- rnorm(100, mean = 3, sd = 1)
ord <- c("terrible", "bad", "meh", "good", "amazing")
nom <- c("bla", "bli", "blub")

rmarkdown::render("./www/deep-dive.Rmd")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
                  tags$style(HTML("
                                .myclass pre {
                                  color: #333;
                                  background-color: #ffffff;
                                  font-family: Ubuntu;
                                  border: 0px !important;
                                }")
                             ), ## define custom css to be used for the verbatim text output, basics found on https://stackoverflow.com/questions/68686995/how-to-change-fill-colour-of-verbatimtextoutput
                navbarPage("Statistics Picker",
                        tabPanel("Home",
                             # Sidebar with all the inputs by users
                             sidebarPanel(
                                 # br(),
                                 numericInput("nvars",
                                              "Um wie viele Variablen geht es?",
                                              min = 1,
                                              max = 3,
                                              step = 1,
                                              value = 1),
                                 radioButtons("scalen",
                                              "Welches Skalenniveau hat Variable 1?",
                                              choices = c("numerisch", "ordinal", "nominal")), # defaults to first value of choices
                                 radioButtons("statstype",
                                              "Was brauchst du?",
                                              choices = c("Deskription", "Inferenz", "Visualisierung", "Döner mit alles")),
                                 sliderInput("bins",
                                    "Number of bins:",
                                    min = 1,
                                    max = 50,
                                    value = 30)
                            # a select input
                            # selectInput("bins", "Number of bins", choices = list(
                            #   Few = list(`One` = 1, `Five` = 5),
                            #   Many = list(`Ten` = 10, `Hundred` = 100)
                            # ), selectize = FALSE)
                             ), #### sidebarPanel()
                            # Explain App and show the actual output
                            mainPanel(
                              h4("Mit diesem Tool kannst du genau herausfinden, welche Statistik du für dein Projekt brauchst. 
                                 Im folgenden musst du einige Angaben machen, z.B. wie viele Variablen du hast und welche Skalenniveaus diese haben. 
                                 Dann werden dir einige Vorschläge gemacht, was du für Statistiken damit rechnen kannst und wie die Ergebnisse visualisiert werden können!"),
                              # h1("Header 1"),
                              # Text Outputs
                              # 
                              # verbatimTextOutput("txtout"),
                              # p("Es geht um 1 Variable"),
                              div(class = "myclass",
                                  verbatimTextOutput("txtout"),
                                  # verbatimTextOutput("scaleout"),
                                  verbatimTextOutput("statstypeout")
                              ),
                              
                              
                              # Plot Output
                              plotOutput("distPlot"),
                            ) #### mainPanel()
                          ),  ### tabPanel("Home", ... )
                        tabPanel("Deep Dive",
                              h4("Hier kommen mehr Informationen zu den einzelnen Statistiken hin."), 
                              
                              fluidPage(
                                htmltools::tags$iframe(src = "deep-dive.html", width = '100%',  height = 1000,  style = "border:none;")
                              )
                              
                              # uiOutput('markdown')
                              # div(includeMarkdown("deep-dive.Rmd"),
                              #     align="justify")
                              # includeHTML("deep-dive.html")
                        ), ### tabPanel("Deep Dive")
                        tabPanel("Beispiele",
                              h4("Hier entstehen Beispiele zu den Statistiken. Unter anderem wird das cars dataset oder so 
                                 angezeigt, um Beispiele für die Variablenarten (Skalenniveaus) zu haben und zu sehen, was man damit
                                 machen kann!")
                        ) ### tabPanel("Beispiele")
                    ) ## navbarPage("Statistics Picker", ...
) # fluidPage 

# Define server logic
server <- function(input, output) {
  
  # output$markdown <- renderUI({
  #   HTML(markdown::markdownToHTML(knit('deep-dive.Rmd', quiet = TRUE)))
  # })
  
  output$txtout <- renderText({
    paste("Es geht um ", input$nvars, ifelse(input$nvars == 1, " Variable", " Variablen"), ". Sie ",
          ifelse(input$nvars == 1, "ist", "sind"), input$scalen, "."
          )
  })
  
  # output$scaleout <- renderText({
  #   paste(input$scalen)
  # })
  
  # output$statstypeout <- renderText({
  #   # Hier müsste es erst eine if Schleife für das Skalenniveau geben und dann einen switch für statstype... 
  #   ifelse(input$statstype == "Deskription" && input$scalen == "numerisch", paste("Hier nutzt man meist den Mittelwert. In R geht das mit 'mean()'."),
  #          paste("Keine Ahnung"))
  # 
  #   
  #   
  # })
  
  observe(
    if(input$statstype == "Deskription"){
      switch(input$scalen,
             "numerisch" = output$statstypeout <- renderText({ paste("Hier nutzt man meist den Mittelwert. In R geht das mit 'mean()'.")}),
             "ordinal" = output$statstypeout <- renderText({paste("Hier nutzt man zum Beispiel den Median. In R geht das mit 'median()'.")}),
             "nominal" = output$statstypeout <- renderText({paste("Hier nutzt man vor allem den Modus. In R geht das etwas schwerfällig.")})
      )
    }
  )
  

  
  
  # browser()
  # output$plot <- renderPlot({
  #   if(input$scale == "numerisch"){
  #     df <- data.frame(x = num)
  #   }
  #   
  #   ggplot2::ggplot(df, x) +
  #     geom_histogram()
  # })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
