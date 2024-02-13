#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
extrafont::loadfonts(quiet = T) # device = "postscript"
# extrafont::loadfonts("C:/Windows/Fonts/", pattern = "Ubuntu")
# windowsFonts(Ubuntu=windowsFont("Ubuntu"))
library(shiny)
# library(shinythemes)
library(shinyjs)
library(dplyr)
library(ggplot2)



# library(knitr)

# Define some default variables
set.seed(666)
num <- rnorm(10, mean = 3, sd = 1) |> round(2)
ord <- factor(c("doof", "doof", "ok","ok","ok", "super", "super", "super", "super", "super"), 
              order = TRUE, levels = c("doof", "ok", "super")) #c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5) # c("terrible", "bad","bad", "meh","meh", "good", "good", "good", "amazing", "amazing", "amazing", "amazing")
nom <- c("bla", "bli", "blub", "bla")

pos_datasets <- c("iris", "mtcars", "Orange") # must be from the packages:base envir coz everything else is just a PITA

# Commented out for faster processing!
# rmarkdown::render("./www/deep-dive.Rmd")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinythemes::shinytheme("united"),
                  useShinyjs(),
                  tags$style(HTML("
                                .myclass pre {
                                  color: #333;
                                  background-color: #ffffff;
                                  font-family: Ubuntu;
                                  border: 0px !important;
                                }
                                ")
                             ), ## define custom css to be used for the verbatim text output, basics found on https://stackoverflow.com/questions/68686995/how-to-change-fill-colour-of-verbatimtextoutput
                navbarPage(
                  div(img(icon("wand-magic-sparkles")), "Statistics Picker"),
                        ### tab: Home ----
                        tabPanel("", icon = icon("house"),
                             p("Mit diesem Tool kannst du genau herausfinden, welche Statistik du für dein Projekt brauchst.
                                 Links musst du einige Angaben machen, z.B. wie viele Variablen du hast und welche Skalenniveaus diese haben. 
                                 Dann werden dir rechts einige Vorschläge gemacht, was du für Statistiken damit rechnen kannst oder wie die 
                               Ergebnisse visualisiert werden können!"),
                             # Sidebar with all the inputs by users
                             sidebarPanel(
                                 # numericInput("nvars",
                                 #              "Um wie viele Variablen geht es?",
                                 #              min = 1,
                                 #              max = 3,
                                 #              step = 1,
                                 #              value = 1),
                                 selectInput("scalen",
                                              "Welches Skalenniveau hat Variable 1?",
                                              choices = c("intervall", "ordinal", "nominal")), # defaults to first value of choices
                                 selectInput("scalen2",
                                             "Welches Skalenniveau hat Variable 2?",
                                             choices = c("keins", "intervall", "ordinal", "nominal")),
                                 radioButtons("statstype",
                                              "Was brauchst du?",
                                              choices = c("Deskription", "Inferenz", "Visualisierung", "Döner mit alles")),
                                 
                                 # hidden(sliderInput("bins",
                                 #                    "Number of bins:",
                                 #                    min = 1,
                                 #                    max = 50,
                                 #                    value = 30))
                             ), #### close sidebarPanel()
                            # Explain App and show the actual output
                            mainPanel(
                              h4(id = "expl_h4", "Erklärung"),
                              # Text Outputs
                              div(class = "myclass",
                                  verbatimTextOutput("statstypeout")
                                  ),
                              h4("Beispieldaten"),
                              div(class = "myclass",
                                  verbatimTextOutput("statsex"),
                                  verbatimTextOutput("var2")
                              ),
                              
                              
                              # Plot Output
                              plotOutput("distPlot"),
                            ) #### close mainPanel()
                          ),  ### close tabPanel("Home", ... )
                        ### tab: Deep Dive ---- 
                        ## Commented out to reduce loading time
                        # tabPanel("Deep Dive",icon = icon("circle-info"),
                        #      fluidPage(
                        #         htmltools::tags$iframe(src = "deep-dive.html", width = '100%',  height = 1000,  style = "border:none;")
                        #       )
                        # ), ### closetabPanel("Deep Dive")
                        ### tab: Beispiele ----
                        tabPanel("Beispiele", icon = icon("code"),
                                 p("Schau dir hier beispielhafte Daten an und analysiere sie!"),
                              sidebarPanel(
                                # SelectInput for which dataset to use
                                selectInput("ex_dataset",
                                            "Welchen Datensatz möchtest du nutzen?",
                                            pos_datasets), # defaults to first value of choices
                                
                                checkboxGroupInput("ex_columns", 
                                                   "Welche Variable(n) möchtest du nutzen?", 
                                                   inline = T),
                                verbatimTextOutput("ex_placeholder"), 
                                checkboxGroupInput("ex_stat", 
                                                   "Welche Statistik möchtest du sehen?", 
                                                   inline = T)
                              ), #### close sidebar
                              mainPanel(
                                # Output: HTML table with requested number of observations
                                # Input: Numeric entry for number of obs to view
                                numericInput(inputId = "obs",
                                             label = "Anzahl angezeigter Zeilen:",
                                             value = 6),
                                tableOutput("ex_data_table"),
                                
                                div(class = "myclass",
                                    verbatimTextOutput("ex_statistic")
                                )
                                
                              ) #### close main panel
                        ) ### close tabPanel("Beispiele")
                        ### ----
                    ) ## close navbarPage("Statistics Picker", ...
) # close fluidPage 

# Define server logic ----
server <- function(input, output) {
  
# Home: Output ----

  
  
  ## Show Plot details for viz
  observeEvent(input$statstype, {
    # shinyjs::toggle("bins", condition = input$statstype %in% c("Visualisierung", "Döner mit alles"))
    ## Show explanation only for conditions other than data viz    
    shinyjs::toggle("expl_h4", condition = input$statstype != "Visualisierung")
    shinyjs::toggle("statstypeout", condition = input$statstype != "Visualisierung")
  })
  
  
  
  ## Text generation for the first variable ----
  output$statstypeout <- renderText(
      if(input$statstype %in% c("Deskription", "Döner mit alles")){
        switch(input$scalen,
               "intervall" = paste("Hier nutzt man meist den Mittelwert. In R geht das mit 'mean()'."),
               "ordinal" = paste("Ordinal sind Daten, bei denen die zugeordneten Zahlen zwar eine echte Reihenfolge abbilden, aber nicht
                                 wirklich gleiche Abstände abbilden, z.B. bei einer Skala von 'doof' bis 'super' (1 bis 5). 
                                 Hier nutzt man zum Beispiel den Median. 
                                 In R geht das mit 'median()'."),
               "nominal" = paste("Hier nutzt man vor allem den Modus.", "\n", "In R geht das zum Beispiel mit getmode() as dem package wobblynameR.")
        )
      } else{
        if(input$scalen2 == "keins"){
          paste("Bitte gib das Skalenniveau für deine zweite Variable an.")
        } else{
          paste("Keine Ahnung")
        }
        } # Für Inferenz
  )
  
  ## Beispiel zur Durchführung ----
  output$statsex <- renderText(
    if(input$statstype %in% c("Deskription", "Visualisierung", "Döner mit alles")){
      switch(input$scalen,
             "intervall" = paste(" Variable: ", paste(num, collapse = ', '),  "\n",
                                 "Mittelwert: ", round(mean(num), 2)),
             "ordinal" = paste(" Variable: ", paste(ord, collapse = ', '),  "\n",
                               "Median: ", quantile(ord, .5, type=1)),
             "nominal" = paste(" Variable: ", paste(nom, collapse = ', '),  "\n",
                               "Modus: ", wobblynameR::getmode(nom))
      )
    } else{paste("Keine Ahnung")} # Für Inferenzstatistik
  )
  
  ## Text generation for second item (only meaningful for inductive statistics?) ----
  output$var2 <- renderText(
    if(input$statstype == "Deskription"){
      switch(input$scalen2,
             "intervall" = paste("intervall!"),
             "ordinal" = paste("ordinal"),
             "nominal" = paste(" nominal!")
      )
    } else if(input$statstype == "Visualisierung"){
      paste(" ") # Bei Visualisierung wollen wir keinen Text output - oder?
    }  else{paste(" ")}
  )


  ## Plot output for Visualisierung and alles ----
    output$distPlot <- renderPlot({
      data <- switch(input$scalen, #
             "intervall" = data.frame(num),
             "ordinal" = data.frame(ord),
             "nominal" = data.frame(nom)
             )
      
      names(data) <- "x"
      
      ### hier if Schleife für wenn var2 existiert, dann vektor mit gleichem switch wie oben und data$y = vektor
      
      ###
      
      # x    <- faithful[, 2]
      # bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      
      if(input$statstype %in% c("Visualisierung", "Döner mit alles")){
        
        # One Var Plot
        if(input$scalen2 == "keins"){
          geom_specs <- switch(input$scalen, # 
                               "intervall" = 
                                 ggplot(data, aes(x, fill = after_stat(x))) + 
                                 geom_histogram(bins = 9) +
                                 scale_y_continuous(breaks = c(0, 1, 2)) +
                                 scale_fill_distiller(palette = 7),
                               
                               "ordinal" = 
                                 ggplot(data, aes(x, fill = after_stat(x))) + 
                                 geom_bar() +
                                 scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5)) +
                                 scale_fill_distiller(palette = 7),
                               
                               "nominal" = 
                                 ggplot(data, aes(x, fill = after_stat(x))) + 
                                 geom_bar() +
                                 scale_y_continuous(breaks = c(0, 1, 2))  +
                                 scale_fill_distiller(palette = 7)
          )
          
          if(input$scalen == "nominal"){
            new_nom <- as.data.frame(table(data.frame(nom)))
            names(new_nom) <- c("Daten", "value")
            p1 <- geom_specs +
              theme_minimal() +
              theme(legend.position = "none",
                    axis.title.y = element_blank(),
                    text=element_text(family="Ubuntu", size = 14),
                    title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
              labs(x = "Daten",
                   # y = "Anzahl",
                   title = "Visualisierung")
            
            p2 <- ggplot(new_nom, aes(x = "", y = value, fill = Daten)) + 
              geom_bar(stat="identity", width=1) +
              coord_polar("y", start=0)+
              theme_void() +
              theme(text=element_text(family="Ubuntu", size = 14),
                    title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
              scale_fill_brewer(palette = 7, direction = -1)
            
            cowplot::plot_grid(p1, p2, ncol = 2, align = "h")
            
          } else{
            geom_specs +
              theme_minimal() +
              theme(legend.position = "none",
                    axis.title.y = element_blank(),
                    text=element_text(family="Ubuntu", size = 14),
                    title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
              labs(x = "Daten",
                   # y = "Anzahl",
                   title = "Visualisierung")
          }
          
          # ggplot(data, aes(x, fill = after_stat(x))) +
          #   geom_histogram(bins = 9) +
          #   scale_y_continuous(breaks = c(0, 1, 2))+
          #   scale_fill_continuous()
          
          
          
        }  else {
          
          ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
            geom_jitter() +
            labs(title = "Placeholder Plot",
                 subtitle = "Hier kommt noch was...")
          
        }
        } else {NA}
        
        
      
      })
    #   renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
  
  # ----
# Beispiel Tab ----
    
  ## Creating the right dataset ----  
  ### Getting the dataset
  ex_data <- reactive({
      get(input$ex_dataset, "package:datasets") # basically transforming the chr, e.g. "iris" to actual data  input
    })
  
  ### Getting the right column names
  observe({
    col_names <- colnames(ex_data())
    updateCheckboxGroupInput(inputId = "ex_columns",
                             choices = col_names,
                             selected = col_names[1])
  })  
  
  ### Create the Datatable
  output$ex_data_table <- renderTable(
      head(ex_data(),  n = input$obs)
    )
    
  ## Creating the right statistic (for CheckBox Auswahl) ----
  observe({
    lvars <- length(input$ex_columns)
    pos_stats <- ifelse(lvars == 1, "Mittelwert", "t-Test")
    
    updateCheckboxGroupInput(inputId = "ex_stat",
                             choices = pos_stats,
                             selected = pos_stats[1])
  })
  
  
  output$ex_placeholder <- renderText(input$ex_columns)
  
  output$ex_statistic <- renderText(
    switch(input$ex_stat,
           "Mittelwert" = paste("Mittelwert: ", mean(ex_data()[[input$ex_columns]]) %>% round(3)), #mean
           "t-Test" = paste("T-Wert: ", t.test(ex_data()[[input$ex_columns[1]]],
                                               ex_data()[[input$ex_columns[2]]])$statistic %>% round(3), "\n",
                            "p-Wert: ", t.test(ex_data()[[input$ex_columns[1]]],
                                               ex_data()[[input$ex_columns[2]]])$p.value %>% round(3)) #ttest
    )
  )
  
  # ex_stat_text <- "Nothing to see yet"
  # 
  # observeEvent(input$ex_stat,{
  #   ex_stat_text <- switch(input$ex_stat,
  #          "Mittelwert" = renderText(
  #            paste("Mittelwert: ", mean(ex_data()[[input$ex_columns]]) %>% round(3))
  #          ), #mean
  #          "t-Test" = renderPrint(
  #            t.test(ex_data()[[input$ex_columns[1]]], 
  #                   ex_data()[[input$ex_columns[2]]])
  #          ) #ttest
  #   )
  # })
  
  # output$ex_statistic <- renderPrint(
  #   t.test(ex_data()[[input$ex_columns[1]]], 
  #          ex_data()[[input$ex_columns[2]]]))
    

    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

# Explanation on reactivity

# reactive({}) allows for reactivity and creation of new var
# observe({}) allows for reactivity

# Example:
# shinyServer(function(input, output){
#   # Create new reactive variable
#   newVar <- reactive({
#     val <- c(input$BLA +10, input$BLA * 3)
#   })
#   
#   output$textString <- renderText({
#     value <- newVar() #access like a function!
#     paste0("Input plus 10 is ", value[1], " and Input times 3 is ", value[2])
#   })
# })