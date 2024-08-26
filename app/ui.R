#

extrafont::loadfonts(quiet = T) # device = "postscript"
# extrafont::loadfonts("C:/Windows/Fonts/", pattern = "Ubuntu")
# windowsFonts(Ubuntu=windowsFont("Ubuntu"))
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(bslib)

# library(knitr)

pos_datasets <- c("iris", "mtcars", "Orange") # must be from the packages:base envir coz everything else is just a PITA

# Commented out for faster processing!
# rmarkdown::render("./www/deep-dive.Rmd")
# rmarkdown::render("./www/StatistikPicker.Rmd")

# Define UI for application that draws a histogram
fluidPage(theme = shinythemes::shinytheme("united"),
                useShinyjs(),
                tags$style(HTML("
                                .myclass {
                                  color: #333;
                                  background-color: #ffffff;
                                  font-family: Ubuntu;
                                  border: 0px !important;
                                  
                                  word-break: break-word;
                                }
                                
                                .nomclass td:first-child {font-weight:bold;}
                                
                                body {padding-top: 60px;}
                                
                                .navbar {min-height: 40px;
                                         padding-top:5px ; 
                                         padding-bottom:0px}
                                
                                .navbar-nav > li > a, .navbar-brand {padding-top:0px !important; 
                                                                     padding-bottom:0px !important;
                                                                     height: 45px;
                                                                     }
                                  
                                ")
                ), ## define custom css to be used for the verbatim text output, basics found on https://stackoverflow.com/questions/68686995/how-to-change-fill-colour-of-verbatimtextoutput
                navbarPage(position = "fixed-top",
                  div(img(icon("wand-magic-sparkles")), "Statistics Picker"),
                  # Create Right Side Text
                  # tags$script(
                  #   HTML("var header = $('.navbar > .container-fluid');
                  #   header.append('<div style=\"float:right\"><a href=\"https://iscience.uni-konstanz.de/\" target=”_blank”><img src=\"./img/UniKonstanz_LogoW.svg\" alt=\"alt\" style=\"float:right;height:50px;\"> </a></div>');
                  #   console.log(header)")),
                  ### tab: Home ----
                  tabPanel("", icon = icon("house"),
                           p("Mit diesem Tool kannst du genau herausfinden, welche Statistik du für dein Projekt brauchst.
                                 Links musst du einige Angaben machen, z.B. wie viele Variablen du hast und welche Skalenniveaus diese haben. 
                                 Dann werden dir rechts einige Vorschläge gemacht, was du für Statistiken damit rechnen kannst oder wie die 
                               Ergebnisse visualisiert werden können!"),
                           # Sidebar with all the inputs by users
                           sidebarPanel(
                             selectInput("scale",
                                         "Welches Skalenniveau hat Variable 1?",
                                         choices = c("intervall", "ordinal", "nominal")), # defaults to first value of choices
                             selectInput("scale2",
                                         "Welches Skalenniveau hat Variable 2?",
                                         choices = c("keins", "intervall", "ordinal", "nominal")),
                             radioButtons("statstype",
                                          "Was hast du vor?",
                                          choices = c("Statistik rechnen", "Visualisierung", "Döner mit alles")),
                           ), #### close sidebarPanel()
                           # Explain App and show the actual output
                           mainPanel(
                             h4("Beispieldaten"),
                             div(class = "myclass",
                                 htmlOutput("statsex"),
                                 htmlOutput("var2data")
                             ),
                             h4(id = "expl_h4", "Erklärung"),
                             # Text Outputs
                             div(class = "myclass",
                                 # verbatimTextOutput("statstypeout")
                                 htmlOutput("statstypeout")
                             ),
                             
                             tags$br(),
                             
                             # Table output for two vars nom
                             div(class = "nomclass",
                                 tableOutput("table")),
                             
                             # Plot Output
                             plotOutput("dataViz"),
                             
                             #4 Make the final row bold using tags$style
                             # tags$style(type="text/css", "td:first-child {font-weight:bold;}")
                           ) #### close mainPanel()
                  ),  ### close tabPanel("Home", ... )
                  ### tab: Deep Dive ---- 
                  ## Commented out to reduce loading time
                  tabPanel("Deep Dive",icon = icon("circle-info"),
                           # img(src='./img/dist.svg', height="50%", width="50%", align = "center"),
                           htmltools::tags$iframe(src = "deep-dive.html", # src = "deep-dive.html", 
                                                  width = '100%',  
                                                  height = 5500,  # does not work as relative currently?!
                                                  style = "border:none;")
                  ), ### closetabPanel("Deep Dive")
                  ### tab: Beispiele ----
                  tabPanel("Beispiele", icon = icon("code"),
                           p("Schau dir hier beispielhafte Daten an und analysiere sie!"),
                           sidebarPanel(
                             # SelectInput for which dataset to use
                             selectInput("ex_dataset",
                                         "Welchen Datensatz möchtest du nutzen?",
                                         c(pos_datasets, "eigene")), # defaults to first value of choices
                             p("Wenn du eigene Daten analysieren möchtest, wähle bitte erst deine Datendatei aus 
                               (Klick auf 'Browse') und wähle dann 'eigene' beim Datensatz."),
                             
                             fileInput('customfile', '',
                                       accept=c('text/csv', 
                                                'text/comma-separated-values,text/plain', 
                                                '.csv')),
                             
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
                  ),  ### close tabPanel("Beispiele")
                  tabPanel("About", icon = icon("code-merge"),
                           p("Der Statistik Picker entsteht im Rahmen des Dissertationsprojekts von Annika Tave Overlander, M.Sc."),
                           tags$br(),
                           p("Besonderer Dank gilt Anne-Sophie Landenberger und Elisabeth Mees für die Mitarbeit am Deep Dive!")
                           
                  ),  ### close tabPanel("About")
                  tabPanel("Simulationen", icon = icon("flask"),
                           navset_pill_list(
                             
                             nav_panel("Coming", p("Soon!")),
                             
                             nav_panel("Hang",p("Tight!"))
                           )
                           
                  ),### close tabPanel("Simulations")
                  nav_spacer(),
                  nav_menu(
                    title = tags$img(src = "./img/UniKonstanz_LogoW.svg", height = "35px"),
                    align = "right",
                    nav_item(tags$a("iscience homepage", href = "https://iscience.uni-konstanz.de/")
                             
                             )
                  )
                  ### ----
                ) ## close navbarPage("Statistics Picker", ...
) # close fluidPage 
