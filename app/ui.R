
extrafont::loadfonts(quiet = T) # device = "postscript"
# extrafont::loadfonts("C:/Windows/Fonts/", pattern = "Ubuntu")
# windowsFonts(Ubuntu=windowsFont("Ubuntu"))
library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(bslib)
library(shiny.pwa)
library(shiny.i18n) # for translations
library(jsonlite)

# library(knitr)

pos_datasets <- c("iris", "mtcars", "Orange") # must be from the packages:base envir coz everything else is just a PITA

# Commented out for faster processing!
# rmarkdown::render("./www/deep-dive.Rmd")
# rmarkdown::render("./www/StatistikPicker.Rmd")

# file with translations
i18n <- Translator$new(translation_json_path="../translations/translation.json")
# i18n <- Translator$new(translation_csvs_path='.../translations/translation_en.csv', translation_csv_config = "config.yml")
# i18n <- Translator$new(translation_csvs_path='.')

i18n$set_translation_language("de") #en

# Define UI for application that draws a histogram
fluidPage(theme = shinythemes::shinytheme("united"),
              shiny.i18n::usei18n(i18n), # initialize the use of translation i18n
          tags$div(
            style='float: right;',
            selectInput(
              inputId='selected_language',
              label=i18n$t('Sprache ändern'),
              choices = setNames(
                i18n$get_languages(),
                c("Deutsch", "English") # Set labels for the languages
              )#,
              # selected = i18n$get_key_translation()
            )
          ), # Add translation option MAYBE THIS NEEDS TO MOVE NOT SURE YET
                useShinyjs(),
                tags$style(HTML("
                                .myclass {
                                  color: #333;
                                  background-color: #ffffff;
                                  font-family: Ubuntu;
                                  border: 0px !important;
                                  
                                  word-break: break-word;
                                }
                                
                                .btn {
                                    background-color: #e85620;
                                    border: none;
                                }
                                
                                .nomclass td:first-child {font-weight:bold;}
                                
                                @media screen and (min-width: 681px) { 
                                body {margin-top: 60px;}
                                
                                 .navbar {min-height: 40px;
                                         padding-top:5px ; 
                                         padding-bottom:0px}
                                
                                .navbar-nav > li > a, .navbar-brand {padding-top:0px !important; 
                                                                     padding-bottom:0px !important;
                                                                     height: 45px;
                                                                     }
                                }
                                
                                @media screen and (max-width: 680px) {
                                body {margin-top: 60px;}
                                }
                                 
                                ")
                ), ## define custom css to be used for the verbatim text output, basics found on https://stackoverflow.com/questions/68686995/how-to-change-fill-colour-of-verbatimtextoutput
                navbarPage(position = "fixed-top", collapsible = TRUE,
                  div(img(icon("wand-magic-sparkles")), "Statistik Picker"),
                  # Create Right Side Text
                  # tags$script(
                  #   HTML("var header = $('.navbar > .container-fluid');
                  #   header.append('<div style=\"float:right\"><a href=\"https://iscience.uni-konstanz.de/\" target=”_blank”><img src=\"./img/UniKonstanz_LogoW.svg\" alt=\"alt\" style=\"float:right;height:50px;\"> </a></div>');
                  #   console.log(header)")),
                  ### tab: Home ----
                  tabPanel("", icon = icon("house"),
                           pwa("https://the-tave.shinyapps.io/Statistik-Picker/", 
                               title = "Statistik Picker",
                               output = "www", icon = "www/icon_Stats-Picker_logo.png",
                               color = "#e85620"),
                           p("Mit diesem Tool kannst du genau herausfinden, welche Statistik du für dein Projekt brauchst.
                              Erst musst du angeben, welche Skalenniveaus deine Variable(n) haben. 
                              Dann werden dir einige Vorschläge gemacht, was du für Statistiken damit rechnen kannst oder wie die 
                              Ergebnisse visualisiert werden können!
                              Wenn du dir nicht sicher bist, welches Skalenniveau auf deine Variable(n) passt, schau im Deep Dive Tab vorbei!"),
                           # Sidebar with all the inputs by users
                           sidebarPanel(
                             selectInput("scale",
                                         i18n$t("Welches Skalenniveau hat Variable 1?"),
                                         choices = c("intervall", "ordinal", "nominal")), # defaults to first value of choices
                             selectInput("scale2",
                                         i18n$t("Welches Skalenniveau hat Variable 2?"),
                                         choices = c("keins", "intervall", "ordinal", "nominal")),
                             radioButtons("statstype",
                                          "Was hast du vor?"|>i18n$t(),
                                          choices = c("Statistik rechnen", 
                                                      "Visualisierung", 
                                                      "Döner mit alles")), # TO DO: integrate function as described in i18n GitHub issue to translate choices
                           ), #### close sidebarPanel()
                           # Explain App and show the actual output
                           mainPanel(
                             h4("Beispieldaten mit Zentralmaß"|>i18n$t()),
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
                           fluidRow(
                             htmltools::tags$iframe(src = "deep-dive.html", # src = "deep-dive.html",
                                                    width = '100%',
                                                    height = 6000,  # does not work as relative
                                                    style = "border:none;")
                           ),
                           
                           htmltools::tags$img(# src="img/DeepDiveViz.png",
                                               srcset="img/DeepDiveViz_long.png 681vw,
                                               img/DeepDiveViz.png 680vw",
                                               sizes="(max-width: 680px) 680vw, (min-width: 681px) 681vw",
                                               
                                               width="90%",
                                               alt="Überblick gängiger Statistiken")
                           
                           
                             # htmltools::tags$picture(
                             #   htmltools::tags$source(media="(min-width: 681px)",
                             #                          width="80%",
                             #                          srcset="img/DeepDiveViz.png")
                             # )
                           
                  ), ### closetabPanel("Deep Dive")
                  ### tab: Beispiele ----
                  # tabPanel("Beispiele", icon = icon("code"),
                  #          p("Schau dir hier beispielhafte Daten an und analysiere sie!
                  #            Du kannst dafür bereits ausgewählte Datensätze direkt aus R nutzen oder auch eigene Daten hochladen."),
                  #          sidebarPanel(
                  #            # SelectInput for which dataset to use
                  #            selectInput("ex_dataset",
                  #                        "Welchen Datensatz möchtest du nutzen?",
                  #                        c(pos_datasets, "eigene")), # defaults to first value of choices
                  #            p("Wenn du eigene Daten analysieren möchtest, wähle bitte erst deine Datendatei aus 
                  #              (Klick auf 'Browse') und wähle dann 'eigene' beim Datensatz."),
                  #            
                  #            fileInput('customfile', '',
                  #                      accept=c('text/csv', 
                  #                               'text/comma-separated-values,text/plain', 
                  #                               '.csv')),
                  #            
                  #            checkboxGroupInput("ex_columns", 
                  #                               "Welche Variable(n) möchtest du nutzen?", 
                  #                               inline = T),
                  #            verbatimTextOutput("ex_placeholder"), 
                  #            checkboxGroupInput("ex_stat", 
                  #                               "Welche Statistik möchtest du sehen?", 
                  #                               inline = T)
                  #          ), #### close sidebar
                  #          mainPanel(
                  #            # Output: HTML table with requested number of observations
                  #            # Input: Numeric entry for number of obs to view
                  #            numericInput(inputId = "obs",
                  #                         label = "Anzahl angezeigter Zeilen:",
                  #                         value = 6),
                  #            
                  #            tableOutput("ex_data_table"),
                  #            
                  #            div(class = "myclass",
                  #                verbatimTextOutput("ex_statistic")
                  #            )
                  #            
                  #          ) #### close main panel
                  # ),  ### close tabPanel("Beispiele")
                  tabPanel("Simulationen", icon = icon("flask"),
                           navset_pill_list(
                             
                             nav_panel("Verteilung: Würfel",
                                       p("Wirf einen oder mehrere digitale 6-seitige Würfel beliebig oft. Wie werden die Ergebnisse verteilt sein?
                                         Was erwartest du, was bei mehreren Würfeln (Augenzahl addiert) passiert - wird sich die Verteilung verändern?"),
                                         numericInput("n",
                                                      "Wie oft möchtest du würfeln?",
                                                      min = 1,
                                                      max = 1000,
                                                      step = 1,
                                                      value = 6),
                                         
                                         numericInput("ndice",
                                                      "Wie viele Würfel möchtest du werfen?",
                                                      min = 1,
                                                      max = 10,
                                                      step = 1,
                                                      value = 1),
                                       
                                       plotOutput("diePlot")
                                       ),
                             
                             nav_panel("Verteilung: Münze",
                                       p("Hier siehst du die Wahrscheinlichkeitsverteilung, bei einem Münzwurf 'Zahl' zu erhalten.
                                       Ob die Münze fair (Wahrscheinlichkeit für Zahl p = 0.5, also fifty-fifty) oder gezinkt (p~Zahl~ > 0.5), verändert
                                         die Verteilung, aber auch so:
                                         In der Realität liegt die Chance für 'Zahl' nicht immer exakt bei 50%. Es kann auch mal vorkommen, dass mehrfach 
                                         hintereinander 'Zahl' untenliegt, obwohl die Münze fair ist. 
                                         Probier es doch mal aus!"),
                                         sliderInput("p",
                                                     "Ist die Münze fair  oder gezinkt?",
                                                     min = 0.5,
                                                     max = 1,
                                                     value = 0.5,
                                                     ticks = F),
                                         
                                         numericInput("coins",
                                                      "Wie oft möchtest du die Münze werfen?",
                                                      min = 1,
                                                      max = 1000,
                                                      step = 1,
                                                      value = 10)
                                         ,
                                         plotOutput("coinPlot")
                             ),
                             
                             nav_panel("Verteilung: Coefficient of variation",
                                       p("Hier kannst du sehen, was der Coefficient of Variation (CV) bedeutet und wie er aus dem
                                         Zusammenspiel von Mittelwert und Standardabweichung entsteht. Achtung: Dies ist nur ein sinnvolles 
                                         Maß für verhätnis-skalierte Variablen mit einem absoluten Nullpunkt."),
                               
                                       numericInput("cv_m",
                                                   "Welchen Mittelwert hat die Verteilung?",
                                                   min = 1,
                                                   max = 100,
                                                   value = 5),
                                     
                                       numericInput("cv_sd",
                                                    "Was ist die Standardabweichung?",
                                                    min = 0,
                                                    max = 70,
                                                    step = .1,
                                                    value = 3),
                                       
                                       plotOutput("cvPlot")
                             ),
                             
                             nav_panel("What's the T?",
                                       p("Was bedeuten eigentlich T-Werte und wie kommen sie zustande? Gib verschiedene Stichprobenmittelwerte sowie 
                                       Standardabweichungen ein und beobachte, was das mit den Verteilungen macht! Unten kannst du dann raten: Welcher 
                                         T-Wert kommt bei dem Stichprobenvergleich heraus?"),
                                
                                       # fluidRow(
                                       #   column(width = 6,
                                       #         numericInput("t_m1",
                                       #                      "Mittelwert von Stichprobe 1:",
                                       #                      min = -500,
                                       #                      max = 500,
                                       #                      value = 7),
                                       # 
                                       #         numericInput("t_sd1",
                                       #                      "Standardabweichung von Stichprobe 1:",
                                       #                      min = -100,
                                       #                      max = 100,
                                       #                      step = .1,
                                       #                      value = 1.8),
                                       # 
                                       #         numericInput("t_n",
                                       #                      "Wie groß sind die Stichproben jeweils?",
                                       #                      min = -1000,
                                       #                      max = 1000,
                                       #                      step = 1,
                                       #                      value = 100)
                                       #  ),
                                       #  column(width = 6,
                                       #         numericInput("t_m2",
                                       #                      "Mittelwert von Stichprobe 2:",
                                       #                      min = -500,
                                       #                      max = 500,
                                       #                      value = 5),
                                       # 
                                       #         numericInput("t_sd2",
                                       #                      "Standardabweichung von Stichprobe 2:",
                                       #                      min = -100,
                                       #                      max = 100,
                                       #                      step = .1,
                                       #                      value = 2.3)
                                       #  ),
                                       # ),
                                       
                                       # fluidRow( #row 1
                                       #   column(width = 6, p(tags$b(tags$u("Stichprobe 1:")))),
                                       #   column(6, p(tags$b(tags$u("Stichprobe 2:"))))
                                       # ),
                                       
                                       fluidRow( #row2
                                         column(width = 3, p(tags$b(tags$u("Stichprobe 1:")))),
                                         column(4, 
                                                numericInput("t_m1",
                                                             "Mittelwert:",
                                                             min = -500,
                                                             max = 500,
                                                             value = 7, width = "90%")
                                                ),
                                         column(5, 
                                                numericInput("t_sd1",
                                                             "Standardabweichung:",
                                                             min = -100,
                                                             max = 100,
                                                             step = .1,
                                                             value = 1.8, width = "90%")
                                                ),
                                         column(3, p(tags$b(tags$u("Stichprobe 2:")))),
                                         column(4,
                                                numericInput("t_m2",
                                                             "Mittelwert:",
                                                             min = -500,
                                                             max = 500,
                                                             value = 5)
                                                ),
                                         column(5,
                                                numericInput("t_sd2",
                                                             "Standardabweichung:",
                                                             min = -100,
                                                             max = 100,
                                                             step = .1,
                                                             value = 2.3)
                                                )
                                       ),
                                       
                                       numericInput("t_n",
                                                     "Wie groß sind die Stichproben jeweils?",
                                                     min = -1000,
                                                     max = 1000,
                                                     step = 1,
                                                     value = 100),
                                       
                                       fluidRow(
                                         plotOutput("tPlot", height = "300px")
                                       ),
                                       
                                       fluidRow(
                                         column(5,
                                                numericInput("tguessval", 
                                                             "Und nun rate mal: What's the T?",
                                                             min = -Inf,
                                                             max = Inf,
                                                             step = .01,
                                                             value = 0, width = "90%")
                                                ),
                                         column(7,
                                                p("Kleiner Tipp: Wenn der erste Mittelwert kleiner ist, ist der T-Wert negativ.")
                                                )
                                         ),
                                         actionButton("tbtn", "Antwort"),
                                         shinyjs::hidden(htmlOutput("tguess"))
                                       
                             ), 
                             
                             nav_panel("Demo: Regressionsmodell",
                                       p("Hier ist eine Demo des Regressionsmodells und was die Werte des Fehlers e und des Steigungsparameters b 
                                         für die Verteilung der Daten bedeuten."),
                                       
                                       sliderInput("lm_e",
                                                   "Wähle mean error e:",
                                                   min = 0,
                                                   max = 10,
                                                   # step = .01,
                                                   value = 1.5),
                                       
                                       sliderInput("lm_b",
                                                   "Wähle slope b:",
                                                   min = -5,
                                                   max = 5,
                                                   # step = .1,
                                                   value = 2),
                                       
                                       plotOutput("lmPlot")
                             ), # close nav_panel
                           ) # close navset_pill_list
                           
                  ),### close tabPanel("Simulations")
                  tabPanel("About", icon = icon("code-merge"),
                           p("Der Statistik Picker entsteht im Rahmen des Dissertationsprojekts von Annika Tave Overlander, M.Sc."),
                           tags$br(),
                           p("Im Menü unter dem Uni Konstanz Logo findest du einige Links, die für dich außerdem hilfreich sein könnten. 
                             Insbesondere das Online R Intro ist gut geeignet, um ein besseres 'Gefühl' für die Statistik zu erlangen! 
                             Daten anschauen und mit ihnen arbeiten ist wichtig für das Verständnis - ähnlich wichtig wie die Kenntnis der Rechnungen.
                             Daher findest du unter dem Tab Simulationen eine kleine (wachsende) Sammlung von Datensimulationen, die z.B. zeigen, wie verschiedene Verteilungen zustande kommen.
                             Wenn du Ideen oder Wünsche für weitere Features, Verteilungen o.Ä. hast, melde dich gern bei mir!"),
                           # tags$a("Mail an Tave", href = "mailto:overlander@uni-konstanz.de"),
                           tags$a(class="btn btn-default", href="mailto:overlander@uni-konstanz.de", "Mail an Tave"),
                           tags$br(),
                           tags$hr(),
                           p("Besonderer Dank gilt Anne-Sophie Landenberger und Elisabeth Mees für die Mitarbeit am Deep Dive!")
                           
                  ),  ### close tabPanel("About")
                  nav_spacer(),
                  nav_menu(
                    title = tags$img(src = "./img/UniKonstanz_LogoW.svg", height = "35px"),
                    align = "right",
                    nav_item(tags$a("iscience homepage", href = "https://iscience.uni-konstanz.de/", target="_blank"),
                             tags$a("iscience GitHub", href = "https://github.com/iscience-kn", target="_blank"),
                             tags$a("Psychological Research with R (online book)", href = "https://the-tave.github.io/psych_research_with_r/", target="_blank")
                             
                             )
                  )
                  ### ----
                ) ## close navbarPage("Statistics Picker", ...
) # close fluidPage 
