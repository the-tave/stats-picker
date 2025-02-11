
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

# pos_datasets <- c("iris", "mtcars", "Orange") # possible datasets for examples, currently not running


# file with translations
i18n <- Translator$new(translation_json_path="./www/translation_withDeepDive.json")

i18n$set_translation_language("de") #en

# Define UI for application 
fluidPage(theme = shinythemes::shinytheme("united"),
          tags$head(
            tags$title("Stats Picker"),
            tags$meta(name = "description", content = "The Stats Picker helps you with statistics with explanations, examples and simulations!"),
            tags$meta(name = "keywords", content = "Shiny, R, Data Visualization, Statistics, Teaching, Learning"),
            tags$link(rel = "stylesheet", type = "text/css", href = "styling.css")
          ),
          
          shiny.i18n::usei18n(i18n), # initialize the use of translation i18n
          tags$div(
            style='float: right;
            width: 140px; 
            padding-left: 20px;', # added sizing to div style
            selectInput(
              inputId='selected_language',
              label=i18n$t('Sprache ändern'),
              choices = setNames(
                i18n$get_languages(),
                c("Deutsch", "English") # Set labels for the languages
              )
            )
          ), # Add translation option #@import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap'); #'Yusei Magic', sans-serif; # @20..48,100..700,0..1,-50..200
                useShinyjs(),
                
           ## define custom css to be used for the verbatim text output, basics found on https://stackoverflow.com/questions/68686995/how-to-change-fill-colour-of-verbatimtextoutput
                navbarPage(position = "fixed-top", collapsible = TRUE, id = "stats",
                  title = div(img(icon("wand-magic-sparkles")), "Statistik Picker"),
                  
                  ### tab: Home ----
                  tabPanel("", icon = icon("house"),
                           value = "home", #added to make tabs in tabPanel linkable?!
                           pwa("https://the-tave.shinyapps.io/Statistik-Picker/", 
                               title = "Statistik Picker",
                               output = "www", icon = "www/icon_Stats-Picker_logo.png",
                               color = "#e85620"),
                           
                           tags$div(class = "fancy-container", 
                                    # tags$div(class="ray-left-up"),
                                    # tags$div(class="ray-left-middle"),
                                    # tags$div(class="ray-left-down"),
                                    # tags$div(class="ray-right-up"),
                                    # tags$div(class="ray-right-middle"),
                                    # tags$div(class="ray-right-down"),
                                    tags$div(class="content",
                                             
                           i18n$t("Mit diesem Tool kannst du genau herausfinden, welche Statistik du für dein Projekt brauchst."),
                           tags$br(),
                           tags$b(i18n$t("Erst musst du angeben, welche Skalenniveaus deine Variablen haben.")),
                           i18n$t("Dann werden dir einige Vorschläge gemacht, was du für Statistiken damit rechnen kannst oder wie die Ergebnisse visualisiert werden können."),
                           tags$br(),
                           i18n$t("Wenn du dir nicht sicher bist, welches Skalenniveau auf deine Variablen passt, schau im "),
                           actionButton("controller", "Deep Dive", style = "padding:3px;"), 
                           tags$p(i18n$t(" Tab vorbei!"), style = "display:inline-block;")
                                    ) 
                           ),
                           
                           
                           tags$br(), tags$br(),
                           
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
                                          choices = c("Statistik rechnen", "Visualisierung", "Döner mit alles")), # TO DO: integrate function 
                           ), #### close sidebarPanel()
                           # Explain App and show the actual output
                           mainPanel(
                             h4("Beispieldaten mit Zentralmaß"|>i18n$t()),
                             div(class = "myclass",
                                 htmlOutput("statsex"),
                                 htmlOutput("var2data")
                             ),
                             
                             h4(id = "statataglance", "Passende Statistik:"|>i18n$t()), #TO DO
                             h5(htmlOutput("ataglanceout")),
                             # Text Outputs
                             # div(class = "myclass",
                             #     htmlOutput("ataglanceout")
                             # ),
                             
                             h4(id = "expl_h4", "Erklärung"|>i18n$t()),
                             # Text Outputs
                             div(class = "myclass",
                                 htmlOutput("statstypeout")
                             ),
                             
                             tags$br(),
                             
                             # Table output for two vars nom
                             div(class = "nomclass",
                                 tableOutput("table")),
                             
                             # Plot Output
                             h4(id = "viz_h4", "Visualisierung"|>i18n$t()),
                             plotOutput("dataViz"),
                            
                           ) #### close mainPanel()
                  ),  ### close tabPanel("Home", ... )
                  ### tab: Deep Dive ---- 
                  tabPanel("Deep Dive",icon = icon("circle-info"),
                           value = "deep-dive",
                           
                           tags$div(class = "fancy-container", 
                                      # tags$div(class="ray-left-up"),
                                      # tags$div(class="ray-left-middle"),
                                      # tags$div(class="ray-left-down"),
                                      # tags$div(class="ray-right-up"),
                                      # tags$div(class="ray-right-middle"),
                                      # tags$div(class="ray-right-down"),
                             tags$div(class="content",
                                    i18n$t("Hier kannst du tiefer in die Statistik einsteigen! Zunächst kannst du dich über das richtige"),
                                      tags$a(href = "#scales", "Skalenniveau" |> i18n$t()),
                                    i18n$t("informieren."),
                                    tags$br(),
                                    i18n$t("Dann findest du einige"),
                                      tags$a(href = "#univar", "Maße für eine Variable." |> i18n$t()),
                                    tags$br(),
                                    i18n$t("Der größte Teil behandelt"),
                                      tags$a(href = "#multivar", "Multivariate Statistik." |> i18n$t()),
                                    tags$br(),
                                    i18n$t("Am Ende gibt es noch eine kleine Erklärung zur"),
                                      tags$a(href = "#factanal", "Faktorenanalyse" |> i18n$t()),
                                    i18n$t("sowie eine"),
                                      tags$a(href = "#summary", "visuelle Zusammenfassung." |> i18n$t())
                             
                                    )
                             ),
                          # tags$a(href = "#multivar", "Navigate down to multivariate stats!"), #TO DO: Add Section at the beginning to explain the Deep Dive and offer easier navigation
                           
                           tags$section(id="scales", style = "width: 80%;",
                           h2("Skalenniveaus"|>i18n$t()),  # google icon!
                           i18n$t("Das Wichtigste für die Auswahl des richtigen statistischen Verfahrens ist die Kenntnis über das Skalenniveau deiner Variablen."),
                           tags$br(),
                           i18n$t("Daher findest du hier eine einfache Entscheidungshilfe um herauszufinden, welches Skalenniveau eine Variable hat:"),
                           tags$br(), tags$br(),
                           tags$img(src="./img/Scales_of_Measurement.png", width = '70%'),
                           tags$br(),
                           ),
                           tags$hr(),
                           
                           tags$section(id="univar",  style = "width: 80%;",
                           h2("Univariat - eine Variable"|>i18n$t()),
                           h3("Modus"|>i18n$t()),
                           i18n$t("Bei Daten, die mindestens nominalskaliert sind (also kategorial), kann man den Modus berechnen. Der Modus als Maß der zentralen Tendenz ist der Wert, den die Variable am häufigsten annimmt (z.B. das lokale Maximum einer Normalverteilung)."),
                           tags$br(),tags$br(),
                           p(
                             i18n$t("Der Modus wird, im Gegensatz zum Mittelwert (bei metrischen Daten), nicht durch extreme Werte bzw. Ausreißer verzerrt"),
                             "(Kaliyadan & Kulkarni, 2019)."
                           ),
                           
                           h3("Median"|>i18n$t()),
                           i18n$t("Bei Daten, die mindestens ordinalskaliert (kategorial mit Reihenfolge) sind, kann man den Median berechnen. Der Median als Maß der zentralen Tendenz ist die Stelle der Verteilung, über bzw. unter der je 50% der Daten liegen."),
                           tags$br(),tags$br(),
                           i18n$t("Der Median wird, im Gegensatz zum Mittelwert, nicht durch extreme Werte bzw. Ausreißer verzerrt (Crump et al., 2018)."),
                           
                           h3("Arithmetischer Mittelwert"|>i18n$t()),
                           i18n$t("Bei Daten, die mindestens intervallskaliert sind (also metrisch), kann man den arithmetischen Mittelwert (Durchschnitt) berechnen. Der Mittelwert als Maß der zentralen Tendenz ist die Summe aller Werte, die die Variable angenommen hat, geteilt durch die Anzahl dieser Werte."),
                           tags$br(),tags$br(),
                           i18n$t("Achtung: Der Mittelwert wird durch extreme Werte bzw. Ausreißer verzerrt. Bei sehr asymmetrischen Verteilungen ist ggf. der Median ein besseres Maß der zentralen Tendenz (Crump et al., 2018)."),
                           tags$br(),
                           tags$img(src = "./img/dist.svg", width = '45%'),
                           
                           h3("Standardabweichung"|>i18n$t()),
                           i18n$t("Die Standardabweichung ist ein Streuungsmaß, gibt also an, wie stark die Daten um den Mittelwert streuen. Je verschiedener die Werte sind, desto größer die Standardabweichung. Sie ist die Wurzel aus der Varianz einer Variablen und benötigt somit das gleiche Skalenniveau wie der Mittelwert."),
                           tags$br(),tags$br(),
                           i18n$t("Innerhalb der ersten Standardabweichungen über und unter dem Mittelwert einer Normalverteilung liegen ca. 68% der Daten. Innerhalb der ersten zwei Standardabweichungen über und unter dem Mittelwert einer Normalverteilung liegen mehr als 95% der Daten."),
                           plotOutput("dd_sdplot", width = '50%', height = '200px'),
                           ),
                           
                           tags$hr(),
                           
                           tags$section(id="multivar",  style = "width: 80%;",
                           h2("Multivariat - mehrere Variablen"|>i18n$t()),
                           h3("t-Test"),
                           i18n$t("Allgemein vergleicht der t-Test Mittelwerte mithilfe einer t-verteilten Statistik, es handelt sich also um einen parametrischen Test."),
                           i18n$t("Je nach Datenlage und Fragestellung kann man eine Stichprobe gegen einen Referenzwert testen, oder aber zwei Stichproben(-mittelwerte) gegeneinander. Man hat also zwei mindestens intervallskalierte Variablen, die man miteinander vergleichen möchte."),
                           tags$br(),
                           i18n$t("Bei einer gerichteten Hypothese erfolgt die Testung einseitig, bei einer ungerichteten Hypotheses testet man zweiseitig. Ist man sich nicht sicher über die Richtung des erwarteten Effekts, lohnt es sich, zweiseitig zu testen. In theoretisch gut begründeten Fällen kann man auch einseitig testen; dies erhöht die Wahrscheinlichkeit, einen Effekt aufzudecken."),
                           tags$br(),tags$br(),
                           i18n$t("Annahmen des t-Tests:"),
                           tags$ul(
                             tags$li("ausreichend große Stichprobe (Faustregel n=30)"|>i18n$t()),
                             tags$li("normalverteilte Daten"|>i18n$t())
                           ),
                           tags$br(),
                           i18n$t("Sind die Annahmen verletzt, kann man auf nicht-parametrische Alternativen des jeweiligen t-Tests ausweichen (Crump et al., 2018)."),
                           tags$br(),
                           tags$img(src = "./img/Bild2.png", width = '65%'),
                           
                           h4("Einstichproben t-Test"|>i18n$t()),
                           i18n$t("Der Einstichproben t-Test vergleicht einen Stichprobenmittelwert mit einem geschätzten oder festgelegten Populationsmittelwert, um zu schauen, ob die Stichprobe mit ausreichender Wahrscheinlichkeit aus dieser Population stammt oder aus einer anderen."),
                           tags$br(),
                           i18n$t("Dazu braucht man Mittelwert und Standardabweichung der Stichprobe und der Population und den Standardfehler der Mittelwertsverteilung. Da so gut wie immer die Parameter der Population unbekannt sind, muss man diese schätzen."),
                           tags$br(),tags$br(),
                           i18n$t("Ist der t-Test signifikant, gibt es eine Abweichung zwischen der Stichprobe und der Population, die mit ausreichend großer Wahrscheinlichkeit nicht zufällig zustandegekommen ist. Ist er nicht signifikant, geht man davon aus, dass die Stichprobe aus der Population stammt."),
                           tags$br(),tags$br(),
                           i18n$t("Nicht-parametrische Alternative: Wilcoxon Signed-Rank Test"),
                           
                           h4("Zweistichproben t-Test (abhängig)"|>i18n$t()),
                           i18n$t("Der abhängige t-Test ist dem Einstichproben t-Test sehr ähnlich. Er wird häufig für within-subjects Experimente genutzt (z.B. die Gedächtnisleistung einer Person vor einem Training wird mit ihrer Leistung nach einem Training verglichen) oder wenn Versuchpersonen aus zwei Gruppen miteinander verbunden sind (z.B. Zwillinge, Paare etc.)."),
                           tags$br(),
                           i18n$t("Die Berechnung ist ähnlich wie beim Einstichproben t-Test (Crump et al., 2018)."),
                           tags$br(),tags$br(),
                           i18n$t("Nicht-parametrische Alternative: Wilcoxon Signed-Rank Test"),
                           
                           h4("Zweistichproben t-Test (unabhängig)"|>i18n$t()),
                           i18n$t("Der Zweistichproben t-Test (auch unabhängiger t-Test) wird für between-subjects Experimente (z.B. der Mittelwertsvergleich zweier Gruppen, die verschiedene Treatments bekamen) genutzt (Crump et al., 2018)."),
                           tags$br(),tags$br(),
                           i18n$t("Nicht-parametrische Alternative:"),
                           tags$ul(
                             tags$li("Welch-Test (wenn die Annahme der Varianzhomogenität der beiden unabhängigen Gruppen verletzt ist)"|>i18n$t()),
                             tags$li("Mann-Whitney U-Test (wenn die Annahme der Normalverteilung verletzt ist; Variable darf ordinalskaliert sein)"|>i18n$t())
                           ),
                           
                           h3("ANOVA"),
                           i18n$t("Eine Varianzanalyse (ANOVA) ähnelt strukturell den t-Tests. Die F-Statistik, die bei einer ANOVA verwendet wird, ist eine quadrierte t-Statistik. Man nutzt Varianzanalysen, um herauszufinden, ob gefundene Mittelwertsunterschiede zwischen mehr als zwei Gruppen überzufällig sind oder nur durch Zufall oder Messfehler zustande kamen."),
                           tags$br(),
                           i18n$t("Das ist hilfreich, wenn es z.B. mehr als nur zwei Experimentalbedingungen gab, die miteinander verglichen werden sollen. Einfach mehrere t-Tests zu berechnen würde die Wahrscheinlichkeit eines Alpha-Fehlers erhöhen und ist daher keine sinnvolle Alternative."),
                           tags$br(),
                           p(i18n$t("Die Wahrscheinlichkeit, mind. ein signifikantes Testergebnis zu erhalten, steigt mit der Anzahl der Paarvergleiche um 1 - (1 - alpha)"),
                             tags$sup("Anzahl Paarvergleiche"|>i18n$t()), "(Janczyk & Pfister, 2015).",
                             i18n$t("Bei vier Gruppen (=sechs Paarvergleiche) wäre die Wahrscheinlichkeit eines falsch-positiven Ergebnisses also nicht mehr 5%, sondern schon"), "1-(1-0.05)",
                             tags$sup("6"), " = 26,5%!"
                             ),
                           tags$br(),
                           i18n$t("Annahmen der ANOVA:"),
                           tags$ul(
                             tags$li("intervallskalierte Daten"|>i18n$t()),
                             tags$li("unabhängige und zufällige Ziehung von k Stichproben"|>i18n$t()),
                             tags$li("gleiche Größe der k Stichproben (oder Normalverteilung der Daten und Varianzhomogenität der k samples müssen gelten; Crump et al., 2018)"|>i18n$t())
                           ),
                           tags$img(src = "./img/anova.svg", width = '55%'),
                           
                           h4("Einfaktorielle ANOVA"|>i18n$t()),
                           i18n$t("Eine einfaktorielle ANOVA benutzt man, wenn man eine unabhängige Variable (UV; Faktor) mit mindestens zwei (sinnvollerweise mindestens drei, sonst ginge auch ein t-Test) Faktorstufen hat. Man vergleicht dann im Prinzip auch die Mittelwerte der Faktorstufen miteinander, geht aber einen “Umweg” über die Varianzen."),
                           tags$br(),
                           i18n$t("Die Versuchspersonen der einzelnen Faktorstufen sind dabei unkorreliert, wie beim independent-samples t-Test. Die resultierende F-Statistik ergibt das Verhältnis aus erklärbarer Varianz durch die experimentelle Manipulation und der Fehlervarianz."),
                           tags$br(), tags$br(),
                           i18n$t("Mit einer ANOVA kann man nur herausfinden, ob mindestens eine Faktorstufe sich signifikant von mindestens einer weiteren unterscheidet. Um herauszufinden, welche Faktorstufen sich unterscheiden, muss man post-hoc Tests durchführen."),
                           tags$br(),
                           i18n$t("Man kann genau einen Haupteffekt finden, da es nur einen Faktor gibt. Um mögliche Interaktionen aufzudecken, braucht man Daten von mindestens zwei unabhängigen Variablen (Crump et al., 2018)."),
                           tags$br(),
                           i18n$t("Gängige post-hoc Tests (Auswahl):"),
                           tags$ul(
                             tags$li("Tukey HSD Test"|>i18n$t()),
                             tags$li("Least Significant Difference (LSD)"|>i18n$t()),
                             tags$li("Bonferroni"|>i18n$t())
                           ),
                           
                           h4("ANOVA mit Messwiederholung"|>i18n$t()),
                           i18n$t("Eine repeated-measures ANOVA nutzt man für within-subjects Designs. Man hat eine unabhängige Variable (UV; Faktor) mit mindestens zwei bzw. drei Faktorstufen."),
                           i18n$t("Im Gegensatz zur one-factor ANOVA sind aber die Versuchspersonen nicht unabhängig voneinander, sondern man erhebt z.B. Daten derselben Personen zu drei Messzeitpunkten und vergleicht dann sinngemäß jede Person mit sich selbst zu verschiedenen Zeitpunkten."),
                           tags$br(),
                           i18n$t("Nach wie vor werden aber nur Daten einer unabhängigen Variable erhoben, weshalb es auch hier nur Haupteffekte geben kann und noch keine Interaktionen (Crump et al., 2018)."),
                           
                           h4("Faktorielle ANOVA"|>i18n$t()),
                           i18n$t("Um Interaktionen zweier (oder mehrerer) Faktoren finden zu können, braucht man eine faktorielle ANOVA, d.h. man hat nun mehr als nur eine unabhängige Variable mit mehreren Faktorstufen. Beliebte Forschungsdesigns, wie das 2x2 factorial design, können mithilfe einer faktoriellen ANOVA analysiert werden."),
                           tags$br(),
                           i18n$t("Faktorielle ANOVAs erlauben es, Haupteffekte und Interaktionseffekte der Faktoren auf die abhängige Variable zu messen und können sowohl für within- als auch für between-subjects Experimente genutzt werden (Crump et al., 2018)."),
                           
                           h3("Regression"),
                           h4("Lineare Regression"|>i18n$t()),
                           i18n$t("Bei der linearen Regression möchte man mithilfe einer oder mehrerer unabhängigen Variablen (Prädiktoren) eine abhängige Variable vorhersagen. Die unabhängigen Variablen wie auch die abhängigen Variable sind metrisch."),
                           i18n$t("Grafisch dargestellt ist die Regressionsgerade die Linie, die die Daten am besten beschreibt, d.h. zu der die Abstände von jedem Datenpunkt eines Scatterplots aus minimal sind."),
                           tags$br(),
                           i18n$t("Diese Abstände zeigen den Messfehler an. Gäbe es keinen Messfehler, würden alle Datenpunkte auf der Regressionsgerade liegen (Crump et al., 2018; UZH, 2023)."),
                           plotOutput("dd_regplot", width = '60%'),
                           
                           h4("Logistische Regression"|>i18n$t()),
                           i18n$t("Die Logistische Regression ist ein statistisches Modell, das verwendet wird, um die Wahrscheinlichkeit eines bestimmten Ergebnisses vorherzusagen, wenn die abhängige Variable binär (z. B. Ja/Nein, Erfolg/Misserfolg) ist."),
                           i18n$t("Im Gegensatz zur linearen Regression, die eine kontinuierliche Variable vorhersagt, sagt die logistische Regression die Wahrscheinlichkeit eines Ereignisses voraus, die zwischen 0 und 1 liegt."),
                           tags$br(),
                           i18n$t("Die logistische Regression nutzt die Logit-Funktion, um die Beziehung zwischen den unabhängigen Variablen (Prädiktoren) und der Wahrscheinlichkeit des Auftretens eines bestimmten Ereignisses zu modellieren."),
                           tags$br(),
                           plotOutput("dd_logregplot", width = '60%'),
                           
                           h3("Chi", tags$sup("2")),
                            
                           p(i18n$t("Der"), "Chi", tags$sup("2"), i18n$t("Test (Kontingenzanalyse) gehört zu den nicht-parametrischen Verfahren, es wird also keine Annahme über die Verteilung der zugrundeliegenden Daten gemacht.") ),
                           
                           i18n$t("Er untersucht den Zusammenhang zweier nominal- oder ordinalskalierter Variablen, die in einer “Kreuztabelle” gegenübergestellt werden, indem beobachtete Häufigkeiten der Daten mit den erwarteten Häufigkeiten verglichen werden (nicht Mittelwerte!, s. t-Tests)."),
                           tags$br(),
                           
                           p(i18n$t("Der"), "Chi", tags$sup("2"), i18n$t("Test nutzt die"), "Chi", tags$sup("2"), i18n$t("Statistik, ansonsten funktioniert das Signifikanztesten analog zu den bereits genannten Verfahren."),
                             "Chi", tags$sup("2"), i18n$t("Tests können auf ein- und mehrdimensionale Zusammenhänge angewandt werden (Lowry, 1998; UZH, 2023).")),
                           ),
                           
                           tags$hr(),
                           
                          tags$section(id="factanal",  style = "width: 80%;",
                           h2("Faktorenanalyse"|>i18n$t()),
                           i18n$t("Faktorenanalysen gehören zu den Interdependenzanalysen. Sie werden genutzt, um Strukturen in den Daten zu entdecken (explorative Faktorenanalyse) oder erwartete Strukturen zu bestätigen (konfirmatorische Faktorenanalyse)."),
                           tags$br(),
                           i18n$t("Dabei ist das Ziel, hoch korrelierende Variablen zu übergeordneten Faktoren zusammenzufassen. Gefundene Faktoren sollten möglichst gering mit anderen Faktoren korrelieren."),
                           tags$br(), tags$br(),
                           i18n$t("Für eine Faktorenanalyse braucht man:"),
                           tags$ul(
                             tags$li("eine ausreichend große Stichprobe"|>i18n$t()),
                             tags$li("ausreichend viele Variablen"|>i18n$t()),
                             tags$li("intervallskalierte Variablen (Häufig werden dennoch ordinalskalierte Variablen verwendet.)"|>i18n$t())
                           ),
                           tags$br(),
                           i18n$t("Bei explorativen Faktorenanalysen (EFA) hat man keine Hypothesen über die Struktur, die geprüft werden soll, wie bei der konfirmatorischen Faktorenanalyse (CFA), die ein strukturüberprüfendes Verfahren darstellt (UZH, 2023)."),
                          ),
                          
                           tags$hr(),
                           
                          tags$section(id="summary", 
                           h2("Übersicht der gängigen Statistiken"|>i18n$t()),
                           tags$img(src="img/DeepDiveViz.png",
                                    alt="Überblick gängiger Statistiken",
                                    width = '90%'),
                           
                           # draw io try
                           
                           # tags$script(type="text/javascript", src="https://app.diagrams.net/js/viewer-static.min.js"),
                           # 
                           # tags$div(class="mxgraph", 
                           #          style="max-width:100%;border:1px solid transparent;", 
                           #          data-mxgraph="{&quot;highlight&quot;:&quot;#0000ff&quot;,&quot;nav&quot;:true,&quot;resize&quot;:true,&quot;xml&quot;:&quot;&lt;mxfile host=\&quot;Electron\&quot; agent=\&quot;Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) draw.io/26.0.4 Chrome/128.0.6613.186 Electron/32.2.5 Safari/537.36\&quot; version=\&quot;26.0.4\&quot;&gt;&lt;diagram name=\&quot;Page-1\&quot; id=\&quot;74e2e168-ea6b-b213-b513-2b3c1d86103e\&quot;&gt;&lt;mxGraphModel dx=\&quot;1500\&quot; dy=\&quot;887\&quot; grid=\&quot;1\&quot; gridSize=\&quot;10\&quot; guides=\&quot;1\&quot; tooltips=\&quot;1\&quot; connect=\&quot;1\&quot; arrows=\&quot;1\&quot; fold=\&quot;1\&quot; page=\&quot;1\&quot; pageScale=\&quot;1\&quot; pageWidth=\&quot;1100\&quot; pageHeight=\&quot;850\&quot; background=\&quot;none\&quot; math=\&quot;0\&quot; shadow=\&quot;0\&quot;&gt;&lt;root&gt;&lt;mxCell id=\&quot;0\&quot;/&gt;&lt;mxCell id=\&quot;1\&quot; parent=\&quot;0\&quot;/&gt;&lt;mxCell id=\&quot;77e6c97f196da883-1\&quot; value=\&quot;Pool\&quot; style=\&quot;swimlane;html=1;childLayout=stackLayout;startSize=20;rounded=0;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;1\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;70\&quot; y=\&quot;40\&quot; width=\&quot;960\&quot; height=\&quot;750\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-2\&quot; value=\&quot;Lane 1\&quot; style=\&quot;swimlane;html=1;startSize=20;\&quot; parent=\&quot;77e6c97f196da883-1\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry y=\&quot;20\&quot; width=\&quot;160\&quot; height=\&quot;730\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-8\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-2\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;20\&quot; y=\&quot;65\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-9\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-2\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;20\&quot; y=\&quot;155\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-10\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-2\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;560\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-26\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-8\&quot; target=\&quot;77e6c97f196da883-11\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-27\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-11\&quot; target=\&quot;77e6c97f196da883-9\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;&gt;&lt;Array as=\&quot;points\&quot;&gt;&lt;mxPoint x=\&quot;240\&quot; y=\&quot;155\&quot;/&gt;&lt;mxPoint x=\&quot;70\&quot; y=\&quot;155\&quot;/&gt;&lt;/Array&gt;&lt;/mxGeometry&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-28\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-9\&quot; target=\&quot;77e6c97f196da883-12\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-30\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-13\&quot; target=\&quot;77e6c97f196da883-9\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-31\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-16\&quot; target=\&quot;77e6c97f196da883-9\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-32\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-20\&quot; target=\&quot;77e6c97f196da883-9\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;&gt;&lt;Array as=\&quot;points\&quot;&gt;&lt;mxPoint x=\&quot;20\&quot; y=\&quot;690\&quot;/&gt;&lt;mxPoint x=\&quot;20\&quot; y=\&quot;370\&quot;/&gt;&lt;mxPoint x=\&quot;70\&quot; y=\&quot;370\&quot;/&gt;&lt;/Array&gt;&lt;/mxGeometry&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-33\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-13\&quot; target=\&quot;77e6c97f196da883-15\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-39\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-20\&quot; target=\&quot;77e6c97f196da883-23\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-40\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-23\&quot; target=\&quot;77e6c97f196da883-24\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;107ba76e4e335f99-1\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-17\&quot; target=\&quot;77e6c97f196da883-18\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;107ba76e4e335f99-2\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-22\&quot; target=\&quot;77e6c97f196da883-14\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;&gt;&lt;Array as=\&quot;points\&quot;&gt;&lt;mxPoint x=\&quot;660\&quot; y=\&quot;610\&quot;/&gt;&lt;mxPoint x=\&quot;660\&quot; y=\&quot;550\&quot;/&gt;&lt;mxPoint x=\&quot;400\&quot; y=\&quot;550\&quot;/&gt;&lt;/Array&gt;&lt;/mxGeometry&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;107ba76e4e335f99-3\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeColor=#000000;strokeWidth=1;fontFamily=Verdana;fontSize=8;fontColor=#000000;\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-22\&quot; target=\&quot;77e6c97f196da883-19\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;107ba76e4e335f99-4\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-22\&quot; target=\&quot;77e6c97f196da883-10\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;&gt;&lt;Array as=\&quot;points\&quot;&gt;&lt;mxPoint x=\&quot;660\&quot; y=\&quot;610\&quot;/&gt;&lt;mxPoint x=\&quot;660\&quot; y=\&quot;550\&quot;/&gt;&lt;mxPoint x=\&quot;80\&quot; y=\&quot;550\&quot;/&gt;&lt;/Array&gt;&lt;/mxGeometry&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;107ba76e4e335f99-5\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-10\&quot; target=\&quot;77e6c97f196da883-19\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;&gt;&lt;Array as=\&quot;points\&quot;&gt;&lt;mxPoint x=\&quot;80\&quot; y=\&quot;650\&quot;/&gt;&lt;mxPoint x=\&quot;490\&quot; y=\&quot;650\&quot;/&gt;&lt;mxPoint x=\&quot;490\&quot; y=\&quot;610\&quot;/&gt;&lt;/Array&gt;&lt;/mxGeometry&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;107ba76e4e335f99-6\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-1\&quot; source=\&quot;77e6c97f196da883-14\&quot; target=\&quot;77e6c97f196da883-19\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-3\&quot; value=\&quot;Lane 2\&quot; style=\&quot;swimlane;html=1;startSize=20;\&quot; parent=\&quot;77e6c97f196da883-1\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;160\&quot; y=\&quot;20\&quot; width=\&quot;160\&quot; height=\&quot;730\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-11\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-3\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;65\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-4\&quot; value=\&quot;Lane 3\&quot; style=\&quot;swimlane;html=1;startSize=20;\&quot; parent=\&quot;77e6c97f196da883-1\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;320\&quot; y=\&quot;20\&quot; width=\&quot;160\&quot; height=\&quot;730\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-12\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-4\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;155\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-13\&quot; value=\&quot;\&quot; style=\&quot;rhombus;whiteSpace=wrap;html=1;rounded=0;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-4\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;40\&quot; y=\&quot;240\&quot; width=\&quot;80\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-14\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-4\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;560\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-29\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-4\&quot; source=\&quot;77e6c97f196da883-12\&quot; target=\&quot;77e6c97f196da883-13\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-5\&quot; value=\&quot;Lane 4\&quot; style=\&quot;swimlane;html=1;startSize=20;\&quot; parent=\&quot;77e6c97f196da883-1\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;480\&quot; y=\&quot;20\&quot; width=\&quot;160\&quot; height=\&quot;730\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-15\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-5\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;240\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-16\&quot; value=\&quot;\&quot; style=\&quot;rhombus;whiteSpace=wrap;html=1;rounded=0;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-5\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;40\&quot; y=\&quot;320\&quot; width=\&quot;80\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-17\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-5\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;400\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-19\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-5\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;560\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-20\&quot; value=\&quot;\&quot; style=\&quot;rhombus;whiteSpace=wrap;html=1;rounded=0;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-5\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;40\&quot; y=\&quot;640\&quot; width=\&quot;80\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-34\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeColor=#000000;strokeWidth=1;fontFamily=Verdana;fontSize=8;fontColor=#000000;\&quot; parent=\&quot;77e6c97f196da883-5\&quot; source=\&quot;77e6c97f196da883-15\&quot; target=\&quot;77e6c97f196da883-16\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-35\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-5\&quot; source=\&quot;77e6c97f196da883-16\&quot; target=\&quot;77e6c97f196da883-17\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-36\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-5\&quot; source=\&quot;77e6c97f196da883-19\&quot; target=\&quot;77e6c97f196da883-20\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-6\&quot; value=\&quot;Lane 5\&quot; style=\&quot;swimlane;html=1;startSize=20;\&quot; parent=\&quot;77e6c97f196da883-1\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;640\&quot; y=\&quot;20\&quot; width=\&quot;160\&quot; height=\&quot;730\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-18\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-6\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;400\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-21\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-6\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;480\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-22\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-6\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;560\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-23\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-6\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;640\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-37\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-6\&quot; source=\&quot;77e6c97f196da883-18\&quot; target=\&quot;77e6c97f196da883-21\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-38\&quot; style=\&quot;edgeStyle=orthogonalEdgeStyle;rounded=1;html=1;labelBackgroundColor=none;startArrow=none;startFill=0;startSize=5;endArrow=classicThin;endFill=1;endSize=5;jettySize=auto;orthogonalLoop=1;strokeWidth=1;fontFamily=Verdana;fontSize=8\&quot; parent=\&quot;77e6c97f196da883-6\&quot; source=\&quot;77e6c97f196da883-21\&quot; target=\&quot;77e6c97f196da883-22\&quot; edge=\&quot;1\&quot;&gt;&lt;mxGeometry relative=\&quot;1\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-7\&quot; value=\&quot;Lane 6\&quot; style=\&quot;swimlane;html=1;startSize=20;\&quot; parent=\&quot;77e6c97f196da883-1\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;800\&quot; y=\&quot;20\&quot; width=\&quot;160\&quot; height=\&quot;730\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;mxCell id=\&quot;77e6c97f196da883-24\&quot; value=\&quot;\&quot; style=\&quot;rounded=1;whiteSpace=wrap;html=1;shadow=0;labelBackgroundColor=none;strokeWidth=1;fontFamily=Verdana;fontSize=8;align=center;\&quot; parent=\&quot;77e6c97f196da883-7\&quot; vertex=\&quot;1\&quot;&gt;&lt;mxGeometry x=\&quot;30\&quot; y=\&quot;640\&quot; width=\&quot;100\&quot; height=\&quot;60\&quot; as=\&quot;geometry\&quot;/&gt;&lt;/mxCell&gt;&lt;/root&gt;&lt;/mxGraphModel&gt;&lt;/diagram&gt;&lt;/mxfile&gt;&quot;,&quot;toolbar&quot;:&quot;pages zoom layers lightbox&quot;,&quot;page&quot;:0}"),
                           # 
                           
                           
                           
                           h2("Literatur"|>i18n$t()),
                           
                           tags$div(
                             style="line-height: 2; margin-left: 2em; text-indent:-2em;",
                             
                             p("Crump, M. J. C., Navarro, D. J., & Suzuki, J. (2018).",
                             tags$i("Answering questions with data."),
                             tags$a(href="https://www.crumplab.com/statistics/", "https://www.crumplab.com/statistics/.")
                             ),
                             
                             p("Janczyk, M., & Pfister, R. (2015)",
                               tags$i("Inferenzstatistik verstehen: Von A wie Signifikanztest bis Z wie Konfidenzintervall."),
                               "Springer.",
                               tags$a(href="https://doi.org/10.1007/978-3-662-47106-7", "https://doi.org/10.1007/978-3-662-47106-7.")
                             ),
                             
                             p("Kaliyadan, F., & Kulkarni, V. (2019). Types of Variables, Descriptive Statistics, and Sample Size.",
                               tags$i("Indian Dermatology Online Journal, 10"), "(1), 82–86.",
                               tags$a(href="https://doi.org/10.4103/idoj.IDOJ_468_18", "https://doi.org/10.4103/idoj.IDOJ_468_18.")
                             ),
                             
                             p("Lowry, R. (1998).",
                               tags$i("Concepts and Applications of Inferential Statistics."),
                               tags$a(href="http://vassarstats.net/textbook/", "http://vassarstats.net/textbook/.")
                             ),
                             
                             p("UZH. (2023). Datenanalyse mit SPSS. In",
                               tags$i("Universität Zürich: Methodenberatung."),
                               tags$a(href="http://www.methodenberatung.uzh.ch/de/datenanalyse_spss.html", "http://www.methodenberatung.uzh.ch/de/datenanalyse_spss.html.", 
                                      style = "overflow-wrap: break-word;")
                             )
                             # p("xxx",
                             #   tags$i("xxx"),
                             #   tags$a(href="xxx", "xxx")
                             # )
                             
                           ),
                          ),
                           
                           
                           # fluidRow(
                           #   htmltools::tags$iframe(src = "deep-dive.html", # src = "deep-dive.html",
                           #                          width = '100%',
                           #                          height = 6000,  # does not work as relative
                           #                          style = "border:none;")
                           # ),
                           
                           # htmltools::tags$img(# src="img/DeepDiveViz.png",
                           #   srcset="img/DeepDiveViz_long.png 681vw,
                           #                     img/DeepDiveViz.png 680vw",
                           #   sizes="(max-width: 680px) 680vw, (min-width: 681px) 681vw",
                           #   
                           #   width="90%",
                           #   alt="Überblick gängiger Statistiken")
                           
                           
                            
                           
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
                  tabPanel("Simulationen"|>i18n$t(), icon = icon("flask"),
                           navset_pill_list(
                             
                             nav_panel("Verteilung: Würfel"|>i18n$t(),
                                       i18n$t("Wirf einen oder mehrere digitale 6-seitige Würfel beliebig oft. Wie werden die Ergebnisse verteilt sein? Was erwartest du, was bei mehreren Würfeln (Augenzahl addiert) passiert - wird sich die Verteilung verändern?"),
                                         numericInput("n",
                                                      "Wie oft möchtest du würfeln?"|>i18n$t(),
                                                      min = 1,
                                                      max = 1000,
                                                      step = 1,
                                                      value = 6),
                                         
                                         numericInput("ndice",
                                                      "Wie viele Würfel möchtest du werfen?"|>i18n$t(),
                                                      min = 1,
                                                      max = 10,
                                                      step = 1,
                                                      value = 1),
                                       
                                       plotOutput("diePlot")
                                       ),
                             
                             nav_panel("Verteilung: Münze"|>i18n$t(),
                                       i18n$t("Hier siehst du die Wahrscheinlichkeitsverteilung, bei einem Münzwurf 'Zahl' zu erhalten. Ob die Münze fair (Wahrscheinlichkeit für Zahl p = 0.5) oder gezinkt (p ungleich 0.5) ist, verändert die Verteilung der Resultate."),
                                       i18n$t("Aber in der Realität liegt die Chance für 'Zahl' nicht immer exakt bei 50%: Es kann auch mal vorkommen, dass mehrfach hintereinander 'Zahl' untenliegt, obwohl die Münze fair ist. Probier es doch mal aus!"),
                                         sliderInput("p",
                                                     "Was ist die Wahrscheinlichkeit für Zahl?"|>i18n$t(),
                                                     min = 0.5,
                                                     max = 1,
                                                     value = 0.5,
                                                     ticks = F),
                                         
                                         numericInput("coins",
                                                      "Wie oft möchtest du die Münze werfen?"|>i18n$t(),
                                                      min = 1,
                                                      max = 1000,
                                                      step = 1,
                                                      value = 10)
                                         ,
                                         plotOutput("coinPlot")
                             ),
                             
                             nav_panel("Verteilung: Coefficient of variation"|>i18n$t(),
                                       i18n$t("Hier kannst du sehen, was der Coefficient of Variation (CV) bedeutet und wie er aus dem Zusammenspiel von Mittelwert und Standardabweichung entsteht. Achtung: Dies ist nur ein sinnvolles Maß für verhätnis-skalierte Variablen mit einem absoluten Nullpunkt."),
                               
                                       numericInput("cv_m",
                                                   "Welchen Mittelwert hat die Verteilung?"|>i18n$t(),
                                                   min = 1,
                                                   max = 100,
                                                   value = 5),
                                     
                                       numericInput("cv_sd",
                                                    "Was ist die Standardabweichung?"|>i18n$t(),
                                                    min = 0,
                                                    max = 70,
                                                    step = .1,
                                                    value = 3),
                                       
                                       plotOutput("cvPlot")
                             ),
                             
                             nav_panel("What's the T?",
                                       i18n$t("Was bedeuten eigentlich T-Werte und wie kommen sie zustande? Gib verschiedene Stichprobenmittelwerte sowie Standardabweichungen ein und beobachte, was das mit den Verteilungen macht! Unten kannst du dann raten: Welcher T-Wert kommt bei dem Stichprobenvergleich heraus?"),
                                
                                       fluidRow( #row2
                                         column(width = 3, tags$b(tags$u(i18n$t("Stichprobe 1:")))),
                                         column(4, 
                                                numericInput("t_m1",
                                                             "Mittelwert:"|>i18n$t(),
                                                             min = -500,
                                                             max = 500,
                                                             value = 7, width = "90%")
                                                ),
                                         column(5, 
                                                numericInput("t_sd1",
                                                             "Standardabweichung:"|>i18n$t(),
                                                             min = -100,
                                                             max = 100,
                                                             step = .1,
                                                             value = 1.8, width = "90%")
                                                ),
                                         column(3, tags$b(tags$u(i18n$t("Stichprobe 2:")))),
                                         column(4,
                                                numericInput("t_m2",
                                                             "Mittelwert:"|>i18n$t(),
                                                             min = -500,
                                                             max = 500,
                                                             value = 5, width = "90%")
                                                ),
                                         column(5,
                                                numericInput("t_sd2",
                                                             "Standardabweichung:"|>i18n$t(),
                                                             min = -100,
                                                             max = 100,
                                                             step = .1,
                                                             value = 2.3, width = "90%")
                                                )
                                       ),
                                       
                                       numericInput("t_n",
                                                     "Wie groß sind die Stichproben jeweils?"|>i18n$t(),
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
                                                             "Und nun rate mal: What's the T?"|>i18n$t(),
                                                             min = -Inf,
                                                             max = Inf,
                                                             step = .01,
                                                             value = 0, width = "90%")
                                                ),
                                         column(7,
                                                i18n$t("Kleiner Tipp: Wenn der erste Mittelwert kleiner ist, ist der T-Wert negativ.")
                                                )
                                         ),
                                         actionButton("tbtn", "Antwort"|>i18n$t()),
                                         shinyjs::hidden(htmlOutput("tguess"))
                                       
                             ), 
                             
                             nav_panel("Demo: Regressionsmodell"|>i18n$t(),
                                       i18n$t("Hier ist eine Demo des Regressionsmodells und was die Werte des Fehlers e und des Steigungsparameters b für die Verteilung der Daten bedeuten."),
                                       
                                       sliderInput("lm_e",
                                                   "Mean error e:",
                                                   min = 0,
                                                   max = 10,
                                                   # step = .01,
                                                   value = 1.5),
                                       
                                       sliderInput("lm_b",
                                                   "Slope b:",
                                                   min = -5,
                                                   max = 5,
                                                   # step = .1,
                                                   value = 2),
                                       
                                       plotOutput("lmPlot")
                             ), # close nav_panel
                           ) # close navset_pill_list
                           
                  ),### close tabPanel("Simulations")
                  tabPanel("About", icon = icon("code-merge"),
                           i18n$t("Im Menü unter dem Uni Konstanz Logo findest du einige Links, die für dich außerdem hilfreich sein könnten.Insbesondere das Online R Intro ist gut geeignet, um ein besseres 'Gefühl' für die Statistik zu erlangen! Daten anschauen und mit ihnen arbeiten ist wichtig für das Verständnis - ähnlich wichtig wie die Kenntnis der Rechnungen."),
                           tags$br(), tags$br(),
                           i18n$t("Daher findest du unter dem Tab Simulationen eine Sammlung von Datensimulationen, die z.B. zeigen, wie verschiedene Verteilungen zustande kommen. Wenn du Ideen oder Wünsche für weitere Features, Verteilungen o.Ä. hast, melde dich gern bei mir!"),
                           # tags$a("Mail an Tave", href = "mailto:overlander@uni-konstanz.de"),
                           tags$br(), tags$br(),
                           tags$a(class="btn btn-default", href="mailto:overlander@uni-konstanz.de", "E-Mail"),
                           tags$br(),
                           tags$hr(),
                           
                           tags$div(style = "text-align: center;",
                                    # tags$br(),
                                    i18n$t("Um den Stats Picker bequem auf deinem mobilen Gerät der Wahl zu installieren, nutze einfach die 'Zum Startbildschirm hinzufügen' Option in deinem Browser:"),
                                    tags$br(),tags$br(),
                                    
                                    tags$img(src = "./img/install_pwa.png", width = "35%")
                                    ),
                           
                           
                           
                           
                           tags$hr(),
                           i18n$t("Besonderer Dank gilt Anne-Sophie Landenberger und Elisabeth Mees für die Mitarbeit am Deep Dive sowie Patrick Slayer für die Übersetzung!"),
                           tags$br(), tags$br(),
                           i18n$t("Der Statistik Picker entsteht im Rahmen des Dissertationsprojekts von Annika Tave Overlander, M.Sc.")
                           
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
