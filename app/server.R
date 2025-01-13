library(shiny)

# file with translations
i18n <- Translator$new(translation_json_path="../translations/translation_withDeepDive.json")

# Define some default variables
set.seed(123)
num <- rnorm(10, mean = 3, sd = 1) |> round(2)
ord <- factor(c("meh", "meh", "ok","ok","ok", "super", "super", "super", "super", "super"), 
              order = TRUE, levels = c("meh", "ok", "super")) 
nom <- c("bla", "bli", "blub", "bla", "blub", "blub", "bla", "bli", "blub", "blub")

num2 <- rnorm(10, mean = 5, sd = 1) |> round(2)
# ord2 <- factor(c("klein", "klein", "klein","groß","groß", "groß", "riesig", "riesig", "riesig", "riesig"), 
#                order = TRUE, levels = c("klein", "groß", "riesig")) 
nom2 <- sample(LETTERS[1:2], length(nom), replace = T)

# für Plotting
# data <- data.frame(num, ord, nom, num2, ord2, nom2)

new_nom <- as.data.frame(table(data.frame(nom)))
names(new_nom) <- c("Daten", "value")

pos_datasets <- c("iris", "mtcars", "Orange")

# Define server logic ----
function(input, output, session) {
  # Translation
  # for select choices
  i18n_r <- reactive({
    i18n
  })
  
  observeEvent(input$selected_language, {
    lang <- input$selected_language 
    shiny.i18n::update_lang(lang, session)
    i18n_r()$set_translation_language(lang)
  })
  
  ord2 <- reactive({
    switch(input$selected_language,
                 "de" = factor(c("klein", "klein", "klein","groß","groß", "groß", "riesig", "riesig", "riesig", "riesig"), 
                               order = TRUE, levels = c("klein", "groß", "riesig")) ,
                 "en" = factor(c("small", "small", "small","large","large", "large", "huge", "huge", "huge", "huge"), 
                               order = TRUE, levels = c("small", "large", "huge")))
  }) 
  
  observe({
    updateSelectInput(session, "scale", label = i18n_r()$t("Welches Skalenniveau hat Variable 1?"),
                      choices = i18n_r()$t(c("intervall", "ordinal", "nominal"))) 
    
    updateSelectInput(session, "scale2", label = i18n_r()$t("Welches Skalenniveau hat Variable 2?"),
                       choices = i18n_r()$t(c("keins", "intervall", "ordinal", "nominal"))) 
    
    updateRadioButtons(session, "statstype", label = i18n_r()$t("Was hast du vor?"),
                       choices = i18n_r()$t(c("Statistik rechnen", "Visualisierung", "Döner mit alles")))
  })
  
 
  
  
  # Home: Output ----
  ## Show Plot details for viz
  observeEvent(input$statstype, {
    ## Show explanation only for conditions other than data viz    
    # shinyjs::toggle("expl_h4", condition = input$statstype != "Visualisierung")
    shinyjs::toggle("expl_h4", condition = !input$statstype %in% c("Visualisierung", "Visualization"))
    # shinyjs::toggle("statstypeout", condition = input$statstype != "Visualisierung")
    shinyjs::toggle("statstypeout", condition = !input$statstype %in% c("Visualisierung", "Visualization"))
  })
  
  observe({
    # ## Show dataViz only for conditions other than pure statistics
    # shinyjs::toggle("dataViz", condition = input$statstype != "Statistik rechnen") 
    shinyjs::toggle("dataViz", condition = !input$statstype %in% c("Statistik rechnen", "Calculate statistic"))
    # ## show table for 2 vars that can't be shown well in plots
    shinyjs::toggle("table", condition = input$scale == "nominal" && input$scale2 == "nominal") #input$statstype != "Statistik rechnen" &&
    # shinyjs::toggle("var2data", condition = input$scale2 != "keins")
    shinyjs::toggle("var2data", condition = !input$scale2 %in% c("keins", "none"))

  })
  
  
  
  ## Text generation for the first variable ----
  output$statstypeout <- renderText(
    switch(input$scale,
           "intervall" = , "interval" = switch(input$scale2,
                                "keins" = , "none" = paste("Intervallskalierte (oder auch: metrische) Daten sind numerisch - also Zahlen - und sind kontinuierlich. Beispielsweise Alter oder Körpergröße sind intervallskaliert."|>i18n$t(), #t
                                                tags$br(),
                                                "Hier nutzt man meist den Mittelwert. In R geht das mit 'mean()'."|>i18n$t()), #t
                                
                                "intervall" = , "interval" = paste("Bei 2 oder mehr intervallsaklierten Variablen hat man den meisten Spielraum bei der Auswahl an Statistiken und Modellen. Möchte man den Zusammenhang zweier Variablen herausfinden, nutzt man vor allem Korrelationen (in R mit cor()) oder die lineare Regression (in R mit lm())."|>i18n$t(), #t 
                                                    tags$br(),
                                                    "Wenn es um einen Unterschied geht, nutzt man am besten den t-Test (in R mit t.test())."|>i18n$t()), #t
                                
                                "ordinal" = paste("Für eine intervallskalierte Variable (oder auch mehrere), die mit einer kategorialen verglichen werden soll, eignet sich üblicherweise die Varianzanalyse oder ANOVA."|>i18n$t(), #t
                                                  tags$br(),
                                                  "Hierbei wird die kategoriale (oridnal oder nominal) Variable genutzt um Gruppen in der intervallskalierten Variable auf Mittelwertsunterschiede zu testen. In R nutzt man meist den aov() Befehl."|>i18n$t(), #t
                                                  tags$br(),
                                                  "Handelt es sich um eine binäre Variable, nutzt man am besten den t-Test."|>i18n$t()), #t
                                
                                "nominal" = paste("Für eine intervallskalierte Variable (oder auch mehrere), die mit einer kategorialen verglichen werden soll, eignet sich üblicherweise die Varianzanalyse oder ANOVA."|>i18n$t(), #t
                                                  tags$br(),
                                                  "Handelt es sich um eine binäre Variable eignet sich auch die Logistische Regression oder Punktbiseriale Korrelation." |>i18n$t(), #t
                                                  "Wenn es um einen Unterschied geht, nutzt man am besten den t-Test (in R mit t.test())."|>i18n$t()) #t
           ),
           "ordinal" = switch(input$scale2,
                              "keins" = , "none" =  paste("Ordinal sind Daten, bei denen die zugeordneten Zahlen zwar eine echte Reihenfolge, aber keine gleichen Abstände abbilden, z.B. bei einer Skala von 'doof' bis 'super' (1 bis 5)."|>i18n$t(), #t
                                              tags$br(), 
                                              "Hier nutzt man zum Beispiel den Median. In R geht das mit 'median()'."|>i18n$t()), #t
                              
                              "intervall" = , "interval" =  paste("Für eine intervallskalierte Variable (oder auch mehrere), die mit einer kategorialen verglichen werden soll, eignet sich üblicherweise die Varianzanalyse oder ANOVA."|>i18n$t(), #t
                                                  tags$br(),
                                                  "Hierbei wird die kategoriale (oridnal oder nominal) Variable genutzt um Gruppen in der intervallskalierten Variable auf Mittelwertsunterschiede zu testen. In R nutzt man meist den aov() Befehl."|>i18n$t(), #t
                                                  tags$br(),
                                                  "Handelt es sich um eine binäre Variable, nutzt man am besten den t-Test."|>i18n$t()), #t
                              
                              "ordinal" = paste("Bei zwei ordinalskalierten Variablen wird die Reihenfolge der Werte berücksichtigt, ohne von einem festen Abstand zwischen den Kategorien auszugehen. Man nutzt dafür unter anderem den Spearman-Rangkorrelationskoeffizienten, welcher misst, wie stark die Rangordnung der Werte in beiden Variablen zusammenhängt."|>i18n$t(), #t
                                                tags$br(),
                                                "Mit einer Kontingenztabelle kannst du die Häufigkeiten der Wertekombination beider Variablen darstellen."|>i18n$t(), #t
                                                "Anschließend kannst du einen Chi-Quadrat-Test durchführen, um zu prüfen, ob ein statistisch signifikanter Zusammenhang zwischen den Variablen besteht. In R geht das mit 'chisq.test()'."|>i18n$t()), #t
                              
                              "nominal" = paste("Bei einer ordinalen und einer nominalen Variable kannst du wie bei zwei ordinalen in einer Kontingenztabelle die Häufigkeiten der Wertekombinationen beider Variablen darstellen."|>i18n$t(), #t
                                                "Anschließend kannst du einen Chi-Quadrat-Test durchführen, um zu prüfen, ob ein statistisch signifikanter Zusammenhang zwischen den Variablen besteht. In R geht das mit 'chisq.test()'."|>i18n$t(), #t
                                                tags$br(),
                                                "Wenn du die Rangordnung der ordinalen Variable in den Kategorien der nominalen Variable bewerten möchtest, kannst du Kendall's Tau verwenden. Dieser Test ist eine Alternative zum Chi-Quadrat-Test und eignet sich gut, wenn du auch die Ranginformationen berücksichtigen willst."|>i18n$t()) #t
                              ),
           "nominal" = switch(input$scale2,
                              "keins" = , "none" =  paste("Hier nutzt man vor allem den Modus."|>i18n$t(), #t
                                              tags$br(), 
                                              "In R geht das zum Beispiel mit getmode() aus dem package wobblynameR."|>i18n$t()), #t
                              
                              "intervall" = , "interval" =  paste("Für eine intervallskalierte Variable (oder auch mehrere), die mit einer kategorialen verglichen werden soll, eignet sich üblicherweise die Varianzanalyse oder ANOVA."|>i18n$t(), #t
                                                  tags$br(),
                                                  "Hierbei wird die kategoriale (oridnal oder nominal) Variable genutzt um Gruppen in der intervallskalierten Variable auf Mittelwertsunterschiede zu testen. In R nutzt man meist den aov() Befehl."|>i18n$t(), #t
                                                  tags$br(),
                                                  "Handelt es sich um eine binäre Variable eignet sich auch die Logistische Regression oder Punktbiseriale Korrelation."|>i18n$t(), #t
                                                  "Wenn es um einen Unterschied geht, nutzt man am besten den t-Test (in R mit t.test())."|>i18n$t()), #t
                              
                              "ordinal" = paste("Bei einer ordinalen und einer nominalen Variable kannst du wie bei zwei ordinalen in einer Kontingenztabelle die Häufigkeiten der Wertekombinationen beider Variablen darstellen."|>i18n$t(), #t
                                                "Anschließend kannst du einen Chi-Quadrat-Test durchführen, um zu prüfen, ob ein statistisch signifikanter Zusammenhang zwischen den Variablen besteht. In R geht das mit 'chisq.test()'."|>i18n$t(), #t
                                                tags$br(),
                                                "Wenn du die Rangordnung der ordinalen Variable in den Kategorien der nominalen Variable bewerten möchtest, kannst du Kendall's Tau verwenden. Dieser Test ist eine Alternative zum Chi-Quadrat-Test und eignet sich gut, wenn du auch die Ranginformationen berücksichtigen willst."|>i18n$t()), #t
                              
                              "nominal" = paste("Wenn du zwei nominale Variablen analysieren möchtest, kannst du wie bei zwei ordinalen in einer Kontingenztabelle die Häufigkeiten der Wertekombinationen beider Variablen darstellen."|>i18n$t(), #t
                                                "Anschließend kannst du einen Chi-Quadrat-Test durchführen, um zu prüfen, ob ein statistisch signifikanter Zusammenhang zwischen den Variablen besteht. In R geht das mit 'chisq.test()'."|>i18n$t(), #t
                                                tags$br(),
                                                "Wenn dich die Stärke des Zusammenhangs interessiert, kannst du Cramérs V berechnen. Dieser Koeffizient gibt dir einen Wert zwischen 0 (kein Zusammenhang) und 1 (perfekter Zusammenhang) - du erhälst ihn in R z.B. mit dem Befehl 'vcd::assocstats()' (Funktion namens assocstats aus dem package vcd)."|>i18n$t()) #t
                              )
    )
  )
  
  ## Beispiel zur Durchführung ----
  output$statsex <- renderText(
      switch(input$scale,
             "intervall" = , "interval" = paste(" Daten: "|>i18n$t(), paste(num, collapse = ', '),  tags$br(), 
                                 "Mittelwert: "|>i18n$t(), round(mean(num), 2)),
             "ordinal" = paste(" Daten: "|>i18n$t(), paste(ord, collapse = ', '),  tags$br(),
                               "Median: "|>i18n$t(), quantile(ord, .5, type=1)),
             "nominal" = paste(" Daten: "|>i18n$t(), paste(nom, collapse = ', '),  tags$br(),
                               "Modus: "|>i18n$t(), wobblynameR::getmode(nom))
      )
  )
  
  output$var2data <- renderText(
    switch(input$scale2,
           "intervall" = , "interval" =  paste(tags$br(), " 2. Variable: ", paste(num2, collapse = ', '),  tags$br(), 
                               "Mittelwert: "|>i18n$t(), round(mean(num2), 2)),
           "ordinal" = paste( tags$br(), " 2. Variable: ", paste(ord2(), collapse = ', '),  tags$br(),
                             "Median: "|>i18n$t(), quantile(ord2(), .5, type=1)),
           "nominal" = paste( tags$br(), " 2. Variable: ", paste(nom2, collapse = ', '),  tags$br(),
                             "Modus: "|>i18n$t(), wobblynameR::getmode(nom2))
    )
  )
  
  # Table for vars that can't be plotted
  output$table <- renderTable(xtable::xtable(table(nom2, nom) %>% addmargins()),  
                              digits = 0,rownames = T)
  
  ## Plot output for Visualisierung and alles ----
  output$dataViz <- renderPlot({
    
    switch(input$scale2,
           "keins" = , "none" =  switch(input$scale,
                            "intervall" = , "interval" =
                              ggplot() +
                              geom_histogram(aes(num,  fill = after_stat(x)), bins = 9) +
                              scale_y_continuous(breaks = c(0, 1, 2, 3)) +
                              scale_fill_distiller(palette = 7)+
                              theme_minimal() +
                              theme(legend.position = "none",
                                    axis.title.y = element_blank(),
                                    text=element_text(family="Ubuntu", size = 14),
                                    title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
                              labs(x = "Daten"|>i18n$t(),
                                   title = "Visualisierung"|>i18n$t()),

                            "ordinal" =
                              ggplot() +
                              geom_bar(aes(ord,  fill = after_stat(x))) +
                              scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5)) +
                              scale_fill_distiller(palette = 7)+
                              theme_minimal() +
                              theme(legend.position = "none",
                                    axis.title.y = element_blank(),
                                    text=element_text(family="Ubuntu", size = 14),
                                    title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
                              labs(x = "Daten"|>i18n$t(),
                                   title = "Visualisierung"|>i18n$t()),

                            "nominal" =
                              cowplot::plot_grid(ggplot() + #plot1
                                                   geom_bar(aes(nom, fill = after_stat(x))) +
                                                   scale_y_continuous(breaks = c(0, 1, 2))  +
                                                   scale_fill_distiller(palette = 7) +
                                                   theme_minimal() +
                                                   theme(legend.position = "none",
                                                         axis.title.y = element_blank(),
                                                         text=element_text(size = 14), #family="Ubuntu", 
                                                         title = element_text(size = 16, color = 'gray15')) + #family="Ubuntu", 
                                                   labs(x = "Daten"|>i18n$t(),
                                                        title = "Visualisierung"|>i18n$t()),

                                                 ggplot(new_nom, aes(x = "", y = value, fill = Daten)) + #plot2
                                                   geom_bar(stat="identity", width=1) +
                                                   coord_polar("y", start=0)+
                                                   theme_void() +
                                                   theme(text=element_text(size = 14), #family="Ubuntu", 
                                                         title = element_text(size = 16, color = 'gray15')) + # family="Ubuntu", 
                                                   scale_fill_brewer(palette = 7, direction = -1),
                                                 ncol = 2, align = "h")
                            ),

           "intervall" = , "interval" =  switch(input$scale,
                                "intervall" = , "interval" =
                                  ggplot(tibble(x = num,
                                                y = num2), 
                                         aes(x, y)) +
                                  geom_jitter(color = "darkorange", size = 2.5)+
                                  geom_smooth(method=lm, fullrange = TRUE, color="darkorange2", 
                                              se=FALSE, formula = 'y ~ x') +
                                  theme_minimal() +
                                  theme(legend.position = "none",
                                        # axis.title.y = element_blank(),
                                        text=element_text(family="Ubuntu", size = 14),
                                        title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
                                  labs(x = "Variable 1",
                                       y = "Variable 2",
                                       title = "Visualisierung"|>i18n$t(),
                                       subtitle = "Zwei intervallskalierte Variablen"|>i18n$t()) +
                                  xlim(1,5) +
                                  ylim(1,7),
                                
                                "ordinal" = 
                                  ggplot(tibble(x = num2,
                                                y = ord), 
                                         aes(x, y, color = y, fill = y)) +
                                  geom_violin()+
                                  theme_minimal() +
                                  theme(legend.position = "none",
                                        text=element_text(family="Ubuntu", size = 14),
                                        title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
                                  labs(x = "Variable 2",
                                       y = "Variable 1",
                                       title = "Visualisierung"|>i18n$t(),
                                       subtitle = "Ordinal & Intervall") +
                                  scale_fill_brewer(palette = 7)+
                                  scale_color_brewer(palette = 7)+
                                  # xlim(1.5,5) +
                                  coord_flip(),
                                  
                                "nominal" =
                                  ggplot(tibble(x = nom,
                                                y = num2), 
                                         aes(x, y, color = x, fill = x)) +
                                  geom_boxplot(alpha = .8)+
                                  theme_minimal() +
                                  theme(legend.position = "none",
                                        text=element_text(family="Ubuntu", size = 14),
                                        title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
                                  labs(x = "Variable 1",
                                       y = "Variable 2",
                                       title = "Visualisierung"|>i18n$t(),
                                       subtitle = "Nominal & Intervall") +
                                  scale_fill_brewer(palette = 7) +
                                  scale_color_brewer(palette = 7)
                                ),
             
             
             
           "ordinal" = switch(input$scale,
                              "intervall" = , "interval" =
                                ggplot(tibble(x = num,
                                              y = ord2()), 
                                       aes(x, y, color = y, fill = y)) +
                                geom_violin()+
                                theme_minimal() +
                                theme(legend.position = "none",
                                      text=element_text(family="Ubuntu", size = 14),
                                      title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
                                labs(x = "Variable 1",
                                     y = "Variable 2",
                                     title = "Visualisierung"|>i18n$t(),
                                     subtitle = "Intervall & Ordinal") +
                                scale_fill_brewer(palette = 7)+
                                scale_color_brewer(palette = 7)+
                                xlim(1.5,5),
                              
                              "ordinal" =
                              ggplot(tibble(x = ord,
                                            y = ord2()), 
                                     aes(x, fill = y)) +
                                geom_bar(position="dodge")+
                                theme_minimal() +
                                theme(#legend.position = "none",
                                  text=element_text(family="Ubuntu", size = 14),
                                  title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
                                labs(x = "Variable 1",
                                     y = "Anzahl"|>i18n$t(),
                                     title = "Visualisierung"|>i18n$t(),
                                     subtitle = "Ordinal & Ordinal",
                                     fill = "Variable 2") +
                                scale_fill_brewer(palette = 7) +
                                scale_color_brewer(palette = 7),
                              
                              "nominal" =
                                ggplot(tibble(x = nom,
                                              y = ord2()), 
                                       aes(x, fill = y)) +
                                geom_bar(position="dodge")+
                                theme_minimal() +
                                theme(#legend.position = "none",
                                  text=element_text(family="Ubuntu", size = 14),
                                  title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
                                labs(x = "Variable 1",
                                     y = "Anzahl"|>i18n$t(),
                                     title = "Visualisierung"|>i18n$t(),
                                     subtitle = "Nominal & Ordinal",
                                     fill = "Variable 2") +
                                scale_fill_brewer(palette = 7) +
                                scale_color_brewer(palette = 7) +
                                scale_y_continuous(breaks =c(0,1,2))
                              
                              ),
           "nominal" = switch(input$scale,
                              "intervall" =  , "interval" =
                                ggplot(tibble(x = nom2,
                                              y = num), 
                                       aes(x, y, color = x, fill = x)) +
                                geom_boxplot()+
                                theme_minimal() +
                                theme(legend.position = "none",
                                      text=element_text(family="Ubuntu", size = 14),
                                      title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
                                labs(x = "Variable 2",
                                     y = "Variable 1",
                                     title = "Visualisierung"|>i18n$t(),
                                     subtitle = "Intervall & Nominal") +
                                scale_fill_brewer(palette = 7) +
                                scale_color_brewer(palette = 7) +
                                coord_flip(),
                              
                              "ordinal" =
                                ggplot(tibble(x = ord,
                                              y = nom2), 
                                       aes(x, fill = y)) +
                                geom_bar(position="dodge")+
                                theme_minimal() +
                                theme(#legend.position = "none",
                                  text=element_text(family="Ubuntu", size = 14),
                                  title = element_text(family="Ubuntu", size = 16, color = 'gray15')) +
                                labs(x = "Variable 1",
                                     y = "Anzahl"|>i18n$t(),
                                     title = "Visualisierung"|>i18n$t(),
                                     subtitle = "Nominal & Ordinal",
                                     fill = "Variable 2") +
                                scale_fill_brewer(palette = 7) +
                                scale_color_brewer(palette = 7)
                              )
             )
  })

  # ----
  # Beispiel Tab ----
  
  ## Creating the right dataset ----  
  ### Getting the dataset
  # ex_data <- reactive({
  #   if(input$ex_dataset %in% pos_datasets){
  #     upfile <- get(input$ex_dataset, "package:datasets")
  #   } else {
  #     upfile <- read.csv(input$customfile$datapath, # filepath
  #                        header = T,
  #                        sep = ";")
  #   }
  #   upfile
  #   #   get(input$ex_dataset, "package:datasets") # basically transforming the chr, e.g. "iris" to actual data  input
  # })
  # 
  # ### Getting the right column names
  # observe({
  #   col_names <- colnames(ex_data())
  #   updateCheckboxGroupInput(inputId = "ex_columns",
  #                            choices = col_names,
  #                            selected = col_names[1])
  # })  
  # 
  # ### Create the Datatable
  # output$ex_data_table <- renderTable(
  #   head(ex_data() |> dplyr::select(input$ex_columns),  n = input$obs)
  # )
  # 
  # ## Creating the right statistic (for CheckBox Auswahl) ----
  # observe({
  #   lvars <- length(input$ex_columns)
  #   pos_stats <- ifelse(lvars == 1, "Mittelwert", "t-Test")
  #   
  #   updateCheckboxGroupInput(inputId = "ex_stat",
  #                            choices = pos_stats,
  #                            selected = pos_stats[1])
  # })
  # 
  # 
  # output$ex_placeholder <- renderText(input$ex_columns)
  # 
  # output$ex_statistic <- renderText(
  #   switch(input$ex_stat,
  #          "Mittelwert" = paste("Mittelwert: ", mean(ex_data()[[input$ex_columns]], na.rm = T) %>% round(3)), #mean
  #          "t-Test" = paste("T-Wert: ", t.test(ex_data()[[input$ex_columns[1]]],
  #                                              ex_data()[[input$ex_columns[2]]])$statistic %>% round(3), "\n",
  #                           "p-Wert: ", t.test(ex_data()[[input$ex_columns[1]]],
  #                                              ex_data()[[input$ex_columns[2]]])$p.value %>% round(3)) #ttest
  #   )
  # )
  
  
  # Deep Dive Plots ----
  library(ggridges)
  
  output$dd_sdplot <- renderPlot({
    normal <- data.frame(x = rnorm(10000, mean = 100, sd = 15),
                         dummy = 1)
    
    ggplot(normal, aes(x = x, y = dummy, fill = factor(stat(quantile)))) +
      stat_density_ridges(
        geom = "density_ridges_gradient",
        calc_ecdf = TRUE,
        quantiles = c(0.0235, 0.1585, 0.8385, 0.9735), # quantiles = c(0.0235, 0.1585, 0.4985, 0.8385, 0.9735, 0.997)
        bandwidth = 2.1,
        color = "white"
      ) +
      # geom_vline(xintercept = seq(70, 130, 15), color = "#7f2704") + 
      scale_fill_manual(values = c("#FDD0A2", "#fd8d3c", "#e95420", "#fd8d3c", "#FDD0A2")) +
      scale_x_continuous(breaks = seq(55,145, 15),
                         lim = c(55, 145),
                         labels = c("M-3sd", "M-2sd","M-1sd", "M", "M+1sd", "M+2sd", "M+3sd")) +
      theme_minimal() +
      theme(legend.position = "none",
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            text = element_text(family="Ubuntu", size = 14, color = 'gray15'))
  })
  
  output$dd_regplot <- renderPlot({
    reg <- faux::rnorm_multi(n = 100,
                              mu = c(0, 0.2),
                              sd = c(1, 1.2),
                              r = c(0.7),
                              varnames = c("X", "Y"),
                              empirical = F)
    
    ggplot(reg, aes(x = X, y = Y)) +
      geom_jitter() +
      geom_smooth(formula = 'y ~x', method = lm, se = FALSE, color = "#8C2D04") +
      geom_vline(xintercept = mean(reg$X), color = "#fd8d3c") +
      geom_hline(yintercept = mean(reg$Y), color = "#e95420") +
      theme_minimal() +
      theme(axis.title.x = element_text(color = "#fd8d3c", size = 12),
            axis.title.y = element_text(color = "#e95420", size = 12))
  })
  
  output$dd_logregplot <- renderPlot({
    set.seed(666)
    x <- rnorm(100)
    y <- rbinom(100, 1, prob = plogis(x))
    data <- data.frame(x = x, y = y)
    
    # Logistisches Regressionsmodell anpassen
    # model <- glm(y ~ x, data = data, family = "binomial")
    
    # Visualisierung der Vorhersagewahrscheinlichkeit
    ggplot(data, aes(x = x, y = y)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "glm", formula = 'y ~ x', method.args = list(family = "binomial"), color = "#fd8d3c") +
      labs(# title = "Logistische Regression"|>i18n$t(), 
           x = "Prädiktor (x)"|>i18n$t(), 
           y = "Wahrscheinlichkeit (y)"|>i18n$t()) +
      theme_minimal()
  })

# Simulations Tab ------

# Dice: Normal Distribution
output$diePlot <- renderPlot({

  if (input$n > 1000){
    n <- 1000
    showNotification("Anzahl Würfe darf nicht größer als 1000 sein."|>i18n$t(), duration = 2)
  } else {
    n <- input$n
  }
  
  ndice <- ifelse(input$ndice > 10, 10, input$ndice)
  
    if (input$ndice == 1){
    dice <- sample(1:6, n, T)
  } else if (input$ndice > 1){
    bla <- matrix(sample(1:6, n * ndice, T), nrow = ndice)
    dice <- colSums(bla)
  } 
  
  if(input$ndice > 10) {
    showNotification("Anzahl Würfel darf nicht mehr als 10 sein."|>i18n$t(), duration = 2)
  }
  
  nseqbreaks <- ifelse(input$n < 60,
                       1,
                       ifelse(input$n < 450,
                              5, 
                              10)
  )

  ggplot2::ggplot() +
    geom_bar(aes(dice), fill = "#fd8d3c", color = "#8C2D04") +
    theme_minimal() +
    labs(x = "Augenanzahl"|>i18n$t(),
         y = "Häufigkeit"|>i18n$t(),
         title = "Würfel"|>i18n$t(),
         subtitle = paste0(n, ifelse(n == 1,
                                           " Wurf mit "|>i18n$t(),
                                           " Würfe mit "|>i18n$t()),
                           ndice, ifelse(ndice == 1,
                                               " Würfel"|>i18n$t(),
                                               " Würfeln"|>i18n$t()))) +
    scale_x_discrete(limits = as.character(1:(6*ndice)),
                     labels = as.character(1:(6*ndice)),
                     expand = c(0.01, 0.01)) +
    scale_y_continuous(breaks = seq(0, n, nseqbreaks))
  
  
})

# Coin: Probability

output$coinPlot <- renderPlot({
  
  if (input$coins > 1000){
    coins <- 1000
    showNotification("Anzahl Münzen darf nicht größer als 1000 sein."|>i18n$t(), duration = 2)
  } else {
    coins <- input$coins
  }
  
  vals <- dbinom(1:coins, coins, input$p)

  ggplot2::ggplot() +
    geom_point(aes(1:coins, vals),
               fill = "#fd8d3c", color = "#8C2D04", size = 3) +
    geom_line(aes(1:coins, vals), alpha = .2) +
    theme_minimal() +
    labs(x = "Häufigkeit 'Zahl'"|>i18n$t(),
         y = "Wahrscheinlichkeit 'Zahl'"|>i18n$t(),
         title = "Münze"|>i18n$t())
})

# Distribution and the coefficient of variation

output$cvPlot <- renderPlot({
  m <- input$cv_m
  sd <-input$cv_sd
  
  cvdata <- data.frame(x = rnorm(1000, mean = m, sd = sd))
  
 ggplot2:: ggplot(cvdata, aes(x)) +
    geom_density(fill = "#fd8d3c", color = "#8C2D04", alpha = 0.8) +
    geom_vline(xintercept = m, color = "#8C2D04") +
   labs(x = "X", y = "Y",
        title = paste0("Coefficient of variation = ", sd/m)) +
    theme_minimal() +
    coord_cartesian(xlim = c(m-2*m, m+2*m))
})


# What's the T?

t <- reactive({
  m1 <- input$t_m1
  m2 <- input$t_m2
  sd1 <- input$t_sd1
  sd2 <- input$t_sd2
  n <- input$t_n
  
  t <- data.frame(x =  rnorm(n, m1, sd1),
                  y =  rnorm(n, m2, sd2))
  t
})

tval <- reactive({
  t <- t()
  round(
    (mean(t$x) - mean(t$y)) / sqrt( (sd(t$x)^2/input$t_n) + (sd(t$y)^2/input$t_n)),
    2)
})

output$tPlot <- renderPlot({
  t <- t()
  
  longt <- tidyr::pivot_longer(t, cols = c(x,y))
  
  ggplot(longt, aes(name, value, color = name)) +
    geom_boxplot() +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_color_manual(values = c("#fd8d3c", "#8C2D04")) +
    labs(x = "",
         y = "Werteverteilung"|>i18n$t() 
         )
})

observeEvent(input$tbtn, {
  toggle("tguess")
})

tdiff <- reactive({
  round(abs(tval() - input$tguessval), 2)
})

output$tguess <- renderText(
  # tval()
  
  c(paste("Der echte T-Wert ist "|>i18n$t(), tval(),
        ", das heißt dein geratener Wert weicht um "|>i18n$t(), tdiff(),
        " davon ab."),
    ifelse(tdiff() > 2, "Du kriegst schon noch ein Gefühl dafür!"|>i18n$t(),
           "Super, das war schon nah dran!"|>i18n$t())
  )
  
)

# Demo: lm

output$lmPlot <- renderPlot({
  lmx <- sample(1:10, 100, T)
  lmb <- input$lm_b
  lme <- rnorm(length(lmx))*input$lm_e
  
  lmy <- lmb*lmx + lme
  df <- tibble::tibble(lmx, lmy)
  
  
  # draw the scatter plot with the specified number of bins
  ggplot2::ggplot(df, aes(x = lmx, y = lmy)) +
    geom_jitter(color = "#fd8d3c") +
    stat_smooth(method = "lm", se = FALSE,
                color = "#8C2D04", linewidth = .7) +
    theme_minimal() +
    labs(x = "X", y = "Y")+
    coord_cartesian(ylim=c(-50,50))
  
})


}





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