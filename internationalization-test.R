library(shiny)
library(shiny.i18n)
library(ggplot2)

i18n <- Translator$new(translation_json_path='translations.json')
i18n$set_translation_language('en')

ui <- fluidPage(
  shiny.i18n::usei18n(i18n),
  tags$div(
    style='float: right;',
    selectInput(
      inputId='selected_language',
      label=i18n$t('Change language'),
      choices = i18n$get_languages(),
      selected = i18n$get_key_translation()
    )
  ),
  titlePanel(i18n$t('MtCars Dataset Explorer'), windowTitle=NULL),
  sidebarLayout(
    sidebarPanel(
      width=3,
      tags$h4(i18n$t('Select')),
      varSelectInput(
        inputId='x_select', 
        label=i18n$t('X-Axis'), 
        data=mtcars
      ),
      varSelectInput(
        inputId='y_select', 
        label=i18n$t('Y-Axis'),
        data=mtcars
      )
    ),
    mainPanel(
      plotOutput(outputId='scatter'),
      tags$p(i18n$t('This is description of the plot.'))
    )
  )
)




server <- function(input, output, session) {
  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
  })
  
  output$scatter <- renderPlot({
    col1 <- sym(input$x_select)
    col2 <- sym(input$y_select)
    
    ggplot(mtcars, aes(x= !! col1, y= !! col2)) + 
      geom_point(size=6) + 
      ggtitle(i18n$t('Scatter plot'))
  })
}

shinyApp(ui=ui, server=server)


