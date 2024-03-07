testUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
        checkboxGroupInput(ns('group_by'), 'Group by', choices = c('condition1', 'condition2')),
    ),
    mainPanel(
        textOutput(ns('text'))
    )
  )
}

testServer <- function(id, myReactives){
    moduleServer(id, function(input, output, session){
        output$text <- renderText(input$group_by)
    })
}

