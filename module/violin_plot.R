ViolinPlotUI <- function(id){
  ns <- NS(id)
  fluidPage(
    textInput(ns("violin_text"), "Enter text:"),
    sliderInput(ns('violin_column'), label = 'Number of column', min = 1, max = 10, value = 2),
    plotOutput(ns('VlnPlot'))
  )
}

ViolinPlotServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
  observeEvent(input$violin_text,{
    if(!is.null(input$violin_text)){
      text_list <- unlist(strsplit(input$violin_text, ",\\s*"))
      output$VlnPlot <- renderPlot(VlnPlot(myReactives$seurat_object, text_list, ncol = input$violin_column))
    }
  })
})
}