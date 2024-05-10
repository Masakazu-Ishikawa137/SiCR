# render_plot <- function(plot, plot_width, plot_height){
#     renderPlot({
#       req(plot)
#       plot
#     }, 
#     width = plot_width, height = plot_height)
# }

render_plot <- function(output, outputId, plotReactive, plotWidth, plotHeight) {
  output[[outputId]] <- renderPlot({
    req(plotReactive())
    plotReactive()
  }, width = plotWidth, height = plotHeight)
}

# render_plot2 <- function(output, outputId, plotReactive, input) {
#   output[[outputId]] <- renderPlot({
#     req(plotReactive())
#     plotReactive()
#   }, width = input$plot_width, height = input$plot_height)
# }

# renderplot2 <- function(output, outputId, plotReactive, plotWidth, plotHeight) {
#   output[[outputId]] <- renderPlot({
#     req(plotReactive())
#     plotReactive()
#   }, width = plotWidth, height = plotHeight)
#     setupDownloadPlotHandler(output, input, reactive({
#       plotReactive()
#     })
