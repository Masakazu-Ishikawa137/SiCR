setupDownloadPlotHandler <- function(output, input, plotReactive, filenamePrefix = "plot") {
  output$downloadPlot <- downloadHandler(
    filename = function() {
    　paste(filenamePrefix, "-", format(Sys.time(), format = "%Y-%m-%d-%H-%M-%S"), ".pdf", sep = "")
#      paste(filenamePrefix, "-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = input$plot_width / 72, height = input$plot_height / 72)
      plot <- plotReactive()
      print(plot)
      dev.off()
    }
  )
}