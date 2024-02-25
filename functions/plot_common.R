radiobutton_legendUI <- function(id, label = "Legend"){
  radioButtons(id, label, choices = c("right", "left", "bottom", "top", "none"), selected = "right")
}

slider_plotsizeUI <- function(id, label = "Width", min = 100, max = 2000, value = 500, step = 100){
  sliderInput(id, label, min = min, max = max, value = value, step = step)

}



commonSettingsUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("legend"), "Legend", choices = c("right", "left", "bottom", "top", "none"), selected = "right"),
    sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
    sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
    downloadButton(ns("downloadPlot"), "Download Plot as PDF") 

  )
}

commonSettingsServer <- function(id, session) {
  moduleServer(id, function(input, output, session) {
    list(
      plot_width = reactive(input$plot_width),
      plot_height = reactive(input$plot_height),
      legend = reactive(input$legend)  # 修正された行
    )
  })
}



setupPdfDownloadHandler <- function(id, session, plotGenerator) {
  ns <- NS(id)
  output[[ns("downloadPlot")]] <- downloadHandler(
    filename = function() {
      paste0("plot-", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file, width = 7, height = 5)  # 適切なサイズを設定
      plot <- plotGenerator()
      print(plot)
      dev.off()
    }
  )
}