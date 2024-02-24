Quality_controlUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(                           
      sliderInput(ns('slider_nCount_RNA'), label = "cCount", min = 0, max = 100000, value = c(0, 100000)),
      sliderInput(ns('slider_nFeatures_RNA'), label = "nFeature", min = 0, max = 10000, value = c(0, 10000)),
      sliderInput(ns('slider_percent_mt'), label = "percent mt", min = 0, max = 100, value = 100),
      sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 1, value = 0.1, step = 0.01),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 900, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 900, step = 100),
      downloadButton(ns("downloadPlot"), "Download Plot as PDF"),
      actionButton(ns("quality_control_rerun"), "Re-run"),
    ),
    mainPanel(
      textOutput(ns('rerun_started')),
      plotOutput(ns('plot'))
    ),
  )
}
Quality_controlServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    
  # Update input
  observeEvent(myReactives$seurat_object,
               {
                 seurat_object <- myReactives$seurat_object
                 updateSliderInput(session, 'slider_nCount_RNA', min = min(seurat_object@meta.data$nCount_RNA), max = max(seurat_object@meta.data$nCount_RNA), value = c(min(seurat_object@meta.data$nCount_RNA), max(seurat_object@meta.data$nCount_RNA)))
                 updateSliderInput(session, 'slider_nFeatures_RNA', min = min(seurat_object@meta.data$nFeature_RNA), max = max(seurat_object@meta.data$nFeature_RNA), value = c(min(seurat_object@meta.data$nFeature_RNA), max(seurat_object@meta.data$nFeature_RNA)))
                 updateSliderInput(session, 'slider_percent_mt', min = 0, max = floor(max(seurat_object@meta.data$percent.mt)) + 1, value = max(seurat_object@meta.data$percent.mt))
                 updateCheckboxGroupInput(session, 'subsetting', choices = sort(unique(seurat_object@meta.data$seurat_clusters)))
                 output$change_name <- renderUI({
                   map(as.character(sort(unique(seurat_object@meta.data$seurat_clusters))), ~ textInput(.x, .x))
                 })
               })
  
  #Quality control
  observe({
    if (!is.null(myReactives$seurat_object)) {
      myReactives$quality_sca <- myquality_control_scatterplot(myReactives$seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt, input$point_size)
      myReactives$quality_vln <- myquality_control_violinplot(myReactives$seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt)
    }
  })

  output$plot <- renderPlot({
      req(myReactives$seurat_object)
      myReactives$quality_control_plot <- ggarrange(myReactives$quality_sca, myReactives$quality_vln, ncol = 1)
      myReactives$quality_control_plot
    }, 
    width = plot_width, height = plot_height)

    setupDownloadPlotHandler(output, input, reactive({ myReactives$quality_control_plot }))



  observeEvent(input$quality_control_rerun, {
    output$rerun_started <- renderText('Re-run started')
    myReactives$seurat_object <- myseurat_normalize_umap_rerun(myReactives$seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt)
  })
})
}