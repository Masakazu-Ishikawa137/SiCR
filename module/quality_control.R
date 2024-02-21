Quality_controlUI <- function(id){
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(                           
      sliderInput(ns('slider_nCount_RNA'), label = "cCount", min = 0, max = 100000, value = c(0, 100000)),
      sliderInput(ns('slider_nFeatures_RNA'), label = "nFeature", min = 0, max = 10000, value = c(0, 10000)),
      sliderInput(ns('slider_percent_mt'), label = "percent mt", min = 0, max = 100, value = 100),
      actionButton(ns("quality_control_rerun"), "Re-run"),
    ),
    mainPanel(
      textOutput(ns('rerun_started')),
      plotOutput(ns('quality_sca')),
      plotOutput(ns('quality_vln')),
    ),
  )
}
Quality_controlServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
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
      seurat_object <- myReactives$seurat_object
      output$quality_sca <- renderPlot(myquality_control_scatterplot(seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt))
      output$quality_vln <- renderPlot(myquality_control_violinplot(seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt))
    }
  })
  observeEvent(input$quality_control_rerun, {
    output$rerun_started <- renderText('Re-run started')
    myReactives$seurat_object <- myseurat_normalize_umap_rerun(myReactives$seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt)
  })
})
}