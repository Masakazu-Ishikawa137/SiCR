
geneExpressionUmapUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("color_by"), "Color by", "seurat_clusters"),
      sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10,value = 0.1, step = 0.01),
      sliderInput(ns("label_size"), "Size of labels", min = 0, max = 20, value = 10, step = 1),
      checkboxInput(ns("legend"), "Show legend", value = TRUE),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
    ),
    mainPanel(
      plotOutput(ns("umap_plot")),
      downloadButton(ns("download_data"), "Download UMAP coordinates (.csv)"),
      downloadButton(ns("download_plot"), "Download plot (.pdf)")
    )
  )
}

geneExpressionUmapServer <- function(id, seurat_object, group_cols) {
  moduleServer(id, function(input, output, session) {
    
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(ifelse(input$legend, "right", "none"))
    observe(updateSelectInput(session, "color_by", choices = union("seurat_clusters", group_cols)))
    
    g <- reactive({ 
      DimPlot(
        seurat_object,
        label = TRUE,
        pt.size = input$point_size,
        group.by =input$color_by,
        label.size = input$label_size
      ) + theme(legend.position = legend())
    })
    
    output$umap_plot <- renderPlot(
      g(),
      width  = plot_width,
      height = plot_height
    )
    
    output$download_data <- downloadHandler(
      filename = function() {"UMAP_coordinates.csv"},
      content = function(file) {
        write.csv(seurat_object[["umap"]]@cell.embeddings, file, row.names = TRUE, quote = FALSE) # needs rownames (barcode)
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() {"UMAP.pdf"},
      content = function(file) {
        ggsave(file, plot = g(), width = plot_width(), height = plot_height(), unit = "px", dpi = "screen")
      }
    )
    
  })
}
