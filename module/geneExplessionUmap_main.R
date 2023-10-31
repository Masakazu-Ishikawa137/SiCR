
geneExpressionUmap_mainUI <- function(id) {
  ns <- NS(id)
  mainPanel(
    plotOutput(ns("umap_plot")),
    downloadButton(ns("download_data"), "Download UMAP coordinates (.csv)"),
    downloadButton(ns("download_plot"), "Download plot (.pdf)")
  )
}

geneExpressionUmap_mainServer <- function(id, seurat_object, group_cols) {
  moduleServer(id, function(input, output, session) {
    
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    legend <- reactive(ifelse(input$legend, "right", "none"))
    observe(updateSelectInput(session, "color_by", choices = union("seurat_clusters", group_cols)))
    
    g <- reactive({
      my_colors <- my_palette(length(unique(pull(seurat_object@meta.data, !!input$color_by))), input$palette)
      DimPlot(
        seurat_object,
        label = TRUE,
        pt.size = input$point_size,
        group.by =input$color_by,
        label.size = input$label_size
      ) +
        scale_color_manual(values = my_colors) +
        theme(legend.position = legend())
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
