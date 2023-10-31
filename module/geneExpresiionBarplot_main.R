
geneExpressionBarplot_mainUI <- function(id) {
  ns <- NS(id)
  mainPanel(
    plotOutput(ns("barplot")),
    downloadButton(ns("download_data"), "Download data (.csv)"),
    downloadButton(ns("download_plot"), "Download plot (.pdf)")
  )
}

geneExpressionBarplot_mainServer <- function(id, seurat_metadata, group_cols) {
  moduleServer(id, function(input, output, session) {
    
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    observe({
      updateSelectInput(session, "x",        choices = union("seurat_clusters", group_cols), selected = "sample")
      updateSelectInput(session, "color_by", choices = union("seurat_clusters", group_cols), selected = "seurat_clusters")
    })
    
    # Tally
    geneExpressionTally <- reactive({
      my_tally(
        seurat_metadata,
        group_by = input$x,
        x = input$color_by,
        drop_na = TRUE,
        arrange = FALSE
      )
    })
    
    # Get plot
    geneExpressionPlot <- reactive({
      if(input$ver_or_hori == "horizontal") {
        flip <- TRUE
      } else {
        flip <- FALSE
      }
      my_barplot(
        geneExpressionTally(),
        x = input$x,
        y = "percent",
        fill = input$color_by,
        position = "stack",
        legend_position = input$legend,
        flip = flip,
        palette = input$palette
      )
    })
    
    # outputs
    
    output$barplot <- renderPlot(
      geneExpressionPlot(),
      width  = plot_width,
      height = plot_height
    )
    
    output$download_data <- downloadHandler(
      filename = function() {"gene_expression.csv"},
      content = function(file) {
        write_csv(geneExpressionTally(), file)
      }
    )
    
    output$download_plot <- downloadHandler(
      filename = function() {"gene_expression.pdf"},
      content = function(file) {
        ggsave(file, plot = geneExpressionPlot(), width = plot_width(), height = plot_height(), unit = "px", dpi = "screen")
      }
    )
    
  })
}