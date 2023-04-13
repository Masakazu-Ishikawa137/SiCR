
geneExpressionUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      selectInput(ns("color_by"), "Color by", "seurat_clusters"),
      sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10,value = 0.1, step = 0.01),
      sliderInput(ns("label_size"), "Size of labels", min = 0, max = 20, value = 10, step = 1),
      radioButtons(ns("legend"), "Legend", choices = list("False" = "none", "True" = "right"), selected = "right"),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
    ),
    mainPanel(
      plotOutput(ns("umap_plot"))
    )
  )
}

geneExpressionServer <- function(id, seurat_object, group_cols) {
  moduleServer(id, function(input, output, session) {
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    observe(updateSelectInput(session, "color_by", choices = union("seurat_clusters", group_cols)))
    g <- reactive({ 
      DimPlot(
        seurat_object,
        label = TRUE,
        pt.size = input$point_size,
        group.by =input$color_by,
        label.size = input$label_size
      ) 
    })
    output$umap_plot <- renderPlot(
      g() + theme(legend.position = input$legend),
      width  = plot_width,
      height = plot_height
    )
  })
}
