
geneExpressionUmap_sidebarUI <- function(id) {
  ns <- NS(id)
  sidebarPanel(
    selectInput(ns("color_by"), "Color by", "seurat_clusters"),
    selectInput(ns("palette"), "Color palette", choices = palette_list),
    sliderInput(ns("point_size"), "Size of points", min = 0.01, max = 10,value = 0.1, step = 0.01),
    sliderInput(ns("label_size"), "Size of labels", min = 0, max = 20, value = 10, step = 1),
    checkboxInput(ns("legend"), "Show legend", value = TRUE),
    sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
    sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
  )
}

geneExpressionUmap_sidebarServer <- function(id, seurat_object, group_cols) {
  moduleServer(id, function(input, output, session) {
  })
}
