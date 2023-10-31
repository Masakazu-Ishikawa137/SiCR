
geneExpressionBarplot_sidebarUI <- function(id) {
  ns <- NS(id)
  sidebarPanel(
    selectInput(ns("x"), "X", "sample"),
    selectInput(ns("color_by"), "Color by", "seurat_clusters"),
    selectInput(ns("palette"), "Color palette", choices = palette_list),
    radioButtons(ns("ver_or_hori"), "Vertical or horizontal", choices = list("vertical" = "vertical", "horizontal" = "horizontal"), selected = "vertical"),
    checkboxInput(ns("legend"), "Show legend", value = TRUE),
    sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
    sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
  )
}

geneExpressionBarplot_sidebarServer <- function(id, seurat_metadata, group_cols) {
  moduleServer(id, function(input, output, session) {})
  }