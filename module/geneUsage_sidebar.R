geneUsage_sidebarUI <- function(id, chain_choice) {
  ns <- NS(id)
  sidebarPanel(
    radioButtons(ns("count_or_percent"), "Use cell count or percent?", choices = list("count" = "count", "percent" = "percent"), selected = "count"),
    selectInput(ns("chain"), "Cain", choices = chain_choice),
    selectInput(ns("gene"), "Gene", choices = ""),
    selectInput(ns("group_by"), "Group by", choices = list("sample" = "sample")),
    selectInput(ns("palette"), "Color palette", choices = palette_list),
    radioButtons(ns("ver_or_hori"), "Vertical or horizontal", choices = list("vertical" = "vertical", "horizontal" = "horizontal"), selected = "vertical"),
    checkboxInput(ns("legend"), "Show legend", value = TRUE),
    sliderInput(ns("plot_width"), "Width", min = 100, max = 2000, value = 500, step = 100),
    sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
  )
}



geneUsage_sidebarServer <- function(id, data, group_cols) {
  moduleServer(id, function(input, output, session) {
})
}