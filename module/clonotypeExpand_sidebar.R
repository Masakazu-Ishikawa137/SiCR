
clonotypeExpand_sidebarUI <- function(id){
  ns <- NS(id)
  sidebarPanel(
    radioButtons(ns("heat_or_bar"), "Heatmap or barplot?", choices = list("heatmap" = "heatmap", "barplot" = "barplot"), selected = "barplot"),
    radioButtons(ns("count_or_percent"), "Use cell count or percent?", choices = list("count" = "count", "percent" = "percent"), selected = "count"),
    selectInput(ns("group_by"), "Group by", choices = list("sample" = "sample")),
    selectInput(ns("focus_group"), "Show top 20 in this group", choices = ""),
    selectInput(ns("palette"), "Color palette", choices = ""),
    checkboxInput(ns("legend"), "Show legend", value = TRUE),
    sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
    sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
  )
}


clonotypeExpand_sidebarServer <- function(id, data, group_cols) {
  moduleServer(id, function(input, output, session) {
  })
}