library("alakazam")

clonalAbundance_sidebarUI <- function(id) {
  ns <- NS(id)
  sidebarPanel(
    selectInput(ns("group_by"), "Group by", choices = list("sample" = "sample")),
    selectInput(ns("palette"), "Color palette", choices = palette_list), 
    checkboxInput(ns("sd"), "Show SD", value = TRUE),
    checkboxInput(ns("legend"), "Show legend", value = TRUE),
    sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
    sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
  )
}


clonalAbundance_sidebarServer <- function(id, data, group_cols) {
  moduleServer(id, function(input, output, session) {
})
}

