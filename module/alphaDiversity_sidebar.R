library("alakazam")

alphaDiversity_sidebarUI <- function(id) {
  ns <- NS(id)
  sidebarPanel(
    selectInput(ns("group_by"), "Group by", choices = list("sample" = "sample")),
    selectInput(ns("palette"), "Color palette", choices = palette_list),
    selectInput(ns("q"), "Order of the Hill number (q)", choices = list("all (1-4)" = "all", "1 (Shannon diversity)" = 1, "2 (Simpson’s index)" = 2)),
    checkboxInput(ns("sd"), "Show SD", value = TRUE),
    checkboxInput(ns("legend"), "Show legend", value = TRUE),
    sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
    sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
  )
}


alphaDiversity_sidebarServer <- function(id, data, group_cols) {
  moduleServer(id, function(input, output, session) {
  })
}
