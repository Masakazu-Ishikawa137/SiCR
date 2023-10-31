
antigenPrediction_sidebarUI <- function(id) {
  ns <- NS(id)
  sidebarPanel(
    selectInput(ns("clonotype_id"), "Clonotype id", choices = "")
  )
}

antigenPrediction_sidebarServer <- function(id, data, db_path) {
  moduleServer(id, function(input, output, session) {
  })
}