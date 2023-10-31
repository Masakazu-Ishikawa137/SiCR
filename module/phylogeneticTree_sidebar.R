library(ggtree)
library(dowser)
library(alakazam)

phylogeneticTree_sidebarUI <- function(id) {
  ns <- NS(id)
  sidebarPanel(
    textInput(ns("clone"), "Clone", "clonotype1"),
    actionButton(ns("run"), "Draw tree"),
    sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
    sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
  )
}


phylogeneticTree_sidebarServer <- function(id, bcr_IGH) {
  moduleServer(id, function(input, output, session) {
  })
}