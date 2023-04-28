library(ggtree)
library(dowser)
library(alakazam)

phylogeneticTreeUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      textInput(ns("clone"), "Clone", "clonotype1"),
      actionButton(ns("run"), "Draw tree"),
      sliderInput(ns("plot_width"),  "Width",  min = 100, max = 2000, value = 500, step = 100),
      sliderInput(ns("plot_height"), "Height", min = 100, max = 2000, value = 500, step = 100),
    ),
    mainPanel(
      tableOutput(ns('tmp_table')),
      plotOutput(ns("tree")),
      # downloadButton(ns("download_plot"), "Download plot (.png)")
    )
  )
}


phylogeneticTreeServer <- function(id, bcr_IGH) {
  moduleServer(id, function(input, output, session) {
    
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    
    phyloData <- eventReactive(input$run, {
      make_phylotree_data(bcr_IGH, clone = input$clone)
    })
    
    phyloTree <- reactive({
      clones <- dowser::formatClones(phyloData())
      trees  <- dowser::getTrees(clones)
      plots  <- dowser::plotTrees(trees)
      return(plots)
    })
    
    output$tree <- renderPlot(
      phyloTree()[[1]],
      width  = plot_width,
      height = plot_height
    )

    
  })
}