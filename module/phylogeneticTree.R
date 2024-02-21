library(ggtree)
library(dowser)
library(alakazam)
library(msa)
library(ape)

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
      plotOutput(ns("tree")),
      downloadButton(ns("download_plot"), "Download plot (.pdf)")
    )
  )
}


phylogeneticTreeServer <- function(id, myReactives) {
  moduleServer(id, function(input, output, session) {
    
    # size of plot
    plot_width <- reactive(input$plot_width)
    plot_height <- reactive(input$plot_height)
    

    tree <- eventReactive(input$run, {
      df <- make_germline_df_IGH(myReactives$seurat_object@meta.data)
      df <- df %>% dplyr::filter(BCR_IGH_raw_clonotype_id == input$clone) %>% distinct(BCR_IGH_exact_subclonotype_id, .keep_all = TRUE)
      line <- convert_to_fasta(df)
      writeLines(line, 'sequence.fasta')
      mySequenceFile <- paste0(getwd(), '/sequence.fasta')
      mySequences <- readAAStringSet(mySequenceFile)
      myFirstAlignment <- msa(mySequences)
      shrooms_align_subset_seqinr <- msaConvert(myFirstAlignment, type = "seqinr::alignment")
      shrooms_subset_dist <- seqinr::dist.alignment(shrooms_align_subset_seqinr,matrix = "identity")
      tree <- nj(shrooms_subset_dist)
      plot.phylo(tree, main="Phylogenetic Tree", use.edge.length = T)
    })

    output$tree <- renderPlot(
      tree(),
      width  = plot_width,
      height = plot_height
    )


    # phyloTree <- reactive({
    #   clones <- dowser::formatClones(phyloData())
    #   trees  <- dowser::getTrees(clones)
    #   plots  <- dowser::plotTrees(trees)
    #   plots[[1]] +
    #     geom_tiplab() +
    #     # To avoid label cropping
    #     coord_cartesian(clip = 'off') + 
    #     theme(plot.margin = margin(6,200,6,6))
    # })
    
    # output$tree <- renderPlot(
    #   phyloTree(),
    #   width  = plot_width,
    #   height = plot_height
    # )
    
    # output$download_plot <- downloadHandler(
    #   filename = function() { paste0("phylogenetic_tree_", input$clone, ".pdf") },
    #   content = function(file) {
    #     ggsave(file, plot = phyloTree(), width = plot_width(), height = plot_height(), unit = "px", dpi = "screen")
    #   }
    # )

    
  })
}