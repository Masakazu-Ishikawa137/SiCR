source('setting.R')

ui <- navbarPage(
#  includeCSS("style.css"),
  title = "SiCR: Web Application for Single Cell Repertoire Analysis (Ver. 1.9.1)",
  tabPanel("Upload",
    uploadUI("upload")
  ),
  tabPanel('Dimensional Plot',
    tabsetPanel(
      tabPanel("Clusters",
        dimensional_plotUI("dimplot")
      ),
      tabPanel("Features",
        featureplotUI("featureplot")
      ),
      tabPanel("Highlight",
        highlightUI("highlight")
      ),
    )
  ),
  # tabPanel('Gene Expression',
  #   tabsetPanel(
  #     tabPanel("highlight",
  #       highlightUI("highlight")
  #     ),
  #     tabPanel('Subsetting',
  #       subsettingUI('subsetting')
  #     ),
  #    tabPanel('tracking',
  #      clonotype_trackingUI('clonotype_tracking')
  #    ),
  #   #  tabPanel('Annotaiton',
  #   #    annotationUI('annotation')
  #   #  ),
  #     tabPanel('Barplot',
  #       barplotUI('barplot')
  #     ),
  #     tabPanel("Violin plot",
  #       ViolinPlotUI("violinplot")
  #     ),
  #     tabPanel("Heatmap",
  #       heatmapUI("heatmap")
  #     ),
  #     tabPanel("Dot plot",
  #       dotplotUI("dotplot")
  #     ),
  #     tabPanel('Quality control',
  #       Quality_controlUI("quality_control")
  #     ),
  #     tabPanel('Differential Expression',
  #       findmarkerUI("findmarker")
  #     ),
  #     tabPanel('loupeR',
  #       louperUI("louper")
  #     ),

  #     tabPanel('marker',  # Corrected the spelling from 'tabPalnel' to 'tabPanel'
  #       marker_showUI('marker')
  #     )
  #   )
  # ),
  tabPanel('TCR',
    tabsetPanel(
      tabPanel("TCR alpha diversity",
        alphaDiversityUI("TCR_alpha_diversity")
      ),
      tabPanel("TCR clonal abundance",
        clonalAbundanceUI("TCR_clonal_abundance")
      ),
      tabPanel("TCR clonotype expand",
        clonotypeExpandUI("TCR_clonotype_expand")
      ),
      tabPanel("TCR antigen prediction",
        antigenPredictionUI("TCR_antigen_prediction")
      )
    )
  ),
  tabPanel('BCR',
    tabsetPanel(
      tabPanel("BCR alpha diversity",
        alphaDiversityUI("BCR_alpha_diversity")
      ),
      tabPanel("BCR clonal abundance",
        clonalAbundanceUI("BCR_clonal_abundance")
      ),
      tabPanel("BCR clonotype expand",
        clonotypeExpandUI("BCR_clonotype_expand")
      ),
      tabPanel("BCR antigen prediction",
        antigenPredictionUI("BCR_antigen_prediction")
      ),
      tabPanel("BCR phylogenetic tree",
        phylogeneticTreeUI("BCR_phylogentic_tree")
      )
    )
  )
)

server <- function(input, output, session){
  myReactives <- reactiveValues()
  uploadServer("upload", myReactives)


# dimensional plot
  dimensional_plotServer("dimplot", myReactives)
  featureplotServer('featureplot', myReactives)
  highlightServer("highlight", myReactives)


#   plotlyServer("plotly", myReactives)
# #  subsettingServer('subsetting', myReactives)
# #  annotationServer('annotation', myReactives)
#   barplotServer("barplot", myReactives)
#   heatmapServer("heatmap", myReactives)
#   dotplotServer("dotplot", myReactives)
#   Quality_controlServer('quality_control', myReactives)
#   ViolinPlotServer("violinplot", myReactives)
#   findmarkerServer("findmarker", myReactives)
#   marker_showServer("marker", myReactives)
#   louperServer('louper', myReactives)
#   clonotype_trackingServer('clonotype_tracking', myReactives)
  alphaDiversityServer("TCR_alpha_diversity", myReactives, "TCR_TRB_raw_clonotype_id")
  alphaDiversityServer("BCR_alpha_diversity", myReactives, "BCR_IGH_raw_clonotype_id")
  clonalAbundanceServer("TCR_clonal_abundance", myReactives, "TCR_TRB_raw_clonotype_id")
  clonalAbundanceServer("BCR_clonal_abundance", myReactives, "BCR_IGH_raw_clonotype_id")
  clonotypeExpandServer("TCR_clonotype_expand", myReactives, chain = "TCR_TRB_raw_clonotype_id")
  clonotypeExpandServer("BCR_clonotype_expand", myReactives, "BCR_IGH_raw_clonotype_id")
  antigenPredictionServer("TCR_antigen_prediction", myReactives, db_path = "data/230323_ruft_TCR_antigen_database.tsv")
  antigenPredictionServer("BCR_antigen_prediction", myReactives, chain = "BCR_IGH_raw_clonotype_id", db_path = "data/230323_ruft_BCR_antigen_database.tsv")
  phylogeneticTreeServer("BCR_phylogentic_tree", myReactives)

}

shinyApp(ui, server)