source("setting.R")

ui <- navbarPage(
  #  includeCSS("style.css"),
  title = "SiCR: Web Application for Single Cell Repertoire Analysis (Ver. 1.14.0)",
  tabPanel(
    "Run",
    tabsetPanel(
      tabPanel(
        "First run",
        uploadUI("upload")
      ),
      tabPanel(
        "Re run",
        quality_controlUI("quality_control")
      ),
    ),
  ),
  tabPanel(
    "rerun",
    #  Quality_controlUI("quality_control")
  ),
  tabPanel(
    "Dimensional Plot",
    tabsetPanel(
      tabPanel(
        "Clusters",
        dimensional_plotUI("dimplot")
      ),
      tabPanel(
        "Features",
        featureplotUI("featureplot")
      ),
      tabPanel(
        "Highlight",
        highlightUI("highlight")
      ),
      tabPanel(
        "Annotation",
        annotation_newUI("annotation")
      )
    )
  ),
  tabPanel(
    "Compositional Analysis",
    compositional_analysisUI("compositional_analysis")
  ),
  # tabPanel("Compositional Analysis2",
  #   compositional_analysis_TCRUI("compositional_analysis2")
  # ),
  tabPanel(
    "Gene Expression",
    tabsetPanel(
      tabPanel(
        "Expression Distribution",
        expression_distributionUI("expression_distribution")
      ),
      tabPanel(
        "Differential Expression",
        differential_gene_expressionUI("differential_gene_expression")
      ),
      # tabPanel(
      #   "Volcano Plot",
      #   volcano_plotUI("volcanoplot")
      # ),
      #      tabPanel("Heatmap",
      #        heatmapUI("heatmap")
      #      ),
      #      tabPanel("Dot plot",
      #        dotplotUI("dotplot")
      #      ),
    ),
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
  tabPanel(
    "TCR",
    tabsetPanel(
      tabPanel(
        "Shannon Diversity",
        alphaDiversityUI("TCR_alpha_diversity")
      ),
        tabPanel(
        "Relative abundance",
        RelativeAbundanceUI("TCR_relative_abundance")
      ),
      #     tabPanel(
      #       "TCR clonal abundance",
      #       clonalAbundanceUI("TCR_clonal_abundance")
      #     ),
      #     tabPanel(
      #       "Repertoire overlap",
      #       clonotype_overlapUI("TCR_clonotype_overlap")
      #     ),
      #     tabPanel(
      #       "TCR clonotype expand",
      #       clonotypeExpandUI("TCR_clonotype_expand")
      #     ),
      #     tabPanel(
      #       "TCR antigen prediction",
      #       antigenPredictionUI("TCR_antigen_prediction")
      #     ),
      #     tabPanel(
      #       "clonotype tracking",
      #       clonotype_trackingUI("clonotype_tracking")
      #     ),
      tabPanel(
        "Repertoire Overlap",
        clonotype_overlapUI("TCR_clonotype_overlap")
      ),
      tabPanel(
        "Gene Usage",
        geneUsageUI("TCR_gene_usage", chain = "TCR")
      ),
      tabPanel(
        "Clonotype Abundance",
        clonalAbundanceUI('TCR_clonal_abundance', chain = 'TCR')
      ),
      tabPanel(
        "CDR3 spectratyping",
        cdr3_spectratypingUI('CDR3_spectratyping')
      ),
    )
  ),
  tabPanel(
    "BCR",
    tabsetPanel(
      tabPanel(
        "Diversity",
        alphaDiversityUI("BCR_alpha_diversity")
      ),
      #     tabPanel(
      #       "BCR clonal abundance",
      #       clonalAbundanceUI("BCR_clonal_abundance")
      #     ),
      #     tabPanel(
      #       "BCR clonotype expand",
      #       clonotypeExpandUI("BCR_clonotype_expand")
      #     ),
      #     tabPanel(
      #       "clonotype overlap",
      #       clonotype_overlapUI("BCR_clonotype_overlap")
      #     ),
      #     tabPanel(
      #       "BCR antigen prediction",
      #       antigenPredictionUI("BCR_antigen_prediction")
      #     ),
      #     tabPanel(
      #       "BCR phylogenetic tree",
      #       phylogeneticTreeUI("BCR_phylogentic_tree")
      #     )
    )
  )
)

server <- function(input, output, session) {
  myReactives <- reactiveValues()
  uploadServer("upload", myReactives)
  quality_controlServer("quality_control", myReactives)



  # dimensional plot
  dimensional_plotServer("dimplot", myReactives)
  featureplotServer("featureplot", myReactives)
  highlightServer("highlight", myReactives)
  annotation_newServer("annotation", myReactives)

  # compositional analysis
  # compositional_analysisServer("compositional_analysis", myReactives)
  # # compositional_analysis_TCRServer("compositional_analysis2", myReactives)

  # # gene expression
  # expression_distributionServer("expression_distribution", myReactives)
  # differential_gene_expressionServer("differential_gene_expression", myReactives)
  # volcano_plotServer("volcanoplot", myReactives)
  geneUsageServer("TCR_gene_usage", myReactives, chain = "TCR")
  clonotype_overlapServer("TCR_clonotype_overlap", myReactives, tcr_col = "TCR_pair_CTaa")
  #  clonotype_overlapServer("BCR_clonotype_overlap", myReactives, tcr_col = 'BCR_pair_CTaa')
  #  clonotype_trackingServer('clonotype_tracking', myReactives)
  #   heatmapServer("heatmap", myReactives)
  #   dotplotServer("dotplot", myReactives)
  #   Quality_controlServer('quality_control', myReactives)
  #   ViolinPlotServer("violinplot", myReactives)


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
  clonotype_trackingServer("clonotype_tracking", myReactives)
  cdr3_spectratypingServer("CDR3_spectratyping", myReactives)
  alphaDiversityServer("TCR_alpha_diversity", myReactives, "TCR_TRB_raw_clonotype_id")
  alphaDiversityServer("BCR_alpha_diversity", myReactives, "BCR_IGH_raw_clonotype_id")
  clonalAbundanceServer('TCR_clonal_abundance', myReactives, "TCR")
  # clonalAbundanceServer("TCR_clonal_abundance", myReactives, "TCR_TRB_raw_clonotype_id")
  # clonalAbundanceServer("BCR_clonal_abundance", myReactives, "BCR_IGH_raw_clonotype_id")
  # clonotypeExpandServer("TCR_clonotype_expand", myReactives, chain = "TCR_TRB_raw_clonotype_id")
  # clonotypeExpandServer("BCR_clonotype_expand", myReactives, "BCR_IGH_raw_clonotype_id")
  antigenPredictionServer("TCR_antigen_prediction", myReactives, db_path = "data/230323_ruft_TCR_antigen_database.tsv")
  antigenPredictionServer("BCR_antigen_prediction", myReactives, chain = "BCR_IGH_raw_clonotype_id", db_path = "data/230323_ruft_BCR_antigen_database.tsv")
  phylogeneticTreeServer("BCR_phylogentic_tree", myReactives)
  RelativeAbundanceServer("TCR_relative_abundance", myReactives, chain = "TCR")
}

shinyApp(ui, server)
