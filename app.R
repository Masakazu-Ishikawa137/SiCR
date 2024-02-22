source('setting.R')

ui <- navbarPage(
#  includeCSS("style.css"),
  title = "SiCR: Web Application for Single Cell Repertoire Analysis (Ver. 1.3.0)",
  tabPanel("Upload",
    uploadUI("upload")
  ),
  tabPanel('Gene Expression',
    tabsetPanel(
      tabPanel("Dimensional Plot",
        dimentional_plotUI("dimplot")
      ),
      tabPanel('Feature Plot',
        featureplotUI("featureplot")
      ),
      tabPanel("Violin Plot",
        ViolinPlotUI("violinplot")
      ),
      tabPanel('Quality control',
        Quality_controlUI("quality_control")
      ),
      tabPanel('FindMarker',
        findmarkerUI("findmarker")
      )
    ),
  ),
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
      ),
    ),
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
      ),
    ),
  ),
)


server <- function(input, output, session){
  myReactives <- reactiveValues()
  uploadServer("upload", myReactives)
  dimentional_plotServer("dimplot", myReactives)
  featureplotServer('featureplot', myReactives)
  Quality_controlServer('quality_control', myReactives)
  ViolinPlotServer("violinplot", myReactives)
  findmarkerServer("findmarker", myReactives)
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





# ui <- navbarPage(
#   includeCSS("style.css"),
#   title = "SiCR: Web Application for Single Cell Repertoire Analysis",
#   tags$footer("© 2023 Your Company. All rights reserved."),
#   tabPanel("Upload",
#     sidebarLayout(
#       uploadCellranger_sidebarUI("upload"),
#       uploadCellranger_mainUI("upload"),
#       # downloadButton('downloaddata', 'Download data')
#     ),
#   ),

#   #  tabPanel("Gene Expression", 
# #    tabsetPanel(
# #      tabPanel(
# #        "UMAP", 
# #        sidebarLayout(
# #          geneExpressionUmap_sidebarUI("gene_expression_umap"),
# #          geneExpressionUmap_mainUI("gene_expression_umap")
# #        ),
# #      ),
# #      tabPanel(
# #        "Bar plot",
# #        sidebarLayout(
# #          geneExpressionBarplot_sidebarUI('gene_expression_barplot'),
# #          geneExpressionBarplot_mainUI('gene_expression_barplot')
# #        )
# #      )
#     ),
#   ),
  
#   # tabPanel(
#   #   "TCR",
#   #   tabsetPanel(
#   #     tabPanel(
#   #       "Alpha diversity",
#   #       sidebarLayout(
#   #         alphaDiversity_sidebarUI("tcr_alpha_diversity"),
#   #         alphaDiversity_mainUI("tcr_alpha_diversity")),
#   #       ),
#   #     tabPanel(
#   #       "Clonal abundance",
#   #       sidebarLayout(
#   #         clonalAbundance_sidebarUI("tcr_clonal_abundance"),
#   #         clonalAbundance_mainUI("tcr_clonal_abundance"),
#   #       ),
#   #     ),
#   #     tabPanel(
#   #       "Clonotype expand",
#   #       sidebarLayout(
#   #         clonotypeExpand_sidebarUI("tcr_clonotype_expand"),
#   #         clonotypeExpand_mainUI("tcr_clonotype_expand")
#   #       ),
#   #     ),
#   #     tabPanel(
#   #       "Gene usage",
#   #       sidebarLayout(
#   #         geneUsage_sidebarUI("tcr_gene_usage", chain_choice = list("TRA", "TRB")),
#   #         geneUsage_mainUI("tcr_gene_usage", chain_choice = list("TRA", "TRB")),
#   #       ),
#   #     ),
#   #     tabPanel(
#   #       "Antigen prediction",
#   #       sidebarLayout(
#   #         antigenPrediction_sidebarUI("tcr_antigen_prediction"),
#   #         antigenPrediction_mainUI("tcr_antigen_prediction")
#   #       )
#   #     )
#   #   ),
#   # ),
  
#   # tabPanel("BCR",
#   #   tabsetPanel(
#   #     tabPanel(
#   #       "Alpha diversity",
#   #       sidebarLayout(
#   #         alphaDiversity_sidebarUI("bcr_alpha_diversity"),
#   #         alphaDiversity_mainUI("bcr_alpha_diversity")
#   #       ),
#   #     ),
#   #     tabPanel(
#   #       "Clonal abundance",
#   #       sidebarLayout(
#   #         clonalAbundance_sidebarUI("bcr_clonal_abundance"),
#   #         clonalAbundance_mainUI("bcr_clonal_abundance")
#   #       ),
#   #     ),
#   #     tabPanel(
#   #       "Clonotype expand",
#   #       sidebarLayout(
#   #         clonotypeExpand_sidebarUI("bcr_clonotype_expand"),
#   #         clonotypeExpand_mainUI("bcr_clonotype_expand")
#   #       ),
#   #     ),
#   #     tabPanel(
#   #       "Gene usage",
#   #       sidebarLayout(
#   #         geneUsage_sidebarUI("bcr_gene_usage", chain_choice = list("IGH", "IGL")),
#   #         geneUsage_mainUI("bcr_gene_usage", chain_choice = list("IGH", "IGL")),
#   #       ),
#   #     ),
#   #     tabPanel(
#   #       "Antigen prediction",
#   #       sidebarLayout(
#   #         antigenPrediction_sidebarUI("bcr_antigen_prediction"),
#   #         antigenPrediction_mainUI("bcr_antigen_prediction")
#   #       ),
#   #     ),
#   #     tabPanel(
#   #       "Phylogenetic tree",
#   #       sidebarLayout(
#   #         phylogeneticTree_sidebarUI("bcr_phylogenetic_tree"),
#   #         phylogeneticTree_mainUI("bcr_phylogenetic_tree")
#   #       ),
#   #     ),
#   #   ),
#   # ),
#   # tabPanel("Inquiry",
#   #          h4("If you have any question/suggestion, please fill the form below and send me"),
#   #          shiny::a("Link to Google Form", href = "https://docs.google.com/forms/d/e/1FAIpQLSeIGfGtbFvQKhx6lF9j29nGREMCyRxD_eEcGiqcmrNFORhIMQ/viewform?usp=sf_link", target = "_blank")
#   # ),
# )


# server <- function(input, output, session) {
#   myReactives <- reactiveValues()

#   upload
# #  dataList <- uploadCellrangerServer("upload_cellranger")
#   dataList <- uploadCellranger_mainServer("upload")
  
#   seuratObject <- reactive({ dataList()[["seurat_object"]] })
#   tcrList      <- reactive({ dataList()[["tcr_list"]] })
#   bcrList      <- reactive({ dataList()[["bcr_list"]] })
#   groupCols    <- reactive({ dataList()[["group_cols"]] })
  
# #  seuratObject()@misc$BCR <- bcrList()
# #  seuratObject()@misc$TCR <- tcrList()
# #  seuratObject()@misc$group <- groupCols()
  
#   output$downloaddata = downloadHandler(
#     filename = "data.rds",
#     content = function(file) {
#       saveRDS(dataList, file)
#     }
#   )
  
#   geneExpressionUmap_mainServer("gene_expression_umap", seuratObject(), groupCols())
#   geneExpressionBarplot_mainServer("gene_expression_barplot", seuratObject()@meta.data, groupCols())
  
#   alphaDiversity_mainServer("tcr_alpha_diversity", tcrList()[["TRB"]], groupCols())
#   alphaDiversity_mainServer("bcr_alpha_diversity", bcrList()[["IGH"]], groupCols())
  
#   clonalAbundance_mainServer("tcr_clonal_abundance", tcrList()[["TRB"]], groupCols())
#   clonalAbundance_mainServer("bcr_clonal_abundance", bcrList()[["IGH"]], groupCols())
  
#   clonotypeExpand_mainServer("tcr_clonotype_expand", tcrList()[["TRB"]], groupCols())
#   clonotypeExpand_mainServer("bcr_clonotype_expand", bcrList()[["IGH"]], groupCols())
  
#   geneUsage_mainServer("tcr_gene_usage", tcrList(), groupCols())
#   geneUsage_mainServer("bcr_gene_usage", bcrList(), groupCols())
  
#   antigenPrediction_mainServer("tcr_antigen_prediction", tcrList()[["TRB"]], "data/230323_ruft_TCR_antigen_database.tsv")
#   antigenPrediction_mainServer("bcr_antigen_prediction", bcrList()[["IGH"]], "data/230323_ruft_BCR_antigen_database.tsv")
  
#   phylogeneticTree_mainServer("bcr_phylogenetic_tree", bcrList()[["IGH"]])
  
# }


shinyApp(ui, server)
