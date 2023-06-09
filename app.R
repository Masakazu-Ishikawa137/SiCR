library(ggsci)
library(RColorBrewer)
library(tidyverse)
library(Seurat)
library(shiny)


options(shiny.maxRequestSize=50*1024^2*1000)
options(shiny.port = 8100)

function_file_lst <- list.files("functions", pattern = ".R$", full.names = TRUE)
for (i in function_file_lst) {
  source(i)
}

module_file_lst <- list.files("module", pattern = ".R$", full.names = TRUE)
for (i in module_file_lst) {
  source(i)
}


ui <- navbarPage(
  
  includeCSS("style.css"),
  title = "SiCR: Web Application for Single Cell Repertoire Analysis",
  tags$footer("© 2023 Your Company. All rights reserved."),
  
  tabPanel("Upload",
    tabPanel("Cellranger output", uploadCellrangerUI("upload_cellranger"))
  ),
  
  tabPanel("Gene Expression", 
    tabsetPanel(
      tabPanel("UMAP", geneExpressionUmapUI("gene_expression_umap")),
      tabPanel("Bar plot", geneExpressionBarplotUI("gene_expression_barplot"))
    )
  ),
  
  tabPanel("TCR",
    tabsetPanel(
      tabPanel("Alpha diversity",    alphaDiversityUI("tcr_alpha_diversity")),
      tabPanel("Clonal abundance",   clonalAbundanceUI("tcr_clonal_abundance")),
      tabPanel("Clonotype expand",   clonotypeExpandUI("tcr_clonotype_expand")),
      tabPanel("Gene usage",         geneUsageUI("tcr_gene_usage", chain_choice = list("TRA", "TRB"))),
      tabPanel("Antigen prediction", antigenPredictionUI("tcr_antigen_prediction"))
    )
  ),
  
  tabPanel("BCR",
    tabsetPanel(
      tabPanel("Alpha diversity",    alphaDiversityUI("bcr_alpha_diversity")),
      tabPanel("Clonal abundance",   clonalAbundanceUI("bcr_clonal_abundance")),
      tabPanel("Clonotype expand",   clonotypeExpandUI("bcr_clonotype_expand")),
      tabPanel("Gene usage",         geneUsageUI("bcr_gene_usage", chain_choice = list("IGH", "IGL"))),
      tabPanel("Antigen prediction", antigenPredictionUI("bcr_antigen_prediction")),
      tabPanel("Phylogenetic tree",  phylogeneticTreeUI("bcr_phylogenetic_tree"))
    )
  )
  
)


server <- function(input, output) {
  
  dataList <- uploadCellrangerServer("upload_cellranger")
  
  seuratObject <- reactive({ dataList()[["seurat_object"]] })
  tcrList      <- reactive({ dataList()[["tcr_list"]] })
  bcrList      <- reactive({ dataList()[["bcr_list"]] })
  groupCols    <- reactive({ dataList()[["group_cols"]] })
  
  geneExpressionUmapServer("gene_expression_umap", seuratObject(), groupCols())
  geneExpressionBarplotServer("gene_expression_barplot", seuratObject()@meta.data, groupCols())
  
  alphaDiversityServer("tcr_alpha_diversity", tcrList()[["TRB"]], groupCols())
  alphaDiversityServer("bcr_alpha_diversity", bcrList()[["IGH"]], groupCols())
  
  clonalAbundanceServer("tcr_clonal_abundance", tcrList()[["TRB"]], groupCols())
  clonalAbundanceServer("bcr_clonal_abundance", bcrList()[["IGH"]], groupCols())
  
  clonotypeExpandServer("tcr_clonotype_expand", tcrList()[["TRB"]], groupCols())
  clonotypeExpandServer("bcr_clonotype_expand", bcrList()[["IGH"]], groupCols())
  
  geneUsageServer("tcr_gene_usage", tcrList(), groupCols())
  geneUsageServer("bcr_gene_usage", bcrList(), groupCols())
  
  antigenPredictionServer("tcr_antigen_prediction", tcrList()[["TRB"]], "data/230323_ruft_TCR_antigen_database.tsv")
  antigenPredictionServer("bcr_antigen_prediction", bcrList()[["IGH"]], "data/230323_ruft_BCR_antigen_database.tsv")
  
  phylogeneticTreeServer("bcr_phylogenetic_tree", bcrList()[["IGH"]])
  
}


shinyApp(ui, server)
