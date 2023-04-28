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

  title = "SCiR: Web Application for Single Cell Repertoire Analysis",
  
  tabPanel("Upload",
    tabsetPanel(
      tabPanel("Cellranger output", uploadCellrangerUI("upload_cellranger"))
    )
  ),
  
  tabPanel("Gene Expression", 
    tabsetPanel(
      tabPanel("UMAP", geneExpressionUmapUI("gene_expression_umap")),
      tabPanel("Bar plot", geneExpressionBarplotUI("gene_expression_barplot"))
    )
  ),
  
  tabPanel("TCR",
    tabsetPanel(
      tabPanel("Alpha Diversity",  alphaDiversityUI("tcr_alpha_diversity")),
      tabPanel("Clonotype expand", clonotypeExpandUI("tcr_clonotype_expand")),
      tabPanel("Gene usage",       geneUsageUI("tcr_gene_usage", chain_choice = list("TRA", "TRB")))
    )
  ),
  
  tabPanel("BCR",
    tabsetPanel(
      tabPanel("Alpha Diversity",   alphaDiversityUI("bcr_alpha_diversity")),
      tabPanel("Clonotype expand",  clonotypeExpandUI("bcr_clonotype_expand")),
      tabPanel("Gene usage",        geneUsageUI("bcr_gene_usage", chain_choice = list("IGH", "IGL"))),
      tabPanel("Phylogenetic tree", phylogeneticTreeUI("bcr_phylogenetic_tree")),
    )
  ),
  

  tabPanel("(Data)", dataUI("data")),
  
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
  
  clonotypeExpandServer("tcr_clonotype_expand", tcrList()[["TRB"]], groupCols())
  clonotypeExpandServer("bcr_clonotype_expand", bcrList()[["IGH"]], groupCols())
  
  geneUsageServer("tcr_gene_usage", tcrList(), groupCols())
  geneUsageServer("bcr_gene_usage", bcrList(), groupCols())
  
  phylogeneticTreeServer("bcr_phylogenetic_tree", bcrList()[["IGH"]])
  
  dataServer("data", seuratObject(), tcrList(), bcrList(), groupCols())
  
}


shinyApp(ui, server)