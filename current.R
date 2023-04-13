library(ggsci)
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
  
  "SCiR: Web Application for Single Cell Repertoire Analysis",
  
  tabPanel("Upload",
    tabsetPanel(
      tabPanel("Cellranger output", uploadCellrangerUI("upload_cellranger"))
    )
  ),
  
  tabPanel("Gene Expression", geneExpressionUI("gene_expression")),
  
  tabPanel("TCR",
    tabsetPanel(
      tabPanel("Alpha Diversity",  alphaDiversityUI("tcr_alpha_diversity")),
      tabPanel("Clonotype expand", clonotypeExpandUI("tcr_clonotype_expand")),
      tabPanel("Gene usage",       geneUsageUI("tcr_gene_usage", chain_choice = list("TRA", "TRB")))
    )
  ),
  
  tabPanel("BCR",
    tabsetPanel(
      tabPanel("Alpha Diversity",  alphaDiversityUI("bcr_alpha_diversity")),
      tabPanel("Clonotype expand", clonotypeExpandUI("bcr_clonotype_expand")),
      tabPanel("Gene usage",       geneUsageUI("bcr_gene_usage", chain_choice = list("IGH", "IGL")))
    )
  ),
  

  tabPanel("(Data)", dataUI("data")),
  
  # tags$style(
  #   HTML(".shiny-notification {
  #             height: 80px;
  #             width: 400px;
  #             position:fixed;
  #             top:  calc(50% - 40px);;
  #             left: calc(50% - 200px);;
  #           }"
  #   )
  # ),
  tags$style(HTML(".navbar-header { width:100% }"))
)


server <- function(input, output) {
  
  dataList <- uploadCellrangerServer("upload_cellranger")
  seuratObject <- reactive({ dataList()[["seurat_object"]] })
  tcrList      <- reactive({ dataList()[["tcr_list"]] })
  bcrList      <- reactive({ dataList()[["bcr_list"]] })
  groupCols    <- reactive({ dataList()[["group_cols"]] })
  
  geneExpressionServer("gene_expression", seuratObject(), groupCols())
  
  alphaDiversityServer("tcr_alpha_diversity", tcrList()[["TRB"]], groupCols())
  alphaDiversityServer("bcr_alpha_diversity", bcrList()[["IGH"]], groupCols())
  
  clonotypeExpandServer("tcr_clonotype_expand", tcrList()[["TRB"]], groupCols())
  clonotypeExpandServer("bcr_clonotype_expand", bcrList()[["IGH"]], groupCols())
  
  geneUsageServer("tcr_gene_usage", tcrList(), groupCols())
  geneUsageServer("bcr_gene_usage", bcrList(), groupCols())
  
  dataServer("data", seuratObject(), tcrList(), bcrList(), groupCols())
  
}


shinyApp(ui, server)