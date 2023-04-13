
dataUI <- function(id){
  ns <- NS(id)
  tabsetPanel(
    tabPanel("seurat@meta.data",
             p("seurat_object@meta.dataにTCR/BCRのユーザーメタデータをjoin（join by sample）"),
             p("（TCRもBCRもある場合は、ユーザーメタデータはBCRのものが使われる）"),
             tableOutput(ns('seurat_metadata'))
    ),
    tabPanel("tcl_list",
             h3("tcr_list[['TRA']]"),
             p("TCRのchain=='TRA'の行を抽出 => barcode重複行を除去 => seurat_objectのmeta.dataのcelltypeをjoin"),
             tableOutput(ns('tcr_tra')),
             h3("tcr_list[['TRB']]"),
             p("TCRのchain=='TRB'の行を抽出 => barcode重複行を除去 => seurat_objectのmeta.dataのcelltypeをjoin"),
             tableOutput(ns('tcr_trb'))
    ),
    tabPanel("bcr_list",
             h3("bcr_list[['IGH']]"),
             tableOutput(ns('bcr_igh')),
             h3("bcr_list[['IGL']]"),
             tableOutput(ns('bcr_igl'))
    ),
    tabPanel("group_cols",
             p("グラフのグループに使われるカラム名のベクトル"),
             verbatimTextOutput(ns('group_cols'))
    )
  )
}



dataServer <- function(id, seurat_object, tcr_list, bcr_list, group_cols) {
  moduleServer(id, function(input, output, session) {
    
    output$seurat_metadata <- renderTable(seurat_object@meta.data %>% head())
    output$tcr_tra <- renderTable(tcr_list[["TRA"]] %>% head())
    output$tcr_trb <- renderTable(tcr_list[["TRB"]] %>% head())
    output$bcr_igh <- renderTable(bcr_list[["IGH"]] %>% head())
    output$bcr_igl <- renderTable(bcr_list[["IGL"]] %>% head())
    output$group_cols <- renderText({ str_flatten(group_cols, "\n") })
    
  })
}