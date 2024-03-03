uploadUI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3("Upload cellranger output files"),
      p('Upload these files and press "Run".'),
      h4("1. count file"),
      p("This is the file for clustering."),
      p(".../outs/count/filtered_feature_bc_matrix.h5"),
      h4("2. TCR file (if exists)"),
      p("This is the file for TCR. If you analyzed TCR, upload the csv file"),
      p(".../outs/vdj_t/filtered_contig_annotations.csv"),
      h4("3. BCR file (if exists)"),
      p("This is the file for BCR. If you analyzed BCR, upload the csv file"),
      p(".../outs/vdj_b/filtered_contig_annotations.csv")
    ),
    mainPanel(
      fluidRow(
        column(6,
          fileInput(ns("h5"),  "1. Choose .h5  file"),
          fileInput(ns("tcr"), "2. Choose .tcr file (optional)"),
          fileInput(ns("bcr"), "3. Choose .bcr file (optional)"),
          actionButton(ns("run"), "Run"),
#          actionButton(ns("prepare_download"), "ファイルを準備する"),
#          modalDialog(
#            id = ns("modal_download_ready"),
#            title = "ダウンロード準備完了",
#            "ファイルの準備が完了しました。以下のリンクからダウンロードしてください。",
#            downloadButton(ns("downloadrds"), "Download analyzed file"),
#            easyClose = TRUE,
#            size = "m"
#          ),
#          downloadButton(ns("downloadrds"), "Download analyzed file"),
          textOutput(ns('uploadh5'))
        ),
        column(6,
        ),
      ),
    )
  )
}

uploadServer <- function(id, myReactives){
  moduleServer(id, function(input, output, session){
    observeEvent(input$run,{
      h5_path  <- input$h5$datapath
      tcr_path <- input$tcr$datapath
      bcr_path <- input$bcr$datapath
      h5_path <- "/user/ifrec/mishikawa/SiCR/example/230405_ruft_hcw_vaccine_merge_3000.h5"
      tcr_path <- "/user/ifrec/mishikawa/SiCR/example/230405_ruft_hcw_vaccine_merge_3000_t.csv"
      bcr_path <- "/user/ifrec/mishikawa/SiCR/example/230405_ruft_hcw_vaccine_merge_3000_b.csv"

      if(is.null(h5_path)){
#        print('Uploading RDS')
#        myReactives <- readRDS('/user/ifrec/mishikawa/SiCR/myReactive.rds')
#        print('Uploaded RDS')
         output$upload5h <- renderText('Please upload .h5 file')
      }
      else {
        myReactives$seurat_object <- h5_to_seurat_object(h5_path)
        if (!is.null(tcr_path) & !is.null(myReactives$seurat_object)){
          tcr_table <- csv_to_tcr_dataframe(tcr_path)
          metadata_df <- make_metadata_df(tcr_path)
          myReactives$seurat_object@misc$meta_data <- metadata_df
          myReactives$seurat_object@meta.data <- dplyr::left_join(myReactives$seurat_object@meta.data, tcr_table, by=c('barcode' = 'TCR_pair_barcode'))
          if(is.null(bcr_path)){
            myReactives$seurat_object@meta.data <- dplyr::left_join(myReactives$seurat_object@meta.data, metadata_df, by='sample')
          }

        }
        if (!is.null(bcr_path) & !is.null(myReactives$seurat_object)){
          bcr_table <- csv_to_bcr_dataframe(bcr_path)
          metadata_df <- make_metadata_df(bcr_path)
          myReactives$seurat_object@meta.data <- dplyr::left_join(myReactives$seurat_object@meta.data, bcr_table, by=c('barcode' = 'BCR_pair_barcode'))
          myReactives$seurat_object@meta.data <- dplyr::left_join(myReactives$seurat_object@meta.data, metadata_df, by='sample')
        }
      rownames(myReactives$seurat_object@meta.data) <- myReactives$seurat_object@meta.data$'barcode'
      write.csv(myReactives$seurat_object@meta.data, 'metadata.csv')
      }
    })
#    observeEvent(input$prepare_download, {
      # モーダルダイアログを表示する
#      showModal(modalDialog(
#        title = "処理中",
#        "ボタンが押されました。ファイルの準備中です。しばらくお待ちください。",
#        footer = NULL
#      ))
      # ここでファイルの準備処理を行う
      # 例: データのロードや加工など
      # この処理が終わったら、ダウンロード準備完了のモーダルを表示
#      Sys.sleep(5) # 仮の処理時間
      # ダウンロード準備完了のモーダルを表示
#      removeModal()
#      showModal(modalDialog(
#        title = "ダウンロード準備完了",
#        "ファイルの準備が完了しました。以下のリンクからダウンロードしてください。",
#        downloadButton(ns("downloadrds"), "Download analyzed file"),
#        easyClose = TRUE,
#        size = "m"
#      ))
    })
#    output$downloadrds <- downloadHandler(
#    filename = function() {
#      paste(format(Sys.time(), format = "%Y-%m-%d-%H-%M-%S"), ".rds", sep = "")
#    },
#    content = function(file) {
#      saveRDS(myReactives, file)
#    }
#  )
#  })
}

# uploadCellranger_mainServer <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     observeEvent(input$run,{
#       h5_path  <- input$h5$datapath
#       h5 <- Read10X_h5(h5_path)

#       if (!is.null(names(h5)) & ("Gene Expression" %in% names(h5))) {
#         # names(h5) are NULL OR c('Gene Expression','Antibody Capture')
#         h5 <- h5[["Gene Expression"]]
#       }





#     })
# #    observeEvent(dataList(), {
# #      showNotification("Process completed!")
# #      output$text <- renderText({ "The files were processed successfully." })
# #      output$cellnumber_table <- renderTable(
# #        dataList()[["seurat_object"]]@meta.data %>%
# #          group_by(sample) %>%
# #          summarise("number of cell"=n())
# #      )
      
#       # output$download_rds <- downloadHandler(
#       #   filename = function() {"SCiR_data.rds"},
#       #   content = function(file) {
#       #     withProgress(message = "Your download is being prepared. Please wait.", detail = "", value = 1/2, {
#       #       saveRDS(dataList(), file)
#       #     })
#       #   }
#       # )
#       # 
#       # # To show download RData button
#       # output$status <- renderText({ "end" })
#       # outputOptions(output, "status", suspendWhenHidden = FALSE)
      
#     }) # close observeEvent
    
      
#    dataList <- eventReactive(input$run, {
      
#      h5_path  <- input$h5$datapath
#      tcr_path <- input$tcr$datapath
#      bcr_path <- input$bcr$datapath

      # h5
#       h5 <- Read10X_h5(h5_path)

#       if (!is.null(names(h5)) & ("Gene Expression" %in% names(h5))) {
#         # names(h5) are NULL OR c('Gene Expression','Antibody Capture')
#         h5 <- h5[["Gene Expression"]]
#       }
      
#       seurat_object <- CreateSeuratObject(h5)
#       seurat_object@meta.data <- seurat_object@meta.data %>%
#         mutate(barcode = rownames(.)) %>%
#           mutate(sample = str_remove(barcode, "^.+-"))

#       seurat_object <- NormalizeData(seurat_object)
      
#       seurat_object <- FindVariableFeatures(seurat_object, selection.method = "vst", nfeatures = 2000)
      
#       all.genes <- rownames(seurat_object)
#       seurat_object <- ScaleData(seurat_object, features = all.genes)
#       seurat_object <- RunPCA(seurat_object, features = VariableFeatures(object = seurat_object), npcs = 50)
#       seurat_object <- FindNeighbors(seurat_object, dims = 1:10)
#       seurat_object <- FindClusters(seurat_object, resolution = 0.5)
#       seurat_object <- RunUMAP(seurat_object, dims = 1:10)


# #       # h5
# #       withProgress(message = "Processing h5 (1/8):", detail = "Seurat - making seurat object", value = 1/9, {
# #         h5 <- Read10X_h5(h5_path)
# #         if (!is.null(names(h5)) & ("Gene Expression" %in% names(h5))) { # names(h5) are NULL OR c('Gene Expression','Antibody Capture')
# #           h5 <- h5[["Gene Expression"]]
# #         }
# #         seurat_object <- CreateSeuratObject(h5)
# #         seurat_object@meta.data <- seurat_object@meta.data %>%
# #           mutate(barcode = rownames(.)) %>%
# #           mutate(sample = str_remove(barcode, "^.+-"))
# #         incProgress(1/9, message = "Processing h5 (2/8):", detail = "Seurat - Normalizing data")
# #         seurat_object <- NormalizeData(seurat_object)
# #         incProgress(1/9, message = "Processing h5 (3/8):", detail = "Seurat - Selecting features")
# #         seurat_object <- FindVariableFeatures(seurat_object, selection.method = "vst", nfeatures = 2000)
# #         incProgress(1/9, message = "Processing h5 (4/8):", detail = "Seurat - Scaling")
# #         all.genes <- rownames(seurat_object)
# #         seurat_object <- ScaleData(seurat_object, features = all.genes)
# #         incProgress(1/9, message = "Processing h5 (5/8):", detail = "Seurat - Running PCA")
# #         seurat_object <- RunPCA(seurat_object, features = VariableFeatures(object = seurat_object), npcs = 50)
# #         incProgress(1/9, message = "Processing h5 (6/8):", detail = "Seurat - Culstering the cells")
# #         seurat_object <- FindNeighbors(seurat_object, dims = 1:10)
# #         seurat_object <- FindClusters(seurat_object, resolution = 0.5)
# #         incProgress(1/9, message = "Processing h5 (7/8):", detail = "Seurat - Running UMAP")
# #         seurat_object <- RunUMAP(seurat_object, dims = 1:10)
# #         incProgress(1/9, message = "Processing h5 (8/8):", detail = "ScType - Adding celltype")
# # #        seurat_object <- add_celltype(seurat_object)
# #         incProgress(1/9)
# #       }) # close withProgress
      
#       # tcr
#       # tcr_list: Extract rows of chain=='TRA/TRB' => remove duplicated barcode rows => join celltype in the seurat_object meta.data
#       user_metadata <- NULL
#       tcr_list <- NULL
#       if (!is.null(tcr_path)) {
#         withProgress(message = "Processing tcr:", value = 1/4, {
#           tcr <- read_csv(tcr_path, show_col_types = FALSE) %>%
#             mutate(sample = str_remove(barcode, "^.+-"))
#           colnames(tcr) <- str_replace(colnames(tcr), ":", ".")
#           user_metadata <- tcr %>%
#             select(-any_of(tbcr_default_colnames())) %>%
#             distinct(sample, .keep_all = TRUE)
#           tcr_list <- modify_tbcr(tcr, seurat_object@meta.data)
#           incProgress(3/4)
#         }) # close withProgress
#       }
      
#       # bcr
#       bcr_list <- NULL
#       if (!is.null(bcr_path)) {
#         withProgress(message = "Processing bcr:", value = 1/4, {
#           bcr <- read_csv(bcr_path, show_col_types = FALSE) %>%
#             mutate(sample = str_remove(barcode, "^.+-"))
#           colnames(bcr) <- str_replace(colnames(bcr), ":", ".")
#           user_metadata <- bcr %>%
#             select(-any_of(tbcr_default_colnames())) %>%
#             distinct(sample, .keep_all = TRUE)
#           bcr_list <- modify_tbcr(bcr, seurat_object@meta.data)
#           incProgress(3/4)
#         }) # close withProgress
#       }
      
#       # get colnames for group, and join user_metadata in TCR/BCR (by sample) to seurat_object@meta.data
#       group_cols <- c("sample", "celltype")
#       if (!is.null(user_metadata)) {
#         group_cols <- union(group_cols, colnames(user_metadata))
#         seurat_object@meta.data <- seurat_object@meta.data %>%
#           left_join(user_metadata, by="sample")
#         rownames(seurat_object@meta.data) <- seurat_object@meta.data$barcode
#       }
      
#       # return -> dataList
#       return(list(
#         seurat_object = seurat_object,
#         tcr_list = tcr_list,
#         bcr_list = bcr_list,
#         group_cols = group_cols
#       ))
    
#     }) # close eventReactive
    
#     return(dataList)
      
#   }) # close moduleServer
# }
