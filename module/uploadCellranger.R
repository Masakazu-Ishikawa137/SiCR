#  download .rds section was commented out.

uploadCellrangerUI <- function(id) {
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
          actionButton(ns("run"), "Run")
        ),
        column(6,
          textOutput(ns("text")),
          tableOutput(ns('cellnumber_table')),
          # # To show .rds download button
          # conditionalPanel(
          #   condition = 'output.status == "end"',
          #   ns = ns,
          #   downloadButton(ns("download_rds"), "Download processed data (it take time!)"),
          #   p("*Download data is optional. If you download R data, ..."),
          #   textOutput(ns("downloading_message"))
          # )
        )
      )
    )
  )
}



uploadCellrangerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # set shiny outputs, when the following process was done
    observeEvent(dataList(), {
      showNotification("Process completed!")
      output$text <- renderText({ "The files were processed successfully." })
      output$cellnumber_table <- renderTable(
        dataList()[["seurat_object"]]@meta.data %>%
          group_by(sample) %>%
          summarise("number of cell"=n())
      )
      
      # output$download_rds <- downloadHandler(
      #   filename = function() {"SCiR_data.rds"},
      #   content = function(file) {
      #     withProgress(message = "Your download is being prepared. Please wait.", detail = "", value = 1/2, {
      #       saveRDS(dataList(), file)
      #     })
      #   }
      # )
      # 
      # # To show download RData button
      # output$status <- renderText({ "end" })
      # outputOptions(output, "status", suspendWhenHidden = FALSE)
      
    }) # close observeEvent
    
      
    dataList <- eventReactive(input$run, {
      
      h5_path  <- input$h5$datapath
      tcr_path <- input$tcr$datapath
      bcr_path <- input$bcr$datapath
      
      # h5
      withProgress(message = "Processing h5 (1/8):", detail = "Seurat - making seurat object", value = 1/9, {
        h5 <- Read10X_h5(h5_path)
        if (!is.null(names(h5)) & ("Gene Expression" %in% names(h5))) { # names(h5) are NULL OR c('Gene Expression','Antibody Capture')
          h5 <- h5[["Gene Expression"]]
        }
        seurat_object <- CreateSeuratObject(h5)
        seurat_object@meta.data <- seurat_object@meta.data %>%
          mutate(barcode = rownames(.)) %>%
          mutate(sample = str_remove(barcode, "^.+-"))
        incProgress(1/9, message = "Processing h5 (2/8):", detail = "Seurat - Normalizing data")
        seurat_object <- NormalizeData(seurat_object)
        incProgress(1/9, message = "Processing h5 (3/8):", detail = "Seurat - Selecting features")
        seurat_object <- FindVariableFeatures(seurat_object, selection.method = "vst", nfeatures = 2000)
        incProgress(1/9, message = "Processing h5 (4/8):", detail = "Seurat - Scaling")
        all.genes <- rownames(seurat_object)
        seurat_object <- ScaleData(seurat_object, features = all.genes)
        incProgress(1/9, message = "Processing h5 (5/8):", detail = "Seurat - Running PCA")
        seurat_object <- RunPCA(seurat_object, features = VariableFeatures(object = seurat_object), npcs = 50)
        incProgress(1/9, message = "Processing h5 (6/8):", detail = "Seurat - Culstering the cells")
        seurat_object <- FindNeighbors(seurat_object, dims = 1:10)
        seurat_object <- FindClusters(seurat_object, resolution = 0.5)
        incProgress(1/9, message = "Processing h5 (7/8):", detail = "Seurat - Running UMAP")
        seurat_object <- RunUMAP(seurat_object, dims = 1:10)
        incProgress(1/9, message = "Processing h5 (8/8):", detail = "ScType - Adding celltype")
        seurat_object <- add_celltype(seurat_object)
        incProgress(1/9)
      }) # close withProgress
      
      # tcr
      # tcr_list: Extract rows of chain=='TRA/TRB' => remove duplicated barcode rows => join celltype in the seurat_object meta.data
      user_metadata <- NULL
      tcr_list <- NULL
      if (!is.null(tcr_path)) {
        withProgress(message = "Processing tcr:", value = 1/4, {
          tcr <- read_csv(tcr_path, show_col_types = FALSE) %>%
            mutate(sample = str_remove(barcode, "^.+-"))
          colnames(tcr) <- str_replace(colnames(tcr), ":", ".")
          user_metadata <- tcr %>%
            select(-any_of(tbcr_default_colnames())) %>%
            distinct(sample, .keep_all = TRUE)
          tcr_list <- modify_tbcr(tcr, seurat_object@meta.data)
          incProgress(3/4)
        }) # close withProgress
      }
      
      # bcr
      bcr_list <- NULL
      if (!is.null(bcr_path)) {
        withProgress(message = "Processing bcr:", value = 1/4, {
          bcr <- read_csv(bcr_path, show_col_types = FALSE) %>%
            mutate(sample = str_remove(barcode, "^.+-"))
          colnames(bcr) <- str_replace(colnames(bcr), ":", ".")
          user_metadata <- bcr %>%
            select(-any_of(tbcr_default_colnames())) %>%
            distinct(sample, .keep_all = TRUE)
          bcr_list <- modify_tbcr(bcr, seurat_object@meta.data)
          incProgress(3/4)
        }) # close withProgress
      }
      
      # get colnames for group, and join user_metadata in TCR/BCR (by sample) to seurat_object@meta.data
      group_cols <- c("sample", "celltype")
      if (!is.null(user_metadata)) {
        group_cols <- union(group_cols, colnames(user_metadata))
        seurat_object@meta.data <- seurat_object@meta.data %>%
          left_join(user_metadata, by="sample")
        rownames(seurat_object@meta.data) <- seurat_object@meta.data$barcode
      }
      
      # return -> dataList
      return(list(
        seurat_object = seurat_object,
        tcr_list = tcr_list,
        bcr_list = bcr_list,
        group_cols = group_cols
      ))
    
    }) # close eventReactive
    
    return(dataList)
      
  }) # close moduleServer
}