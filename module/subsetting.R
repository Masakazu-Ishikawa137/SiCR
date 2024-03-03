# Define the UI for the subsetting module
subsettingUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
            p("If you want to subset cells, please download loupe file, and subset using loupe Browser, and upload cell barcode, then press Run"),
            actionButton(ns("run"), 'Download Loupe File'),
            textOutput(ns("loupe_message")),  # Add this line to display the loupe message,
            fileInput(ns("upload_barcode"), "Please upload cell barcode file to subset cells"),
            actionButton(ns("run_subset"), 'Start Subsetting')
        )
    )
}

subsettingServer <- function(id, myReactives) {
  moduleServer(id, function(input, output, session) {
     observeEvent(input$run, {
                        if (!is.null(myReactives$seurat_object)) {
                            seurat_object <- myReactives$seurat_object
                            seurat_object@meta.data <- myReactives$seurat_object@meta.data %>%
                                select('orig.ident','nCount_RNA','nFeature_RNA','percent.mt','RNA_snn_res.0.5','seurat_clusters')
                            loupeR::create_loupe_from_seurat(seurat_object, output_name = 'loupe', force = TRUE)
                            output$loupe_message <- renderText("loupe.cloupe is created at same location of app.R")  # Add this line to display the loupe message
                        }
                    })


    observeEvent(input$run_subset,{
        csv <- read.csv(input$upload_barcode$datapath, header = TRUE, stringsAsFactors = FALSE)
        barcode <- csv$Barcode
        if (!is.null(myReactives$seurat_object)) {
            seurat_object <- subset(myReactives$seurat_object, cells = barcode)
            myReactives$seurat_object <- myseurat_normalize_umap(seurat_object)
        }
    })
    })
}
