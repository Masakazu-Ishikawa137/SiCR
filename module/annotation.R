# Define the UI for the subsetting module
annotationUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
            p("If you want to annotate cells, please download loupe file, and subset using loupe Browser, and upload csv, then press Run"),
            actionButton(ns("run"), 'Download Loupe File'),
            textOutput(ns("loupe_message")),  # Add this line to display the loupe message,
            fileInput(ns("upload_barcode"), "Please upload cell barcode file to subset cells"),
            actionButton(ns("run_subset"), 'Start Subsetting')
        )
    )
}

annotationServer <- function(id, myReactives) {
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
        csv <- read.table(input$upload_barcode$datapath, header = TRUE, fill = TRUE, na.strings = "")
        if (!is.null(myReactives$seurat_object)) {
            metadata <- myReactives$seurat_object@meta.data
            tibble::rownames_to_column(metadata, "Barcode")
            metadata <- dplyr::left_join(metadata, csv, by = "Barcode")
            myReactives$seurat_object@meta.data <- metadata
        }
    })
  })
}
