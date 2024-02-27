louperUI <- function(id){
  ns <- NS(id)
  fluidPage(
    actionButton(ns("run"), 'Download Loupe File')
  )
}

louperServer <- function(id, myReactives){
    moduleServer(id, function(input, output, session){
        observeEvent(input$run, {
            if (!is.null(myReactives$seurat_object)) {
                seurat_object <- myReactives$seurat_object
                seurat_object@meta.data <- myReactives$seurat_object@meta.data %>%
                    select('orig.ident','nCount_RNA','nFeature_RNA','percent.mt','RNA_snn_res.0.5','seurat_clusters')
                loupeR::create_loupe_from_seurat(seurat_object, output_name = 'loupe', force = TRUE)
            }
        })
    })
}