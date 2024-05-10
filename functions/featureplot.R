featureplot <- function(myReactives, input, output, session){
    available_genes <- get_available_genes(reactive(myReactives$seurat_object))
    if (!is.null(input$gene)) {
        text_list <- unlist(strsplit(input$gene, ",\\s*"))
        # Filter only available genes
        valid_genes <- text_list[text_list %in% available_genes()]
              
        if (length(valid_genes) > 0) {
            # Compute DotPlot if valid genes exist
            FeaturePlot(
                myReactives$seurat_object, 
                features = valid_genes, 
                reduction = input$reduction,
                pt.size = input$point_size
            ) + theme(legend.position = input$legend)
        } else {
            # Notify the user if no valid genes exist
            showNotification("The specified genes do not exist in the dataset.", type = "warning")
        }
    }
}