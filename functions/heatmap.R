heatmap <- function(myReactives, input, output, session, valid_genes){
    DoHeatmap(myReactives$seurat_object, 
        features = valid_genes,
        group.by = input$group_by)
}