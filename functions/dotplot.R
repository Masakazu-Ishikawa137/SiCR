dotplot <- function(myReactives, input, output, session, valid_genes){
    DotPlot(myReactives$seurat_object, 
            features = valid_genes,
            group.by = input$group_by
    )
}
