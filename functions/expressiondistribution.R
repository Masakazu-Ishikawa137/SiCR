violinplot <- function(myReactives, input, output, session, valid_genes){
    if(input$split_by == "none"){
        VlnPlot(myReactives$seurat_object, 
            features = valid_genes,
            group.by = input$group_by,
            split.by = NULL,
            ncol = input$feature_column,
            pt.size = input$point_size,
            stack = input$stack,
            flip = input$flip)
    } else{
        VlnPlot(myReactives$seurat_object, 
            features = valid_genes,
            group.by = input$group_by,
            split.by = input$split_by,
            ncol = input$feature_column,
            pt.size = input$point_size,
            stack = input$stack,
            flip = input$flip)
    }
}

dotplot <- function(myReactives, input, output, session, valid_genes){
    DotPlot(myReactives$seurat_object, 
            features = valid_genes,
            group.by = input$group_by
    )
}

heatmap <- function(myReactives, input, output, session, valid_genes){
    DoHeatmap(myReactives$seurat_object, 
        features = valid_genes,
        group.by = input$group_by)
}