

quality_control_plot <- function(myReactives, input){
    myReactives$quality_sca <- myquality_control_scatterplot(myReactives$seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt, input$point_size)
    myReactives$quality_vln <- myquality_control_violinplot(myReactives$seurat_object, input$slider_nFeatures_RNA[1], input$slider_nFeatures_RNA[2], input$slider_nCount_RNA[1], input$slider_nCount_RNA[2], input$slider_percent_mt)
    ggarrange(myReactives$quality_sca, myReactives$quality_vln, ncol = 1)
}
