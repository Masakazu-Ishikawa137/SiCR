# dimplot <- function(myReactives, input, output, session){
#     if (input$split_by == "none") {
#         DimPlot(
#             myReactives$seurat_object,
#             reduction = input$reduction,
#             label = TRUE,
#             pt.size = input$point_size,
#             group.by = input$group_by,
#             label.size = input$label_size
#         ) + theme(legend.position = input$legend) # nolint
#     } else {
#         DimPlot(
#             myReactives$seurat_object,
#             reduction = input$reduction,
#             label = TRUE,
#             pt.size = input$point_size,
#             group.by = input$group_by,
#             split.by = input$split_by,
#             label.size = input$label_size,
#             ncol = input$feature_column
#         ) + theme(legend.position = input$legend) # nolint
#     }
# }

dimplot <- function(myReactives, input, output, session) {
    DimPlot(
        myReactives$seurat_object,
        reduction = input$reduction,
        label = TRUE,
        pt.size = input$point_size,
        group.by = input$group_by,
        label.size = input$label_size
    ) + theme(legend.position = input$legend) # nolint
}
