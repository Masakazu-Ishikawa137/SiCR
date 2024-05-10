draw_highlight <- function(myReactives, input, output, session) {
    valid_focus_group_selection <- intersect(input$focus_group, myReactives$new_choices)
    if (length(valid_focus_group_selection) == length(input$focus_group)) {
        Idents(myReactives$seurat_object) <- input$group_by
        subject <- WhichCells(myReactives$seurat_object, idents = input$focus_group)
        DimPlot(
            myReactives$seurat_object,
            reduction = input$reduction,
            cells.highlight = subject,
            cols.highlight = "darkblue",
            sizes.highlight = input$highlight_size,
            cols = "grey",
            pt.size = input$point_size,
        ) + theme(legend.position = "none")
    }
}
