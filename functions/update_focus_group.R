update_focus_group <- function(myReactives, input, output, session){
    print('update_focus_group')
    myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
    updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = character(0))
}