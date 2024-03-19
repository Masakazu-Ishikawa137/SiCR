clonotype_trackingUI <- function(id){
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
            actionButton(ns('clear_button'), 'Clear Selection'),
            checkboxGroupInput(ns('select_factor'), 'Select factor', choices = NULL),
            checkboxGroupInput(ns('focus_group'), 'Focus group', choices = NULL, selected = character(0)),

        ),
        mainPanel(
            plotOutput(ns('plot')),
            tableOutput(ns('table'))
        )
    )
}

clonotype_trackingServer <- function(id, myReactives){
    moduleServer(id, function(input, output, session){
        observeEvent(myReactives$seurat_object,{
            myReactives$tracking_df <- create_tracking_df(myReactives$seurat_object)
        })

#    toListen <- reactive({ list(myReactives$seurat_object, input$group_by) })
    observeEvent(myReactives$seurat_object, {
  # 新しいgroup_byに基づいてfocus_groupの選択肢を更新
    if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@meta.data)) {
        myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[['sample']]))
        updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = character(0))
    }
}, ignoreNULL = TRUE, ignoreInit = TRUE)



        observeEvent(myReactives$tracking_df,{
            req(myReactives$tracking_df)
            updateCheckboxGroupInput(session, 'select_factor', 'Select factor', choices = sort(unique(myReactives$tracking_df$TCR_TRB_v_gene)), selected = sort(unique(myReactives$tracking_df$TCR_TRB_v_gene)))
        })

        observeEvent(input$clear_button, {
            updateCheckboxGroupInput(session, 'select_factor', choices = sort(unique(myReactives$tracking_df$TCR_TRB_v_gene)))
        })

        tracking_df_filter <- reactive({
            req(myReactives$tracking_df)
            myReactives$tracking_df %>% 
                dplyr::filter(TCR_TRB_v_gene %in% input$select_factor) %>% 
                dplyr::filter(sample %in% input$focus_group)            
        })

        output$plot <- renderPlot({
            req(tracking_df_filter())
            tracking_df_filter() %>% 
                ggplot() + geom_area(aes(x = sample, y = proportion, fill = TCR_TRB_v_gene, group = TCR_TRB_v_gene), position = 'stack')
        })

        # output$plot <- renderPlot({
        #     myReactives$tracking_df %>% 
        #         dplyr::filter(TCR_TRB_v_gene %in% c('TRBV11-2', "TRBV13")) %>% 
        #             ggplot() + geom_area(aes(x = sample, y = proportion, fill = TCR_TRB_v_gene, group = TCR_TRB_v_gene), position = 'stack')
        # })
    })
}