# clonotype_trackingUI <- function(id){
#     ns <- NS(id)
#     sidebarLayout(
#         sidebarPanel(
# #             uiOutput(ns("dynamic_rank_list")),
# #            rank_list("Drag column names to change order", c("1", "2", "3", "4"), "sortable"),
#             actionButton(ns('clear_button'), 'Clear Selection'),
#             checkboxGroupInput(ns('select_factor'), 'Select factor', choices = NULL),
#             checkboxGroupInput(ns('focus_group'), 'Focus group', choices = NULL, selected = character(0)),
#         ),
#         mainPanel(
#             plotOutput(ns('plot'))
#         )
#     )
# }

# clonotype_trackingServer <- function(id, myReactives){
#     moduleServer(id, function(input, output, session){

#         # data <- reactive({
#         #     read.csv('/user/ifrec/mishikawa/SiCR/tracking_df.csv') # 実際のデータパスに置き換えてください
#         # })

#         # samples <- reactive({ unique(data()[['sample']])}) # 実際にはデータに基づいてこの部分を更新
        
#         # output$dynamic_rank_list <- renderUI({
#         #     rank_list(
#         #         text = "サンプルをドラッグして順序を変更：",
#         #         input_id = "ranked_samples",
#         #         labels = samples() # サーバー側で定義したリアクティブなsamplesを使用
#         #     )
#         # })

#         # output$table <- renderPlot({
#         #     data() %>%
#         #     mutate(sample = factor(sample, levels = input$ranked_samples)) %>%
#         #     arrange(match(sample, input$ranked_samples)) %>% # ここで実際に行を並べ替える
#         #     ggplot() + geom_area(aes(x = sample, y = proportion, fill = TCR_TRB_v_gene, group = TCR_TRB_v_gene), position = 'stack')
#         # })


#         # create tracking_df
#         observeEvent(myReactives$seurat_object,{
#             myReactives$tracking_df <- create_tracking_df(myReactives$seurat_object)
#             write.csv(myReactives$tracking_df, 'tracking_df.csv')
#         })

# # #    toListen <- reactive({ list(myReactives$seurat_object, input$group_by) })
# #     observeEvent(myReactives$seurat_object, {
# #   # 新しいgroup_byに基づいてfocus_groupの選択肢を更新
# #     if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@meta.data)) {
# #         myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[['sample']]))
# #         updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = character(0))
# #     }
# # }, ignoreNULL = TRUE, ignoreInit = TRUE)



#         # observeEvent(myReactives$tracking_df,{
#         #     req(myReactives$tracking_df)
#         #     updateCheckboxGroupInput(session, 'select_factor', 'Select factor', choices = sort(unique(myReactives$tracking_df$TCR_TRB_v_gene)), selected = sort(unique(myReactives$tracking_df$TCR_TRB_v_gene)))
#         # })
#         observe({
#             updateCheckboxGroupInput(session, 'select_factor', 'Select factor', choices = sort(unique(data()$TCR_TRB_v_gene)), selected = sort(unique(data()$TCR_TRB_v_gene)))
#         })

#         # Event(myReactives$tracking_df,{
#         #     req(myReactives$tracking_df)
#         #     updateCheckboxGroupInput(session, 'select_factor', 'Select factor', choices = sort(unique(myReactives$tracking_df$TCR_TRB_v_gene)), selected = sort(unique(myReactives$tracking_df$TCR_TRB_v_gene)))
#         # })

#         # observeEvent(input$clear_button, {
#         #     updateCheckboxGroupInput(session, 'select_factor', choices = sort(unique(myReactives$tracking_df$TCR_TRB_v_gene)))
#         # })

#         observeEvent(input$clear_button, {
#             updateCheckboxGroupInput(session, 'select_factor', 'Select factor', choices = sort(unique(data()$TCR_TRB_v_gene)))
#         })


#         filtered_data <- reactive({
#             req(data())
#             data() %>% 
#                 dplyr::filter(TCR_TRB_v_gene %in% input$select_factor) %>%
#                 arrange(match(sample, input$sortable)) 
# #                mutate(sample_ordered = factor(sample, levels = input$ranked_samples)) %>%
# #                dplyr::arrange(sample_ordered)
# #                arrange(match(sample, input$ranked_samples))
#         })

#     #     tracking_df_filter <- reactive({
#     #         req(myReactives$tracking_df)
            
#     #         data <- myReactives$tracking_df %>%
#     #             mutate(sample = factor(sample, levels = input$sortable))
#     #         print(data)
#     # #        myReactives$tracking_df #%>% 
#     # #            dplyr::filter(TCR_TRB_v_gene %in% input$select_factor) %>% 
#     # #            dplyr::filter(sample %in% input$focus_group)            
#     # #        myReactives$tracking_df %>% 
#     # #            dplyr::filter(TCR_TRB_v_gene %in% input$select_factor) %>% 
#     # #            dplyr::filter(sample %in% input$focus_group)            
#     #    })

#         output$sortable <- renderUI({
# #            rank_list("Drag column names to change order", myReactives$tracking_df$'sample', session$ns('sortable'), options = sortable_options(swap = TRUE))
#             rank_list("Drag column names to change order", c('1', '2'), session$ns('sortable'))
#         })
#        output$plot <- renderPlot({
#            filtered_data() %>% 
#                ggplot() + geom_area(aes(x = sample, y = proportion, fill = TCR_TRB_v_gene, group = TCR_TRB_v_gene), position = 'stack')
#        })
#     #    output$plot <- renderPlot({
#     #        req(tracking_df_filter())
#     #        tracking_df_filter() %>% 
#     #            ggplot() + geom_area(aes(x = sample, y = proportion, fill = TCR_TRB_v_gene, group = TCR_TRB_v_gene), position = 'stack')
#     #    })

#         # output$plot <- renderPlot({
#         #     myReactives$tracking_df %>% 
#         #         dplyr::filter(TCR_TRB_v_gene %in% c('TRBV11-2', "TRBV13")) %>% 
#         #             ggplot() + geom_area(aes(x = sample, y = proportion, fill = TCR_TRB_v_gene, group = TCR_TRB_v_gene), position = 'stack')
#         # })
#     })
# }




clonotype_trackingUI <- function(id){
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
#             uiOutput(ns("dynamic_rank_list")),
#            rank_list("Drag column names to change order", c("1", "2", "3", "4"), "sortable"),
            actionButton(ns('clear_button'), 'Clear Selection'),
            checkboxGroupInput(ns('select_factor'), 'Select factor', choices = NULL),
            checkboxGroupInput(ns('focus_group'), 'Focus group', choices = NULL, selected = character(0)),
        ),
        mainPanel(
            plotOutput(ns('plot'))
        )
    )
}

clonotype_trackingServer <- function(id, myReactives){
    moduleServer(id, function(input, output, session){

        # create tracking_df
        observeEvent(myReactives$seurat_object,{
            myReactives$tracking_df <- create_tracking_df(myReactives$seurat_object)
        })

        observeEvent(myReactives$tracking_df, {
            updateCheckboxGroupInput(session, 'select_factor', 'Select factor', choices = sort(unique(myReactives$tracking_df$TCR_TRB_v_gene)), selected = sort(unique(myReactives$tracking_df$TCR_TRB_v_gene)))
        })

        observeEvent(input$clear_button, {
            updateCheckboxGroupInput(session, 'select_factor', 'Select factor', choices = sort(unique(myReactives$tracking_df$TCR_TRB_v_gene)))
        })


        filtered_data <- reactive({
            req(myReactives$tracking_df)
            myReactives$tracking_df %>% 
                dplyr::filter(TCR_TRB_v_gene %in% input$select_factor)        
        })
       output$plot <- renderPlot({
           filtered_data() %>% 
               ggplot() + geom_area(aes(x = sample, y = proportion, fill = TCR_TRB_v_gene, group = TCR_TRB_v_gene), position = 'stack')
       })
    })
}