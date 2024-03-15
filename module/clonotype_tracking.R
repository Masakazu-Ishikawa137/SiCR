clonotype_trackingUI <- function(id){
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
            actionButton(ns('clear_button'), 'Clear Selection'),
            checkboxGroupInput(ns('select_factor'), 'Select factor', choices = NULL),

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
                dplyr::filter(TCR_TRB_v_gene %in% input$select_factor)
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