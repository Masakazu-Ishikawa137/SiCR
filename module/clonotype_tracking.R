clonotype_trackingUI <- function(id){
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
        ),
        mainPanel(
            plotOutput(ns('plot'))
        )
    )
}

clonotype_trackingServer <- function(id, myReactives){
    moduleServer(id, function(input, output, session){
        observeEvent(myReactives$seurat_object,{
                if(!is.null(myReactives$seurat_object)) {
                        myReactives$tracking_df <- myReactives$seurat_object@meta.data %>% 
                                filter(!is.na(TCR_TRB_v_gene)) %>% 
                                group_by(sample, TCR_TRB_v_gene) %>% 
                                summarise(count = n()) %>% 
                                ungroup() %>% 
                                group_by(sample) %>% 
                                mutate(total_count = sum(count)) %>% 
                                ungroup() %>% 
                                mutate(proportion = count / total_count)
                }
        })
        output$plot <- renderPlot({
                if(!is.null(myReactives$tracking_df)) {
                    ggplot(myReactives$tracking_df) +
                        geom_area(aes(x = sample, y = proportion, fill = TCR_TRB_v_gene, group = TCR_TRB_v_gene), position = 'stack')
                }
        })
    })
}