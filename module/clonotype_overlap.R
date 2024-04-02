clonotype_overlapUI <- function(id) {
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
            group_byUI(ns),
            methodUI(ns),
        ),
        mainPanel(
            tableOutput(ns("table")),
            plotOutput(ns("plot"))
        )
    )
}


clonotype_overlapServer <- function(id, myReactives, tcr_col = 'TCR_pair_CTaa') {
    moduleServer(id, function(input, output, session) {
        observeEvent(myReactives$seurat_object, {
            if (!is.null(myReactives$seurat_object) && !is.null(myReactives$seurat_object@misc$meta_data)) {
                group_cols <- group_col(myReactives)
                updateRadioButtons(session, "group_by", choices = group_cols)
            }
        })

        table <- reactive({
            req(myReactives$seurat_object)
            calculate_clonotype_overlap(myReactives, sample_col = input$group_by, tcr_col = tcr_col, method = input$method)
        })

        output$table <- renderTable({
            table()
        })

        output$plot <- renderPlot({
            dt2 <- table() %>%
                rownames_to_column() %>%
                gather(colname, value, -rowname)
            ggplot(dt2, aes(x = rowname, y = colname, fill = value)) +
                geom_tile() +
                geom_text(aes(label = value))
        })
    })
}
