clonotype_overlapUI <- function(id, chain = 'TCR') {
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
            chain_selectionUI(ns, chain),
            clonotype_selectionUI(ns),
            group_byUI(ns),
            focus_groupUI(ns),
            clonotype_overlap_label(ns),
            legend_placeUI(ns),
            methodUI(ns),
            plot_size_and_download_pdfUI(ns),
            downloadButton(ns("downloadTable"), "Download table as CSV"),

        ),
        mainPanel(
            DTOutput(ns("table")),
            plotOutput(ns("plot")),
        )
    )
}


clonotype_overlapServer <- function(id, myReactives, tcr_col = 'TCR_pair_CTaa', chain = 'TCR') {
#clonotype_overlapServer <- function(id, myReactives, chain = 'TCR') {
    moduleServer(id, function(input, output, session) {

            # update group by
        # update group_by
        observeEvent(myReactives$seurat_object, {
            req(myReactives$seurat_object)
            update_group_by(session, myReactives)
        })

        toListen <- reactive({
            list(myReactives$seurat_object, input$group_by)
        })

        observeEvent(toListen(), {
            req(myReactives$seurat_object)
            myReactives$new_choices <- sort(unique(myReactives$seurat_object@meta.data[[input$group_by]]))
            updateCheckboxGroupInput(session, "focus_group", choices = myReactives$new_choices, selected = myReactives$new_choices)
        })

        gene_column <- reactive({
            paste0(chain, "_", input$chain, "_", input$clonotype)
        })


        input_lists <- reactive({
            list(input$chain, input$clonotype, input$group_by, input$focus_group, input$method, input$clonotype_overlap_label)
        })

        observeEvent(input_lists(), {
            req(myReactives$seurat_object)

            gene_column <- paste0(chain, "_", input$chain, "_", input$clonotype)

            df <- myReactives$seurat_object@meta.data %>%
                dplyr::filter(.data[[input$group_by]] %in% input$focus_group) %>%
                select(input$group_by, gene_column) %>%
                na.omit()

            myReactives$clonotype_overlap_df <- calculate_clonotype_overlap2(df, sample_col = input$group_by, tcr_col = gene_column, method = input$method)

            dt2 <- myReactives$clonotype_overlap_df %>%
                rownames_to_column() %>%
                gather(colname, value, -rowname)
            myReactives$clonotype_overlap_plot <- ggplot(dt2, aes(x = rowname, y = colname, fill = value)) +
                geom_tile() +
                scale_fill_gradient(low = "white", high = "blue")

            if (input$clonotype_overlap_label == TRUE) {
            myReactives$clonotype_overlap_plot <- myReactives$clonotype_overlap_plot + geom_text(aes(label = value))
            }
        })

        output$table <- renderDT({
            myReactives$clonotype_overlap_df
        })

        render_plot(
            output,
            "plot",
            reactive({
                myReactives$clonotype_overlap_plot
            }),
            reactive({
                input$plot_width
            }),
            reactive({
                input$plot_height
            })
        )


        output$downloadTable <- downloadHandler(
            filename = function() {
                paste(Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
                write.csv(myReactives$clonotype_overlap_df, file, row.names = FALSE)
            }
        )

        setupDownloadPlotHandler(output, input, reactive({
           myReactives$clonotype_overlap_plot
        }))


    })
}
