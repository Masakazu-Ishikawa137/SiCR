RelativeAbundanceUI <- function(id, chain = "TCR") {
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
            chain_selectionUI(ns, chain),
            clonotype_selectionUI(ns),
            group_byUI(ns),
            focus_groupUI(ns),
            abundance_methodUI(ns),
            uiOutput(ns("index")),
            legend_placeUI(ns),
            plot_size_and_download_pdfUI(ns),
            downloadButton(ns("downloadTable"), "Download table as CSV"),
        ),
        mainPanel(
            DTOutput(ns("table")),
            plotOutput(ns("plot")),
        )
    )
}


RelativeAbundanceServer <- function(id, myReactives, chain = "TCR") {
    moduleServer(id, function(input, output, session) {
        gene_column <- reactive({
            paste0(chain, "_", input$chain, "_", input$clonotype)
        })

        output$index <- renderUI({
            if (input$abundance_method == "Group") {
                relative_abundance_group(session$ns)
            } else if (input$abundance_method == "Indices") {
                relative_abundance_indices(session$ns)
            } else if (input$abundance_method == "Count") {
                relative_abundance_count(session$ns)
            }
        })

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

        df <- reactive({
            req(myReactives$seurat_object)
            df <- myReactives$seurat_object@meta.data
            df <- df %>%
                dplyr::filter(.data[[input$group_by]] %in% input$focus_group) %>%
                select(input$group_by, !!gene_column()) %>%
                na.omit()

            df <- df %>%
                group_by(.data[[input$group_by]], .data[[!!gene_column()]]) %>%
                summarize(count = n()) %>%
                mutate(proportion = count / sum(count))

            df <- df %>% ungroup()

            df <- df %>%
                group_by(.data[[input$group_by]]) %>%
                mutate(rank = rank(-count, ties.method = "min"))

            if (input$abundance_method == "Group") {
                df <- df %>%
                    mutate(Relative_abundance = case_when(
                        proportion <= input$relative_abundance_group_rare ~ "Rare",
                        proportion > input$relative_abundance_group_rare & proportion <= input$relative_abundance_group_small ~ "Small",
                        proportion > input$relative_abundance_group_small & proportion <= input$relative_abundance_group_medium ~ "Medium",
                        proportion > input$relative_abundance_group_medium & proportion <= input$relative_abundance_group_large ~ "Large",
                        proportion > input$relative_abundance_group_large & proportion <= input$relative_abundance_group_hyper ~ "Hyper expanded"
                    ))
            } else if (input$abundance_method == "Count") {
                df <- df %>%
                    mutate(Relative_abundance = case_when(
                        count <= input$relative_abundance_count_rare ~ "Rare",
                        count > input$relative_abundance_count_rare & count <= input$relative_abundance_count_small ~ "Small",
                        count > input$relative_abundance_count_small & count <= input$relative_abundance_count_medium ~ "Medium",
                        count > input$relative_abundance_count_medium & count <= input$relative_abundance_count_large ~ "Large",
                        count > input$relative_abundance_count_large & count <= input$relative_abundance_count_hyper ~ "Hyper expanded"
                    ))
            } else if (input$abundance_method == "Indices") {
                df <- df %>%
                    mutate(Relative_abundance = case_when(
                        rank <= input$relative_abundance_indices_hyper ~ "Hyper expanded",
                        rank > input$relative_abundance_indices_hyper & rank <= input$relative_abundance_indices_large ~ "Large",
                        rank > input$relative_abundance_indices_large & rank <= input$relative_abundance_indices_medium ~ "Medium",
                        rank > input$relative_abundance_indices_medium & rank <= input$relative_abundance_indices_small ~ "Small",
                        rank > input$relative_abundance_indices_small & rank <= input$relative_abundance_indices_rare ~ "Rare"
                    ))
            }
            return(df)
        })

        plot <- reactive({
            levels_order <- c("Hyper expanded", "Large", "Medium", "Small", "Rare")
            df3 <- df()

            # 順序を指定したfactorとして変換します
            df3$Relative_abundance <- factor(df3$Relative_abundance, levels = levels_order)
            df3 %>% ggplot(aes(x = !!sym(input$group_by), fill = Relative_abundance)) +
                geom_bar(stat = "count", position = "fill") +
                theme_classic() +
                scale_y_continuous(expand = c(0, 0))
        })

        render_plot(
            output,
            "plot",
            reactive({
                plot()
                #                myReactives$relative_abundance_plot
            }),
            reactive({
                input$plot_width
            }),
            reactive({
                input$plot_height
            })
        )



        output$table <- renderDT({
            df()
        })



        output$downloadTable <- downloadHandler(
            filename = function() {
                paste(Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
                write.csv(df(), file, row.names = FALSE)
            }
        )
    })
}



# plot <- reactive({
#     levels_order <- c("Hyper expanded", "Large", "Medium", "Small", "Rare")
