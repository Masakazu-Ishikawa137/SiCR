RelativeAbundanceUI <- function(id, chain = "TCR") {
    ns <- NS(id)
    sidebarLayout(
        sidebarPanel(
            chain_selectionUI(ns, chain),
            clonotype_selectionUI(ns),
            group_byUI(ns),
            focus_groupUI(ns),
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
            list(input$chain, input$clonotype, input$group_by, input$focus_group)
        })


        observeEvent(input_lists(), {
            req(myReactives$seurat_object)

            gene_column <- paste0(chain, "_", input$chain, "_", input$clonotype)

            df <- myReactives$seurat_object@meta.data %>%
                dplyr::filter(.data[[input$group_by]] %in% input$focus_group) %>%
                select(input$group_by, gene_column) %>%
                na.omit()

            table <- as.data.frame(t(table(df[[input$group_by]], df[[gene_column]]) %>% prop.table(., 1)))
            names(table) <- c(gene_column, input$group_by, "Freq")
            table <- table %>% mutate(Relative_abundance = case_when(
                Freq <= 1e-03 ~ "Small",
                Freq > 1e-03 & Freq <= 0.01 ~ "Medium",
                Freq > 0.01 & Freq <= 0.1 ~ "Large",
                Freq > 0.1 ~ "Hyperexpanded",
            ))
            # df[[input$group_by]] <- as.character(df[[input$group_by]])
            # df[[gene_column]] <- as.character(df[[gene_column]])
            # table[[input$group_by]] <- as.character(table[[input$group_by]])
            # table[[gene_column]] <- as.character(table[[gene_column]])
            myReactives$relative_abundance_df <- dplyr::left_join(df, table, by = c(input$group_by, gene_column))
            myReactives$relative_abundance_plot <- myReactives$relative_abundance_df %>% ggplot(aes(x = !!sym(input$group_by), fill = Relative_abundance)) +
                geom_bar(stat = "count", position = "fill") +
                theme_classic() +
                scale_y_continuous(expand = c(0, 0))
        })

        render_plot(
            output,
            "plot",
            reactive({
                myReactives$relative_abundance_plot
            }),
            reactive({
                input$plot_width
            }),
            reactive({
                input$plot_height
            })
        )

        output$table <- renderDT({
            myReactives$relative_abundance_df
        })

        output$downloadTable <- downloadHandler(
            filename = function() {
                paste(Sys.Date(), ".csv", sep = "")
            },
            content = function(file) {
                write.csv(myReactives$relative_abundance_df, file, row.names = FALSE)
            }
        )

        setupDownloadPlotHandler(output, input, reactive({
            myReactives$relative_abundance_plot
        }))
    })
}


# data <- read.csv('SiCR/metadata.csv')
# data <- data %>% select(sample, TCR_TRB_raw_clonotype_id) %>% na.omit()
# sample <- 'sample'
# clonotype <- 'TCR_TRB_raw_clonotype_id'
# table <- as.data.frame(t(table(data[[sample]], data[[clonotype]]) %>% prop.table(., 1)))
# names(table) <- c(clonotype, sample, 'Freq')
# table <- table %>% mutate(Relative_abundance = case_when(
#     Freq <= 1e-03 ~ 'Small',
#     Freq > 1e-03 & Freq <= 0.01 ~ 'Medium',
#     Freq > 0.01 & Freq <= 0.1 ~ 'Large',
#     Freq > 0.1 ~ 'Hyperexpanded',
# ))
# data[[sample]] <- as.character(data[[sample]])
# data[[clonotype]] <- as.character(data[[clonotype]])
# table[[sample]] <- as.character(table[[sample]])
# table[[clonotype]] <- as.character(table[[clonotype]])
# df <- dplyr::left_join(data, table, by = c(sample, clonotype))

# df %>% ggplot(aes(x = sample, fill = Relative_abundance)) +
# geom_bar(stat = "count", position = "fill") +
# theme_classic() +
# scale_y_continuous(expand = c(0, 0))
