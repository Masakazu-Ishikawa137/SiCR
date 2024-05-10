calculate_gene_usage <- function(myReactives, x, fill){
  str(x)
  metadata <- myReactives$seurat_object@meta.data %>%
    drop_na(any_of(c(x, fill))) %>% 
     group_by(!!sym(fill)) %>% 
     count(!!sym(x))
#     summarise(n_unique = dplyr::n_distinct(x))
#     count(x)
    #  drop_na(any_of(c(x, !!sym(fill)))) %>% 
    #  select(x, !!sym(fill)) %>%
    #  group_by(!!sym(fill)) %>%
    #  count(x)
  
  count_sum <- metadata %>% summarise(total = sum(n))
  metadata <- merge(metadata, count_sum, by = fill)
  metadata$proportion <- metadata$n / metadata$total
  metadata <- metadata %>% select(fill, x, proportion)
  
  return(metadata)
}

#        myReactives$gene_usage_df <- calculate_gene_usage_v_j(myReactives, chain, input$gene, input$group_by, input$focus_group)

calculate_gene_usage_v_j <- function(myReactives, chain, input_gene, fill, focus_group){
  v_gene_column <- paste0(chain, "_", input_gene, "_", "v_gene")
  j_gene_column <- paste0(chain, "_", input_gene, "_", "j_gene")

  metadata <- myReactives$seurat_object@meta.data %>%
#    drop_na(any_of(c(v_gene_column, j_gene_column, fill)))
    # drop_na(any_of(c(v_gene_column, j_gene_column, fill))) %>%
    # dplyr::filter(fill %in% focus_group)
  table <- as.data.frame(table(metadata[[v_gene_column]], metadata[[j_gene_column]]))
  names(table) <- c('from', 'to', 'value')
  table$sum <- sum(table$value)
  table <- table %>% mutate(proportion = value/sum)
  table <- table %>% select(from, to, proportion)
  return(table)
}