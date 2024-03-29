gene_list <- function(markers, gene_label, number_gene_label){
    if(gene_label == "Manhattan"){
            markers %>% arrange(desc(`Manhattan distance`)) %>% head(., number_gene_label) %>% rownames(.)
    }
    else if(gene_label == "Euclidean"){
            markers %>% arrange(desc(`Euclidean distance`)) %>% head(., number_gene_label) %>% rownames(.)
    }
    else if(gene_label == "foldchange"){
            markers %>% arrange(desc(`avg_log2FC`)) %>% head(., number_gene_label) %>% rownames(.)
    }
    else if(gene_label == "significance"){
            markers %>% arrange(`p_val_adj`) %>% head(., number_gene_label) %>% rownames(.)
    }
    

}

volcano <- function (markers, 
                     alpha = 1, 
                     size = 1, 
                     xintercept_low = -1, 
                     xintercept_high = 1, 
                     yintercept = 1, 
                     pct = 0.01, 
                     avg_log2FC = 0.25, 
                     gene_label = "significance", 
                     number_gene_label = 10) 
{
    library(tidyverse)
    library(ggrepel)
    
    
    markers$'gene_id' <- rownames(markers)
    
    
    markers <- markers %>% 
        mutate(`Manhattan distance` = abs(p_val_adj)+abs(avg_log2FC)) %>%
        mutate(`Euclidean distance` = sqrt((p_val_adj)^2+(avg_log2FC)^2)) %>%
        dplyr::filter(pct.1 >= pct & pct.2 >= pct & avg_log2FC >= avg_log2FC) %>%
        mutate(significance = case_when(
            -log10(p_val_adj) > yintercept & avg_log2FC >= xintercept_high ~ 'UP', 
            -log10(p_val_adj) > yintercept & avg_log2FC <= xintercept_low ~ 'DOWN',
            TRUE ~ 'NO'
            ))
    
    gene_label <- markers %>% dplyr::filter(significance != "NO") %>% gene_list(., gene_label, number_gene_label)

    
    markers <- markers %>%
        mutate(label = case_when(
            gene_id %in% gene_label ~ gene_id,
            TRUE ~ NA_character_
        ))
    

    ggplot(markers, aes(x = avg_log2FC, y = -log10(p_val_adj), label = label)) + 
           geom_point(aes(col = significance), alpha = alpha, size = size) + 
           theme_minimal() +
           geom_vline(xintercept = c(xintercept_low, xintercept_high), linetype = "dashed") +
           geom_hline(yintercept = yintercept, linetype = 'dashed') +
           scale_color_manual(values = c('UP' = 'red', 'DOWN' = 'blue', 'NO' = 'grey')) +
           geom_text_repel()
}