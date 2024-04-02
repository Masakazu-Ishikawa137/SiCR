csv_to_tcr_dataframe <- function(tcr_path){
    tcr <- read_csv(tcr_path, show_col_types = FALSE) %>% # read csv
            dplyr::select(tbcr_default_colnames())       # remove metadata
    TRA <- tcr %>% 
        dplyr::filter(chain == "TRA") %>% 
            distinct(barcode, .keep_all = TRUE)
    names(TRA) <- str_c("TCR_TRA_", names(TRA), sep="")
    TRB <- tcr %>% 
        dplyr::filter(chain == "TRB") %>% 
            distinct(barcode, .keep_all = TRUE)
    names(TRB) <- str_c("TCR_TRB_", names(TRB), sep="")
    
    pair <- combineTCR(tcr, cells = "T-AB", samples = "NA", ID =NA, filterMulti = TRUE)
    pair <- pair$NA_NA
    pair['barcode'] <- str_replace_all(pair$barcode, pattern = "NA_NA_", replacement = "")
    pair["sample"] <- NULL
    pair["ID"] <- NULL
    names(pair) <- str_c("TCR_pair_", names(pair), sep="")

    tcr_all <- dplyr::full_join(pair, TRA, by=c("TCR_pair_barcode" = "TCR_TRA_barcode"))

    tcr_all <- dplyr::full_join(tcr_all, TRB, by=c("TCR_pair_barcode" = "TCR_TRB_barcode"))
    tcr_all <- tcr_all %>% mutate(TCR = case_when(
        TCR_TRA_is_cell == "TRUE" & TCR_TRB_is_cell == "TRUE" ~ "Pair",
        TCR_TRA_is_cell == "TRUE" & is.na(TCR_TRB_is_cell) ~ "Only_TRA",
        is.na(TCR_TRA_is_cell) & TCR_TRB_is_cell == "TRUE" ~ "Only_TRB",
        TRUE ~ "No"
    ))

    # expand
    tcr_all <- tcr_all %>%
        mutate(sample = str_remove(TCR_pair_barcode, "^.+-")) %>%
        group_by(sample, TCR_TRB_raw_clonotype_id) %>%
        mutate(TCR_count_per_sample = ifelse(is.na(TCR_TRB_raw_clonotype_id), 0, n())) %>%
        ungroup() %>% # グループ化を解除
        mutate(TCR_expand = case_when(
            TCR_count_per_sample > 2 ~ ">2",
            TRUE ~ as.character(TCR_count_per_sample)
        ))
    tcr_all <- tcr_all %>% dplyr::select(-sample)
    return(tcr_all)
}
