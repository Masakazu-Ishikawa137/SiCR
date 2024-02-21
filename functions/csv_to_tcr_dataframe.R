csv_to_tcr_dataframe <- function(tcr_path){
    tcr <- read_csv(tcr_path, show_col_types = FALSE) %>% # read csv
            dplyr::select(tbcr_default_colnames())       # remove metadata
    print('TRA start')
    TRA <- tcr %>% 
        dplyr::filter(chain == "TRA") %>% 
            distinct(barcode, .keep_all = TRUE)
    names(TRA) <- str_c("TCR_TRA_", names(TRA), sep="")
    print(TRA)
    print('TRB start')
    TRB <- tcr %>% 
        dplyr::filter(chain == "TRB") %>% 
            distinct(barcode, .keep_all = TRUE)
    names(TRB) <- str_c("TCR_TRB_", names(TRB), sep="")
    print(TRB)

    print('pair start')
    
    pair <- combineTCR(tcr, cells = "T-AB", samples = "NA", ID =NA, filterMulti = TRUE)
    pair <- pair$NA_NA
    pair['barcode'] <- str_replace_all(pair$barcode, pattern = "NA_NA_", replacement = "")
    pair["sample"] <- NULL
    pair["ID"] <- NULL
    names(pair) <- str_c("TCR_pair_", names(pair), sep="")
    print('concatenate TRA start')

    tcr_all <- dplyr::full_join(pair, TRA, by=c("TCR_pair_barcode" = "TCR_TRA_barcode"))
    print('concatenate TRB start')

    tcr_all <- dplyr::full_join(tcr_all, TRB, by=c("TCR_pair_barcode" = "TCR_TRB_barcode"))
    return(tcr_all)
}
