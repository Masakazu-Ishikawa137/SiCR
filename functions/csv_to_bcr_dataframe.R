csv_to_bcr_dataframe <- function(bcr_path){
    bcr <- read_csv(bcr_path, show_col_types = FALSE) %>% # read csv
            dplyr::select(tbcr_default_colnames())       # remove metadata

    IGH <- bcr %>% 
        dplyr::filter(chain == "IGH") %>% 
            distinct(barcode, .keep_all = TRUE)
    names(IGH) <- str_c("BCR_IGH_", names(IGH), sep="")

    IGL <- bcr %>% 
        dplyr::filter(chain == "IGL" | chain == "IGK") %>% 
            distinct(barcode, .keep_all = TRUE)
    names(IGL) <- str_c("BCR_IGL_", names(IGL), sep="")
 
    
    pair <- combineBCR(bcr, samples = "NA", ID = NA)
    pair <- pair$NA_NA
    pair['barcode'] <- str_replace_all(pair$barcode, pattern = "NA_NA_", replacement = "")
    pair["sample"] <- NULL
    pair["ID"] <- NULL
    names(pair) <- str_c("BCR_pair_", names(pair), sep = "")

    bcr_all <- dplyr::full_join(pair, IGH, by=c("BCR_pair_barcode" = "BCR_IGH_barcode"))
    bcr_all <- dplyr::full_join(bcr_all, IGL, by=c("BCR_pair_barcode" = "BCR_IGL_barcode"))
    return(bcr_all)
}