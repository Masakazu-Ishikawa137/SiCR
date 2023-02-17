library(alakazam)
library(immunarch)
library(Seurat)
library(tidyverse)
library(scRepertoire)
library(hrbrthemes)
library(ggsci)

mysinglecell_add_metadata <- function(seurat_object, csv){
    csv <- read.csv(csv)
    str_split(csv$"barcode", "-", simplify=TRUE) %>% .[,2] -> csv$"Sample"
    csv %>% dplyr::select(-barcode,-is_cell,-contig_id,-high_confidence,-length,-chain,-v_gene,-d_gene,-j_gene,-c_gene,-full_length,-productive,-fwr1,-fwr1_nt,-cdr1,-cdr1_nt,-fwr2,-fwr2_nt,-cdr2,-cdr2_nt,-fwr3,-fwr3_nt,-cdr3,-cdr3_nt,-fwr4,-fwr4_nt,-reads,-umis,-raw_clonotype_id,-raw_consensus_id,-exact_subclonotype_id) %>% unique() %>% dplyr::select(Sample, everything()) -> metadata
    rowname <- rownames(seurat_object@meta.data)
    dplyr::left_join(seurat_object@meta.data, metadata, by=c("Sample")) -> metadata
    rownames(metadata) <- rowname
    seurat_object@meta.data <- metadata
    return(seurat_object)
}
myTCR_dataframe <- function(sample){
    sample <- read.csv(sample)
    combinedTCR <- combineTCR(sample, samples = "NA", ID = NA, cells = "T-AB", filterMulti = TRUE)
    combinedTCR <- combinedTCR$NA_NA
    str_split(combinedTCR$barcode, pattern="_", simplify=TRUE) %>% .[,3] -> combinedTCR$barcode
    combinedTCR$sample <- NULL
    combinedTCR$ID <- NULL
    names(combinedTCR) <- str_c("pair_", names(combinedTCR))
    sample %>% dplyr::filter(chain == "TRA") -> TRA
    names(TRA) <- str_c("TRA_", names(TRA))
    TRA %>% distinct(TRA_barcode,.keep_all=TRUE)　-> TRA
    sample %>% dplyr::filter(chain == "TRB") -> TRB
    names(TRB) <- str_c("TRB_", names(TRB))
    TRB %>% distinct(TRB_barcode,.keep_all=TRUE)　-> TRB
    dplyr::left_join(combinedTCR, TRA, by=c("pair_barcode" = "TRA_barcode")) -> data_TRA
    dplyr::left_join(data_TRA, TRB, by=c("pair_barcode" = "TRB_barcode")) -> all_data
    names(all_data) <- str_c("TCR_", names(all_data))
    return(all_data)
}

myconcat_seumeta_tcr <- function(seurat_object, tcr){
    tibble::rownames_to_column(seurat_object@meta.data, "barcode") -> metadata
    dplyr::left_join(metadata, tcr, by=c("barcode" = "TCR_pair_barcode")) -> metadata
    tibble::column_to_rownames(metadata, "barcode") -> seurat_object@meta.data
    return(seurat_object)
}