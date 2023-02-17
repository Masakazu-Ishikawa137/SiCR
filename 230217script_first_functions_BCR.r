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


myBCR_dataframe <- function(sample){
    sample <- read.csv(sample)
    combinedBCR <- combineBCR(sample, samples = "NA", ID = "NA", removeMulti = TRUE)
    combinedBCR <- combinedBCR$NA_NA
    str_split(combinedBCR$barcode, pattern="_", simplify=TRUE) %>% .[,3] -> combinedBCR$barcode
    combinedBCR$sample <- NULL
    combinedBCR$ID <- NULL
    names(combinedBCR) <- str_c("pair_", names(combinedBCR))
    sample %>% dplyr::filter(chain == "IGH") -> IGH
    names(IGH) <- str_c("IGH_", names(IGH))
    IGH %>% distinct(IGH_barcode,.keep_all=TRUE)　-> IGH
    sample %>% dplyr::filter(chain != "IGH") -> IGL
    names(IGL) <- str_c("IGL_", names(IGL))
    IGL %>% distinct(IGL_barcode,.keep_all=TRUE)　-> IGL
    dplyr::left_join(combinedBCR, IGH, by=c("pair_barcode" = "IGH_barcode")) -> data_IGH
    dplyr::left_join(data_IGH, IGL, by=c("pair_barcode" = "IGL_barcode")) -> all_data
    names(all_data) <- str_c("BCR_", names(all_data))
    return(all_data)
}

myconcat_seumeta_bcr <- function(seurat_object, bcr){
    tibble::rownames_to_column(seurat_object@meta.data, "barcode") -> metadata
    dplyr::left_join(metadata, bcr, by=c("barcode" = "BCR_pair_barcode")) -> metadata
    tibble::column_to_rownames(metadata, "barcode") -> seurat_object@meta.data
    return(seurat_object)
}