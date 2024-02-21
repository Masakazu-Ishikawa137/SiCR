make_germline_df_IGH <- function(metadata_df){
    IGH <- metadata_df %>% select(starts_with("BCR_IGH"))
    IGH <- IGH %>%
        mutate(full_length_nt = paste(BCR_IGH_fwr1_nt, BCR_IGH_cdr1_nt, BCR_IGH_fwr2_nt, BCR_IGH_cdr2_nt, BCR_IGH_fwr3_nt, BCR_IGH_cdr3_nt, BCR_IGH_fwr4_nt, sep = ""))
    database_IGHV <- read.csv('data/imgt_human_IGHV.tsv')
    names(database_IGHV) <- c('IGHV_X', 'IGHV_gene', 'IGHV_sequence')
    database_IGHJ <- read.csv('data/imgt_human_IGHJ.tsv')
    names(database_IGHJ) <- c('IGHJ_X', 'IGHJ_gene', 'IGHJ_sequence')
    IGH <- dplyr::left_join(IGH, database_IGHV, by=c('BCR_IGH_v_gene' = 'IGHV_gene'))
    IGH <- dplyr::left_join(IGH, database_IGHJ, by=c('BCR_IGH_j_gene' = 'IGHJ_gene'))
    IGH <- IGH %>% mutate(CDR3_IGH_N = strrep("N", nchar(BCR_IGH_cdr3_nt)))
    IGH <- IGH %>% mutate(germline_IGH = paste(IGHV_sequence, CDR3_IGH_N, IGHJ_sequence, sep = ""))
    return(IGH)
}
