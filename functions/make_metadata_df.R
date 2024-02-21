make_metadata_df <- function(csv_path){
    csv <- read_csv(csv_path, show_col_types = FALSE) %>% # read csv
        mutate(sample = str_remove(barcode, "^.+-"))    
    minus_column <- c('barcode','is_cell','contig_id','high_confidence','length','chain','v_gene','d_gene','j_gene','c_gene','full_length','productive','fwr1','fwr1_nt','cdr1','cdr1_nt','fwr2','fwr2_nt','cdr2','cdr2_nt','fwr3','fwr3_nt','cdr3','cdr3_nt','fwr4','fwr4_nt','reads','umis','raw_clonotype_id','raw_consensus_id','exact_subclonotype_id')
    df <- csv %>% select(-minus_column) %>% distinct(sample, .keep_all=TRUE) %>% select(sample, everything())
    return(df)
}
