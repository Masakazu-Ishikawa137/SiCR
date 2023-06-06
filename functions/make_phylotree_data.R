
make_phylotree_data <- function(bcr_IGH, clone, ighv_reference="data/221124_ighv_reference.csv"){
  
  ighv_reference <- read.csv(ighv_reference, row.names = 1)
  
  data <- bcr_IGH %>%
    select(contig_id, raw_clonotype_id, v_gene, j_gene, ends_with("nt")) %>%
    filter(raw_clonotype_id == clone & !is.na(v_gene))
  
  if (nrow(data) == 0){ 
    stop(paste0("Clone '", clone, "' is not found."))
  }
  
  # Get the name of the top v gene based on the count (Is it possible for there to be multiple types of V genes?)
  top_v_gene <- as.data.frame(table(data$v_gene)) %>%
    arrange(desc(Freq)) %>%
    pull(Var1) %>%
    .[1] %>%
    as.character() # from factor to character
  
  # Get the sequence of the V gene from ighv_reference and add it to the data
  # There are often multiple rows in ighv_reference with the same IGHV_id (v gene name) but different sequences
  # In the above case, for now, we use the topmost one. Is it really acceptable though...?
  sequence <- ighv_reference %>%
    filter(IGHV_id == top_v_gene) %>%
    pull(Sequence) %>%
    .[1]
  data$germline_alignment <- data$germline_alignment_d_mask <- sequence
  
  # add junction_length column (the character number of cdr3_nt)
  data <- data %>%
    mutate(junction_length = nchar(cdr3_nt))
  
  # add sequence_alignment column（unite, by default, delete the source columns after the merge）
  data <- data %>%
    unite(
      "sequence_alignment",
      c("fwr1_nt","cdr1_nt","fwr2_nt","cdr2_nt","fwr3_nt","cdr3_nt","fwr4_nt"),
      sep = ""
    )
  
  # rename columns
  data <- data %>%
    rename(
      sequence_id = contig_id,
      clone_id = raw_clonotype_id,
      v_call = v_gene,
      j_call = j_gene
    )
  
  return(data)
  
}