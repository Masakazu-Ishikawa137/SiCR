
make_phylotree_data <- function(bcr_IGH, clone, ighv_reference="data/221124_ighv_reference.csv"){
  
  ighv_reference <- read.csv(ighv_reference, row.names = 1)
  
  data <- bcr_IGH %>%
    select(contig_id, raw_clonotype_id, v_gene, j_gene, ends_with("nt")) %>%
    filter(raw_clonotype_id == clone & !is.na(v_gene))
  
  if (nrow(data) == 0){ 
    stop(paste0("Clone '", clone, "' is not found."))
  }
  
  # カウント数がトップのv geneの名称を取得（v geneが複数種類あることってあるの？）
  top_v_gene <- as.data.frame(table(data$v_gene)) %>%
    arrange(desc(Freq)) %>%
    pull(Var1) %>%
    .[1] %>%
    as.character() # factorになってるので
  
  # ighv_referenceから該当v_geneのSequenceを取得し、データに追加
  # ighv_referenceにはIGHV_id(v_gene名)が同じ行が複数あることが結構ある（Sequenceが違う）。
  # とりあえず一番上を使うので良いらしい。良いのかな。
  sequence <- ighv_reference %>%
    filter(IGHV_id == top_v_gene) %>%
    pull(Sequence) %>%
    .[1]
  data$germline_alignment <- data$germline_alignment_d_mask <- sequence
  
  # junction_length列（cdr3_ntの長さ）を追加
  data <- data %>%
    mutate(junction_length = nchar(cdr3_nt))
  
  # sequence_alignment列を追加（uniteはデフォだと結合元の列は削除される）
  data <- data %>%
    unite(
      "sequence_alignment",
      c("fwr1_nt","cdr1_nt","fwr2_nt","cdr2_nt","fwr3_nt","cdr3_nt","fwr4_nt"),
      sep = ""
    )
  
  # カラム名変更
  data <- data %>%
    rename(
      sequence_id = contig_id,
      clone_id = raw_clonotype_id,
      v_call = v_gene,
      j_call = j_gene
    )
  
  return(data)
  
}