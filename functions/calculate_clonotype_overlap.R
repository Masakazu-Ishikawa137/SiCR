repertoire_overlap_jaccard <- function(lst){
    #jaccard
    df <- data.frame()
    for (i in names(lst)){
        for (j in names(lst)){
            if(i == j){
                df[i, j] <- NA
            }
            else{
                numerator <- length(intersect(lst[[i]], lst[[j]]))
                denominator <- length(lst[[i]]) + length(lst[[j]]) - numerator
                df[i,j] <- numerator/denominator            
            }
        }
    }
    return(df)
}

repertoire_overlap_public <- function(lst){
    df <- data.frame()
    for (i in names(lst)){
        for (j in names(lst)){
            if(i == j){
                df[i, j] <- NA
            }
            else{
                df[i, j] <- length(intersect(lst[[i]], lst[[j]]))
            }
        }
    }
    return(df)
}

repertoire_overlap_tversky <- function(lst){
    df <- data.frame()
    for (i in names(lst)){
        for (j in names(lst)){
            if(i == j){
                df[i, j] <- NA
            }
            else{
                same <- length(intersect(lst[[i]], lst[[j]]))
                factor_one_unique <- length(lst[[i]])
                factor_two_unique <- length(lst[[j]])
                df[i,j] <- same/(same + (factor_one_unique - same + factor_two_unique - same)/2)
            }
        }
    }
    return(df)

}

calculate_clonotype_overlap <- function(myReactives, sample_col = "sample", tcr_col = "TCR_pair_CTaa", method = "public") {
    df <- myReactives$seurat_object@meta.data
    # サンプル名のリストを取得
    samples <- unique(df[[sample_col]])

    # サンプルごとのユニークなTCR_pair_CTaaのリストを作成
    unique_CDR3 <- list()
    for (i in samples) {
        # サンプルでフィルタリング
        j <- df %>% dplyr::filter((!!sym(sample_col)) == i)

        # ユニークなTCR_pair_CTaaを取得
        unique_CDR3[[i]] <- unique(j[[tcr_col]][!is.na(j[[tcr_col]])])
        #    unique_CDR3[[i]] <- unique(j[[tcr_col]])
    }

    names(unique_CDR3) <- samples
    if (method == "public") {
        return(repertoire_overlap_public(unique_CDR3))
    }
    if (method == "jaccard") {
        return(repertoire_overlap_jaccard(unique_CDR3))
    }
    if (method == "tversky") {
        return(repertoire_overlap_tversky(unique_CDR3))
    }
}
