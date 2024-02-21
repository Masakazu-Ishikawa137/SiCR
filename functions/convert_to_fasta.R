convert_to_fasta <- function(data) {
    fasta_output <- c()
    fasta_output <- append(fasta_output, '>germline')
    fasta_output <- append(fasta_output, data$germline_IGH[1])
    for (i in 1:nrow(data)) {
        fasta_output <- append(fasta_output, paste0(">", data$BCR_IGH_exact_subclonotype_id[i]))
        fasta_output <- append(fasta_output, data$full_length_nt[i])
    }
    return(fasta_output)
}
