library(alakazam)
library(immunarch)
library(Seurat)
library(tidyverse)
library(scRepertoire)
library(hrbrthemes)
library(ggsci)

#########GEX
mysinglecell_h5_to_umap <- function(h5){
    h5 <- Read10X_h5(h5)
    seurat_object <- CreateSeuratObject(h5)
    str_split(rownames(seurat_object@meta.data), "-", simplify=TRUE) %>% .[,2] -> seurat_object@meta.data$Sample
    seurat_object <- NormalizeData(seurat_object)
    seurat_object <- FindVariableFeatures(seurat_object, selection.method = "vst", nfeatures = 2000)
    all.genes <- rownames(seurat_object)
    seurat_object <- ScaleData(seurat_object, features = all.genes)
    seurat_object <- RunPCA(seurat_object, features = VariableFeatures(object = seurat_object))
    seurat_object <- FindNeighbors(seurat_object, dims = 1:10)
    seurat_object <- FindClusters(seurat_object, resolution = 0.5)
    seurat_object <- RunUMAP(seurat_object, dims = 1:10)
    return (seurat_object)
}

sctype_load_gslist <- function(type="Immune system"){
    lapply(c("dplyr","Seurat","HGNChelper"), library, character.only = T)
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R")
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R")

    # get cell-type-specific gene sets from our in-built database (DB)
    gs_list = gene_sets_prepare("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/ScTypeDB_short.xlsx", type) # e.g. Immune system, Liver, Pancreas, Kidney, Eye, Brain
    return (gs_list)
}

sctype_typescore_normal_normalization2 <- function(data, gslist){
    lapply(c("dplyr","Seurat","HGNChelper"), library, character.only = T)
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R")
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R")
    ex.max <- sctype_score(scRNAseqData=data@assays$RNA@scale.data, scaled=TRUE, gs = gslist$gs_positive, gs2 = gslist$gs_negative)
    return (ex.max)
}

sctype_clresults <- function(pbmc, es.max){
    lapply(c("dplyr","Seurat","HGNChelper"), library, character.only = T)
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R")
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R")
    cL_resutls = do.call("rbind", lapply(unique(pbmc@meta.data$seurat_clusters), function(cl){
        es.max.cl = sort(rowSums(es.max[ ,rownames(pbmc@meta.data[pbmc@meta.data$seurat_clusters==cl, ])]), decreasing = !0)
        head(data.frame(cluster = cl, type = names(es.max.cl), scores = es.max.cl, ncells = sum(pbmc@meta.data$seurat_clusters==cl)), 10)
    }))
        sctype_scores = cL_resutls %>% group_by(cluster) %>% top_n(n = 1, wt = scores)  

    # set low-confident (low ScType score) clusters to "unknown"
    sctype_scores$type[as.numeric(as.character(sctype_scores$scores)) < sctype_scores$ncells/4] = "Unknown"
    return (sctype_scores)
}

sctype_merge <- function(immune.combined.sct, sctype_scores){
    lapply(c("dplyr","Seurat","HGNChelper"), library, character.only = T)
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/gene_sets_prepare.R")
    source("https://raw.githubusercontent.com/IanevskiAleksandr/sc-type/master/R/sctype_score_.R")
    immune.combined.sct@meta.data$celltype = ""
    for(j in unique(sctype_scores$cluster)){
      cl_type = sctype_scores[sctype_scores$cluster==j,]; 
      immune.combined.sct@meta.data$celltype[immune.combined.sct@meta.data$seurat_clusters == j] = as.character(cl_type$type[1])}
    return (immune.combined.sct)
}

mysinglecell_celltyping_normal_normalization2 <- function(seurat_object, type="Immune system"){
    suppressWarnings({sctype_load_gslist(type)}) -> gslist
    sctype_typescore_normal_normalization2(seurat_object, gslist) -> es.max
    sctype_clresults(seurat_object, es.max) -> result
    sctype_merge(seurat_object, result) -> final
    return(final)
}

########TCR
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

############Basic run
GEX <- function(h5){
    seurat_object <- mysinglecell_h5_to_umap(h5)
    seurat_object <- mysinglecell_celltyping_normal_normalization2(seurat_object)
    return (seurat_object)
}

TCR <- function(seurat_object, tcr){
    mysinglecell_add_metadata(seurat_object, tcr) -> seurat_object
    myTCR_dataframe(tcr) -> tcr_dataframe
    myconcat_seumeta_tcr(seurat_object, tcr_dataframe) -> seurat_object    
    return (seurat_object)
}

BCR <- function(seurat_object, bcr){
     mysinglecell_add_metadata(seurat_object, bcr) -> seurat_object
     myBCR_dataframe(bcr) -> bcr_dataframe
     myconcat_seumeta_bcr(seurat_object, bcr_dataframe) -> seurat_object    
     return (seurat_object)
}
