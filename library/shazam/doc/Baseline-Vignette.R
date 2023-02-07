## ---- eval=TRUE, warning=FALSE, message=FALSE---------------------------------
# Load example data
library(shazam)
library(alakazam)
data(ExampleDb, package="alakazam")
# Subset for faster demonstration
ExampleDb <- subset(ExampleDb, c_call %in% c("IGHA", "IGHG"))

## ---- eval=TRUE, warning=FALSE, results="hide"--------------------------------
# Collapse clonal groups into single sequences
clones <- collapseClones(ExampleDb, cloneColumn="clone_id", 
                         sequenceColumn="sequence_alignment", 
                         germlineColumn="germline_alignment_d_mask", 
                         regionDefinition=IMGT_V, 
                         method="thresholdedFreq", minimumFrequency=0.6,
                         includeAmbiguous=FALSE, breakTiesStochastic=FALSE, 
                         nproc=1)

## ----eval=F, warning=F, results="hide"----------------------------------------
#  # Subset to sequences with clone_id=3170
#  db_3170 <- subset(ExampleDb, clone_id == 3170)
#  dim(db_3170)
#  colnames(db_3170)
#  
#  # Generate a ChangeoClone object for lineage construction
#  clone_3170 <- makeChangeoClone(db_3170, seq="sequence_alignment", germ="germline_alignment")
#  
#  # Run the lineage reconstruction
#  dnapars_exec <- "/usr/local/bin/dnapars"
#  graph_3170 <- buildPhylipLineage(clone_3170, dnapars_exec, rm_temp=TRUE)
#  
#  # Generating a data.frame from the lineage tree graph object,
#  # and merge it with clone data.frame
#  graph_3170_df <- makeGraphDf(graph_3170, clone_3170)
#  dim(graph_3170_df)
#  colnames(graph_3170_df)

## ---- eval=TRUE, warning=FALSE, results="hide"--------------------------------
# Count observed mutations and append mu_count columns to the output
observed <- observedMutations(clones, 
                              sequenceColumn="clonal_sequence",
                              germlineColumn="clonal_germline",
                              regionDefinition=IMGT_V, nproc=1)
# Count expected mutations and append mu_exptected columns to the output
expected <- expectedMutations(observed, 
                              sequenceColumn="clonal_sequence",
                              germlineColumn="clonal_germline",
                              targetingModel=HH_S5F,
                              regionDefinition=IMGT_V, nproc=1)

## ---- eval=TRUE, warning=FALSE, results="hide"--------------------------------
# Calculate selection scores using the output from expectedMutations
baseline <- calcBaseline(expected, testStatistic="focused", 
                         regionDefinition=IMGT_V, nproc=1)

## ---- eval=FALSE, warning=FALSE, results="hide"-------------------------------
#  # Calculate selection scores from scratch
#  baseline <- calcBaseline(clones, testStatistic="focused",
#                           regionDefinition=IMGT_V, nproc=1)

## ---- eval=FALSE, warning=FALSE, results="hide"-------------------------------
#  # Calculate selection on charge class with the mouse 5-mer model
#  baseline_mk_rs5nf <- calcBaseline(clones, testStatistic="focused",
#                           regionDefinition=IMGT_V,
#                           targetingModel=MK_RS5NF,
#                           mutationDefinition=CHARGE_MUTATIONS,
#                           nproc=1)

## ---- eval=TRUE, warning=FALSE, results="hide"--------------------------------
# Combine selection scores by time-point
grouped_1 <- groupBaseline(baseline, groupBy="sample_id")

## ---- eval=TRUE, warning=FALSE, results="hide"--------------------------------
# Subset the original data to switched isotypes
db_sub <- subset(ExampleDb, c_call %in% c("IGHM", "IGHG"))

# Collapse clonal groups into single sequence
clones_sub <- collapseClones(db_sub, cloneColumn="clone_id",
                             sequenceColumn="sequence_alignment",
                             germlineColumn="germline_alignment_d_mask",
                             regionDefinition=IMGT_V, 
                             method="thresholdedFreq", minimumFrequency=0.6,
                             includeAmbiguous=FALSE, breakTiesStochastic=FALSE, 
                             nproc=1)

# Calculate selection scores from scratch
baseline_sub <- calcBaseline(clones_sub, testStatistic="focused", 
                             regionDefinition=IMGT_V, nproc=1)

# Combine selection scores by time-point and isotype
grouped_2 <- groupBaseline(baseline_sub, groupBy=c("sample_id", "c_call"))

## ---- eval=FALSE, warning=FALSE, results="hide"-------------------------------
#  # First group by subject and status
#  subject_grouped <- groupBaseline(baseline, groupBy=c("status", "subject"))
#  
#  # Then group the output by status
#  status_grouped <- groupBaseline(subject_grouped, groupBy="status")

## ---- eval=TRUE---------------------------------------------------------------
testBaseline(grouped_1, groupBy="sample_id")

## ---- eval=TRUE, warning=FALSE------------------------------------------------
# Set sample and isotype colors
sample_colors <- c("-1h"="seagreen", "+7d"="steelblue")
isotype_colors <- c("IGHM"="darkorchid", "IGHD"="firebrick", 
                    "IGHG"="seagreen", "IGHA"="steelblue")

# Plot mean and confidence interval by time-point
plotBaselineSummary(grouped_1, "sample_id")

# Plot selection scores by time-point and isotype for only CDR
plotBaselineSummary(grouped_2, "sample_id", "c_call", groupColors=isotype_colors,
                    subsetRegions="cdr")

# Group by CDR/FWR and facet by isotype
plotBaselineSummary(grouped_2, "sample_id", "c_call", facetBy="group")

## ---- eval=TRUE, warning=FALSE------------------------------------------------
# Plot selection PDFs for a subset of the data
plotBaselineDensity(grouped_2, "c_call", groupColumn="sample_id", colorElement="group", 
                    colorValues=sample_colors, sigmaLimits=c(-1, 1))

## ---- eval=FALSE, warning=FALSE, results="hide"-------------------------------
#  # Get indices of rows corresponding to IGHA in the field "db"
#  # These are the same indices also in the matrices in the fileds "numbOfSeqs",
#  # "binomK", "binomN", "binomP", and "pdfs"
#  # In this example, there is one row of IGHA for each sample
#  dbIgMIndex <- which(grouped_2@db[["c_call"]] == "IGHG")
#  
#  grouped_2 <- editBaseline(grouped_2, "db", grouped_2@db[-dbIgMIndex, ])
#  grouped_2 <- editBaseline(grouped_2, "numbOfSeqs", grouped_2@numbOfSeqs[-dbIgMIndex, ])
#  grouped_2 <- editBaseline(grouped_2, "binomK", grouped_2@binomK[-dbIgMIndex, ])
#  grouped_2 <- editBaseline(grouped_2, "binomN", grouped_2@binomN[-dbIgMIndex, ])
#  grouped_2 <- editBaseline(grouped_2, "binomP", grouped_2@binomP[-dbIgMIndex, ])
#  grouped_2 <- editBaseline(grouped_2, "pdfs",
#                            lapply(grouped_2@pdfs, function(pdfs) {pdfs[-dbIgMIndex, ]} ))
#  
#  # The indices corresponding to IGHA are slightly different in the field "stats"
#  # In this example, there is one row of IGHA for each sample and for each region
#  grouped_2 <- editBaseline(grouped_2, "stats",
#                            grouped_2@stats[grouped_2@stats[["c_call"]] != "IGHA", ])

