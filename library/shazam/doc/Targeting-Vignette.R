## ---- eval=TRUE, warning=FALSE, message=FALSE---------------------------------
# Load example data
library(shazam)
data(ExampleDb, package="alakazam")

# Subset to IGHG for faster usage demonstration
db <- subset(ExampleDb, c_call == "IGHG")

## ---- eval=FALSE--------------------------------------------------------------
#  # Create substitution model using silent mutations
#  sub_model <- createSubstitutionMatrix(db, model="s",
#                                        sequenceColumn="sequence_alignment",
#                                        germlineColumn="germline_alignment_d_mask",
#                                        vCallColumn="v_call")

## ---- eval=FALSE--------------------------------------------------------------
#  # Create mutability model using silent mutations
#  mut_model <- createMutabilityMatrix(db, sub_model, model="s",
#                                      sequenceColumn="sequence_alignment",
#                                      germlineColumn="germline_alignment_d_mask",
#                                      vCallColumn="v_call")

## ---- eval=FALSE--------------------------------------------------------------
#  # Number of silent mutations used for estimating 5-mer mutabilities
#  mut_model@numMutS
#  # Number of replacement mutations used for estimating 5-mer mutabilities
#  mut_model@numMutR
#  # Mutability and source as a data.frame
#  head(as.data.frame(mut_model))

## ---- eval=FALSE--------------------------------------------------------------
#  # Extend models to include ambiguous 5-mers
#  sub_model <- extendSubstitutionMatrix(sub_model)
#  mut_model <- extendMutabilityMatrix(mut_model)

## ---- eval=FALSE--------------------------------------------------------------
#  # Create targeting model matrix from substitution and mutability models
#  tar_matrix <- createTargetingMatrix(sub_model, mut_model)

## ---- eval=TRUE, warning=FALSE------------------------------------------------
# Collapse sequences into clonal consensus
clone_db <- collapseClones(db, cloneColumn="clone_id", 
                           sequenceColumn="sequence_alignment",
                           germlineColumn="germline_alignment_d_mask",
                           nproc=1)
# Create targeting model in one step using only silent mutations
# Use consensus sequence input and germline columns
model <- createTargetingModel(clone_db, model="s", sequenceColumn="clonal_sequence", 
                              germlineColumn="clonal_germline", vCallColumn="v_call")

## ---- eval=TRUE, warning=FALSE, fig.width=7, fig.height=7.5-------------------
# Generate hedgehog plot of mutability model
plotMutability(model, nucleotides="A", style="hedgehog")
plotMutability(model, nucleotides="C", style="hedgehog")

## ---- eval=TRUE, warning=FALSE, fig.width=7, fig.height=4.5-------------------
# Generate bar plot of mutability model
plotMutability(model, nucleotides="G", style="bar")
plotMutability(model, nucleotides="T", style="bar")

## ---- eval=TRUE, warning=FALSE------------------------------------------------
# Calculate distance matrix
dist <- calcTargetingDistance(model)

