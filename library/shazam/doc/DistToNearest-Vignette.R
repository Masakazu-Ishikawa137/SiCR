## ---- eval=TRUE, warning=FALSE, message=FALSE---------------------------------
library(shazam)
library(dplyr)
data(ExampleDb, package="alakazam")
ExampleDb %>%
    count(sample_id)

## ---- eval=TRUE, warning=FALSE------------------------------------------------
# Use nucleotide Hamming distance and normalize by junction length
dist_ham <- distToNearest(ExampleDb %>% filter(sample_id == "+7d"), 
                          sequenceColumn="junction", 
                          vCallColumn="v_call_genotyped", jCallColumn="j_call",
                          model="ham", normalize="len", nproc=1)

# Use genotyped V assignments, a 5-mer model and no normalization
dist_s5f <- distToNearest(ExampleDb %>% filter(sample_id == "+7d"), 
                          sequenceColumn="junction", 
                          vCallColumn="v_call_genotyped", jCallColumn="j_call",
                          model="hh_s5f", normalize="none", nproc=1)

## ---- eval=FALSE, warning=FALSE-----------------------------------------------
#  # Single-cell mode
#  # Group cells in a one-stage process (VJthenLen=FALSE) and using
#  # both heavy and light chain sequences (onlyHeavy=FALSE)
#  
#  data(Example10x, package="alakazam")
#  dist_sc <- distToNearest(Example10x, cellIdColumn="cell_id", locusColumn="locus",
#                           VJthenLen=FALSE, onlyHeavy=FALSE)

## ---- eval=TRUE, warning=FALSE, fig.width=7-----------------------------------
# Generate Hamming distance histogram
library(ggplot2)
p1 <- ggplot(subset(dist_ham, !is.na(dist_nearest)),
             aes(x=dist_nearest)) + 
    theme_bw() + 
    xlab("Hamming distance") + 
    ylab("Count") +
    scale_x_continuous(breaks=seq(0, 1, 0.1)) +
    geom_histogram(color="white", binwidth=0.02) +
    geom_vline(xintercept=0.12, color="firebrick", linetype=2)
plot(p1)

## ---- eval=TRUE, warning=FALSE, fig.width=7-----------------------------------
# Generate HH_S5F distance histogram
p2 <- ggplot(subset(dist_s5f, !is.na(dist_nearest)),
             aes(x=dist_nearest)) + 
    theme_bw() + 
    xlab("HH_S5F distance") + 
    ylab("Count") +
    scale_x_continuous(breaks=seq(0, 50, 5)) +
    geom_histogram(color="white", binwidth=1) +
    geom_vline(xintercept=7, color="firebrick", linetype=2)
plot(p2)

## ---- eval=TRUE, warning=FALSE, fig.width=7-----------------------------------
# Find threshold using density method
output <- findThreshold(dist_ham$dist_nearest, method="density")
threshold <- output@threshold

# Plot distance histogram, density estimate and optimum threshold
plot(output, title="Density Method")

# Print threshold
print(output)

## ---- eval=TRUE, warning=FALSE, fig.width=7-----------------------------------
# Find threshold using gmm method
output <- findThreshold(dist_ham$dist_nearest, method="gmm", model="gamma-gamma")

# Plot distance histogram, Gaussian fits, and optimum threshold
plot(output, binwidth=0.02, title="GMM Method: gamma-gamma")

# Print threshold
print(output)

## ----fields, eval=TRUE, warning=FALSE-----------------------------------------
dist_fields <- distToNearest(ExampleDb, model="ham", normalize="len", 
                             fields="sample_id", nproc=1)

## ---- eval=TRUE, warning=FALSE, fig.width=7-----------------------------------
# Generate grouped histograms
p4 <- ggplot(subset(dist_fields, !is.na(dist_nearest)), 
             aes(x=dist_nearest)) + 
    theme_bw() + 
    xlab("Grouped Hamming distance") + 
    ylab("Count") +
    geom_histogram(color="white", binwidth=0.02) +
    geom_vline(xintercept=0.12, color="firebrick", linetype=2) +
    facet_grid(sample_id ~ ., scales="free_y")
plot(p4)

## ----cross, eval=TRUE, warning=FALSE------------------------------------------
dist_cross <- distToNearest(ExampleDb, sequenceColumn="junction", 
                            vCallColumn="v_call_genotyped", jCallColumn="j_call",
                            model="ham", first=FALSE, 
                            normalize="len", cross="sample_id", nproc=1)

## ---- eval=TRUE, warning=FALSE, fig.width=7-----------------------------------
# Generate cross sample histograms
p5 <- ggplot(subset(dist_cross, !is.na(cross_dist_nearest)), 
             aes(x=cross_dist_nearest)) + 
    theme_bw() + 
    xlab("Cross-sample Hamming distance") + 
    ylab("Count") +
    geom_histogram(color="white", binwidth=0.02) +
    geom_vline(xintercept=0.12, color="firebrick", linetype=2) +
    facet_grid(sample_id ~ ., scales="free_y")
plot(p5)

## ----subsample, eval=TRUE, warning=FALSE--------------------------------------
# Explore V-J-junction length groups sizes to use subsample
# Show the size of the largest groups
library(alakazam)
top_10_sizes <- ExampleDb %>%
     group_by(junction_length) %>% # Group by junction length
     do(alakazam::groupGenes(., first=TRUE)) %>% # Group by V and J call
     mutate(GROUP_ID=paste(junction_length, vj_group, sep="_")) %>% # Create group ids
     ungroup() %>%
     group_by(GROUP_ID) %>% # Group by GROUP_ID
     distinct(junction) %>% # Count unique junctions per group
     summarize(SIZE=n()) %>% # Get the size of the group
     arrange(desc(SIZE)) %>% # Sort by decreasing size
     select(SIZE) %>% 
     top_n(10) # Filter to the top 10
top_10_sizes

# Use 30 to subsample
# NOTE: This is a toy example. Subsampling to 30 sequence with real data is unwise
dist <- distToNearest(ExampleDb, sequenceColumn="junction", 
                      vCallColumn="v_call_genotyped", jCallColumn="j_call",
                      model="ham", 
                      first=FALSE, normalize="len",
                      subsample=30)

