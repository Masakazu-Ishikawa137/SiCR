## -----------------------------------------------------------------------------
# Imports
library(airr)
library(tibble)

# Read Rearrangement example file
f1 <- system.file("extdata", "rearrangement-example.tsv.gz", package="airr")
rearrangement <- read_rearrangement(f1)
glimpse(rearrangement)

## -----------------------------------------------------------------------------
# Read Repertoire example file
f2 <- system.file("extdata", "repertoire-example.yaml", package="airr")
repertoire <- read_airr(f2)
glimpse(repertoire)

# Read GermlineSet example file
f3 <- system.file("extdata", "germline-example.json", package="airr")
germline <- read_airr(f3)
glimpse(germline)

## -----------------------------------------------------------------------------
x1 <- file.path(tempdir(), "airr_out.tsv")
write_rearrangement(rearrangement, x1)

## -----------------------------------------------------------------------------
x2 <- file.path(tempdir(), "airr_repertoire_out.yaml")
write_airr(repertoire, x2, format="yaml")

x3 <- file.path(tempdir(), "airr_germline_out.json")
write_airr(germline, x3, format="json")

## -----------------------------------------------------------------------------
# Validate Rearrangement data.frame
validate_rearrangement(rearrangement)

# Validate an AIRR Data Model
validate_airr(repertoire)

# Validate AIRR Data Model records individual 
validate_airr(germline, each=TRUE)

