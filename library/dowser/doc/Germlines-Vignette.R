## ---- eval=FALSE, warning=FALSE, message=FALSE--------------------------------
#  library(dowser)
#  library(dplyr)
#  
#  data(ExampleAirr)
#  
#  # Read in IMGT-gapped sequences
#  references = readIMGT(dir = file.path("germlines", "human", "vdj"))
#  
#  # remove germline alignment columns for this example
#  db = select(ExampleAirr, -"germline_alignment",
#      -"germline_alignment_d_mask")
#  
#  # Reconstruct germline sequences
#  ExampleAirr = createGermlines(db, references, nproc=1)
#  
#  # Check germline of first row
#  ExampleAirr$germline_alignment_d_mask[1]
#  
#  # "CAGGTGCAGCTGGTGGAGTCTGGGGGA...GGCTTGGTCAAGCCTGGAGGGTCCCTGAGACTCTCCTGTGCAGCCTCTGGATTCACCTTC............AGTGACTACTACATGAGCTGGATCCGCCAGGCTCCAGGGAAGGGGCTGGAGTGGGTTTCATACATTAGTAGTAGT......AGTAGTTACACAAACTACGCAGACTCTGTGAAG...GGCCGATTCACCATCTCCAGAGACAACGCCAAGAACTCACTGTATCTGCAAATGAACAGCCTGAGAGCCGAGGACACGGCCGTGTATTACTGTGCGAGAGNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNTGGTTCGACCCCTGGGGCCAGGGAACCCTGGTCACCGTCTCCTCAG"

