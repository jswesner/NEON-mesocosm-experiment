if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("flowCore")

library(flowCore)
library(tidyverse)

# load fcs file
test_fcs <- read.FCS("data/phytoplankton/2023-01-12T15-29-26_exars_2022_01_2/exars_2022_01_2_BF_Dead.fcs", 
                     truncate_max_range = F)

# extract body sizes
sizes = tibble(micron = test_fcs@exprs[,1]) 
