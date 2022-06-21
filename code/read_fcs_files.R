library(flowCore)
library(tidyverse)

test_fcs <- read.FCS("data/phytoplankton/2188A22012126FL_BF.fcs")

sizes = tibble(micron = test_fcs@exprs[,1])


plot(sort(sizes$micron))
