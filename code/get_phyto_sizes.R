# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("flowCore")

library(flowCore)
library(tidyverse)


filenames_fcs <- list.files("data/phytoplankton/", pattern = "*.fcs", recursive = T)

# empty object to put data into
data_fcs = NULL

# add files to empty object
for(i in seq_along(filenames_fcs)){
  data_fcs[[i]] = read.FCS(paste0("data/phytoplankton/", filenames_fcs[i]))
}

#
get_sizes = function(x){
  tibble(micron = x@exprs[,1]) %>% 
    mutate(file = x@description$GUID)
}

# create the body size dataset
sizes = bind_rows(lapply(data_fcs, FUN = get_sizes))

