library(tidyverse)


# load data ---------------------------------------------------------------
macro_lw_coeffs = read_csv("data/macro_lw_coeffs.csv") # published length-weight coefficients

lengths_fixed = read_csv("data/lengths_fixed.csv")

# convert mm length to mg mass
dw_raw = lengths_fixed %>% 
  filter(correct_stage != "adult") %>% 
  mutate(taxon = str_to_sentence(taxon)) %>% 
  left_join(macro_lw_coeffs) %>% 
  mutate(dw_mg = a*length_mm^b) 
  
write_csv(dw_raw, file = "data/dw_raw.csv")


