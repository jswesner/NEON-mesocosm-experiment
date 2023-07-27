library(tidyverse)

macro_lw_coeffs <- read_csv("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/neon_size_spectra/data/raw_data/inverts/macro_lw_coeffs.csv") %>% 
  pivot_longer(cols = c(subphylum, class, order, family, genus, taxon),
               names_to = "group",
               values_to = "taxon") %>% 
  group_by(taxon, formula) %>% 
  summarize(a = mean(a),
            b = mean(b)) %>% 
  filter(formula == "M = aL^b") %>% 
  bind_rows(tibble(taxon = "daphnia",   # from Sterner et al https://aslopubs.onlinelibrary.wiley.com/doi/pdf/10.4319/lo.1993.38.4.0857
                   a = -2.7,
                   b = 2.57)) %>% 
  write_csv(., file = "data/macro_lw_coeffs")
