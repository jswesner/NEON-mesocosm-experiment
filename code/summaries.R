library(tidyverse)
library(brms) 
library(isdbayes)
library(janitor)
library(tidybayes)
library(flextable)

# load models
fit_isd = readRDS("models/fit_isd_clauset.rds")
brm_mean_larvae = readRDS(file = "models/brm_mean_larvae_rawdata_lognorm.rds")

# load raw (unculled) data
dat = readRDS(file = "data/dw_fixed.rds") %>% ungroup
lengths_fixed <- read_csv("data/lengths_fixed.csv")

size_table = dat %>% 
  mutate(correct_taxon2 = case_when(correct_taxon == "Ceratopogonidae" ~ "Diptera",
                                   correct_taxon == "Chironomidae" ~ "Diptera",
                                   correct_taxon == "Dytiscidae" ~ "Coleoptera",
                                   TRUE ~ correct_taxon))  %>% 
  group_by(correct_taxon2) %>% 
  mutate(min_size = min(dw_mg),
         median_size = median(dw_mg),
         max_size = max(dw_mg)) %>% 
  group_by(correct_taxon2, min_size, median_size, max_size) %>% 
  tally() %>% 
  arrange(-n) %>% 
  ungroup %>% 
  add_row(correct_taxon2 = "Overall",
          n = nrow(dat),
          min_size = min(dat$dw_mg),
          median_size = median(dat$dw_mg),
          max_size = max(dat$dw_mg)) %>% 
  select(correct_taxon2, n, everything())

saveRDS(size_table, file = "tables/size_table.rds")

# taxa and dragons -------------------------------------------------------------------------
dat = readRDS(file = "data/dw_fixed.rds") %>% ungroup

dat %>% 
  group_by(temp_treat, nutrient_treat, correct_taxon, tank) %>%
  tally() %>% 
  filter(correct_taxon == "Odonata") %>% 
  group_by(temp_treat, nutrient_treat) %>% 
  reframe(sum(n))
  
dat %>% 
  filter(correct_taxon == "Odonata") %>% 
  group_by(temp_treat) %>% 
  median_qi(dw_mg) %>% 
  arrange(temp_treat)

emergence = readRDS(file = "data/emergence_wrangled.rds")

emergence



# isd ---------------------------------------------------------------------
as_draws_df(fit_isd) %>% 
  clean_names() %>% 
  select(starts_with("b_")) %>%
  mutate(.draw = row_number()) %>% 
  pivot_longer(cols = -.draw) %>% 
  group_by(name) %>% 
  reframe(prob = sum(value>0)/max(.draw))


fit_isd$data %>% 
  select(-dw_mg) %>% 
  distinct() %>% 
  add_epred_draws(fit_isd, re_formula = NULL) %>% 
  group_by(tank) %>% 
  median_qi(.epred) %>% 
  arrange(.epred)

fit_isd$data %>% 
  select(-dw_mg, -tank, -counts, xmin, xmax) %>% 
  mutate(counts = 1,
         xmin = min(fit_isd$data$xmin),
         xmax = max(fit_isd$data$xmax)) %>% 
  distinct() %>% 
  add_epred_draws(fit_isd, re_formula = NA) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration, -xmin, -xmax, -counts) %>%
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = (heated - ambient)) %>% 
  group_by(nutrient_treat) %>% 
  median_qi(diff)

fit_isd$data %>% 
  select(-dw_mg, -tank, -counts, xmin, xmax) %>% 
  mutate(counts = 1,
         xmin = min(fit_isd$data$xmin),
         xmax = max(fit_isd$data$xmax)) %>% 
  distinct() %>% 
  add_epred_draws(fit_isd, re_formula = NA) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration, -xmin, -xmax, -counts) %>%
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = (nutrient - ambient)) %>% 
  group_by(temp_treat) %>% 
  median_qi(diff)

fit_isd$data %>% 
  select(-dw_mg, -tank, -counts, xmin, xmax) %>% 
  mutate(counts = 1,
         xmin = min(fit_isd$data$xmin),
         xmax = max(fit_isd$data$xmax)) %>% 
  distinct() %>% 
  add_epred_draws(fit_isd, re_formula = NA) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration, -xmin, -xmax, -counts) %>%
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = (heated - ambient)) %>% 
  group_by(nutrient_treat) %>% 
  reframe(prob = sum(diff>0)/max(.draw))

fit_isd$data %>% 
  select(-dw_mg, -tank, -counts, xmin, xmax) %>% 
  mutate(counts = 1,
         xmin = min(fit_isd$data$xmin),
         xmax = max(fit_isd$data$xmax)) %>% 
  distinct() %>% 
  add_epred_draws(fit_isd, re_formula = NA) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration, -xmin, -xmax, -counts) %>%
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = (nutrient - ambient)) %>% 
  group_by(temp_treat) %>% 
  reframe(prob = sum(diff>0)/max(.draw))

fit_isd$data %>% 
  select(-dw_mg, -tank, -counts, xmin, xmax) %>% 
  mutate(counts = 1,
         xmin = min(fit_isd$data$xmin),
         xmax = max(fit_isd$data$xmax)) %>% 
  distinct() %>% 
  add_epred_draws(fit_isd, re_formula = NA) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration, -xmin, -xmax, -counts) %>%
  group_by(nutrient_treat, temp_treat) %>% 
  median_qi(.epred)



# mean sizes --------------------------------------------------------------

brm_mean_larvae = readRDS(file = "models/brm_mean_larvae_rawdata_lognorm.rds")

size_posts = brm_mean_larvae$data %>% distinct(temp_treat, nutrient_treat) %>%
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus)) %>% 
  add_linpred_draws(brm_mean_larvae, re_formula = NA) %>% 
  mutate(.epred = exp(.linpred))

size_medians = size_posts %>% 
  group_by(temp_plus, nut_plus) %>% 
  median_qi(.epred)  %>%
  select(-.width, -.point, -.interval) %>% 
  janitor::adorn_rounding(digits = 2, rounding = "half up")

saveRDS(size_medians, "tables/size_medians.rds")

flextable(size_medians) %>% 
  set_header_labels(temp_plus = "Temperature",
                    nut_plus = "Nutrients",
                    .epred = "Median",
                    .lower = "Low95", 
                    .upper = "Upper95")


