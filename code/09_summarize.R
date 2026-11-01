library(tidyverse)
library(brms) 
library(isdbayes)
library(janitor)
library(tidybayes)
library(flextable)

# load models
fit_isd = readRDS("models/fit_isd_morin_gpp_predictor.rds")

# load raw (unculled) data
dat = readRDS(file = "data/dw_fixed.rds") %>% 
  filter(dw_mg > 0) %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) %>% 
  filter(correct_taxon != "Daphnia") %>% 
  ungroup

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
  select(correct_taxon2, n, everything()) |> 
  mutate(biomass = n*median_size)

saveRDS(size_table, file = "tables/size_table.rds")
write_csv(size_table, file = 'tables/size_table.csv')

size_table |>
  filter(correct_taxon2 != 'Overall') |> 
  mutate(prop_abund = n/sum(n),
         prop_biomass = biomass/sum(biomass))

# taxa and dragons -------------------------------------------------------------------------
dat = readRDS(file = "data/dw_fixed.rds") %>%
  filter(correct_taxon != "Daphnia") %>%  
  filter(dw_mg > 0) %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) %>% 
  filter(correct_taxon != "Daphnia") %>% 
  ungroup()

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

# plot temperature effect on gpp ----------------------------------------------------
treatments = readRDS("data/treatments.rds")

d = fit_isd$data2 |> 
  left_join(treatments |> distinct(temp_treat, temp_plus, nutrient_treat, nut_plus)) |> 
  ungroup()

days = tibble(date = c("2022-06-22", "2022-06-28"),
              name = c("june_22_gpp", "june_28_gpp"),
              day_of_experiment = paste("Day", julian(as.Date(date)) - julian(as.Date("2022-05-27"))))

gpp_temp = d  |> 
  distinct(tank, june_28_gpp, june_22_gpp, temp_plus) |> 
  pivot_longer(cols = starts_with("june_")) |>
  left_join(days |> select(-date)) |> 
  ggplot(aes(x = temp_plus, y = value, color = temp_plus)) +
  geom_boxplot(aes(width = 0.3)) +
  geom_point() +
  labs(y = expression("Gross production ( mg"~O[2]~L^-1~d^-1~") - Day 32")) +
  facet_wrap(~day_of_experiment) +
  guides(color = "none") +
  scale_color_colorblind()

ggsave(gpp_temp, file = "plots/gpp_temp.jpg", width = 5, height = 5)


# plot gpp correlation ----------------------------------------------------

days = tibble(date = c("2022-06-22", "2022-06-28"),
              day_of_experiment = julian(as.Date(date)) - julian(as.Date("2022-05-27")))

gpp_correlation = fit_isd$data2 |> 
  distinct(tank, june_28_gpp, june_22_gpp) |> 
  ggplot(aes(x = june_22_gpp, y = june_28_gpp)) +
  geom_point() +
  labs(x = expression("Gross production ( mg"~O[2]~L^-1~d^-1~") - Day 26"),
       y = expression("Gross production ( mg"~O[2]~L^-1~d^-1~") - Day 32")) +
  xlim(0, 70) +
  ylim(0, 70) +
  geom_abline(linetype = "dashed")

ggsave(gpp_correlation, file = "plots/gpp_correlation.jpg", width = 5, height = 5)
