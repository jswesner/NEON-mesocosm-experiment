library(tidyverse)
library(janitor)
library(tidybayes)

gpp <- lapply(readRDS("data/gppPosteriors.rds"), function(x) x$summary) %>% bind_rows() %>% 
  separate(tank, into = c("tank", "time")) %>% 
  mutate(tank = parse_number(tank),
         date = case_when(time == 1 ~ "2022-06-22", T ~ "2022-06-28")) %>% 
  clean_names() %>% 
  left_join(readRDS(file = "data/dat_clauset_xmins.rds") %>% distinct(tank, nutrient_treat, temp_treat)) %>% 
  as_tibble()

saveRDS(gpp, file = "data/gpp_wrangled.rds")

gpp %>% 
  ggplot(aes(x = interaction(temp_treat, nutrient_treat), y = gpp)) + 
  geom_point() +
  facet_wrap(~date)


fit_isd = readRDS("models/fit_isd_clauset.rds")

# no correlation in gpp between tanks and dates (i.e., gpp varied a lot within tanks), so use both dates as predictors
gpp %>% 
  select(tank, gpp, nutrient_treat, temp_treat, time) %>% 
  pivot_wider(names_from = time, values_from = gpp) %>% 
  ggplot(aes(x = `1`, y = `2`)) + 
  geom_point()

gpp_wide = gpp %>% 
  select(tank, gpp, nutrient_treat, temp_treat, time) %>% 
  pivot_wider(names_from = time, values_from = gpp) %>% 
  mutate(gpp_1_s = scale(`1`),
         gpp_2_s = scale(`2`))

dat_gpp = fit_isd$data %>% left_join(gpp_wide)

fit_isd_gpp = update(fit_isd, formula = . ~ temp_treat * nutrient_treat + gpp_1_s + gpp_2_s + (1 | tank),
                     iter = 1000, chains = 1, newdata = dat_gpp)

saveRDS(fit_isd_gpp, file = "models/fit_isd_gpp.rds")

conditional_effects(fit_isd_gpp)

treatments = fit_isd$data %>% distinct(temp_treat, nutrient_treat) %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus))


# posts individual tanks ---------------------------------
gpp_wide = gpp %>% select(tank, gpp, date) %>% 
  mutate(date = paste0("gpp_", date)) %>% 
  pivot_wider(names_from = date, values_from = gpp)
dw_new = readRDS(file = "data/dat_clauset_xmins.rds")

post_dots = dw_new %>%
  distinct(temp_treat, nutrient_treat, tank,
           xmin, xmax, counts) %>% 
  left_join(gpp_wide) %>% 
  add_epred_draws(fit_isd, re_formula = NULL)

post_dots %>% 
  pivot_longer(cols = starts_with("gpp")) %>% 
  ggplot(aes(x = value, y = .epred, color = name)) + 
  stat_pointinterval() + 
  facet_wrap(temp_treat~nutrient_treat, scales = "free") 
