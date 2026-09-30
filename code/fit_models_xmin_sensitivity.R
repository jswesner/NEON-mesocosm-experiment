library(tidyverse)
library(tidybayes)
library(brms) 
library(isdbayes)
library(poweRlaw)
library(janitor)


# refit isd_morin ---------------------------------------------------------

fit_isd_morin = readRDS(file = "models/fit_isd_morin.rds")
mods_list = list()
dat_unculled = readRDS(file = "data/dw_fixed.rds")
mod_dummy = readRDS(file = "models/mod_dummy.rds")


for(i in 1:5){
  
  length_multiplier = c(2, 5, 10, 15, 20)
  mesh_cut_mm = 0.25
  length_cutoff_mm = mesh_cut_mm*length_multiplier[[i]]
  
  dat_cut = dat_unculled %>% 
    filter(length_mm >= length_cutoff_mm) %>%
    mutate(length_cutoff = length_cutoff_mm,
           length_multiplier = length_multiplier[[i]]) %>% 
    group_by(tank) %>% 
    mutate(xmin = min(dw_mg),
           xmax = max(dw_mg)) 
  
  
  mods_list[[i]] = update(fit_isd_morin, newdata = dat_cut, chains = 1, iter = 1000)
  
}

saveRDS(mods_list, file = "models/mods_list_fullmod.rds")

mods_list_fullmod = readRDS("models/mods_list_fullmod.rds")

posts_fullmod = list()

for(i in 1:length(mods_list_fullmod)){
  dat = mods_list_fullmod[[i]]$data
  posts_fullmod[[i]] = dat %>% distinct(temp_treat, nutrient_treat) %>% 
    mutate(ith = i, counts = 1, xmin = 0.01, xmax = 1000) %>% 
    add_epred_draws(mods_list_fullmod[[i]], re_formula = NA)
}

bind_rows(posts_fullmod) %>% 
  left_join(tibble(ith = 1:5,
                   length_multiplier = c(2, 5, 10, 15, 20))) %>% 
  ggplot(aes(x = temp_treat, color = nutrient_treat,
             y = .epred)) +
  stat_pointinterval(position = position_dodge(width = 0.5)) +
  facet_wrap(~length_multiplier, nrow = 1)



# get n
mods_n_list = list()

for(i in 1:length(mods_list)){
  mods_n_list[[i]] = mods_list[[i]]$data %>% group_by(tank, temp_treat, nutrient_treat) %>%
    tally() %>% 
    mutate(ith = i) %>%  
    left_join(tibble(ith = 1:5,
                     length_multiplier = c(2, 5, 10, 15, 20))) 
}

mods_n_list %>% bind_rows %>% 
  ggplot(aes(x = tank, y = n, color = as.factor(length_multiplier))) +
  geom_point() +
  facet_wrap(~length_multiplier, nrow = 1)

mods_n_list %>% bind_rows %>% 
  group_by(length_multiplier) %>% 
  filter(n <= 50)

# fit individual samples --------------------------------------------------


mean_multiplier <- 10
sd_multiplier <- 1.5

shape <- (mean_multiplier / sd_multiplier)^2
scale <- sd_multiplier^2 / mean_multiplier
hist(rgamma(10000, shape = shape, scale = scale))
mean(rgamma(10000, shape = shape, scale = scale))

for(i in 1:100){
  
  length_multiplier = c(2, 5, 10, 15, 20)
  mesh_cut_mm = 0.25
  length_cutoff_mm = mesh_cut_mm*length_multiplier
  
  dat_cut = dat_unculled %>% 
    filter(length_mm >= length_cutoff_mm) %>%
    mutate(length_cutoff = length_cutoff_mm,
           length_multiplier = length_multiplier) %>% 
    group_by(tank) %>% 
    mutate(xmin = min(dw_mg),
           xmax = max(dw_mg)) %>% 
    group_split()
  
  
  mods_cut <- lapply(seq_along(dat_cut), function(i) {
    
    cat("Fitting model", i, "of", length(dat_cut), "\n")
    
    mod_tmp <- mod_dummy
    
    fit <- update(
      mod_tmp,
      newdata = dat_cut[[i]],
      data2 = dat_cut[[i]],
      iter = 1000,
      chains = 1
    )
    
    fit$cut_iteration <- i
    fit
  })
  
  mods_list[[i]] = mods_cut
  
}

saveRDS(unlist(mods_list, recursive = F), file = "models/mods_list_uniform.rds")

mods_list = readRDS(file = "models/mods_list_uniform.rds")

get_summaries = function(model = NULL){
  summary(model)$fixed %>% 
    as_tibble() %>% 
    merge(model$data2 %>% distinct(tank, length_cutoff, xmin, xmax, treatment, nutrient_treat, temp_treat)) %>% 
    mutate(n = nrow(model$data2))
}

mods_summaries = lapply(mods_list, get_summaries)

saveRDS(mods_summaries, file = "posteriors/mods_summaries_uniform.rds")

bind_rows(mods_summaries) %>%
  clean_names() %>% 
  ggplot(aes(x = length_cutoff, y = estimate)) +
  geom_pointrange(aes(ymin = l_95_percent_ci, ymax = u_95_percent_ci),
                  position = position_jitter(width = 0.1)) +
  facet_wrap(~treatment)

bind_rows(mods_summaries) %>%
  clean_names() %>% 
  group_by(tank, treatment) %>% 
  median_qi(estimate) %>%
  group_by(treatment) %>% 
  mutate(grand_median = median(estimate)) %>% 
  ggplot(aes(x = treatment, y = estimate)) +
  geom_pointrange(aes(ymin = .lower, ymax = .upper), 
                  position= position_jitter(width = 0.1, height = 0)) +
  geom_point(aes(y = grand_median, color = "red", size = 2))


mods_list[[2]]
