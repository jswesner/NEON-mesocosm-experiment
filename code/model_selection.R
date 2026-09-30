library(tidybayes)
library(tidyverse)
library(brms)
library(isdbayes)

source("code/custom_functions/isdbayes_model_selection.R")

# load data and models -----------------------------------------
# landreth_fishmacros_data = readRDS(file = "data/landreth_fishmacros_data.rds")

# load models
fit_a = readRDS("models/fit_isd_morin_gpp_predictor.rds")
fit_b = readRDS("models/fit_isd_morin_gpp_predictor_additive28th.rds")
fit_c = readRDS("models/fit_isd_morin_gpp_predictor_additive22th.rds")
fit_d = readRDS("models/fit_isd_temponly.rds")
fit_e = readRDS("models/fit_isd_interceptonly.rds")

model_list = list(fit_a = fit_a,
                fit_b = fit_b,
                fit_c = fit_c,
                fit_d = fit_d,
                fit_e = fit_e)


# get waic for intercept only

for(i in 1:length(model_list)){
  model_waic = get_waic_isd(model_list[[i]], nsamples = 1000, ndraws = 500, resp = "dw_mg") %>% 
    mutate(model_name = names(model_list[i]),
           model_formula = as.character(model_list[[i]]$formula)[1])
  
  model_list[[i]]$criteria = model_waic
}

saveRDS(model_list, file = "posteriors/model_list.rds")

waic_list = lapply(model_list, brms::waic)

waics = tibble(mods = letters[c(1, 2, 3, 4, 5)],
               waic = sapply(waic_list, function(x) x$estimate[[3]]),
               waic_se = sapply(waic_list, function(x) x$estimate[[6]]))

waics |> 
  ggplot(aes(x = mods, y = waic, ymin = waic - waic_se, ymax = waic + waic_se)) +
  geom_pointinterval()


# posterior averaging -----------------------------------------------------
model_list = readRDS(file = "posteriors/model_list.rds")

# filter to only subset of models
# filtered_model_names <- names(model_list)[!grepl("PC", names(model_list), ignore.case = TRUE)]
filtered_model_names <- names(model_list)[grepl("agtopo|trophic|intercept", names(model_list), ignore.case = TRUE)]
filtered_model_list <- model_list[filtered_model_names]
saveRDS(filtered_model_list, file = "models/temporary/filtered_model_list.rds")

# get model weights (adpated from https://github.com/paul-buerkner/brms/blob/master/R/model_weights.R)
waic_values = numeric(length(filtered_model_list)) 

for(i in 1:length(filtered_model_list)) waic_values[i] = filtered_model_list[[i]]$criteria$waic

se_values = numeric(length(filtered_model_list))
for(i in 1:length(filtered_model_list)) se_values[i] = filtered_model_list[[i]]$criteria$waic_se
min_waic <- min(waic_values)
adjusted_diffs <- (waic_values - min_waic) / se_values
weights <- exp(-adjusted_diffs / 2)
model_weights <- weights / sum(weights)  # Normalize to sum to 1

waic_weights = tibble(waic = waic_values, model_weights = model_weights, model = names(filtered_model_list))

# add model weights to data2 in models
for(i in 1:length(filtered_model_list)) filtered_model_list[[i]]$data2 = list(model_weight = model_weights[i])

model_posts = list()

for(i in 1:length(filtered_model_list)){
  model_posts[[i]] = as_draws_df(filtered_model_list[[i]]) %>% 
    mutate(model = names(filtered_model_list[i]),
           weight = filtered_model_list[[i]]$data2$model_weight)
}

post_average_parameters = bind_rows(model_posts) %>% 
  # filter(.draw <= 1000) %>% 
  select(.draw, starts_with("b_"), model, weight) %>% 
  mutate(across(starts_with("b_"), ~ replace_na(.x, 0))) %>%
  group_by(.draw) %>% 
  summarize(across(starts_with("b_"), ~ weighted.mean(.x, weight))) %>% 
  mutate(data = "fish + macros")

saveRDS(post_average_parameters, file = "posteriors/post_average_parameters.rds")

post_average_parameters_all = bind_rows(model_posts) %>% 
  # filter(.draw <= 1000) %>% 
  select(.draw, starts_with("b_"), starts_with("sd_"), starts_with("r_"), model, weight) %>% 
  mutate(across(starts_with("b_"), ~ replace_na(.x, 0))) %>%
  mutate(across(starts_with("sd_"), ~ replace_na(.x, 0))) %>% 
  mutate(across(starts_with("r_"), ~ replace_na(.x, 0))) %>%
  group_by(.draw) %>% 
  summarize(across(starts_with("b_"), ~ weighted.mean(.x, weight)),
            across(starts_with("sd_"), ~ weighted.mean(.x, weight)),
            across(starts_with("r_"), ~ weighted.mean(.x, weight))) %>% 
  mutate(data = "fish + macros")


saveRDS(post_average_parameters_all, file = "posteriors/post_average_parameters_all.rds")

# plot 
post_average_parameters %>%
  select(-b_Intercept) %>% 
  pivot_longer(starts_with("b_")) %>% 
  group_by(name) %>% 
  mutate(median_value = median(value), na.rm = T) %>% 
  ggplot(aes(y = reorder(name, median_value), x = value, color = median_value)) + 
  stat_pointinterval() +
  geom_vline(xintercept = 0)

