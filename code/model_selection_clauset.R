library(tidybayes)
library(tidyverse)
library(brms)
library(isdbayes)

source("code/custom_functions/isdbayes_model_selection.R")

# load data and models -----------------------------------------
# landreth_fishmacros_data = readRDS(file = "data/landreth_fishmacros_data.rds")

# load models
fit_a = readRDS("models/fit_isd_clauset_gpp_predictor.rds")
fit_b = readRDS("models/fit_isd_clauset_gpp_predictor_additive28th.rds")
fit_c = readRDS("models/fit_isd_clauset_gpp_predictor_additive22th.rds")
fit_d = readRDS("models/fit_isd_temponly_clauset.rds")
fit_e = readRDS("models/fit_isd_interceptonly_clauset.rds")

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

# saveRDS(model_list, file = "posteriors/model_list.rds")

waic_list = lapply(model_list, brms::waic)

waics = tibble(mods = letters[c(1, 2, 3, 4, 5)],
               waic = sapply(waic_list, function(x) x$estimate[[3]]),
               waic_se = sapply(waic_list, function(x) x$estimate[[6]]))

waics |> 
  ggplot(aes(x = mods, y = waic, ymin = waic - waic_se, ymax = waic + waic_se)) +
  geom_pointinterval()
