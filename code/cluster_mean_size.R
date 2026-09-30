library(brms)

dat = readRDS(file = "data/dw_fixed.rds") 

brm_mean_larvae = readRDS(file = "models/test_larvae_gamma.rds")

brm_mean_larvae = update(brm_mean_larvae, newdata = dat)

saveRDS(brm_mean_larvae, file = "models/brm_mean_larvae.rds")