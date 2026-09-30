library(tidyverse)
library(brms) 
library(isdbayes)
library(tidybayes)
library(poweRlaw)
# fitting parts are silenced. They may take minutes to hours to refit.
# model isd ---------------------------------------------------------------

dat_clauset_xmins = readRDS(file = "data/dat_clauset_xmins.rds") %>% mutate(xmin_quantile = "gm") # geometric mean of xmin
dat_clauset_xmins_upper = readRDS(file = "data/dat_clauset_xmins_upper.rds") %>% mutate(xmin_quantile = "0.1") # upper 90% CI of xmin
dat_clauset_xmins_lower = readRDS(file = "data/dat_clauset_xmins_lower.rds") %>% mutate(xmin_quantile = "90") # lower 10%CI of xmin
fit_isd = readRDS("models/fit_isd_clauset.rds")
fit_xmin_quantile = readRDS(file = "models/fit_xmin_quantile.rds")

# fit_isd = brm(dw_mg| vreal(counts, xmin, xmax) ~ temp_treat*nutrient_treat +
#                 (1|tank),
#               data = dat_clauset_xmins,
#               stanvars = stanvars,
#               family = paretocounts(),
#               chains = 4, iter = 2000,
#               # file = "models/fit_isd.rds",
#               prior = c(prior(normal(-2, 0.5), class = "Intercept"),
#                         prior(normal(0, 0.2), class = "b")))

# fit_isd = update(fit_isd, newdata = dat_clauset_xmins_upper, chains = 1, iter = 1000)
# saveRDS(fit_isd, file = "models/fit_isd_clauset_upper.rds")

conditional_effects(fit_isd, effects = "temp_treat:nutrient_treat")

fit_isd_clauset_lower = readRDS("models/fit_isd_clauset_lower_nodaphnia.rds")

conditional_effects(fit_isd_clauset_lower, effects = "temp_treat:nutrient_treat")

fit_isd_clauset_upper = readRDS("models/fit_isd_clauset_upper_nodaphnia.rds")

conditional_effects(fit_isd_clauset_upper, effects = "temp_treat:nutrient_treat")

mod = fit_xmin_quantile

posts_lower = mod$data %>% distinct(temp_treat, nutrient_treat) %>%
  mutate(counts = 1,
         xmin = min(mod$data$xmin),
         xmax = max(mod$data$xmax)) %>%
  add_epred_draws(mod, re_formula = NA)

posts_lower_dots = mod$data %>% distinct(temp_treat, nutrient_treat, tank, xmin, xmax) %>%
  mutate(counts = 1) %>%
  add_epred_draws(mod, re_formula = NULL)

posts_lower %>%
  ggplot(aes(x = temp_treat, y = .epred, color = nutrient_treat)) +
  stat_pointinterval(position = position_dodge(width = 0.2)) +
  stat_pointinterval(.width = 0, data = posts_lower_dots, aes(group = tank))

posts_lower %>%
  ungroup %>%
  select(ends_with("_treat"), .draw, .epred) %>%
  pivot_wider(names_from = temp_treat, values_from = .epred) %>%
  mutate(diff = heated - ambient) %>%
  group_by(nutrient_treat) %>%
  median_qi(diff)




# check ks fit ------------------------------------------------------------

bounded_pareto_mle <- function(data, xmin, xmax) {
  # Filter data within bounds
  data <- data[data >= xmin & data <= xmax]
  
  # Negative log-likelihood function
  neg_log_likelihood <- function(alpha) {
    if (alpha <= 0) return(Inf)
    n <- length(data)
    log_const <- log(1 - (xmin / xmax)^alpha)
    ll <- n * log(alpha) + n * alpha * log(xmin) - (alpha + 1) * sum(log(data)) - n * log_const
    return(-ll)  # Return negative log-likelihood
  }
  
  # Optimize to find alpha
  result <- optimize(neg_log_likelihood, interval = c(0.01, 10), maximum = FALSE)
  
  return(list(alpha = result$minimum, logLik = -result$objective))
}




ks_data = list()

xmin_all = bind_rows(dat_clauset_xmins, 
                     dat_clauset_xmins_lower,
                     dat_clauset_xmins_upper)

dat_list = xmin_all %>% group_by(tank, xmin_quantile) %>% group_split()

for(i in 1:length(dat_list)){
  data = dat_list[[i]] %>% 
    pull(dw_mg)
  tank = unique(dat_list[[i]]$tank)
  
  mle <- bounded_pareto_mle(data, xmin = min(data), xmax = max(data))
  alpha <- mle$alpha
  
  # Empirical CDF
  ecdf_data <- ecdf(data)
  xmin = min(data)
  xmax = max(data)
  pareto_cdf <- function(x) {
    1 - ((xmin / x)^alpha - (xmin / xmax)^alpha) / (1 - (xmin / xmax)^alpha)
  }
  
  # KS statistic
  ks = max(abs(ecdf_data(data) - pareto_cdf(data)))
  ks_data[[i]] = tibble(ks = ks, 
                        tank = tank,
                        xmin_quantile = unique(dat_list[[i]]$xmin_quantile))
}


# check ks fit sims ------------------------------------------------------------
dat_unculled = read_csv("data/dw_fixed.csv") %>% 
  filter(dw_mg > 0) %>% 
  group_by(tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) %>% 
  # filter(correct_taxon != "Daphnia") %>% 
  left_join(dat_clauset_xmins %>% distinct(tank, xmin) %>% rename(xmin_clauset = xmin))


# Run bootstrap xmin procedure; silenced b/c it takes ~5 minutes. Completed version is loaded below.
# xmin_list <- list()

dat_list = dat_unculled %>% 
  group_by(tank) %>% 
  group_split()

bounded_pareto_mle <- function(data, xmin, xmax) {
  # Filter data within bounds
  data <- data[data >= xmin & data <= xmax]
  
  # Negative log-likelihood function
  neg_log_likelihood <- function(alpha) {
    if (alpha <= 0) return(Inf)
    n <- length(data)
    log_const <- log(1 - (xmin / xmax)^alpha)
    ll <- n * log(alpha) + n * alpha * log(xmin) - (alpha + 1) * sum(log(data)) - n * log_const
    return(-ll)  # Return negative log-likelihood
  }
  
  # Optimize to find alpha
  result <- optimize(neg_log_likelihood, interval = c(0.01, 10), maximum = FALSE)
  
  return(list(alpha = result$minimum, logLik = -result$objective))
}


ks_data = list()

xmin_all = bind_rows(dat_clauset_xmins, 
                     dat_clauset_xmins_lower,
                     dat_clauset_xmins_upper)

dat_list = xmin_all %>% group_by(tank, xmin_quantile) %>% group_split()

for(i in 1:length(dat_list)){
  data = dat_list[[i]] %>% pull(dw_mg)
  tank = unique(dat_list[[i]]$tank)
  
  mle <- bounded_pareto_mle(data, xmin = min(data), xmax = max(data))
  alpha <- mle$alpha
  
  # Empirical CDF
  ecdf_data <- ecdf(data)
  xmin = min(data)
  xmax = max(data)
  pareto_cdf <- function(x) {
    1 - ((xmin / x)^alpha - (xmin / xmax)^alpha) / (1 - (xmin / xmax)^alpha)
  }
  
  # KS statistic
  ks = max(abs(ecdf_data(data) - pareto_cdf(data)))
  ks_data[[i]] = tibble(ks = ks, 
                        tank = tank,
                        xmin_quantile = unique(dat_list[[i]]$xmin_quantile))
}




bind_rows(ks_data) %>% 
  left_join(dat %>% ungroup %>% distinct(tank, nutrient_treat, temp_treat)) %>% 
  ggplot(aes(x = tank, y = ks, color = xmin_quantile)) +
  geom_point() +
  ylim(0, 0.5) 

ks_list = bind_rows(ks_data) %>% 
  group_by(tank) 


dat_xmin_quantile = xmin_all %>% 
  left_join(ks_list) %>% 
  group_by(tank) %>% 
  filter(ks == min(ks)) 


# fit_xmin_quantile = update(fit_isd_clauset, newdata = dat_xmin_quantile)
# saveRDS(fit_xmin_quantile, file = "models/fit_xmin_quantile.rds")

conditional_effects(fit_xmin_quantile, effects = "temp_treat:nutrient_treat")

fit_xmin_quantile$data %>% 
  group_by(temp_treat, nutrient_treat, tank) %>% 
  tally() %>% 
  arrange(n) %>% 
  left_join(post_dots %>% group_by(temp_treat, nutrient_treat, tank) %>% 
              median_qi(.epred)) %>% 
  ggplot(aes(x = n, y = .epred)) + 
  geom_point()

n_original = fit_isd$data %>% 
  group_by(temp_treat, nutrient_treat, tank) %>% 
  tally() %>% 
  arrange(n) %>% 
  distinct(tank, n) %>% 
  rename(n_original = n)

n_culled = fit_xmin_quantile$data %>% 
  group_by(temp_treat, nutrient_treat, tank) %>% 
  tally() %>% 
  arrange(n)  %>% 
  left_join(posts_lower_dots %>% group_by(temp_treat, nutrient_treat, tank) %>% 
              median_qi(.epred))

n_culled %>% 
  left_join(n_original) %>% 
  mutate(diff = (n_original  - n)/n_original) %>% 
  ggplot(aes(x = diff, y = .epred, color = interaction(temp_treat, nutrient_treat))) +
  geom_point() +
  geom_abline() +
  facet_grid(temp_treat ~ nutrient_treat)


n_culled %>% 
  left_join(n_original) %>% 
  mutate(diff = 1 - (n_original  - n)/n_original) %>% 
  ggplot(aes(x = temp_treat, y = diff)) +
  geom_violin(aes(fill = nutrient_treat)) + 
  geom_point(position = position_dodge(width = 0.8),
             aes(group = nutrient_treat))


# proportion of size range culled -----------------------------------------

dat = read_csv("data/dw_fixed.csv") %>% 
  filter(dw_mg > 0) %>% 
  group_by(tank) %>% 
  # filter(correct_taxon != "Daphnia") %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) 

xmin_list = readRDS(file = "models/xmin_list.rds")

prop_list = list()

for(i in 1:max(bind_rows(xmin_list) %>% pull(rep))){
  
  prop_list[[i]] = dat %>% 
    group_by(tank, temp_treat, nutrient_treat) %>% 
    mutate(xmin = min(dw_mg),
           xmax = max(dw_mg)) %>% 
    select(tank, xmin, xmax) %>% 
    distinct() %>% 
    left_join(bind_rows(xmin_list) %>% group_by(tank) %>% filter(rep == i)) %>%  
    mutate(prop_culled_range_mg = 1 - (xmax - xmin_clauset) / (xmax - xmin))
}

saveRDS(prop_list, file = "data/prop_list.rds")


prop_list = readRDS(file = "data/prop_list.rds")

bind_rows(prop_list) %>% 
  ggplot(aes(x = interaction(temp_treat, nutrient_treat),
             y = prop_culled_range_mg)) + 
  geom_jitter(shape = ".", width = 0.2)


bind_rows(prop_list) %>% 
  group_by(temp_treat, nutrient_treat, tank) %>% 
  reframe(mean_prop = mean(prop_culled_range_mg, na.rm = T),
          gm_prop = exp(mean(log(prop_culled_range_mg)))) %>%
  group_by(temp_treat, nutrient_treat) %>%
  reframe(mean = mean(mean_prop),
          gm_mean = mean(gm_prop),
          sd = sd(mean_prop))
