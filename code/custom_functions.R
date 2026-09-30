library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)
library(ggthemes)
theme_set(theme_default())

make_isd_plots = function(model){
  isd_preds_bytank = model$data %>% 
    group_by(tank) %>% 
    mutate(xmin = min(dw_mg), xmax = max(dw_mg)) %>%
    distinct(temp_treat, june_28_gpp_s, june_22_gpp_s, xmin, xmax, tank) %>% 
    mutate(counts = 1) %>% 
    add_epred_draws(model, re_formula = NULL)
  
  isd_lines_bytank = isd_preds_bytank %>% 
    group_by(temp_treat, june_28_gpp_s, june_22_gpp_s, xmin, xmax, tank) %>% 
    median_qi(.epred) %>%
    select(-.width, -.point, -.interval) %>% 
    pivot_longer(cols = starts_with("."),
                 values_to = "lambda") %>% 
    expand_grid(interval = 1:30) %>% 
    group_by(name, temp_treat, june_28_gpp_s, june_22_gpp_s, tank) %>% 
    # mutate(x = seq(min(xmin), max(xmax), length.out = max(interval))) %>%
    mutate(x = xmin + (xmax - xmin) * (seq(0, 1, length.out = max(interval)))^10) %>%
    mutate(prob_y = pparetocounts(x = x, xmin = xmin, xmax = xmax, lambda = lambda)) %>%
    ungroup %>% 
    left_join(dw_fixed %>% group_by(tank) %>% reframe(max_y = max(row_number()))) %>% 
    left_join(renumber)
  
  raw_data = model$data %>% 
    arrange(tank, -dw_mg) %>% 
    group_by(tank) %>% 
    mutate(y = row_number()/max(row_number())) %>% 
    left_join(renumber)
  
  isd_lines_bytank %>% 
    select(-interval, -lambda) %>% 
    pivot_wider(names_from = name, values_from = prob_y) %>% 
    mutate(treatment = paste(temp_treat, june_28_gpp_s, june_22_gpp_s, sep = '_')) %>% 
    ggplot(aes(x = x, y = .epred)) +
    geom_point(data = raw_data, aes(x = dw_mg, y = y, color = temp_nut_plus),
               shape = 1, alpha = 0.3, 
               size = 0.6) +
    geom_lineribbon(aes(group = interaction(temp_treat, june_28_gpp_s, june_22_gpp_s, tank),
                        ymin = .lower, ymax = .upper), 
                    linewidth = 0.2, alpha = 0.9) +
    ggh4x::facet_grid2(temp_nut_plus~new_tank) +
    scale_x_log10() +
    # scale_y_log10() +
    # coord_cartesian(ylim = c(0.5e-02, NA)) +
    scale_color_colorblind() +
    guides(color = "none") +
    NULL
}



# effect sizes and effect probs -------------------------------------------

# get probabilities of treatment a - treatment b
get_effect_sizes = function(model = NA, re_formula = NA){
  # global mean lambda
  
  posts = model$data %>% 
    select(-dw_mg, -tank, -xmin, -xmax) %>% 
    mutate(xmin = min(model$data$xmin),
           xmax = max(model$data$xmax)) %>% 
    distinct() %>% 
    add_epred_draws(model, re_formula = re_formula)
  
  global_mean_lambda = posts %>% 
    group_by(.draw) %>% 
    reframe(.epred = mean(.epred)) %>% 
    # median_qi(.epred) %>% 
    mutate(metric = "global_mean_lambda",
           model_name = model$name)
  
  # heat effect size
  effect_heat_ambient = posts %>% 
    ungroup %>% 
    select(-.row, -.chain, -.iteration, -xmin, -xmax, -counts) %>%
    pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
    mutate(.epred = (heated - ambient)) %>% 
    group_by(june_28_gpp_s, june_22_gpp_s) %>% 
    # median_qi(.epred) %>% 
    mutate(metric = "effect_size = heat > ambient",
           model_name = model$name)
  
  # nutrient effect size
  effect_nutrient_ambient = effect_nutrient_ambient = posts %>% 
    ungroup %>% 
    select(-.row, -.chain, -.iteration, -xmin, -xmax, -counts) %>%
    pivot_wider(names_from = june_28_gpp_s, june_22_gpp_s, values_from = .epred) %>% 
    mutate(.epred = (nutrient - ambient)) %>% 
    group_by(temp_treat) %>% 
    # median_qi(.epred) %>% 
    mutate(metric = "effect_size = nutrient > ambient",
           model_name = model$name)
  
  
  list(global_mean_lambda,
       effect_heat_ambient,
       effect_nutrient_ambient)
  
}

# get probabilities of treatment > treatment b
get_lambda_probs = function(model = model, re_formula = NA){
  
  
  posts = model$data %>% 
    select(-dw_mg, -tank, -xmin, -xmax) %>% 
    mutate(xmin = min(model$data$xmin),
           xmax = max(model$data$xmax)) %>% 
    distinct() %>% 
    add_epred_draws(model, re_formula = re_formula)
  
  # prob of heat > ambient
  prob_heat_ambient = posts %>% 
    ungroup %>% 
    select(-.row, -.chain, -.iteration, -xmin, -xmax, -counts) %>%
    pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
    mutate(.epred = (heated - ambient)) %>% 
    group_by(june_28_gpp_s, june_22_gpp_s) %>% 
    reframe(prob = sum(.epred>0)/max(.draw)) %>% 
    mutate(metric = "Prob = heat > ambient",
           model_name = model$name)
  
  # prob of nutrient > ambient
  prob_nutrient_ambient = posts %>% 
    ungroup %>% 
    select(-.row, -.chain, -.iteration, -xmin, -xmax, -counts) %>%
    pivot_wider(names_from = june_28_gpp_s, june_22_gpp_s, values_from = .epred) %>% 
    mutate(.epred = (nutrient - ambient)) %>% 
    group_by(temp_treat) %>% 
    reframe(prob = sum(.epred>0)/max(.draw)) %>% 
    mutate(metric = "Prob = nutrient > ambient",
           model_name = model$name)
  
  list(prob_heat_ambient,
       prob_nutrient_ambient)
}

