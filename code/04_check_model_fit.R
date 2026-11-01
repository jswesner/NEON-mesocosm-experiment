library(tidyverse)
library(brms) 
library(isdbayes)
library(janitor)
library(tidybayes)

theme_set(brms::theme_default(base_family = "Arial"))

# load models
fit_a = readRDS("models/fit_isd_morin_gpp_predictor.rds")
# fit_b = readRDS("models/fit_isd_morin_gpp_predictor_additive28th.rds")
# fit_c = readRDS("models/fit_isd_morin_gpp_predictor_additive22th.rds")
# fit_d = readRDS("models/fit_isd_temponly.rds")
# fit_e = readRDS("models/fit_isd_interceptonly.rds")

treatments = readRDS(file = "data/treatments.rds") # update treatment names
dat = fit_a$data2 |> 
  left_join(treatments)

# pp_checks
pp_check(fit_a, type = "dens_overlay_grouped", group = "tank") + scale_x_log10()
# pp_check(fit_b, type = "dens_overlay_grouped", group = "tank") + scale_x_log10()
# pp_check(fit_c, type = "dens_overlay_grouped", group = "tank") + scale_x_log10()
# pp_check(fit_d, type = "dens_overlay_grouped", group = "tank") + scale_x_log10()
# pp_check(fit_e, type = "dens_overlay_grouped", group = "tank") + scale_x_log10()

post_pred_data = pp_check(fit_a, type = "dens_overlay_grouped", group = "tank")@data |> 
  mutate(group = parse_number(as.character(group))) |> 
  rename(tank = group) |> 
  left_join(dat |> ungroup() |> distinct(tank, temp_treat, temp_plus)) |> 
  mutate(tank_fix = case_when(temp_treat == "heated" ~ tank - 1, T ~ tank))


pp_check_plot = post_pred_data |> 
  ggplot(aes(x = value))  + 
  geom_density(aes(group = rep_id, color = is_y_label)) +
  scale_color_manual(values = c("#011f4b", "#d1e1ec"),
                     labels = scales::label_parse()) +
  facet_grid(tank_fix ~ temp_plus) +
  scale_x_log10() +
  labs(color = "",
       x = "mgDM") +
  theme(strip.text.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())
  
ggsave(pp_check_plot, file = "plots/pp_check_plot.jpg", width = 6.5, height = 9)



# bayes_p

mod_list = list(fit_a = fit_a
                # ,
                # fit_b = fit_b,
                # fit_c = fit_c,
                # fit_d = fit_d,
                # fit_e = fit_e
                )

preds_list = list()
raw_list = list()
bayes_p_list = list()


for(i in 1:length(mod_list)){
  
  ndraws = 500
  mod_name = names(mod_list)[[i]]
  
  epreds = mod_list[[i]]$data %>% 
    select(-dw_mg) |> 
    distinct() %>% 
    add_epred_draws(mod_list[[i]], ndraws = ndraws)
  
  preds = epreds %>% 
    left_join(mod_list[[i]]$data %>% group_by(tank) %>% mutate(individual_id = row_number()) %>% 
                select(tank, individual_id), relationship = "many-to-many") %>% 
    ungroup %>% 
    mutate(yrep = rparetocounts(nrow(.), lambda = .epred, xmin = xmin, xmax = xmax)) 
  
  preds_list[[i]] = preds %>% 
    group_by(tank, .draw, 
             across(any_of("temp_treat")), 
             across(any_of("june_22_gpp_s")), 
             across(any_of("june_28_gpp_s"))) %>% 
    reframe(gm_preds = exp(mean(log(yrep)))) %>% 
    mutate(mod_name = mod_name)
  
  raw_list[[i]] = mod_list[[i]]$data %>% 
    group_by(tank) %>% 
    reframe(gm_raw = exp(mean(log(dw_mg)))) %>% 
    mutate(mod_name = mod_name)
  
  
  bayes_p_list[[i]] = preds_list[[i]] %>% 
    left_join(raw_list[[i]]) %>% 
    mutate(diff_gm = gm_preds - gm_raw) %>% 
    group_by(tank, across(any_of("temp_treat")), across(any_of("june_22_gpp_s")), across(any_of("june_28_gpp_s"))) %>% 
    reframe(bayes_p_gm = sum(diff_gm<0)/ndraws) %>% 
    mutate(mod_name = mod_name)
  
}


raw_gm = bind_rows(raw_list) %>% 
  rename(gm_preds = gm_raw)  |> 
  left_join(dat |> ungroup() |> distinct(tank, temp_treat, temp_plus))

post_pred_gm = bind_rows(preds_list)  |> 
  left_join(dat |> ungroup() |> distinct(tank, temp_treat, temp_plus)) |> 
  ggplot(aes(x = reorder(tank, gm_preds), y = gm_preds)) +
  geom_jitter(width = 0.2, shape = ".", aes(color = "Posterior Predictive")) +
  geom_point(data = raw_gm, aes(color = "Raw Data")) +
  scale_y_log10() +
  facet_wrap(~temp_plus, scales = "free_x") +
  labs(y = "Geometric mean body size (mgDM)",
       x = "Mesocosm",
       color = "") +
  scale_color_manual(values = c("#d1e1ec", "#011f4b"),
                     guide = guide_legend(override.aes = list(size = 2, shape = 16))) +
  theme(legend.position = "top",
        legend.text = element_text(size = 9))

ggsave(post_pred_gm, file = "plots/post_pred_gm.jpg", width = 6.5, height = 6)



