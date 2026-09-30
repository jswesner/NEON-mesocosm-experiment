library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)
library(ggthemes)
library(brms)
library(ggpubfigs)
theme_set(ggpubfigs::theme_simple())

# load model
fit_isd = readRDS("models/fit_isd_morin.rds")

treatments = fit_isd$data %>% distinct(temp_treat, nutrient_treat) %>% 
  mutate(temp_plus = case_when(temp_treat == "heated" ~ "+heat", T ~ "-heat"),
         nut_plus = case_when(nutrient_treat == "ambient" ~ "-nutrients", T ~ "+nutrients"),
         temp_nut_plus = paste0(temp_plus, " | ", nut_plus))

saveRDS(treatments, file = "data/treatments.rds")


mod = fit_isd  
# posts individual tanks ---------------------------------
post_dots = mod$data %>% 
  select(-dw_mg) %>% 
  distinct()  %>% 
  left_join(treatments) %>% 
  mutate(nut_jitter = case_when(nut_plus == "+nutrients" ~ 0.05, T ~ -0.05)) %>% 
  mutate(heat_01 = case_when(temp_plus == "+heat" ~ 1 + nut_jitter, TRUE ~ 0 + nut_jitter)) %>% 
  add_epred_draws(mod, re_formula = NULL)

# posts treatments ---------------------------------
post_lines = mod$data %>% 
  select(-dw_mg, -tank) %>% 
  distinct() %>% 
  add_epred_draws(mod, re_formula = NA) %>% 
  group_by(temp_treat, nutrient_treat, .draw) %>% 
  reframe(.epred = mean(.epred))  %>% # average over different xmins and xmaxs
  left_join(treatments) %>% 
  mutate(nut_jitter = case_when(nut_plus == "+nutrients" ~ 0.05, T ~ -0.05)) %>% 
  mutate(heat_01 = case_when(temp_plus == "+heat" ~ 1 + nut_jitter, TRUE ~ 0 + nut_jitter))


# plot effect sizes and different model structures ------------------------

all_effects_bind = readRDS(file = "posteriors/all_effect_bind.rds") %>% # made in "code/summaries.R"
  mutate(comparison = case_when(metric == "effect_size = heat > ambient" & group == "ambient" ~ "heat effect: no nutrients",
                                metric == "effect_size = heat > ambient" & group == "nutrient" ~ "heat effect: nutrients",
                                metric == "effect_size = nutrient > ambient" & group == "ambient" ~ "nutrient effect: no heat",
                                T ~ "nutrient effect: heat"))


comparison_labels = all_effects_bind %>% ungroup %>% distinct(model_name) %>% 
  mutate(comparison = "nutrient effect: no heat",
         model_name = case_when(model_name == "fit_isd.rds" ~ "Main model in text",
                                model_name == "fit_isd_binmin.rds" ~ "xmin as bin mode",
                                model_name == "fit_isd_clauset_no1012.rds" ~ "Without tanks 10/12",
                                T ~ "Raw xmin for tanks 10/12"),
         .epred = 0.45)
                
compare_models = all_effects_bind %>% 
  filter(metric != "global_mean_lambda") %>% 
  ggplot(aes(y = comparison, x = .epred)) +
  stat_slab(aes(fill = model_name, alpha = model_name),
            position = position_dodge(width = -0.4)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  scale_alpha_manual(values = c(0.8, 0.5, 0.5, 0.5)) +
  labs(y = "Comparison",
       x = "Effect Size") +
  guides(fill = "none",
         alpha = "none", color = "none") +
  geom_text(data = comparison_labels, aes(label = model_name, color = model_name),
            position = position_dodge2(width = 0.4, padding = 0.2, reverse = T), hjust = 0,
            size = 2.2)


ggsave(compare_models, file = "plots/compare_models.jpg", width = 6, height = 6, dpi = 400)




# plots ---------------------------------

plot_epreds = post_dots %>%
  group_by(tank, temp_nut_plus, temp_treat, nutrient_treat, nut_plus, temp_plus) %>%
  median_qi(.epred) %>%
  ggplot(aes(x = nut_plus, y = .epred)) +
  geom_point(shape = 1,
             size = 0.5
             # aes(color = nut_plus)
  ) +
  stat_pointinterval(data = post_lines, aes(color = nut_plus), alpha = 0.5) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  facet_wrap(~temp_plus) +
  geom_line(data = post_lines %>% filter(.draw <= 1000),
            aes(group = .draw, color = nut_plus),
            alpha =0.01) +
  guides(color = "none") +
  labs(x = "",
       y = "\u03bb") +
  NULL


ggsave(plot_epreds, file = "plots/plot_epreds.jpg", width = 4.5, height = 3, dpi = 400)



# alternate arrangement
plot_epreds_alternate = post_dots %>%
  group_by(tank, temp_nut_plus, temp_treat, nutrient_treat, nut_plus, temp_plus, heat_01) %>%
  median_qi(.epred) %>%
  ggplot(aes(x = heat_01, y = .epred)) +
  stat_pointinterval(data = post_lines, aes(color = nut_plus, shape = nut_plus)) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  # facet_wrap(~nut_plus) +
  geom_line(data = post_lines,
            aes(group = interaction(.draw, nut_plus), color = nut_plus),
            alpha =0.01, linewidth = 0.1) +
  geom_line(data = post_lines %>% group_by(nut_plus, temp_plus, heat_01) %>% 
              median_qi(.epred) ,
            aes(color = nut_plus)) +
  geom_point(aes(shape = nut_plus, color = nut_plus),
             position = position_jitter(width = 0.01)) +
  guides(shape = "none", 
         color = "none") +
  labs(x = "",
       y = "\u03bb",
       color = "") +
  scale_shape_manual(values = c(1, 2)) +
  scale_x_continuous(limits = c(-0.6, 1.6),
                     breaks = c(0, 1),
                     labels = c("-heat", "+heat")) +
  NULL

ggsave(plot_epreds_alternate, file = "plots/plot_epreds_alternate.jpg", width = 5, height = 5, dpi = 400)



# compare tanks 10/12 -----------------------------------------------------

fit_isd_old1012 = readRDS(file = "models/fit_isd_clauset_old1012.rds")
fit_isd_no1012 = readRDS("models/fit_isd_clauset_no1012.rds")
fit_isd = readRDS("models/fit_isd_clauset.rds")

conditional_effects(fit_isd_old1012, effect = "temp_treat:nutrient_treat")
conditional_effects(fit_isd_no1012, effect = "temp_treat:nutrient_treat")
conditional_effects(fit_isd, effect = "temp_treat:nutrient_treat")

# summaries ---------------------------------------------------------------

# range of lambda across tanks
post_dots %>% 
  group_by(tank, temp_treat, nutrient_treat) %>% 
  median_qi(.epred) %>% 
  filter(.epred == max(.epred)|.epred == min(.epred))

# grand median lambda
post_lines %>% 
  median_qi(.epred)

# lambda by treatment
post_lines %>% 
  group_by(temp_treat, nutrient_treat) %>% 
  median_qi(.epred)

# grand nutrient treatment effect, averaged over temperature
post_lines %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01)) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  median_qi(diff)

# grand nutrient treatment probabilities, averaged over temperature
post_lines  %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01)) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  group_by(.draw) %>% 
  reframe(diff = mean(diff)) %>% 
  reframe(prob_greater = sum(diff >0)/max(.draw))

# nutrient treatment effect at each temperature
post_lines  %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01))%>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  group_by(temp_treat) %>% 
  median_qi(diff)

# prob of nutrient treatment effect at each temperature
post_lines %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01))%>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  group_by(temp_treat) %>% 
  reframe(prob_greater = sum(diff >0)/max(.draw))

# Does the nutrient effect differ by temperature?
post_lines  %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01)) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  select(temp_treat, .draw, diff) %>% 
  pivot_wider(names_from = temp_treat, values_from = diff) %>% 
  mutate(diff = heated - ambient) %>% 
  median_qi(diff)

post_lines  %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01)) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = .epred) %>% 
  mutate(diff = nutrient - ambient) %>% 
  select(temp_treat, .draw, diff) %>% 
  pivot_wider(names_from = temp_treat, values_from = diff) %>% 
  mutate(diff = heated - ambient) %>% 
  reframe(prob_greater = sum(diff > 0)/max(.draw))

# What is the grand temperature effect, averaged across nutrients
post_lines  %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01)) %>% 
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = heated - ambient) %>% 
  median_qi(diff)

post_lines %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01)) %>% 
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = heated - ambient)  %>% 
  reframe(prob_greater = sum(diff > 0)/max(.draw))

# What is the grand temperature effect at each level of nutrients
post_lines %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01)) %>% 
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = heated - ambient) %>% 
  group_by(nutrient_treat) %>% 
  median_qi(diff)

# What is the prob of temperature effect at effect at each level of nutrients
post_lines %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01)) %>% 
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = heated - ambient) %>% 
  group_by(nutrient_treat) %>% 
  reframe(prob_greater = sum(diff > 0)/max(.draw))

# Does the temperature effect differ by nutrients
post_lines %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01)) %>% 
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = heated - ambient) %>% 
  select(nutrient_treat, .draw, diff) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = diff) %>% 
  mutate(diff = nutrient - ambient) %>% 
  median_qi(diff)

# Does the temperature effect differ by nutrients
post_lines %>% 
  select(c(-ends_with("plus"), -nut_jitter, -heat_01)) %>% 
  pivot_wider(names_from = temp_treat, values_from = .epred) %>% 
  mutate(diff = heated - ambient) %>% 
  select(nutrient_treat, .draw, diff) %>% 
  pivot_wider(names_from = nutrient_treat, values_from = diff) %>% 
  mutate(diff = nutrient - ambient) %>% 
  reframe(prob_greater = sum(diff > 0)/max(.draw))

