library(tidyverse)
library(tidybayes)
library(brms)
library(ggthemes)
library(viridis)

dw = readRDS(file = "data/dw_fixed.rds") %>% 
  mutate(tank = as.integer(tank))

# load models
# fit_nodaphnia = readRDS(file = "models/fit_nodaphnia_tank.rds")
fit_withdaphnia = readRDS(file = "models/fit_withdaphnia.rds")

newdat = fit_withdaphnia$data %>% group_by(dw_mg, xmin ,xmax, temp_treat, nutrient_treat, tank) %>% 
  reframe(counts = sum(counts))

fit_withdaphnia = update(fit_withdaphnia, newdata = newdat,
                         iter = 2000, chains = 4)
saveRDS(fit_withdaphnia, file = "models/fit_withdaphnia.rds")





fit = fit_withdaphnia
fit_posts = fit$data %>% 
  group_by(temp_treat, nutrient_treat, tank) %>% 
  mutate(xmin = min(dw_mg),
         xmax = max(dw_mg)) %>% 
  distinct(temp_treat, tank, nutrient_treat, xmin, xmax) %>%
  mutate(counts = 1) %>% 
  add_epred_draws(fit, re_formula = NULL) %>% 
  group_by(temp_treat, tank, nutrient_treat, xmin, xmax) %>% 
  median_qi(.epred)
  
fit_posts_list = fit_posts %>% group_by(temp_treat, nutrient_treat, xmin, xmax, tank) %>% 
  group_split()

temp = lapply(fit_posts_list, function(df){
  df %>% expand_grid(x = seq(xmin, xmax, length.out = 50)) %>% 
    mutate(prob_yx = (1 - (x^(.epred + 1) - (xmin^(.epred + 1))) / ((xmax)^(.epred + 1) - (xmin^(.epred + 1)))),
           dw_mg = x,
           prob_yx_lower = (1 - (x^(.lower + 1) - (xmin^(.lower + 1))) / ((xmax)^(.lower + 1) - (xmin^(.lower + 1)))),
           prob_yx_upper = (1 - (x^(.upper + 1) - (xmin^(.upper + 1))) / ((xmax)^(.upper + 1) - (xmin^(.upper + 1))))) 
}
)

nsamples = fit$data %>% group_by(temp_treat, nutrient_treat, tank) %>% 
  tally()


group_ids = dw %>% 
  distinct(tank, treatment) %>% 
  group_by(treatment) %>% 
  arrange(treatment) %>% 
  mutate(id = 1:max(row_number()))

isd_posts = bind_rows(temp) %>% 
  left_join(nsamples) %>% 
  mutate(n_yx = prob_yx*n,
         n_yx_lower = prob_yx_lower*n,
         n_yx_upper = prob_yx_upper*n,
         treatment = paste0(temp_treat,"_", nutrient_treat)) %>% 
  left_join(group_ids)


isd_plot_data = dw %>% 
  # filter(correct_taxon != "Daphnia") %>%
  arrange(treatment, -dw_mg, tank) %>% 
  group_by(treatment, tank) %>% 
  mutate(order = 1:max(row_number())) %>% 
  group_by(correct_taxon) %>% 
  mutate(max_size = round(max(dw_mg, 0)),
         correct_taxon = as.factor(correct_taxon),
         correct_taxon = fct_reorder(correct_taxon, max_size),
         correct_taxon = paste0(max_size, "_", correct_taxon)) %>% 
  left_join(group_ids)
         
  
isd_panel_plot = isd_plot_data %>% 
  ggplot(aes(x = dw_mg, group = tank)) +
  geom_point(aes(size = dw_mg, 
                 y = order,
                 color = treatment),
             shape = 1,
             alpha = 0.5) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10() +
  facet_grid(treatment ~ id) +
  geom_line(data = isd_posts, aes(y = n_yx)) +
  geom_ribbon(data = isd_posts, aes(ymin = n_yx_lower, ymax = n_yx_upper),
              alpha = 0.4) +
  coord_cartesian(ylim = c(1, NA)) +
  scale_color_colorblind() +
  scale_size_continuous(breaks = c(0.01, 0.1, 1, 10, 30)) +
  theme_default() +
  guides(color = "none") +
  labs(x = "mgDM Individual",
       size = "mgDM",
       y = "Number of individuals >= x") +
  theme(legend.position = "top",
        axis.text.x = element_text(size = 7,
                                   angle = 45,
                                   hjust = 1))


ggview::ggview(isd_panel_plot, width = 7.5, height = 7)
ggsave(isd_panel_plot, file = "plots/isd_panel_plot.jpg", width = 7.5, height = 7)



epred_posts = fit$data %>% 
  distinct(tank, xmin, xmax, temp_treat, nutrient_treat) %>% 
  mutate(counts = 1) %>% 
  add_epred_draws(fit, re_formula = NULL) %>% 
  mutate(new_x = case_when(temp_treat == "ambient" ~ 1,
                           TRUE ~ 2)) %>% 
  mutate(adjust = case_when(nutrient_treat == "ambient" ~ -0.1,
                            TRUE ~ 0.1))

group_mean_posts = fit$data %>% 
  distinct(xmin, xmax, temp_treat, nutrient_treat) %>% 
  mutate(counts = 1) %>% 
  add_epred_draws(fit, re_formula = NA) %>% 
  mutate(new_x = case_when(temp_treat == "ambient" ~ 1,
                           TRUE ~ 2)) %>% 
  mutate(adjust = case_when(nutrient_treat == "ambient" ~ -0.1,
                            TRUE ~ 0.1))

lambda_plot = epred_posts %>% 
  group_by(temp_treat, nutrient_treat, new_x, adjust, tank) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = new_x + adjust, y = .epred)) + 
  stat_halfeye(data = group_mean_posts, aes(y = .epred, fill = nutrient_treat),
               alpha = 0.6) +
  # geom_pointinterval(aes(y = .epred, ymin = .lower, ymax = .upper,
  #                        group = interaction(tank, temp_treat, nutrient_treat)),
  #                    position = position_jitter(width = 0.005),
  #                    linewidth = 0.2,
  #                    color = "gray60") + 
  geom_point(aes(y = .epred),
             shape = 21,
             size = 1) +
  scale_x_continuous(breaks = c(1, 2),
                     labels = c("ambient", "heated")) + 
  labs(x = "Heat",
       y = "\u03bb") +
  scale_fill_colorblind() +
  theme_default() +
  labs(fill = "Nutrients")


ggview::ggview(lambda_plot, width = 6, height = 4)
ggsave(lambda_plot, file = "plots/lambda_plot.jpg", width = 6, height = 4)








