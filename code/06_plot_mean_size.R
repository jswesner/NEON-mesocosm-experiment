library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)
library(ggthemes)
theme_set(theme_default())


# larvae ------------------------------------------------------------------

# brm_mean_larvae = readRDS(file = "models/brm_mean_larvae_rawdata_lognorm.rds")
brm_mean_larvae = readRDS(file = "models/brm_mean_larvae.rds")
treatments = readRDS("data/treatments.rds") 

dat = brm_mean_larvae$data %>% 
  left_join(treatments)

# trt
post_trt = dat %>% distinct(temp_treat, nutrient_treat) %>% 
  mutate(mean_dw = brm_mean_larvae$data2$mean_dw) %>% 
  add_epred_draws(brm_mean_larvae, re_formula = NA) %>% 
  mutate(.epred = .epred) %>% 
  left_join(treatments) %>% 
  mutate(correct_taxon = "Grand Mean", # dummy value for labling in the taxa plot
         median = 10e10) # dummy value to ensure it comes last in the taxa plot

post_trt %>% 
  ggplot(aes(x = temp_nut_plus, color = temp_nut_plus, y = .epred)) +
  # geom_jitter(data = dat, aes(y = dw_mg), width = 0.12, size = 1, shape = ".",
  #             alpha = 0.2) + 
  stat_pointinterval(point_interval = "median_hdci") +
  scale_y_log10() +
  coord_flip() +
  guides(color = "none") +
  scale_color_colorblind() + 
  NULL



plot_larval_mean_size = post_trt %>%
  ggplot(aes(x = .epred, y = temp_nut_plus)) +
  geom_point(data = dat, aes(x = dw_mg),
              shape = "|", size = 0.9) +
  stat_halfeye(color = "red", size = 0.3) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  # scale_x_log10() +
  guides(color = "none") +
  labs(x = "Individual Size (mgDM)",
       y = "Treatment") +
  NULL


post_trt %>% 
  group_by(temp_nut_plus) %>% 
  median_qi(.epred)

# trt*taxon
post_trt_taxa_mean_larvae = dat %>% distinct(temp_treat, nutrient_treat, correct_taxon) %>% 
  mutate(mean_dw = brm_mean_larvae$data2$mean_dw) %>% 
  add_epred_draws(brm_mean_larvae, re_formula = NULL) %>% 
  mutate(.epred = .epred) %>% 
  left_join(treatments) %>% 
  group_by(correct_taxon) %>% 
  mutate(correct_taxon == case_when(correct_taxon == "Daphnia" ~ "Zooplankton",
                                    TRUE ~ correct_taxon)) %>% 
  mutate(median = median(.epred)) %>% 
  bind_rows(post_trt) # add grand mean


plot_taxa_size_larvae = post_trt_taxa_mean_larvae  %>% 
  ggplot(aes(x = .epred, y = reorder(correct_taxon, -median))) + 
  ggridges::geom_density_ridges(aes(fill = temp_plus),
                                linewidth = 0.1) +
  scale_x_log10() +
  facet_wrap(~nut_plus) +
  scale_fill_colorblind() + 
  labs(y = "Taxon",
       x = "Individual Mass (mgDM)") +
  theme(legend.title = element_blank(),
        text = element_text(size = 8),
        legend.text = element_text(size = 6),
        axis.line = element_line(linewidth = 0.2),
        legend.key.size = unit(0.2, 'cm'))


ggsave(plot_taxa_size_larvae, file = "plots/plot_taxa_size_larvae.jpg", width = 5, height = 3, dpi = 400)

# adults ------------------------------------------------------------------

brm_mean_ind_mass_adults <- readRDS("models/brm_mean_ind_mass_adults.rds")

emergence = readRDS(file = "data/emergence_wrangled.rds") %>% 
  mutate(mean_ind = mean(mean_individual_mgdw, na.rm = T)) %>% 
  mutate(day_of_experiment =  julian(as.Date(date)) - julian(as.Date("2022-05-27"))) 

post_trt_adults = emergence %>% distinct(heat, nutrients, temp_nut_plus, mean_ind, date, nut_plus, temp_plus) %>% 
  add_epred_draws(brm_mean_ind_mass_adults, re_formula = NULL) %>% 
  mutate(day_of_experiment =  julian(as.Date(date)) - julian(as.Date("2022-05-27"))) %>% 
  mutate(.epred = .epred*mean_ind)  


plot_adult_mean_size = post_trt_adults %>%
  ggplot(aes(x = day_of_experiment, y = .epred*1000, fill = nut_plus)) +
  geom_line(data = . %>% filter(.draw <= 300),
            aes(group = .draw), alpha = 0.007) +
  geom_point(data = emergence %>% filter(mean_individual_mgdw>0), 
             aes(y = mean_individual_mgdw*1000),
             size = 0.2) +
  # stat_halfeye(alpha = 0.8)  +
  geom_boxplot(aes(group = day_of_experiment), outlier.shape = NA,
               alpha = 0.6, linewidth = 0.2) +
  scale_color_colorblind() +
  scale_fill_colorblind() +
  facet_grid(nut_plus ~ temp_plus) +
  # scale_y_log10() +
  guides(color = "none",
         fill = "none") +
  labs(y = "Individual Size (mgDM)",
       x = "Day of Experiment") +
  # facet_wrap(~date) +
  NULL


library(cowplot)

plot_individual_size =  plot_grid(
  (plot_adult_mean_size + labs(subtitle = "a) Adults") + theme(text = element_text(size = 8),
                                                               title = element_text(size = 9))),
   (plot_larval_mean_size + labs(subtitle = "b) Larvae")+ theme(text = element_text(size = 8),
                                                                title = element_text(size = 9))),
  ncol = 1, rel_heights = c(1, 0.6))

ggsave(plot_individual_size, file = "plots/plot_individual_size.jpg", width = 4, height = 6,
       dpi = 400)


post_trt_adults %>% 
  group_by(date, temp_plus, .draw) %>%
  reframe(.epred = mean(.epred)) %>% 
  group_by(date, temp_plus) %>% 
  median_qi(.epred*1000) %>% 
  filter(date %in% c(min(date), max(date)))


post_trt_adults %>% 
  group_by(date, temp_plus) %>% 
  median_qi(.epred*1000) 
