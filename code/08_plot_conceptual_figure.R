library(tidyverse)
library(isdbayes)
library(brms)
library(ggthemes)
library(ggrepel)

set.seed(234234)

larvae = tibble(larvae = rparetocounts(lambda = -1.6)) %>% 
  mutate(larvae = larvae) %>% 
  arrange(-larvae) %>% 
  mutate(id = row_number())

adults = larvae %>% filter(larvae >= 5) %>% 
  sample_n(size = 30, replace = T) %>% 
  rename(adults = larvae)

larvae_adults = left_join(larvae, adults) %>% 
  pivot_longer(cols = -id) %>% 
  mutate(name = fct_relevel(name, "larvae")) %>% 
  mutate(id_adult = case_when(name == "adults" ~ id + 1.2*id, TRUE ~ id),
         value = case_when(name == "adults" ~ value + 0.1*value, TRUE ~ value))

pparetocounts = function(x, xmin, xmax, lambda){(1 - (x^(lambda + 1) - (xmin^(lambda+1)))/(xmax^(lambda + 1) - (xmin^(lambda+1))))}

larvae_line = tibble(x = seq(1, 1000, length.out = 100)) %>% 
  rowwise() %>% 
  mutate(xmin = 1, xmax = 1000, lambda = -1.6) %>% 
  mutate(prob_y = pparetocounts(x = x, xmin = xmin, xmax = xmax, lambda = lambda)) %>% 
  ungroup %>% 
  mutate(id_adult = prob_y*300) 

labels = larvae_adults %>% filter(id == 85)

library(ggrepel)

plot_isd_emerge = larvae_adults %>% 
  ggplot(aes(x = value, y = id_adult)) +
  geom_line(aes(group = id), alpha = 0.2) + 
  geom_point(aes(color = name, shape = name), size = 0.7) +
  geom_line(data = larvae_line, aes(x = x)) +
  scale_x_log10() +
  scale_color_colorblind() +
  scale_y_log10(breaks = c(1, 10, 100, 300),
                 labels = c("0.001", "0.01", "0.1", "1")) +
  scale_shape_manual(values = c(1, 17)) +
  guides(color = "none",
         shape = "none") +
  coord_cartesian(ylim = c(1, NA)) +
  labs(y = "Pr (X \u2265 x)",
       x = "Mass") +
  geom_text_repel(data = labels %>% filter(name == "larvae"), 
                  label = "Larval ISD",
                  nudge_y = -0.2,
                  nudge_x = -0.3,
                  size = 2) +
  geom_text_repel(data = labels %>% filter(name != "larvae"), 
                  label = "Emerged adults",
                  color = "#e69f00",
                  nudge_y = 0.2,
                  nudge_x = 0.3,
                  size = 2) +
  theme(axis.text.y = element_blank())
  

ggsave(plot_isd_emerge, file = "plots/plot_isd_emerge.jpg", 
       width = 3, height = 3)



larvae_adults %>% 
  ggplot(aes(x = name, y = value, color = name, 
             shape = name)) +
  geom_point() +
  scale_color_colorblind() +
  scale_y_log10() +
  NULL
