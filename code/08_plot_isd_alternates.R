library(tidyverse)
library(brms) 
library(tidybayes)
library(isdbayes)
library(ggthemes)
theme_set(brms::theme_default(base_family = "bayesplotbase_family"))
source('code/custom_functions.r')
source('code/custom_functions/pparetocounts_vec.r')

treatments = readRDS("data/treatments.rds")

d = readRDS(file = "data/dat_morin_xmins.rds") |> 
  left_join(treatments |> distinct(temp_treat, temp_plus, nutrient_treat, nut_plus)) |> 
  ungroup()


gpp_range = c(-1, 0, 1)

gpp_attributes = readRDS(file = "data/gpp_wrangled.rds") |> 
  group_by(date) |> 
  reframe(mean = mean(gpp),
          sd = sd(gpp))

tibble(gpp_s = c(-3, -1, 0, 1, 2)) |> 
  expand_grid(gpp_attributes) |> 
  mutate(gpp_mgo2 = (gpp_s*sd) + mean) |> 
  arrange(gpp_s, date)

mod_names = c("models/fit_isd_morin_gpp_predictor.rds",
              "models/fit_isd_morin_gpp_predictor_22th.rds",
              "models/fit_isd_morin_gpp_predictor_28th.rds",
              "models/fit_isd_morin_gpp_predictor_nutrient.rds")

mods = lapply(mod_names, readRDS)


posts = list()


for(i in 1:length(mods)){

  posts[[i]] = d |> 
    distinct(temp_treat, temp_plus, nutrient_treat, nut_plus) |> 
    expand_grid(june_28_gpp_s = gpp_range) |>
    mutate(june_22_gpp_s = june_28_gpp_s) |> 
    mutate(gpp_sum = june_22_gpp_s + june_28_gpp_s) |> 
    mutate(
      gpp_cat = case_when(
        gpp_sum < 0 ~ "GPP[Low]",
        gpp_sum > 0 ~ "GPP[High]",
        gpp_sum == 0 ~ "GPP[Medium]")) |>
    mutate(gpp_cat = fct_relevel(gpp_cat, "GPP[Low]", "GPP[Medium]")) |> 
    mutate(xmin = 0.01, xmax = 1000, counts = 1) |> 
    mutate(model = mod_names[[i]]) |> 
    add_epred_draws(mods[[i]], re_formula = NA) |> 
    group_by(temp_plus, temp_treat, .draw, gpp_cat, model) |> 
    reframe(.epred = mean(.epred))

}

plot_names = c("a) ~temp*GPP1*GPP2 + ... (main model)",
               "b) ~temp*GPP1 + ...",
               "c) ~temp*GPP2 + ...",
               "d) ~temp*GPP1*GPP2 + nutrients + ...")

plots_compare = list()

for(i in 1:length(posts)){
  plots_compare[[i]] = posts[[i]] |> 
    ggplot(aes(x = temp_plus, y = .epred,
               color = temp_plus)) +
    geom_line(data = . %>% filter(.draw <= 300),
              aes(group = .draw),
              alpha = 0.05,
              linewidth = 0.05) +
    stat_pointinterval() +
    facet_grid(~gpp_cat, labeller = label_parsed) +
    scale_color_colorblind() +
    guides(color = "none") +
    labs(y = "\u03bb",
         x = "Temperature Treatment",
         subtitle = plot_names[[i]]) + 
    ylim(-2, -0.5) +
    theme(strip.text = element_text(size = 9))
}

library(patchwork)

ab = (plots_compare[[1]] + plots_compare[[2]]) + plot_layout(axis_titles = "collect")
cd = (plots_compare[[3]] + plots_compare[[4]]) + plot_layout(axis_titles = "collect")
compare_isd_models = (ab/cd) + plot_layout(axis_titles = "collect")

ggsave(compare_isd_models, file = "plots/compare_isd_models.jpg", width = 7, height = 7)



# other gpp scenarios -----------------------------------------------------
# set lowest gpp to 0.1mgo2/l/d

gpp_attributes |>
  mutate(gpp_s = (0.1 - mean)/sd)
  

gpp_range = c(-3, 0, 3)

gpp_attributes = readRDS(file = "data/gpp_wrangled.rds") |> 
  group_by(date) |> 
  reframe(mean = mean(gpp),
          sd = sd(gpp))



mod_names = c("models/fit_isd_morin_gpp_predictor.rds",
              "models/fit_isd_morin_gpp_predictor_22th.rds",
              "models/fit_isd_morin_gpp_predictor_28th.rds",
              "models/fit_isd_morin_gpp_predictor_nutrient.rds")

mods = lapply(mod_names, readRDS)


posts = list()


for(i in 1:length(mods)){

  posts[[i]] = d |> 
    distinct(temp_treat, temp_plus, nutrient_treat, nut_plus) |> 
    expand_grid(june_28_gpp_s = c(-2.45, 0, 1)) |>
    mutate(june_22_gpp_s = june_28_gpp_s) |> 
    mutate(june_22_gpp_s = case_when(june_22_gpp_s == min(june_22_gpp_s) ~ june_22_gpp_s - 0.27,
                                     T ~ june_22_gpp_s)) |> 
    mutate(gpp_sum = june_22_gpp_s + june_28_gpp_s) |> 
    mutate(gpp_cat = case_when(gpp_sum < 0 ~ "a) Low GPP",
                               gpp_sum > 0 ~ "c) High GPP",
                               gpp_sum == 0 ~ "b) Medium GPP")) |>
    mutate(xmin = 0.01, xmax = 1000, counts = 1) |> 
    mutate(model = mod_names[[i]]) |> 
    add_epred_draws(mods[[i]], re_formula = NA) |> 
    group_by(temp_plus, temp_treat, .draw, gpp_cat, model) |> 
    reframe(.epred = mean(.epred))

}

plot_names = c("a) ~temp*GPP1*GPP2 + ... (main model)",
               "b) ~temp*GPP1 + ...",
               "c) ~temp*GPP2 + ...",
               "d) ~temp*GPP1*GPP2 + nutrients + ...")

plots_compare = list()

for(i in 1:length(posts)){
  plots_compare[[i]] = posts[[i]] |> 
    ggplot(aes(x = temp_plus, y = .epred,
               color = temp_plus)) +
    geom_line(data = . %>% filter(.draw <= 300),
              aes(group = .draw),
              alpha = 0.05,
              linewidth = 0.05) +
    stat_pointinterval() +
    facet_grid(~gpp_cat) +
    scale_color_colorblind() +
    guides(color = "none") +
    labs(y = "\u03bb",
         x = "Temperature Treatment",
         subtitle = plot_names[[i]]) + 
    ylim(-2, -0.5) +
    theme(strip.text = element_text(size = 9))
}

library(patchwork)

ab = (plots_compare[[1]] + plots_compare[[2]]) + plot_layout(axis_titles = "collect")
cd = (plots_compare[[3]] + plots_compare[[4]]) + plot_layout(axis_titles = "collect")
(ab/cd) + plot_layout(axis_titles = "collect")

# ggsave(compare_isd_models, file = "plots/compare_isd_models.jpg", width = 7, height = 7)


extreme_mod_predictions = bind_rows(posts) |>
  ungroup() |>  
  select(temp_plus, .draw, .epred, model, gpp_cat) |>
  pivot_wider(names_from = temp_plus, values_from = .epred) |> 
  mutate(diff = `+heat` - `-heat`) |> 
  group_by(model, gpp_cat) |> 
  filter(gpp_cat == "a) Low GPP") |> 
  reframe(prob = sum(diff>0)/max(.draw),
          meanplus = mean(`+heat`),
          sdplus = sd(`+heat`),
          meanminus = mean(`-heat`),
          sdminus = sd(`-heat`), 
          meandiff = mean(diff),
          lowerdiff = quantile(diff, 0.025),
          upperdiff = quantile(diff, 0.975)) |> 
  mutate(gpp_mgo2ld = 0.1)

write_csv(extreme_mod_predictions, file = "tables/extreme_mod_predictions.csv")

  