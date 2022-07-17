# Load o2 data and combine with metadata

source("./code/02_clean_data.R")
#### 02_clean_data.R loads: #####
# `exp_data` = the o2 and temperature data and metadata
# `grp1_dates` = the dates in the first sampling
# `grp2_dates`= the dates in the second sampling
# functions for gpp and er estimation
######

exp_data %>%
  ggplot(aes(x = date_time, y = o2_do_mg_l))+
  geom_point(aes(color = temp_treat), size = 1.1)+
  geom_line(aes(linetype = nutrient_treat))+
  scale_x_datetime(date_breaks = "1 day", date_labels = "%m/%y")+
  scale_color_manual(values = c("blue","red"))+
  scale_linetype_manual(values = c("solid","dotted"))+
  facet_wrap(~tank)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

exp_data %>%
  ggplot(aes(x = date_time, y = temp_deg_c))+
  geom_point(aes(color = temp_treat), size = 1.1)+
  geom_line(aes(linetype = nutrient_treat))+
  scale_x_datetime(date_breaks = "8 hours", date_labels = "%m/%y %H:00")+
  scale_color_manual(values = c("blue","red"))+
  scale_linetype_manual(values = c("solid","dotted"))+
  facet_wrap(~tank)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

exp_data %>%
  ggplot(aes(x = date_time, y = o2_do_pct_sat))+
  geom_point(aes(color = temp_treat), size = 1.1)+
  geom_line(aes(linetype = nutrient_treat))+
  scale_x_datetime(date_breaks = "8 hours", date_labels = "%m/%y %H:00")+
  scale_color_manual(values = c("blue","red"))+
  scale_linetype_manual(values = c("solid","dotted"))+
  facet_wrap(~tank)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# estimate patterns of GP, NP, and R
exp_dataList = exp_data %>%
  ungroup %>%
  named_group_split(tank)

# debug(estimateDuskDawn)
dusk_dawnMetEstimates = exp_dataList %>%
  purrr::map(~.x %>% named_group_split(run) %>%
  purrr::map(~estimateDuskDawn(.x)) %>%
    bind_rows(.id = 'run')) %>%
  bind_rows(.id = 'tank') %>%
  left_join(exp_data %>%
              ungroup %>%
              dplyr::select(tank, temp_treat, nutrient_treat) %>%
              dplyr::mutate(tank = as.character(tank)), by = "tank")

# create boxplot of 
dusk_dawnMetEstimates %>%
  ggplot()+
  geom_boxplot(aes(x = temp_treat, y = NP_mg_o2_l_hr, fill = nutrient_treat)) +
  scale_x_discrete(name = "Temperature treatment")+
  scale_y_continuous(name = expression("Net production ( mg"~O[2]~L^-1~hr^-1~")"),
                     expand = c(0.01,0.01))+
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

# create boxplot of 
dusk_dawnMetEstimates %>%
  ggplot()+
  geom_boxplot(aes(x = temp_treat, y = Rnight_mg_o2_l_hr, fill = nutrient_treat))+
  scale_x_discrete(name = "Temperature treatment")+
  scale_y_continuous(name = expression("Respiration ( mg"~O[2]~L^-1~hr^-1~")"),
                     limits = c(NA, 0), expand = c(0.01,0.01))+
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

# create boxplot of 
dusk_dawnMetEstimates %>%
  ggplot()+
  geom_boxplot(aes(x = temp_treat, y = GP_mg_o2_l_hr, fill = nutrient_treat))+
  scale_x_discrete(name = "Temperature treatment")+
  scale_y_continuous(name = expression("Gross production ( mg"~O[2]~L^-1~hr^-1~")"),
                     limits = c(0, NA), expand = c(0.01,0.01))+
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

dusk_dawnMetEstimates %>%
  ggplot()+
  geom_boxplot(aes(x = temp_treat, y = (GP_mg_o2_l_hr/abs(Rnight_mg_o2_l_hr)), fill = nutrient_treat))+
  scale_x_discrete(name = "Temperature treatment")+
  scale_y_continuous(name = expression("GPP:ER"),
                     limits = c(0.5,NA),expand = c(0.01,0.01))+
  geom_hline(aes(yintercept = 1))+
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))

# scatter plot of GP & R
dusk_dawnMetEstimates %>%
  ggplot()+
  geom_point(aes(x = GP_mg_o2_l_hr, y = abs(Rnight_mg_o2_l_hr), color = temp_treat, fill = nutrient_treat), shape = 21, size =3, stroke = 1.3)+
  geom_abline()+
  geom_smooth(aes(x = GP_mg_o2_l_hr, y = abs(Rnight_mg_o2_l_hr)), method = 'lm', se = FALSE)+
  scale_y_continuous(name = expression("Respiration ( -mg"~O[2]~L^-1~hr^-1~")"),
                     limits = c(0,NA), expand = c(0.01,0.01))+
  scale_x_continuous(name = expression("Gross production ( mg"~O[2]~L^-1~hr^-1~")"),
                     limits = c(0,NA), expand = c(0.01,0.01))+
  scale_color_manual(values = c("blue","red"))+
  scale_fill_manual(values = c("blue","red"))


### Estimate GPP from full interpolated o2 signal
# fit a model to each of the diel curves to interpolate o2
tank1_1 = exp_data %>% named_group_split(tank) %>% .[[1]] %>% filter(run == 'run1') %>% data.frame %>% ungroup

tank1_2 = exp_data %>% named_group_split(tank) %>% .[[1]] %>% filter(run == 'run2') %>% data.frame %>% ungroup

tank1_2 %>%
  ggplot()+
  geom_point(aes(x = date_time, y = o2_do_mg_l))+
  geom_smooth(aes(x =date_time, y = o2_do_mg_l), method = 'gam',  se = TRUE)


o2_form = brms::bf(o2_do_mg_l ~ s(date_time, bs = 'tp') + fcov(v.o2),
                   data = tank1)

fit_o2_gam = function(df = NULL,...){
  require(brms)
  require(mgcv)
  # if(!is.formula(form) | is.na(as.formula(form))) stop("`form` must be formula object or coercible")
  
  tankID =paste0("t",unique(df$tank))
  runID = gsub("run","",unique(df$run))
  file_name = paste0("./data/models/o2_GAM_",tankID,"_",runID,".rds")
 
  formDefault = bf(o2_do_mg_l ~ s(run_hr, bs = 'tp'))
  formOtherwise = bf(o2_do_mg_l ~ s(run_hr, bs = 'tp', k = 9))
  
  if(nrow(df) > 10){
  model = brm(formDefault,
        data = df,
        family = gaussian(),
        cores = 5, seed = 42,
        iter = 1.5e4, warmup = 1e4, thin = 10,
        file = file_name,
        control = list(adapt_delta = 0.99),
        save_pars = save_pars(all = TRUE),
        sample_prior = TRUE,
        file_refit = 'on_change',
        backend = 'cmdstanr')
  } else{
    model = brm(formOtherwise,
                data = df,
                family = gaussian(),
                cores = 5, seed = 42,
                iter = 1.5e4, warmup = 1e4, thin = 10,
                file = file_name,
                control = list(adapt_delta = 0.99),
                save_pars = save_pars(all = TRUE),
                sample_prior = TRUE,
                file_refit = 'on_change',
                backend = 'cmdstanr')
  }
# 
#     ,
#                   otherwise = function(...){
#                     brm(formOtherwise,
#                         data = df,
#                         family = gaussian(),
#                         cores = 5, seed = 42,
#                         iter = 1.2e4, warmup = 1e4, thin = 10,
#                         file = file_name,
#                         sample_prior = TRUE,
#                         control = list(adapt_delta = 0.99),
#                         save_pars = save_pars(all = TRUE),
#                         file_refit = 'on_change',
#                         backend = 'cmdstanr')
#                     },
#                   quiet = TRUE
#                   )
}

tankList = exp_data %>% ungroup %>% named_group_split(tank) %>% purrr::map(~.x %>% named_group_split(run))
debug(fit_o2_gam)
tankList %>% purrr::walk(~.x %>% purrr::walk(~fit_o2_gam(.x)))


# fit a single hierarchical GAM of o2
# create an interaction term for run*tank
exp_data = exp_data %>%
  dplyr::mutate(tank_run = interaction(tank, run, drop = FALSE))
# hierForm = bf(o2_do_mg_l ~ s(run_hr, bs = 'tp')  te(run_hr, tank, run, bs = c("cr","re","re"), full = TRUE))
hierForm = bf(o2_do_mg_l ~ tank * run + s(run_hr, by = tank_run))

brm(hierForm,
                   data = exp_data, 
                   family = gaussian(),
                   cores = 5, seed = 42,
                   iter = 1.5e4, warmup = 1e4, thin = 10,
                   file = "./data/models/o2_GAM_full.rds",
                   control = list(adapt_delta = 0.99),
                   save_pars = save_pars(all = TRUE),
                   sample_prior = TRUE,
                   file_refit = 'on_change',
                   backend = 'cmdstanr')

hierModel = readRDS(file = "./data/models/o2_GAM_full.rds" )
plot()






ceff = conditional_effects(o2_GAM_t1_1)
ceff_plot = data.frame(
  run_hr = ceff$run_hr$run_hr,
  estimate = ceff$run_hr$estimate__,
  upper = ceff$run_hr$upper__,
  lower = ceff$run_hr$lower__
)



ceff_plot %>%
  ggplot() +
  geom_ribbon(aes(x = run_hr, ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(aes(x = run_hr, y = estimate)) +
  geom_point(data = tank1_1, aes(x = run_hr, y = o2_do_mg_l))


ceff$run_hr$lower__

tank_gam_form = bf(o2_do_mg_l ~ s(run_hr, bs = 'tp'))

get_prior(tank_gam_form, data = tank1_1, family = gaussian())


x = brms::brm(bf(o2_do_mg_l ~ s(run_hr, bs = 'tp', k = 9)),
          data = tank1_1,
          family = gaussian(),
          cores = 4, seed = 42,
          iter = 7000, warmup = 5000, thin = 10,
          # control = list(adapt_delta = 0.99),
          # save_pars = save_pars(all = TRUE),
          file = "./data/models/o2_GAM_t1_2.rds",
          file_refit = "on_change",
          backend = 'cmdstanr'
          )

plot(x)
conditional_smooths(x)

#### Spare(d) code ####
# get background air pressure
# rwunderground::history(rwunderground::set_location(zip_code = 57069), date = 20220620)
# rnoaa::ncdc(stationid = "GHCND:USW00014944", startdate = "2022-06-17", enddate = "2022-06-18", datasetid = "NORMAL_HLY")

# detect_sampling = function(df = NULL){
#   require(plyr)
#   require(dplyr)
#   require(magrittr)
#   date_timeCol = names(df[sapply(df, function(x) inherits(x, "POSIXt"))])
#   # dayCol = names(df[sapply(df, function(x) inherits(x, "difftime"))])
#   day = 'day'
#   # set the variables for day of experiment, time of day and 
#   df = df %>% ungroup %>%
#     dplyr::mutate(date = as.Date(date_time, tz = attr(df[[date_timeCol]], "tzone")),
#                   !!day := (!!as.symbol(date_timeCol) - min(!!as.symbol(date_timeCol)))+1,
#                   TOD = case_when(lubridate::hour(!!as.symbol(date_timeCol)) < 12 ~ "morning",
#                                   TRUE ~ "evening")) %>%
#     dplyr::arrange(date_timeCol) %>%
#     group_by(day, TOD) %>%
#     dplyr::mutate(time_point = 1:n()) %>%
#     ungroup %>%
#     dplyr::mutate(!!as.symbol(date_timeCol) := sort(!!as.symbol(date_timeCol)),
#                   dayDiff = c(NA,diff(eval(day))),
#                   g=with(rle(dayDiff),{ rep(seq_along(lengths), lengths)}))
#   
#   
#   return(NULL)
# }
