# Load o2 data and combine with metadata

source("./code/02_clean_data.R")
#### 02_clean_data.R loads: #####
# `exp_data` = the o2 and temperature data and metadata
# `grp1_dates` = the dates in the first sampling
# `grp2_dates`= the dates in the second sampling
# functions for gpp and er estimation
##
#### Data plotting -----
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

#### Estimate metabolism from Dawn-Dusk-Dawn -----
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


### Estimate GPP from full interpolated o2 signal ----
# fit a model to each of the diel curves to interpolate o2
tank1_1 = exp_data %>% named_group_split(tank) %>% .[[1]] %>% filter(run == 'run1') %>% data.frame %>% ungroup

tank1_2 = exp_data %>% named_group_split(tank) %>% .[[1]] %>% filter(run == 'run2') %>% data.frame %>% ungroup

tank1_1 %>%
  ggplot()+
  geom_point(aes(x = date_time, y = o2_do_mg_l))+
  geom_smooth(aes(x =date_time, y = o2_do_mg_l), method = 'gam',  se = TRUE)


o2_form = brms::bf(o2_do_mg_l ~ s(date_time, bs = 'tp') + fcov(v.o2),
                   data = tank1)

tankList = exp_data %>% ungroup %>% named_group_split(tank) %>% purrr::map(~.x %>% named_group_split(run))

### Run the gams for temp and o2
debug(fit_o2_gam)
# tankList %>% purrr::walk(~.x %>% purrr::walk(~fit_o2_gam(.x)))
# 
# tankList %>% purrr::walk(~.x %>% purrr::walk(~fit_temp_gam(.x)))


# estimate metabolism from bayesian model with LakeMetabolizer
tankNameList = exp_data %>% dplyr::select(tank, run) %>% 
  dplyr::mutate(names = paste0("t",tank,"_",run)) %>% 
  ungroup %>% dplyr::select(names)%>% 
  distinct %>% 
  unlist %>% 
  sapply(.,function(a) gsub("run","",a)) %>% unname

# Estimate metabolism parameters from modal o2 and temp estimates ----
# purrr::walk(tankNameList, ~estimateContinuous(tankID = .x))

# Get list of files from all metabolism models ----
metabFiles = list.files("./data/models",".*bayes.*.rds", full.names = TRUE)

## Extract the summaries and relevant plots ----
expMetab = metabFiles %>% purrr::map(~extract_metab(metabModel = .x) %>% pluck('summary')) %>%
  bind_rows %>%
  dplyr::mutate(tankMod = as.numeric(gsub("t(\\d{1,2})_\\d{1}","\\1", tank)),
                run = gsub("t\\d{1,2}_(\\d{1})","\\1", tank)) %>%
  dplyr::select(-tank) %>%
  dplyr::rename(tank = 'tankMod') %>%
  left_join(exp_metadata, by ='tank')


expMetab %>%
  ggplot()+
  geom_boxplot(aes(x = temp_treat, y = GPP, fill = nutrient_treat))+
  geom_hline(aes(yintercept = 0))+
  theme()

expMetab %>%
  ggplot()+
  geom_boxplot(aes(x = temp_treat, y = R, fill = nutrient_treat))+
  geom_hline(aes(yintercept = 0))

expMetab %>%
  ggplot()+
  geom_boxplot(aes(x = temp_treat, y = NEP, fill = nutrient_treat))+
  geom_hline(aes(yintercept = 0))

# convert all the gpp coefficient estimates for each tank by multiplying GPP coefficient by par and summing across all time ----
# debugonce(convert_metab)
expMetabFull = metabFiles %>% purrr::map(~convert_metab(metabModel = .x)) %>% setNames(.,nm = tankNameList)

# plot histogram of GPP estimates from t1_1
hist(expMetabFull[[2]]$gpp.out)

saveRDS(expMetabFull, "./data/gppPosteriors.rds")

####

tank1_1new = with(tank1_1gam$data,
                   expand.grid(run_hr = seq(min(run_hr),max(run_hr), length = 50))) %>%
  dplyr::mutate(tankID = tankID)
  
tank1_1pred = cbind(tank1_1new,
                   predict(tank1_1gam,
                           tank1_1new,
                           se.fit = FALSE,
                           type='response')) %>%
  dplyr::select(run_hr, o2_est = 'Estimate', o2_Q2.5 = 'Q2.5', o2_Q97.5 = 'Q97.5')
# debugonce(streamMetabolizer::calc_DO_sat)
tank1_1pred = cbind(tank1_1pred, 
                    predict(tank1_1temp,
                            tank1_1pred,
                            se.fit = FALSE,
                            type = 'response')) %>%
  dplyr::select(run_hr, o2_est, o2_Q2.5, o2_Q97.5, temp_est = 'Estimate', temp_Q2.5 = 'Q2.5', temp_Q97.5 = 'Q97.5') %>%
  dplyr::mutate(date.time = as.POSIXct(run_hr*3600, origin = as.POSIXct("2022-05-21 05:30:00")),
                solar.time = streamMetabolizer::calc_solar_time(date.time, longitude = site_coords[2]),
                light = streamMetabolizer::calc_light(solar.time,
                                                      latitude = site_coords[1],
                                                      longitude = site_coords[2]),
                DO_sat = calc_DO_sat(temp_est,
                                                           pressure.air = (28.96/0.029530)),
                DO_pctsat = (o2_est/DO_sat)*100) %>%
  dplyr::select(date.time, solar.time, run_hr, light, DO_sat, o2_est, DO_pctsat,
                everything()) %>%
  dplyr::mutate(across(matches('o2'), ~ifelse(.x <=0,0.01,.x))) %>%
  dplyr::mutate(delo2_est = diff(c(NA,o2_est)),
                K600 = LakeMetabolizer::k.cole(data.frame(datetime = solar.time,
                                                             wnd = wind.scale.base(1,1)))[['k600']],
                k.gas = k600.2.kGAS.base(k600 = K600, temp_est, gas = "O2"), 
                z.mix = 0.3) %>%
  dplyr::select(date.time, irr = 'light', do.sat = 'DO_sat', do.obs = 'o2_est', wtr = 'temp_est', k.gas, z.mix)

#Bayesian
bayes.res = metab.bayesian(do.obs = tank1_1pred[['do.obs']],
                           do.sat = tank1_1pred[['do.sat']],
                           k.gas = tank1_1pred[['k.gas']],
                           z.mix = tank1_1pred[['z.mix']],
                           irr = tank1_1pred[['irr']],
                           wtr = tank1_1pred[['wtr']],
                           datetime = tank1_1pred[['date.time']])


plot(bayes.res)


## Lake metabolizer data
data.path = system.file('extdata/', package="LakeMetabolizer")

sp.data = load.all.data('sparkling', data.path)
ts.data = sp.data$data #pull out just the timeseries data
#calculate U10 and add it back onto the original
u10 = wind.scale(ts.data)
ts.data = rmv.vars(ts.data, 'wnd', ignore.offset=TRUE) #drop old wind speed column
 
ts.data = merge(ts.data, u10)                          #merge new u10 into big dataset
#Now calculate k600 using the Cole method
k600.cole = k.cole(ts.data)
ts.data = merge(ts.data, k600.cole)
kgas = k600.2.kGAS(ts.data)
ts.data = rmv.vars(merge(kgas, ts.data), 'k600')
o2.sat = o2.at.sat(ts.data[,c('datetime','wtr_0')])
ts.data = merge(o2.sat, ts.data)
z.mix = ts.meta.depths(get.vars(ts.data, 'wtr'), seasonal=TRUE)
names(z.mix) = c('datetime','z.mix', 'bottom')
#set z.mix to bottom of lake when undefined
 z.mix[z.mix$z.mix <=0 | is.na(z.mix$z.mix), 'z.mix'] = 20 
ts.data = merge(ts.data, z.mix[,c('datetime','z.mix')])
 

#Bayesian
bayes.res = metab(ts.data, method='bayesian',
                  wtr.name='wtr_0.5', 
                  do.obs.name='doobs_0.5', 
                  irr.name='par')
?LakeMetabolizer::metab.bayesian

data.path = system.file('extdata/Alder/2018/', package="LakeMetabolizer")
sp.data = load.all.data('Castle2018', data.path)

tank1_1pred %>%
  ggplot() +
  geom_point(aes(x = date.time, y = delo2_est))+
  scale_y_continuous(limits = c(0,NA))


modG_pred <- with(exp_data,
                  expand.grid(run_hr=seq(min(run_hr), max(run_hr), length=51),
                              run=levels(as.factor(run)),
                              tank=levels(as.factor(tank))))

modG_pred <- cbind(modG_pred,
                   predict(hierModel1, 
                           modG_pred, 
                           se.fit=TRUE, 
                           type="response",
                           re_formula = NA)) %>%
  group_by(tank, run_hr) %>%
  dplyr::summarise(across(c(Estimate, Q2.5, Q97.5), ~mean(.x)))
#### Fit a single model ----
# fit a single hierarchical GAM of o2
# create an interaction term for run*tank
exp_data = exp_data %>%
  dplyr::mutate(tank = as.factor(tank),
                tod = chron::times(format(date_time, "%H:%M:%S")),
                tank_run = interaction(tank, run, drop = FALSE))
temp_plot = exp_data %>%
  ggplot()+
  geom_line(aes(x = run_hr, y = temp_deg_c, group = run, 
              color = run))+
  geom_point(aes(x = run_hr,y = temp_deg_c, group = run, color = run))+
  facet_wrap(~tank);temp_plot

exp_data %>%
  ggplot()+
  geom_point(aes(x = run_hr, y = o2_do_pct_sat, color = run))+
  facet_wrap(~tank)

# hierForm = bf(o2_do_mg_l ~ s(run_hr, bs = 'tp')  te(run_hr, tank, run, bs = c("cr","re","re"), full = TRUE))
modForm = brms::bf(o2_do_mg_l ~ s(run_hr, bs = 'tp', k = 15, m = 2)+
                     s(run_hr, by = tank)+
                     s(run, tank, bs = 're'))

modTest1 = brm(modForm,
                data = exp_data, 
                family = gaussian(),
                cores = 6, seed = 42, chains = 3,
                threads = threading(2),
                iter = 2e3, warmup = 1e3, thin = 10,
                # control = list(adapt_delta = 0.99),
                file = "./data/models/o2_GAM_simple.rds",
                save_pars = save_pars(all = TRUE),
                sample_prior = TRUE,
                backend = 'cmdstanr')
pp_check(modTest1)
modModel1 = update(modTest1,
                    cores = 6, seed = 42, chains = 3,
                    threads = threading(3),
                    iter = 1e4, warmup = 7e3, thin = 10,
                    file = "./data/models/o2_GAM_simple.rds",
                    # control = list(adapt_delta = 0.99),
                    save_pars = save_pars(all = TRUE),
                    sample_prior = TRUE,
                    file_refit = 'on_change',
                    backend = 'cmdstanr')
modModel1 = readRDS("./data/models/o2_GAM_simple.rds")
modModel1 = brms::add_criterion(modModel1, c('loo'), file = "./data/models/o2_GAM_simple.rds")
loo::loo(modModel1)
bayesplot::pp_check(modTest1)
### Hierarchical structure in the splines
hierForm = brms::bf(o2_do_mg_l ~ s(run_hr, bs = 'tp', k = 15, m =2) +
                      s(run_hr, tank, bs = 'fs', k = 15, m = 2) +
                      s(run, tank, bs = 're'))
hierTest1 = brm(hierForm,
                data = exp_data, 
                family = gaussian(),
                cores = 6, seed = 42, chains = 3,
                threads = threading(2),
                iter = 3e3, warmup = 2e3, thin = 10,
                # control = list(adapt_delta = 0.99),
                save_pars = save_pars(all = TRUE),
                sample_prior = TRUE,
                backend = 'cmdstanr')
hierTest1 = add_criterion(hierTest1, 'loo', file = "./data/models/o2_GAM_full.rds")

hierModel1 = update(hierTest1,
                    cores = 6, seed = 42, chains = 3,
                    threads = threading(2),
                    iter = 1e4, warmup = 7e3, thin = 10,
                    file = "./data/models/o2_GAM_full.rds",
                    # control = list(adapt_delta = 0.99),
                    save_pars = save_pars(all = TRUE),
                    sample_prior = TRUE,
                    file_refit = 'on_change',
                    backend = 'cmdstanr')

hierModel1 = readRDS("./data/models/o2_GAM_full.rds")
hierModel1 = brms::add_criterion(hierModel1, c('loo'), file = "./data/models/o2_GAM_full.rds")
loo::loo(hierModel1)
bayesplot::pp_check(hierModel1)

hierTest2 = brm(hierForm,
                data = exp_data, 
                family = lognormal(),
                cores = 6, seed = 42, chains = 3,
                threads = threading(2),
                iter = 3e3, warmup = 2e3, thin = 10,
                # control = list(adapt_delta = 0.99),
                save_pars = save_pars(all = TRUE),
                sample_prior = TRUE,
                backend = 'cmdstanr')
hierTest2 = add_criterion(hierTest2, 'loo', file = "./data/models/o2_GAM_full2.rds",
)
loo::loo_compare(hierTest1, hierTest2)
pp_check(hierTest2)

hierModel2 = update(hierTest2,
                    cores = 6, seed = 42, chains = 3,
                    threads = threading(2),
                    iter = 1e4, warmup = 7e3, thin = 10,
                    file = "./data/models/o2_GAM_full2.rds",
                    # control = list(adapt_delta = 0.99),
                    save_pars = save_pars(all = TRUE),
                    sample_prior = TRUE,
                    file_refit = 'on_change',
                    backend = 'cmdstanr')

hierModel2 = readRDS("./data/models/o2_GAM_full2.rds")
hierModel2 = brms::add_criterion(hierModel1, c('loo'), file = "./data/models/o2_GAM_full2.rds")
loo::loo(hierModel2)
bayesplot::pp_check(hierModel2)
loo::loo_compare(hierModel1, hierModel2)
## run on a different model form ----
hierForm3 = bf(o2_do_mg_l ~ s(run_hr, bs = 'tp', k = 15, m = 2) +
                 s(run_hr, tank, bs = 'fs', k = 15, m = 2))

hierTest3 = brm(hierForm3,
                data = exp_data, 
                family = gaussian(),
                cores = 6, seed = 42, chains = 3,
                threads = threading(2),
                iter = 3e3, warmup = 2e3, thin = 10,
                control = list(adapt_delta = 0.95),
                save_pars = save_pars(all = TRUE),
                sample_prior = TRUE,
                backend = 'cmdstanr')
pp_check(hierTest3)
hierTest3 = add_criterion(hierTest3, 'loo')
loo::loo_compare(hierTest1, hierTest2, hierTest3)

hierModel3 = update(hierTest3,
    cores = 6, seed = 42, chains = 3,
    threads = threading(2),
    iter = 1e4, warmup = 7e3, thin = 10,
    file = "./data/models/o2_GAM_full3.rds",
    control = list(adapt_delta = 0.95),
    save_pars = save_pars(all = TRUE),
    sample_prior = TRUE,
    file_refit = 'on_change',
    backend = 'cmdstanr')

hierModel3 = readRDS(file = "./data/models/o2_GAM_full3.rds")
hierModel3 = brms::add_criterion(hierModel2, c('loo'),
                                 file = "./data/models/o2_GAM_full3.rds")
loo::loo_compare(hierModel1, hierModel2, hierModel3)

summary(hierModel2)
modG_pred <- with(exp_data,
                      expand.grid(run_hr=seq(min(run_hr), max(run_hr), length=51),
                                  run=levels(as.factor(run)),
                                  tank=levels(as.factor(tank))))

# make the prediction, add this and a column of standard errors to the prediction
# data.frame. Predictions are on the log scale.
modG_pred <- cbind(modG_pred,
                       predict(hierModel1, 
                               modG_pred, 
                               se.fit=TRUE, 
                               type="response",
                               re_formula = NA)) %>%
  group_by(tank, run_hr) %>%
  dplyr::summarise(across(c(Estimate, Q2.5, Q97.5), ~mean(.x)))

ggplot(data=exp_data, aes(x= run_hr, y=o2_do_mg_l)) +
  facet_wrap(~tank) +
  geom_ribbon(aes(ymin=Q2.5, ymax=Q97.5, x=run_hr),
              data=modG_pred, 
              alpha=0.3, 
              inherit.aes=FALSE) +
  geom_line(aes(y=Estimate), data=modG_pred) +
  geom_point() #+
  labs(x=expression(CO[2] ~ concentration ~ (mL ~ L^{-1})),
       y=expression(CO[2] ~ uptake ~ (mu*mol ~ m^{-2})))



hierModel = readRDS(file = "./data/models/o2_GAM_full.rds" )

predictDf = exp_data %>%
  dplyr::select(tank,date_time, run, run_hr, tank_run) %>%
  bind_cols(predict(hierModel, .))




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
