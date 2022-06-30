# Script to estimate GPP from O2 measurements
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
source("./code/metabolism_functions.R")
# library(rnoaa)
# library(rwunderground)
theme_set(theme_minimal())
# import oxygen data 
o2_data = read.csv(file = "./data/o2_temp.csv", TRUE) %>%
  dplyr::mutate(date_time = as.POSIXct(date_time, format = "%Y-%m-%dT%H:%M:%SZ"),
                # calculate the DO percent saturation
                o2_do_pct_sat = streamMetabolizer::calc_DO_sat(temp.water = temp_deg_c,
                                                               pressure.air = (28.96/0.029530)))

# import experimental design metadata 

meta_data = read.csv(file = "./data/treatments.csv", TRUE)

# 

exp_data = o2_data %>%
  left_join(meta_data %>%
              dplyr::select(tank, temp_treat, nutrient_treat), by = "tank")

exp_data %>%
  ggplot(aes(x = date_time, y = o2_do_mg_l))+
  geom_point(aes(color = temp_treat), size = 1.1)+
  geom_line(aes(linetype = nutrient_treat))+
  scale_x_datetime(date_breaks = "8 hours", date_labels = "%m/%y %H:00")+
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

# estimate patterns of GP, NP, and R
exp_dataList = exp_data %>%
  named_group_split(tank)

# debugonce(estimateDuskDawn)
dusk_dawnMetEstimates = lapply(exp_dataList, function(x) estimateDuskDawn(x)) %>%
  bind_rows(.id = 'tank') %>%
  left_join(meta_data %>%
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

#### Spare(d) code ####
# get background air pressure
# rwunderground::history(rwunderground::set_location(zip_code = 57069), date = 20220620)
# rnoaa::ncdc(stationid = "GHCND:USW00014944", startdate = "2022-06-17", enddate = "2022-06-18", datasetid = "NORMAL_HLY")
