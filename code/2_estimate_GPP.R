# Script to estimate GPP from O2 measurements
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(rnoaa)
library(rwunderground)
theme_set(theme_minimal())
# import oxygen data 
o2_data = read.csv(file = "./data/o2_temp.csv", TRUE) %>%
  dplyr::mutate(date_time = as.POSIXct(date_time, format = "%Y-%m-%dT%H:%M:%SZ", tz= "America/Chicago"),
                # calculate the DO percent saturation
                o2_do_pct_sat = streamMetabolizer::calc_DO_sat(temp.water = temp_deg_c,
                                                               pressure.air = (28.96/0.029530)))

# import experimental design metadata 

meta_data = read.csv(file = "./data/treatments.csv", TRUE)

# 

exp_data = o2_data %>%
  left_join(meta_data %>%
              dplyr::select(tank, temp_treat, nutrient_treat), by = "tank")

estimateDuskDawn = function(df = NULL,...){
  require(plyr)
  require(dplyr)
  require(magrittr)
  
  date_timeCol = names(df[sapply(df, function(x) inherits(x, "POSIXt"))])
  dayCol = names(df[sapply(df, function(x) inherits(x, "difftime"))])
  day = 'day'
  
  df = df %>% ungroup %>%
    dplyr::mutate(!!day := as.integer(julian(!!as.symbol(date_timeCol)) - min(julian(!!as.symbol(date_timeCol)))+1),
                  TOD = case_when(lubridate::hour(!!as.symbol(date_timeCol)) < 12 ~ "morning",
                                  TRUE ~ "evening")) %>%
    group_by(day, TOD) %>%
    dplyr::arrange(sort(!!as.symbol(date_timeCol))) %>%
    dplyr::mutate(time_point = 1:n())
  
  # test that >1 day of data exist for mornings
  if(length(unique(df[which(grepl("morning",df[["TOD"]])),dayCol])) <= 1) stop("GPP calculations require >1 day of oxygen measurements")
  
  # Calculate NP
  
    
}
debugonce(estimateDuskDawn)
estimateDuskDawn(exp_data %>% filter(tank == 1))


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

exp_data %>% 
  group_by(temp_treat, nutrient_treat) %>%
  dplyr::summarise(max_o2 = max(o2_do_mg_l, na.rm = TRUE),
                   min_o2 = min(o2_do_mg_l, na.rm = TRUE)) %>%
  ggplot()+
  geom_boxplot(aes(x = temp_treat, y = max_o2, group = nutrient_treat))



#### Spare(d) code ####
# get background air pressure
# rwunderground::history(rwunderground::set_location(zip_code = 57069), date = 20220620)
# rnoaa::ncdc(stationid = "GHCND:USW00014944", startdate = "2022-06-17", enddate = "2022-06-18", datasetid = "NORMAL_HLY")
