library(tidyverse)

image_j_tech_test <- read_csv("data/image_j_tech_test.csv")


image_j_tech_test %>% 
  ggplot(aes(x = sample, y = length_mm, color = method)) + 
  geom_point(position = position_dodge(width = 0.5)) + 
  geom_boxplot(outlier.shape = NA, aes(group = interaction(sample, method)),
               position = position_dodge(width = 0.5),
               width = 0.2)
