library(FSA)
library(tidyverse)
library(isdbayes)

# example from here: https://fishr-core-team.github.io/fishR/blog/posts/2020-1-2_vonB_plots_2/


vb1 = makeGrowthFun(type="von Bertalanffy", pname="Typical")

seed = round(runif(1, 1, 100000), 0)
set.seed(seed = seed)

t = isdbayes::rparetocounts(n = 1000, lambda = -1.5, xmin = 0.01, xmax = 10)  %>% # "age" or time. uses a pareto to simulate more smaller time points (i.e., individuals)
  sort()

t[length(t) + 1] <- 0

t = sort(t)

# t = seq(0.1, 10, length.out = 100) # "age" or time
Linf = 1 # asymptotic average length. I'm assuming here that it is mass in milligrams (i.e., average size is 1 mg)
K = 1 # body growth rate coefficient. units are per year
t0 = 0 # modeling artifact. Always make it zero. Not meant to be biologically relevant

a = vb1(t = t, 
    Linf = Linf*1, # larger "max" size
    K = K*0.9, # slower early growth rate
    t0 = t0)

b = vb1(t = t, 
        Linf = Linf*1, # smaller "max" size
        K = K*1.1, # faster early growth rate
        t0 = t0)

dat = tibble(a = a, b = b) %>%
  mutate(time = t) %>% 
  pivot_longer(cols = -time) 

dat %>%
  filter(time > 0) %>% 
  ggplot(aes(x = time, y = value, color = name)) +
  geom_point(alpha = 0.1) +
  geom_line() +
  NULL
# 
# 
dat %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name)


# tibble(a = a, b = b) %>%
#   mutate(time = t) %>% 
#   filter(time > 0) %>% 
#   mutate(min_diff = min(b) - min(a)) %>% # make the minimum sizes equal
#   mutate(b = b - min_diff) %>%
#   select(-min_diff) %>%
#   pivot_longer(cols = -time) %>% 
#   arrange(name, -value) %>% 
#   group_by(name) %>% 
#   mutate(order = row_number()) %>% 
#   ggplot(aes(x = value + 0.0001, y = order, color = name)) + 
#   geom_point() +
#   scale_x_log10() +
#   scale_y_log10()


dat_mod = dat %>% 
  filter(time > 0) %>% 
  group_by(name) %>% 
  mutate(x = value,
         counts = 1, 
         xmin = min(x),
         xmax = max(x))


dummy_mod = readRDS('models/dummy_mod.rds')

fita = update(dummy_mod, newdata = dat_mod %>% filter(name == "a"))
fitb = update(dummy_mod, newdata = dat_mod %>% filter(name == "b"))

fita
fitb
