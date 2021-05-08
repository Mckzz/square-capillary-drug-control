library(tidyverse)
library(ggplot2)

print(cAMP_etc, n= 50)

means.sd <-
  cAMP_etc %>% 
  group_by(Drug, exp) %>% 
  ## now compute mean and sd:
  summarize(across(everything(), #how does the across everything work? is it that chr columns get created based on num columns that are summarized?
                   tibble::lst(mean = mean, sd = sd)))

print(means.sd)




##### makes strip charts with mean and stdv (from percents)
ggplot(cAMP_etc, aes(y = pct.change, x = Drug, colour= exp)) +
  geom_jitter(size = 2, pch = 1, position = position_dodge(width = 0.7)) +
  labs(x = "exposure", y = "% change") + #labels axes
  theme_classic() +  #takes out background
  stat_summary(
    fun.data = mean_sdl, position = position_dodge(width = 0.5), geom = "errorbar", width = 0.1, fun.args = list(mult=1)) +
  stat_summary(
    fun = mean, geom = "point", position = position_dodge(width = 0.5),
    size = 3)
