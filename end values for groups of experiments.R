library(tidyverse)
library(ggplot2)

rm(Amil_cAMP_8_Br_cGMP_)
print(cAMP_etc, n= 50)

means.sd <-
  cAMP_etc %>% 
  mutate(Drug = as_factor(Drug)) %>%
  select(-indvd) %>%
  group_by(Drug, exp) %>% 
  ## now compute mean and sd:
  summarize(across(everything(), 
                   tibble::lst(mean = mean, sd = sd)))

print(means.sd)

##### makes strip charts with mean and stdv (from percents)

cAMP_etc$Drug <- factor(cAMP_etc$Drug, levels=c("8-Br-cAMP_1_mM", "8-Br-cGMP_1_mM", "Fsk_IBMX", "8-CPT_0.5_mM", "N6Bnz_0.5_mM"))

ggplot(cAMP_etc, 
       aes(y = pct.change, 
           x = Drug, colour= exp)) +
  geom_jitter(size = 2, 
              pch = 1, 
              position = position_dodge(width = 0.7)) +
  labs(x = "exposure", y = "% change") + #labels axes
  stat_summary(
    fun.data = mean_sdl, 
    position = position_dodge(width = 0.5), 
    geom = "errorbar", 
    width = 0.1, 
    fun.args = list(mult=1)) +
  stat_summary(
    fun = mean, 
    geom = "point", 
    position = position_dodge(width = 0.5),
    size = 3) +
  theme_classic() + #takes out background#
  theme(legend.position = "none")
