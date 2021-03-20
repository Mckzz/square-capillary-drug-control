install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)


####area plateaus
rm(X8_Br_cAMP_rep_2)


drug_curve <- X2020_Dec_11_5_HT_meth
print(drug_curve, n=50)

drug_curve.long <- drug_curve %>% 
  pivot_longer(cols=c(`control`, `treatment`), 
               names_to = "exposure", values_to = "area")

print(drug_curve.long, n=100)

#so that $pH could be assigned colours
drug_curve.long$exposure <- as.factor(drug_curve.long$exposure)

###### single larva normalized to T0

#making % change column
drug_curve.long.pct <- drug_curve.long %>%
  group_by(exposure) %>%
  mutate(
    area.pct.change = ((area - area[13]) / area[13]
    )*100) %>%
  ungroup() #why the ungroup?

print(drug_curve.long.pct, n=100)

ggplot(data = drug_curve.long.pct, aes(min, area.pct.change, group = exposure, colour = factor(exposure))) +
  geom_point(aes(colour = factor(exposure))) +
  geom_line(data= drug_curve.long.pct[!is.na(drug_curve.long.pct$area.pct.change),]) +
  #ylim(-2, 8) +
  labs(x = "min", y = "area % change") + 
  #ggtitle("slide 3, replicate 6") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
  theme_classic()
  theme(legend.position = "none")

###### single larva absolute values
ggplot(data = drug_curve.long, aes(min, area, group = exposure, colour = factor(exposure))) +
  geom_point(aes(colour = factor(exposure))) +
  geom_line(data= drug_curve.long[!is.na(drug_curve.long$area),]) +
  labs(x = "min", y = "area") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
  theme_classic()


######################################    an experiment of n= ...    ########################################
rm(X5_HT_1_uM_ALL)

combined_curve <- X8_br_cGMP_1_mM_ALL_cut_off_before_lys
print(combined_curve, n=50)

combined_curve.long <- combined_curve %>% 
  pivot_longer(cols=c(`control`, `treatment`), 
               names_to = "exposure", values_to = "area")

print(combined_curve.long, n=100)

#making % change column
combined_curve.long.pct <- combined_curve.long %>%
  group_by(exposure, larva) %>%
  mutate(
    area.pct.change = ((area - area[13]) / area[13]
    )*100) %>%
  ungroup() #why the ungroup?

print(combined_curve.long.pct, n=336)

#so that $pH could be assigned colours
combined_curve.long$exposure <- as.factor(combined_curve.long$exposure)

## compute means and sd
mean.sd <-
  combined_curve.long.pct %>%
  select(-larva) %>% ## exclude larva
  group_by(exposure, min) %>% ## group by min
  ## now compute mean and sd:
  summarize(across(everything(), na.rm= T,
                   tibble::lst(mean = mean, sd = sd))) 

print(mean.sd, n= 62)

ggplot(data = mean.sd, aes(min, area.pct.change_mean, group = exposure, colour = factor(exposure))) +
  geom_point(aes(colour = factor(exposure))) +
  geom_line(data= mean.sd[!is.na(mean.sd$area.pct.change_mean),]) +
  geom_errorbar(mapping = aes(x = min,
                              ymin = area.pct.change_mean - area.pct.change_sd,
                              ymax = area.pct.change_mean + area.pct.change_sd), 
                width = 4,
                size = 0.75) +
  #ylim(-3, 27) +
  #ggtitle("N6Bnz only, minus larva 4") +
  labs(x = "min", y = "mean area") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
  theme_classic() +
  theme(legend.position = "none")
