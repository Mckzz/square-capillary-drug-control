install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
library(lmerTest)

####area plateaus
drug_curve <- X2021_Feb_19_8_CPT_0_5_mM_rep_2
print(drug_curve, n=50)

drug_curve.long <- drug_curve %>% 
  pivot_longer(cols=c(`control`, `treatment`), 
               names_to = "exposure", values_to = "area")

print(drug_curve.long, n=100)

#so that $pH could be assigned colours
drug_curve.long$exposure <- as.factor(drug_curve.long$exposure)

#making % change column
drug_curve.long.pct <- drug_curve.long %>%
  group_by(exposure) %>%
  mutate(
    area.pct.change = ((area - area[13]) / area[13]
    )*100) %>%
  ungroup() #why the ungroup?

print(drug_curve.long.pct, n=100)

# remove over night time point
drug_curve.long.pct <- drug_curve.long.pct %>%
  mutate(area.pct.change = replace(area.pct.change, min == 260, NA))

####### plot normalized to end equilibration
ggplot(data = drug_curve.long.pct, aes(min, area.pct.change, group = exposure, colour = factor(exposure))) +
  geom_point(aes(colour = factor(exposure))) +
  geom_line(data= drug_curve.long.pct[!is.na(drug_curve.long.pct$area.pct.change),]) +
  #ylim(-2, 8) +
  labs(x = "Min", y = "Area % change") + 
  #ggtitle("slide 3, replicate 6") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
  theme_classic() #+
 # theme(legend.position = "none")

###### single larva absolute values
ggplot(data = drug_curve.long, aes(min, area, group = exposure, colour = factor(exposure))) +
  geom_point(aes(colour = factor(exposure))) +
  geom_line(data= drug_curve.long[!is.na(drug_curve.long$area),]) +
  labs(x = "min", y = "area") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
  theme_classic()

##
######################################    an experiment of n= ...    ########################################
combined_curve <- X2021_Feb_8_CPT_0_5_mM_ALL
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

# remove over night time point (dont do for 1 mM 8-Br-cAMP because 260 is real (all went to 270))
#combined_curve.long.pct <- combined_curve.long.pct %>%
 # mutate(area.pct.change = replace(area.pct.change, min == 260, NA))

## compute means and sd
mean.sd <-
  combined_curve.long.pct %>%
  select(-larva) %>% ## exclude larva
  group_by(exposure, min) %>% ## group by min
  ## now compute mean and sd:
  summarize(across(everything(), na.rm= T,
                   tibble::lst(mean = mean, sd = sd))) 

print(mean.sd, n= 62)


ggplot(data = mean.sd,
       aes(min, 
           area.pct.change_mean, 
           group = exposure, 
           colour = factor(exposure))) +
  geom_point(aes(colour = factor(exposure))) +
  geom_line(data= mean.sd[!is.na(mean.sd$area.pct.change_mean),]) +
  geom_errorbar(mapping = aes(x = min,
                              ymin = area.pct.change_mean - area.pct.change_sd,
                              ymax = area.pct.change_mean + area.pct.change_sd), 
                width = 4,
                size = 0.75) +
  #ylim(-3, 15) +
  #ggtitle("N6Bnz 0.5 mM, trivittatus") +
  labs(x = "Min", y = "Mean area change (%)") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 16)) +
  theme_classic() +
  theme(legend.position = "none")


###############################     stats     ################################

#for equlb and last time points

#min = 240 as stopping point in the % change data to compare treat/ control differences at
end.pct <- combined_curve.long.pct %>%
  filter(min == 240)

print(end.pct, n= 16)

# produces a mean for the tratment that is the difference from the intercept (here, the control)


prior <- list(
  R = list(V = 1, nu = 0.2), 
  G = list(G = list(V = 2, nu = 0.2)))
  #B = list(mu = 0, V=I*1e+10))

mcmod.end.pct <-
  MCMCglmm::MCMCglmm(
    area.pct.change ~ exposure, random = ~larva,
    data = end.pct, scale = FALSE, prior = prior,
    nitt = 1300000, thin = 1000, burnin = 300000, 
    verbose = FALSE
  )
summary(mcmod.end.pct)

# How important is larval identity?
mean(mcmod.end.pct$VCV[,1]/(mcmod.end.pct$VCV[,1] + mcmod.end.pct$VCV[,2]))



####################  frequentist  #######################

mod <- lmer(area.pct.change ~ exposure + (1|larva), data = end.pct)
summary(mod)
anova(mod)

# could use lme4:: but gets rid of p value. Otherwise exact same estimates





