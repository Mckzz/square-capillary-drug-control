install.packages("tidyverse")
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(Hmisc)
rm(Hmisc)
install.packages("Hmisc")

####area plateaus
rm(X8_Bromo_cAMP_thaps_8_BromocAMP)

drug_curve <- X8_Bromo_cAMP_thaps_8_BromocAMP

print(drug_curve, n=50)

drug_curve.long <- drug_curve %>% 
  pivot_longer(cols=c(`control`, `treatment`), 
               names_to = "exposure", values_to = "area")

print(drug_curve.long, n=50)

#so that $pH could be assigned colours
drug_curve.long$exposure <- as.factor(drug_curve.long$exposure)

ggplot(data = drug_curve.long, aes(min, area, group = exposure)) +
  geom_line(aes(colour = factor(exposure))) +
  labs(title= "2020 Nov 25, cAMP at 60 min, thaps + cAMP at 120 min", x = "min", y = "area") + 
  theme_classic()
