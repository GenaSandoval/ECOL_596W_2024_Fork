###T test

library(readr)
HornedLizards <- read_csv("datasets/HornedLizards.csv")
View(HornedLizards)

library(dplyr)
library(ggplot2)
library(dslabs)

mean_horn <- HornedLizards %>%
  group_by(Survival) %>%
  summarize(mean_horn = mean(squamosalHornLength, na.rm = TRUE))
print(mean_horn)

###Line 105 has an NA that needs to be removed
HornedLizards[105,"squamosalHornLength"]
lizards_clean <- HornedLizards[-c(105), ]
var(lizards_clean$squamosalHornLength[HornedLizards$Survival=="living"])
var(HornedLizards$squamosalHornLength[HornedLizards$Survival=="killed"])

###Hand calculating t stat and df
#N_og=185 total_N_clean=184 N_living=155-1(NA)=154 N_killed=30
#t=(living_mean-dead_mean)/sqrt((living_sample_variance/N_living)+(dead_sample_variance/N_dead))
#living_mean-dead_mean=24.3-22=2.3
#living_sample_variance/N_living=6.93/154=0.045
#dead_sample_variance/N_dead=7.34/30=0.24
#t=2.3/sqrt(0.045+0.24)=4.31
#df=(N_living+N_dead)-2
#df=(total_N)-2=184-2-182

###Calculalting p-value for 1 tailed test
t_value <- 4.31
df <- 182
p_value <- 1 - pt(t_value, df)
p_value
print(p_value)
###Super small p-value meaning we can reject the null hypothesis, since the dead lizards have significantly shorter horns than the living lizards

##check to see if p-value is reasonable
boxplot(squamosalHornLength~Survival, data=lizards_clean)
###in class
t.test(living,killed,var.equal=TRUE,alternative="greater")
t

