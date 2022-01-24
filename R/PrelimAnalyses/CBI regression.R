#CBI regression
#analysis for the four fires manuscript: CFS and BCWS 
#author: Kira Hoffman
#date: January 11, 2022

library(ggplot2)
library(tidyverse)
library(patchwork)
library(MuMIn)
library(MASS)
library(visreg)
library(ggpubr)
library(tidyverse)
library(data.table)


#CBI correlations with median dNBR

#Fig. CBI and dNBR regression
#this data includes the Nadina, Shovel, Verdun, and Island Lake fires
#includes field CBI sampling in 2020 and 2021
#one datapoint was removed as it was erronious
cbi<-read.csv("CBI.csv")

cbi

mdn<-(cbi[,1])
cor<-(cbi[,2])
val<-(cbi[,3])

cor
val


cor(x= cor, y =val)
#first pass wth 2020 data
#0.6658745

#when we add in the additional 2021 data
#0.72

ggplot(cbi, aes(x = val, y = cor)) +
  geom_point() +
  stat_smooth()

model <- lm(val ~ cor, data = cbi)

summary(model)

confint(model)

sigma(model)*100/mean(cbi$val)

#final figure
ggplot(cbi, aes(val, cor)) +
  geom_point() +
  stat_smooth(method = lm) +
  labs(title= "Relationship between CBI and dNBR",
       x = "Composite burn index (CBI)",
       y = "Differenced normalized burn ratio (dNBR)")   

