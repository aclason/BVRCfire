#CBI regression
#analysis for the four fires manuscript: CFS and BCWS 
#author: Kira Hoffman (alana edits)
#date: January 11, 2022

library(ggplot2)
#library(tidyverse) #none of these libraries are needed, mostly just base r with ggplot
#library(patchwork)
#library(MuMIn)
#library(MASS)
#library(visreg)
library(ggpubr)
#library(data.table)


#CBI correlations with median dNBR

#Fig. CBI and dNBR regression
#this data includes the Nadina, Shovel, Verdun, and Island Lake fires
#includes field CBI sampling in 2020 and 2021
#one datapoint was removed as it was erronious
cbi<-read.csv("./Inputs/CBI.csv")

cbi #just note that your first column reads in weird here. I've noticed this with the new excel and read.csv
names(cbi) <- c("med_dNBR","dNBR_cor","CBI_tot")
#avoid making and dealing with everything as vectors if you can...
#mdn<-(cbi[,1])
#cor<-(cbi[,2]) #don't use function names as object names if you can avoid it
#val<-(cbi[,3])

summary(lm(med_dNBR ~ CBI_tot, data=cbi)) #pass the data.frame and call the column names

#cor(x= cor, y =val)
#first pass wth 2020 data
#0.6658745

#when we add in the additional 2021 data
#0.72

ggplot(cbi, aes(x = CBI_tot, y = med_dNBR)) +
  geom_point() +
  geom_smooth(method=lm)

#### i didn't use anything below here.

model <- lm(val ~ cor, data = cbi)

summary(model)

confint(model)

sigma(model)*100/mean(cbi$val) #you need to reassign the vector back to the data.frame if you want to call it as part of the data.frame

#final figure
ggplot(cbi, aes(val, cor)) +
  geom_point() +
  stat_smooth(method = lm) +
  labs(title= "Relationship between CBI and dNBR",
       x = "Composite burn index (CBI)",
       y = "Differenced normalized burn ratio (dNBR)")   

