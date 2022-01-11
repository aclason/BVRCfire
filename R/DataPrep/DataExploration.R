#manuscript: Ecological legacies of broadcast burning
#author: Kira Hoffman
#date: October 20, 2021

library(ggplot2)
library(tidyverse)
library(patchwork)
library(MuMIn)
library(MASS)
library(visreg)
library(ggpubr)
library(tidyverse)
library(data.table)

## ANALYSIS

#set the fires of interest
study_fireTable <- fread("./Inputs/StudyFireList.csv") #all potential

study_fireTable

FiresOfInterest <- c( "R11796","R11498","R21721","R11921")

dt <- data.table()
Plantations <- data.table()
for(j in 1:length(FiresOfInterest)){
  dt <- fread(paste0("./Outputs/IndividualFires/",FiresOfInterest[j],"_Firedat_SP.csv")) # Alana needs to push "IndividualFires" to github
  dt[, ':='(FireID = FiresOfInterest[j])]
  dt[,.N,by=OPENING_ID]
  dt[,.N]
  Plantations <- rbind(Plantations,dt,fill=TRUE)
}
#to look at a certain varible
Plantations$PileBurn

Plantations$dNBR_sc



#Exploratory stats

ggplot(Plantations, aes(factor(SitePrepped), Mn_dNBR, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(BroadBurn), Mn_dNBR, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(DebrisBurn), Mn_dNBR, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(DebrisPiledBurn), Mn_dNBR, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(Soil), Mn_dNBR, color=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(DebrisPiled), Mn_dNBR, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(DebrisMade), Mn_dNBR, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(WBurn), Mn_dNBR, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(Brushed), Mn_dNBR, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(GrassSeed), Mn_dNBR, colour=FireID)) +
  geom_boxplot() 


#making new columns in the datatable to look at the effect of burn types and groupings

Plantations[,NoBurn:=as.factor(ifelse(PileBurn==0 & BroadBurn== 0 & SpotBurn==0 & WBurn==0,1,0))]

data.frame(table(Plantations$NoBurn))

Plantations[,AnyBurn:=as.factor(ifelse(PileBurn==1 | BroadBurn== 1 | SpotBurn==1 | WBurn==1,1,0))]

data.frame(table(Plantations$AnyBurn))

Plantations[,BroadBurn:=as.factor(ifelse(PileBurn==0 | BroadBurn== 1 | SpotBurn==1 | WBurn==1,1,0))]

data.frame(table(Plantations$BroadBurn))

data.frame(table(Plantations$WBurn))


Plantations[,PileNoBurn:=as.factor(ifelse(DebrisPiled ==1 & BroadBurn == 0 |
                                         DebrisPiled ==1 & SpotBurn == 0 |
                                         DebrisPiled ==1 & PileBurn ==0,1,0))]
                             
data.frame(table(Plantations$PileNoBurn))

Plantations[,DebrisPile_BroadBurn:=as.factor(ifelse(DebrisPiled ==1 & BroadBurn == 1 |
                                            DebrisPiled ==1 & SpotBurn == 1 |
                                            DebrisPiled ==1 & PileBurn==0,1,0))]

data.frame(table(Plantations$DebrisPile_BroadBurn))


Plantations[,Pile_Burn:=as.factor(ifelse(DebrisPiled ==1 & BroadBurn == 0 |
                                            DebrisPiled ==1 & SpotBurn == 0 |
                                            DebrisPiled ==1 & PileBurn==1,1,0))]
data.frame(table(Plantations$Pile_Burn))


Plantations[,DebrisMade_Burn:=as.factor(ifelse(DebrisMade ==1 & BroadBurn == 1 |
                                              DebrisMade ==1 & SpotBurn == 1 |
                                              DebrisMade ==1 & PileBurn==0,1,0))]
data.frame(table(Plantations$DebrisMade_Burn))


Plantations[,DebrisMade_NoBurn:=as.factor(ifelse(DebrisMade ==1 & BroadBurn == 0 |
                                                 DebrisMade ==1 & SpotBurn == 0 |
                                                 DebrisMade ==1 & PileBurn==0,1,0))]
data.frame(table(Plantations$DebrisMade_NoBurn))


Plantations[,SoilDist_Burn:=as.factor(ifelse(Soil ==1 & BroadBurn == 1 |
                                            Soil ==1 & SpotBurn == 1 |
                                            Soil ==1 & PileBurn==1,1,0))]

data.frame(table(Plantations$SoilDist_Burn))

Plantations[,SoilDist_NoBurn:=as.factor(ifelse(Soil ==1 & BroadBurn == 0 |
                                               Soil ==1 & SpotBurn == 0 |
                                               Soil ==1 & PileBurn==0,1,0))]

data.frame(table(Plantations$SoilDist_NoBurn))

data.frame(table(Plantations$SitePrepped))

Plantations[,Brush_Burn:=as.factor(ifelse(Brushed ==1 & BroadBurn == 1 |
                                               Brushed ==1 & SpotBurn == 1 |
                                               Brushed ==1 & PileBurn==1,1,0))]
Plantations[,Brush_NoBurn:=as.factor(ifelse(Brushed ==1 & BroadBurn == 0 |
                                            Brushed ==1 & SpotBurn == 0 |
                                            Brushed ==1 & PileBurn==0,1,0))]


ggplot(Plantations, aes(factor(NoBurn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(AnyBurn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(PileNoBurn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(Pile_Burn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(DebrisMade_Burn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(DebrisMade_NoBurn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(SoilDist_Burn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(SoilDist_NoBurn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(Brush_Burn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(Brush_NoBurn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(DebrisPile_BroadBurn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 

ggplot(Plantations, aes(factor(BroadBurn), dNBR_sc, colour=FireID)) +
  geom_boxplot() 




+
  labs(title= "Relationship between CBI and dNBR",
       x = "Composite burn index (CBI)",
       y = "Differenced normalized burn ratio (dNBR)")   




ggplot(data=Plantations, )

mod1 <- glm(Sev~Burn+Age+Dem+Bas+Asp+fire+dob+cov, data = all, family = gaussian)
mod2 <- glm(Sev~Burn+Age+Dem+Bas, data = all, family = gaussian)
mod3 <- glm(Sev~Burn+Age+Dem+Bas+t_area, data = all, family = gaussian)

summary(mod1)
summary(mod2)
summary(mod3)

summary(mod1)$coef
summary(mod2)$coef
summary(mod3)$coef

#assessing deviance
1 - pchisq(deviance(identity),df.residual(identity))

print(identity)

confint(mod1)
coef(mod1)

#looking at the spread of the data
gg_jitter <- ggplot(data = all, aes(x = Sev, y = Dem)) + 
  geom_jitter(width = 0, height = 0.05) +
  ylab("TPI") +
  xlab("Fire severity (dNBR)")

gg_jitter

options(na.action = "na.fail")
dredge(mod1)


get.models(identity)
stepAIC(identity)

#95% CI slope
confint(mod2)
confint(mod3)
#compare the res sum of square
anova(mod2)
anova(mod3)


#CBI correlations with median dNBR

#Fig. CBI and dNBR regression
cbi<-read.csv("CBI.csv")

mdn<-(cbi[,1])
cor<-(cbi[,2])
val<-(cbi[,3])

cor(x= val, y =cor)
#0.6658745

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

       

#relationship between basal area and severity
bas_plot<-ggplot(all, aes(Bas, Sev, color =fire)) +
  geom_point() +
  facet_grid(Burn ~ .) +
  labs(title= "Relationship between basal area and dNBR",
       x = "Total basal area (m2)",
       y="dNBR")

#relationship between Age and Severity    
age_plot<-ggplot(all, aes(Age, Sev, color =fire)) +
  geom_point() +
  facet_grid(Burn ~ .) +
  labs(title= "Relationship between plantation age and dNBR",
       x = "Plantation age in years",
       y="dNBR")


#relationship between TPI and severity
tpi_plot<-ggplot(all, aes(Dem, Sev, color =fire)) +
  geom_point() +
  facet_grid(Burn ~ .) +
  labs(title= "Relationship between TPI and dNBR",
       x = "Topographic position index (TPI)",
       y ="dNBR")

# multiple plots side by side_FINAl figure
age_plot / tpi_plot / bas_plot + plot_layout(c(1,3))

###################################################

#Fig fire weather 

fwi<-read.csv("all_fires_FWI.csv")
fwi<-read.csv("All_fires_FWI_4.csv")

dnbr<-(fwi[,1])
treat<-(fwi[,2])
fire<-(fwi[,3])
dob<-(fwi[,4])
date<-(fwi[,5])
temp<-(fwi[,6])
prec<-(fwi[,7])
rh<-(fwi[,8])
wind<-(fwi[,9])
bui<-(fwi[,10])
fw<-(fwi[,11])


#rh
rh_plot<-ggplot(data = fwi, aes(x = rh, y = dnbr, color =fire)) +geom_point() +
  labs(
    x = "Mean % relative humidity",
    y = "dNBR") +
  theme(text=element_text(size = 14))

#temperature
temp_plot<-ggplot(data = fwi, aes(x = temp, y = dnbr, color =fire)) +geom_point() +labs(
  x = "Mean temperature in degrees C",
  y = "dNBR") +
  theme(text=element_text(size = 14))

#wind
wind_plot<-ggplot(data = fwi, aes(x = wind, y = dnbr, color =fire)) +geom_point() +labs(
  x = "Mean wind in meters per second",
  y = "dNBR") +
  theme(text=element_text(size = 14))

#precipitation
prec_plot<-ggplot(data = fwi, aes(x = prec, y = dnbr, color =fire)) +geom_point() +labs(
  x = "Precipitation in mm",
  y = "dNBR") +
  theme(text=element_text(size = 14))


# multiple plots side by side
rh_plot / temp_plot / wind_plot/prec_plot + plot_layout(c(1,4))    


##############################
#Appendix 

#basal area and plantation age
bas1_plot<-ggplot(all, aes(Bas, Age, color =Burn)) +
  geom_point() +
  labs(title= "Relationship between basal area and Plantation Age",
       x = "Total basal area (m2)",
       y="Plantation age in years")

bas1_plot +guides(fill = FALSE)


#####boxplot of fire severity by fire
ggplot(all, aes(x = fire, y = Sev, colour = fire)) +
  geom_boxplot() +
  scale_shape_manual(values = c(1,2,3,4)) +
  scale_colour_brewer(palette = "Set1")

ggplot(all, aes(x = fire, y = fw, colour = fire)) +
  geom_boxplot() +
  scale_shape_manual(values = c(1,2,3,4)) +
  scale_colour_brewer(palette = "Set1")



#bui for inclusion 
ggplot(data = fwi, aes(x = bui, y = dnbr, color =fire)) +geom_point() +labs(
  x = "Build up index",
  y = "dNBR") +
  theme(text=element_text(size = 14))

ggplot(data = fwi, aes(x = fw, y = fire, color =fire)) +geom_boxplot() +labs(
  x = "Fire weather index",
  y = "Fire") +
  theme(text=element_text(size = 14))


cor(x= bui, y =fw)
