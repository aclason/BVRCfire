### Data summaries and visualizations 
# A. Clason & Ingrid Farnell
# March, 2022

#libraries
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster","sf")) # geo comp.,
ls <- append(ls,c("ggplot2","ggarrange"))
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)

#Read in data that went into the Random Forest analysis:
ctg_variables <- c("BEC", "BroadBurn", "Brushed", "DebrisMade", "DebrisPiled", "Fertil", "MechUnk", 
                   "OPENING_ID", "PileBurn", "Prune", "Soil", "Spaced", 
                   "SpotBurn", "WBurn")
datPath <- "./Inputs/" #"C:/Users/farne/Documents/"

ctg_variables <- c("BEC", "BroadBurn", "Brushed", "DebrisMade", "DebrisPiled", "Fertil", "MechUnk", 
                   "OPENING_ID", "PileBurn", "Prune", "Soil", "Spaced", 
                   "SpotBurn", "WBurn","dNBRCAT")
datPath <-  "./Inputs/"  #"C:/Users/farne/Documents/" 

Chutanli <- fread(paste0(datPath,"G41607dat270.csv"))
Chutanli <- Chutanli %>%
  mutate_at((colnames(Chutanli)[colnames(Chutanli) %in% ctg_variables]), factor) %>%
  dplyr::select(-c("dNBRReSamp")) #can remove if you remembered to save without row.names
Chutanli[,dNBR := dNBR*1000]
Chutanli[HistoricFires==0 ,HistoricFires:=100]
Chutanli_dist <- dist(Chutanli[,.(x,y)], method = "euclidean")
Chutanli_pcnm <- pcnm(Chutanli_dist)
Chutanli[, c("PCNM1","PCNM2", "PCNM3",
             "PCNM4", "PCNM5","PCNM6") := .(Chutanli_pcnm$vectors[,"PCNM1"],
                                            Chutanli_pcnm$vectors[,"PCNM2"],
                                            Chutanli_pcnm$vectors[,"PCNM3"],
                                            Chutanli_pcnm$vectors[,"PCNM4"],
                                            Chutanli_pcnm$vectors[,"PCNM5"],
                                            Chutanli_pcnm$vectors[,"PCNM6"])]

Tezzeron <- fread(paste0(datPath,"G51632dat270.csv"))
Tezzeron <- Tezzeron %>%
  mutate_at((colnames(Tezzeron)[colnames(Tezzeron) %in% ctg_variables]), factor)%>%
  dplyr::select(-c("dNBRReSamp"))
Tezzeron[,dNBR := dNBR*1000]
Tezzeron_dist <- dist(Tezzeron[,.(x,y)], method = "euclidean")
Tezzeron_pcnm <- pcnm(Tezzeron_dist)
Tezzeron[, c("PCNM1","PCNM2", "PCNM3",
             "PCNM4", "PCNM5","PCNM6") := .(Tezzeron_pcnm$vectors[,"PCNM1"],
                                            Tezzeron_pcnm$vectors[,"PCNM2"],
                                            Tezzeron_pcnm$vectors[,"PCNM3"],
                                            Tezzeron_pcnm$vectors[,"PCNM4"],
                                            Tezzeron_pcnm$vectors[,"PCNM5"],
                                            Tezzeron_pcnm$vectors[,"PCNM6"])]

Shovel <- fread(paste0(datPath,"R11498dat270.csv"))
Shovel <- Shovel %>%
  mutate_at((colnames(Shovel)[colnames(Shovel) %in% ctg_variables]), factor)%>%
  dplyr::select(-c("dNBRReSamp"))
Shovel[,dNBR := dNBR*1000]
Shovel[HistoricFires==0 ,HistoricFires:=100]
Shovel_dist <- dist(Shovel[,.(x,y)], method = "euclidean")
Shovel_pcnm <- pcnm(Shovel_dist)
Shovel[, c("PCNM1","PCNM2", "PCNM3",
           "PCNM4", "PCNM5","PCNM6") := .(Shovel_pcnm$vectors[,"PCNM1"],
                                          Shovel_pcnm$vectors[,"PCNM2"],
                                          Shovel_pcnm$vectors[,"PCNM3"],
                                          Shovel_pcnm$vectors[,"PCNM4"],
                                          Shovel_pcnm$vectors[,"PCNM5"],
                                          Shovel_pcnm$vectors[,"PCNM6"])]

Verdun <- fread(paste0(datPath,"R11796dat270.csv"))
Verdun <- Verdun %>%
  mutate_at((colnames(Verdun)[colnames(Verdun) %in% ctg_variables]), factor)%>%
  dplyr::select(-c("dNBRReSamp"))
Verdun[,dNBR := dNBR*1000]
Verdun[HistoricFires==0 ,HistoricFires:=100]
Verdun_dist <- dist(Verdun[,.(x,y)], method = "euclidean")
Verdun_pcnm <- pcnm(Verdun_dist)
Verdun[, c("PCNM1","PCNM2", "PCNM3",
           "PCNM4", "PCNM5","PCNM6") := .(Verdun_pcnm$vectors[,"PCNM1"],
                                          Verdun_pcnm$vectors[,"PCNM2"],
                                          Verdun_pcnm$vectors[,"PCNM3"],
                                          Verdun_pcnm$vectors[,"PCNM4"],
                                          Verdun_pcnm$vectors[,"PCNM5"],
                                          Verdun_pcnm$vectors[,"PCNM6"])]

Island <- fread(paste0(datPath,"R11921dat270.csv"))
Island <- Island %>%
  mutate_at((colnames(Island)[colnames(Island) %in% ctg_variables]), factor)%>%
  dplyr::select(-c("dNBRReSamp"))
Island[,dNBR := dNBR*1000]
Island[HistoricFires==0 ,HistoricFires:=100]
Island_dist <- dist(Island[,.(x,y)], method = "euclidean")
Island_pcnm <- pcnm(Island_dist)
Island[, c("PCNM1","PCNM2", "PCNM3",
           "PCNM4", "PCNM5","PCNM6") := .(Island_pcnm$vectors[,"PCNM1"],
                                          Island_pcnm$vectors[,"PCNM2"],
                                          Island_pcnm$vectors[,"PCNM3"],
                                          Island_pcnm$vectors[,"PCNM4"],
                                          Island_pcnm$vectors[,"PCNM5"],
                                          Island_pcnm$vectors[,"PCNM6"])]

Nadina <- fread(paste0(datPath,"R21721dat270.csv"))
Nadina <- Nadina %>%
  mutate_at((colnames(Nadina)[colnames(Nadina) %in% ctg_variables]), factor)%>%
  dplyr::select(-c("dNBRReSamp"))
Nadina[,dNBR := dNBR*1000]
Nadina[HistoricFires==0 ,HistoricFires:=100]
Nadina_dist <- dist(Nadina[,.(x,y)], method = "euclidean")
Nadina_pcnm <- pcnm(Nadina_dist)
Nadina[, c("PCNM1","PCNM2", "PCNM3",
           "PCNM4", "PCNM5","PCNM6") := .(Nadina_pcnm$vectors[,"PCNM1"],
                                          Nadina_pcnm$vectors[,"PCNM2"],
                                          Nadina_pcnm$vectors[,"PCNM3"],
                                          Nadina_pcnm$vectors[,"PCNM4"],
                                          Nadina_pcnm$vectors[,"PCNM5"],
                                          Nadina_pcnm$vectors[,"PCNM6"])]

#ordisurf(Chutanli_xy, scores(Chutanli_pcnm, choices=1), bubble = 4, main = "PCNM 1")
#plot(Chutanli_pcnm$values) #most the variation is in the first 20 or so eigenvectors

#ggplot()+
# geom_point(aes(y=Chutanli$dNBR, x= Chutanli_pcnm$vectors[,17]))+
#geom_smooth(aes(y=Chutanli$dNBR, x= Chutanli_pcnm$vectors[,17]), method="gam")

#create datasets with variables to include in analysis. We are keeping them (cat response and continuous response) as seperate datasets, because it's imbedded in the code below to pass the entire object and not specify which columns to ignore
Chutanli_sf_con <- sf::st_as_sf(Chutanli[,-c("dNBRCAT")], coords = c("x", "y"))
Chutanli_sf_cat <- sf::st_as_sf(Chutanli[,-c("dNBR")], coords = c("x", "y"))

Tezzeron_sf_con <- sf::st_as_sf(Tezzeron[,-c("dNBRCAT")], coords = c("x", "y"))
Tezzeron_sf_cat <- sf::st_as_sf(Tezzeron[,-c("dNBR")], coords = c("x", "y"))

Shovel_sf_con <- sf::st_as_sf(Shovel[,-c("dNBRCAT")], coords = c("x", "y"))
Shovel_sf_cat <- sf::st_as_sf(Shovel[,-c("dNBR")], coords = c("x", "y"))

Verdun_sf_con <- sf::st_as_sf(Verdun[,-c("dNBRCAT")], coords = c("x", "y"))
Verdun_sf_cat <- sf::st_as_sf(Verdun[,-c("dNBR")], coords = c("x", "y"))

Island_sf_con <- sf::st_as_sf(Island[,-c("dNBRCAT")], coords = c("x", "y"))
Island_sf_cat <- sf::st_as_sf(Island[,-c("dNBR")], coords = c("x", "y"))

Nadina_sf_con <- sf::st_as_sf(Nadina[,-c("dNBRCAT")], coords = c("x", "y"))
Nadina_sf_cat <- sf::st_as_sf(Nadina[,-c("dNBR")], coords = c("x", "y"))


#Important covariates in Nadina
a <- ggplot(Nadina)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")

b <- ggplot(Nadina)+
  geom_point(aes(y=dNBR,x=PineCov))+
  geom_smooth(aes(y=dNBR,x=PineCov),method="gam")

c <- ggplot(Nadina)+
  geom_point(aes(y=dNBR,x=BASAL_AREA))+
  geom_smooth(aes(y=dNBR,x=BASAL_AREA),method="gam")

d <- ggplot(Nadina)+
  geom_point(aes(y=dNBR,x=maxT))+
  geom_smooth(aes(y=dNBR,x=maxT),method="gam")

e <- ggplot(Nadina)+
  geom_boxplot(aes(y=dNBR,x=Spaced))

f <- ggplot(Nadina)+
  geom_boxplot(aes(y=dNBR,x=DebrisPiled))

ggarrange(a,b,c,d,e,f)

## Shovel
a <- ggplot(Shovel)+
  geom_point(aes(y=dNBR,x=CROWN_CLOS))+
  geom_smooth(aes(y=dNBR,x=CROWN_CLOS),method="gam")

b <- ggplot(Shovel)+
  geom_point(aes(y=dNBR,x=isi))+
  geom_smooth(aes(y=dNBR,x=isi),method="gam")

c <- ggplot(Shovel)+
  geom_point(aes(y=dNBR,x=HistoricFires))+
  geom_smooth(aes(y=dNBR,x=HistoricFires),method="gam")

d <- ggplot(Shovel)+
  geom_point(aes(y=dNBR,x=SpruceCov))+
  geom_smooth(aes(y=dNBR,x=SpruceCov),method="gam")

e <- ggplot(Shovel)+
  geom_boxplot(aes(y=dNBR,x=BroadBurn))

f <- ggplot(Shovel)+
  geom_boxplot(aes(y=dNBR,x=DebrisMade))

ggarrange(a,b,c,d,e,f)


#plantation Age for all fires:
a <- ggplot(Nadina)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Nadina")

b <- ggplot(Shovel)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Shovel")

c <- ggplot(Island)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Island")

d <- ggplot(Verdun)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Verdun")

e <- ggplot(Tezzeron)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Tezzeron")

f <- ggplot(Chutanli)+
  geom_point(aes(y=dNBR,x=PlantAge))+
  geom_smooth(aes(y=dNBR,x=PlantAge),method="gam")+
  xlab("Chutanli")

ggarrange(a,b,c,d,e,f)



########################################################################################################

library(ggpubr)
a <- ggplot(data=Chutanli, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  xlab(NULL)+
  ggtitle("Chutlani")

b <- ggplot(data=Shovel, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Shovel")

c <- ggplot(data=Nadina, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Nadina")

d <- ggplot(data=Island, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ggtitle("Island")

e <- ggplot(data=Verdun, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  xlim(c(0,60))+
  ylab(NULL)+
  ylim(c(-500,1200))+
  ggtitle("Verdun")

f <- ggplot(data=Tezzeron, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="lm",aes(colour=dNBRCAT,fill=dNBRCAT))+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylab(NULL)+
  ylim(c(-500,1200))+
  ggtitle("Tezzeron")

ggarrange(a,b,c,d,e,f)


a <- ggplot(data=Chutanli, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_smooth(method="gam")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  xlab(NULL)+
  ggtitle("Chutlani")

b <- ggplot(data=Shovel, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="gam")+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Shovel")

c <- ggplot(data=Nadina, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="gam")+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Nadina")

d <- ggplot(data=Island, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="gam")+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylim(c(-500,1200))+
  ggtitle("Island")

e <- ggplot(data=Verdun, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  theme(legend.position = "none")+
  geom_smooth(method="gam")+
  xlim(c(0,60))+
  ylab(NULL)+
  ylim(c(-500,1200))+
  ggtitle("Verdun")

f <- ggplot(data=Tezzeron, aes(x=PlantAge, y=dNBR))+
  geom_point(aes(colour=dNBRCAT))+
  theme_minimal() +
  geom_smooth(method="gam")+
  theme(legend.position = "none")+
  xlim(c(0,60))+
  ylab(NULL)+
  ylim(c(-500,1200))+
  ggtitle("Tezzeron")

ggarrange(a,b,c,d,e,f)







