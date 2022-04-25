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


#total area of the fire
fires <- fire_per_sel %>% filter(FIRE_NUMBE %in% Fire_shortList)
fires$TotFireArea <- st_area(fires)
firesDT <- as.data.table(fires)
firesDT[,TotFireArea := unclass(TotFireArea)/10000]
firesDTmerge <- firesDT[,.(FIRE_NUMBE,FIRE_YEAR,TotFireArea)]
firesDT[,sum(TotFireArea)]
############## All plantations summed ##########
#figuring out area wthin each fire severity category for the whole fire  - might have done this already??
Fire_Sev <- severity_sel %>% filter(FIRE_NUMBE %in% Fire_shortList)
#For every fire, calculate the area (ha) of each severity polygon
Fire_Sev$FireSevArea <- st_area(Fire_Sev)
Fire_SevDT <- as.data.table(Fire_Sev)
FireSevSum <- Fire_SevDT[,round(sum(unclass(FireSevArea)/10000),0),by=c("FIRE_NUMBE","BURN_SEVER")]
setnames(FireSevSum,"V1","FireSev")
#Openings and fire severity
PlantAgeLong <- melt(PlantationsAge[,.(FireID,OPENING_ID,High,Medium,Low,Unburned,TotOpenArea)],
                     id.vars = c("FireID","OPENING_ID","TotOpenArea"))
PlantAgeLong[,PropArea:= value*TotOpenArea]
PlantSevSum <- PlantAgeLong[,round(sum(na.omit(PropArea)),0),by=c("FireID","variable")]
setnames(PlantSevSum,"V1","PlantSev")
PlantFire_Sev <- merge(FireSevSum,PlantSevSum, by.x=c("FIRE_NUMBE","BURN_SEVER"),by.y=c("FireID","variable"))
PlantFire_Sev_Tot <- merge(PlantFire_Sev,firesDTmerge, by="FIRE_NUMBE")
PlantFire_Sev_Tot[,sum(PlantSev),by="FIRE_NUMBE"]
PlantFire_Sev_Tot[,sum(PlantSev)]
PlantFire_Sev_Tot[,sum(PlantSev)]



############### Unioned together ##############
#versus when unioned together (accounting for spatial overlap)
Plant_union <- fread("./Outputs/IndividualFires/PropBurnDat.csv")
Plant_union <- Plant_union %>% filter(FIRE_NUMBE %in% Fire_shortList)
Plant_union[,sum(PlantAreaSev),by="FIRE_NUMBE"]
Plant_union[,sum(na.omit(PlantAreaSev))] #unioned ==22713 - can't be right


PlantFire_Sev_PlUnion <-merge(PlantFire_Sev,Plant_union, by=c("FIRE_NUMBE","BURN_SEVER"))







