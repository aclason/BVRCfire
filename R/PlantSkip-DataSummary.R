### Data summaries and visualizations 
# A. Clason & Ingrid Farnell
# March, 2022

#--libraries
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster","sf")) # geo comp.,
ls <- append(ls,c("ggplot2","ggarrange"))
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#-----------------------------Load data----------------------------------------#
datPath <- "C:/Users/farne/Documents/" #"./Inputs/"   
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"

#--Read in data that went into the Random Forest analysis:
Chutanli <- fread(paste0(datPath,"G41607dat270.csv"))
Chutanli[,dNBR := dNBR*1000]
Chutanli[HistoricFires==0 ,HistoricFires:=100]

Tezzeron <- fread(paste0(datPath,"G51632dat270.csv"))
Tezzeron[,dNBR := dNBR*1000]
#Tezzeorn[HistoricFires==0 ,HistoricFires:=100] # no historic fires

Shovel <- fread(paste0(datPath,"R11498dat270.csv"))
Shovel[,dNBR := dNBR*1000]
Shovel[HistoricFires==0 ,HistoricFires:=100]

Verdun <- fread(paste0(datPath,"R11796dat270.csv"))
Verdun[,dNBR := dNBR*1000]
Verdun[HistoricFires==0 ,HistoricFires:=100]

Island <- fread(paste0(datPath,"R11921dat270.csv"))
Island[,dNBR := dNBR*1000]
Island[HistoricFires==0 ,HistoricFires:=100]

Nadina <- fread(paste0(datPath,"R21721dat270.csv"))
Nadina[,dNBR := dNBR*1000]
Nadina[HistoricFires==0 ,HistoricFires:=100]

# Study fire perimeters
StudyFirePerims <- read_sf("./Inputs/Shapefiles/Study_fire_perimeters.shp")

# dNBR rasters
FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
dNBR_list <- list.files(paste0(SpatialFilesPath,"/Inputs/Rasters/dNBR/"),
                       pattern = paste(FiresOfInterest, sep = "", collapse = "|"), 
                       recursive = FALSE, 
                       full.names=TRUE)

# Plantation pred shapfile
plant_list <- list.files(paste0(SpatialFilesPath,"/Inputs/Shapefiles/PlantationPreds/"),
                         pattern = ".shp", 
                         recursive = FALSE, 
                         full.names=TRUE)

#----------------------------Data Summaries------------------------------------#
#--- Percent silvicultural treatment / fire
#this is based off pixels that were analysed

Silvic_Vars <- c("BroadBurn", "Brushed", "DebrisMade", "DebrisPiled", "Fertil", "PileBurn", 
                 "Prune", "Soil", "Spaced", "SpotBurn", "TotalRows")
FireName <- c("Chutanli","Nadina","Shovel","Island","Verdun","Tezzeron")
dat_list <- list(Chutanli, Nadina, Shovel, Island, Verdun, Tezzeron)
SilvicTable <- list()

# Count number of pixels that == 1 (by summing) in each treatment
for(i in 1:length(FireName)){
  dat <- dat_list[[i]]
  dat[,TotalRows:= 1]
  SilvicOutput<- dat[, lapply(.SD, sum), .SDcols = (colnames(dat) %in% Silvic_Vars)]
  SilvicTable[[i]] <- SilvicOutput[,Fire:=FireName[i]]
}

# Percent of pixels in each treatment
SilvicPC <- SilvicTable %>% 
  reduce(full_join) %>% # make into one table
  dplyr::select("Fire", everything()) %>% # move "Fire" to first column
  mutate_at(vars(-c(TotalRows,Fire)), ~round(./TotalRows*100, 1)) %>% # Divide  by total rows to get % silviculture type/fire
  dplyr::select(-c(TotalRows)) # Drop total rows


#--- Percent burn severity class within plantations
# this is at the pixel level, area within plantations







#--- total area of the fire
fires <- fire_per_sel %>% filter(FIRE_NUMBE %in% Fire_shortList) # IF: what file is this using?
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









