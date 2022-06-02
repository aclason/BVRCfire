### Data summaries and visualizations 
# A. Clason & Ingrid Farnell
# March, 2022

# This script gathers summary statistics for the manuscript
# 1. Percent silvicultural treatment per fire
#    a. only pixels analysed
#    b. the whole fire area
# 2. Percent burn severity class within plantations
# 3. Burn severity variability within plantations (% burn severity in each class in each opening ID)


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
FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
study_fires <- read_sf(paste0(SpatialFilesPath, "./Inputs/Study_fire_perimeters/Study_fire_perimeters.shp"))

plant_list <- list.files(paste0(SpatialFilesPath,"/Inputs/Shapefiles/PlantationPreds/"),
                         pattern = ".shp", 
                         recursive = FALSE, 
                         full.names=TRUE)
# dNBR rasters
dNBR_list <- list.files(paste0(SpatialFilesPath,"/Inputs/Rasters/dNBR_CAT/"),
                       pattern = ".tif", 
                       recursive = FALSE, 
                       full.names=TRUE)


# Plantation pred rasters
variable_list <- list.files(paste0(SpatialFilesPath, "/Inputs/Rasters/PlantationPreds/"),
                            pattern =  paste(FiresOfInterest, sep = "", collapse = "|"),
                            recursive = TRUE,
                            full.names = TRUE)
variable_list <- grep("tif", variable_list, value=TRUE)
# Drop OpenID, None and SitePrepped
variable_list <- grep("OpenID|None|SitePrepped|OPENING_ID", variable_list, value = TRUE, invert = TRUE)
# For now remove this one because the raster contains only NA's -- Alana to fix and then remove this line
#variable_list <- grep("R11498_SpotBurn", variable_list, value = TRUE, invert = TRUE)
variables <- sapply(variable_list, raster)
# Rename the variables 
variable.name <- lapply(str_split(variable_list,"/"), function(x) grep(".tif", x, value=TRUE))
variable.name <- str_split(variable.name, ".tif", simplify = TRUE)[,1]
names(variables) <- variable.name


# Plantation pred shapfile
plant_list <- list.files(paste0(SpatialFilesPath,"/Inputs/Shapefiles/PlantationPreds/"),
                         pattern = ".shp", 
                         recursive = FALSE, 
                         full.names=TRUE)



#----------------------------Data Summaries------------------------------------#
#--- 1. Percent silvicultural treatment / fire 
#-- a. this is based off pixels that were analysed

Silvic_Vars <- c("BroadBurn", "Brushed", "DebrisMade", "DebrisPiled", "Fertil", "PileBurn", 
                 "Prune", "Soil", "Spaced", "SpotBurn", "TotalRows")
FireName <- c("Chutanli","Nadina","Shovel","Island","Verdun","Tezzeron")
dat_list <- list(Chutanli, Nadina, Shovel, Island, Verdun, Tezzeron)
SilvicTable <- list()

# Count number of pixels that == 1 (by summing) in each treatment per fire
for(i in 1:length(FireName)){
  dat <- as.data.table(dat_list[i])
  dat[,TotalRows:= 1]
  Silvic <- dat[, lapply(.SD, sum), .SDcols = (colnames(dat) %in% Silvic_Vars)]
  SilvicTable[[i]] <- Silvic[,Fire:=FireName[i]]
}

# Percent of pixels in each treatment in each fire
SilvicPCTable <- SilvicTable %>% 
  reduce(full_join) %>% # make into one table
  dplyr::select("Fire", everything()) %>% # move "Fire" to first column
  mutate_at(vars(-c(TotalRows,Fire)), ~round(./TotalRows*100, 1)) %>% # Divide  by total rows to get % silviculture type/fire
  dplyr::select(-c(TotalRows)) # Drop TotalRows


#-- b. this is based off whole fire

FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
SilvicFire <- list()

for(i in 1:length(FiresOfInterest)){
  # Bring in plantation preds to stack in each fire
  allPlantRasts <- variables[c(grep(FiresOfInterest[i],variables))]
  fireID <- str_extract(names(allPlantRasts[1]),FiresOfInterest[i])
  SimpleRastnames <- str_remove(str_remove(names(allPlantRasts),FiresOfInterest[i]),"_")
  names(allPlantRasts) <- SimpleRastnames
  #stack the simplified names and assign to fire id rast name
  assign(paste0(fireID,"rasts"), stack(allPlantRasts))
  PlantStack <- stack(allPlantRasts)
  
  # Get total number of non NA cells in the fire
  TotalCells <- ncell(PlantStack)-freq(PlantStack, value=NA)
  Total <- as.data.table(as.list(TotalCells))
  Total[, Fire := FiresOfInterest[i]]
  Total <- melt(Total, id.vars=c("Fire"))
  setnames(Total, "value", "total")
  
  # Get number of freq ==1 for each silvicultural treatment 
  SilvicFrq <- freq(PlantStack, value=1, merge=TRUE)
  Silvic <- as.data.table(as.list(SilvicFrq))
  Silvic[, Fire := FiresOfInterest[i]]
  Silvic <- melt(Silvic, id.vars=c("Fire"))
  
  # Join tables & calculate % of the fire area that had each silvicultural treatment applied
  Silvic <- full_join(Silvic, Total)
  Silvic[, PC:= round((value/total)*100, 1)]
  SilvicFire[[i]] <- Silvic # final table below
}

# Percent burn severity/category per fire
SilvicFirePCtable <- SilvicFire %>% 
  reduce(full_join) %>% # make into one table
  dplyr::select("Fire", everything()) # move "Fire" to first column
SilvicFirePCtable <- dcast(SilvicFirePCtable, 
                          Fire ~ variable,
                          value.var = "PC")


#--- 2. Percent burn severity class within plantations
# this is at the pixel level, area within plantations (pixels in whole fire)

FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
BurnSevPC <- list()
plantBurnSevPC <- list()
plantBurnSevVar <- list()


for(i in 1:length(FiresOfInterest)){
  dNBR <- raster(dNBR_list[i])
  plant <- st_read(plant_list[i])
  # clip dNBR to plantation boundaries
  dNBRPlant <- mask(dNBR, plant)
  #-- Percent burn severity/category per fire (final table created below, outside of loop)
  BurnSev <- as.data.table(freq(dNBRPlant))
  BurnSev[,PC := round(count/(ncell(dNBRPlant)-freq(dNBRPlant, value=NA))*100,1)]
  BurnSev[, Fire := (FiresOfInterest[i])]
  BurnSevPC[[i]] <- BurnSev
  
#--- 3. Burn severity variability within plantations ## not sure how we want to use this
  plantBurnSev <- extract(dNBR, plant, df=TRUE)
  colnames(plantBurnSev)[2] <- "BurnSev"
  setDT(plantBurnSev)
  # count for each opening / burn severity category
  plantBurnSev <- plantBurnSev %>%
    count(ID, BurnSev)
  plantBurnSev[, total := sum(n), by = ID]
  plantBurnSev[, PC := round((n/total)*100, 1), by = ID]
  # Percent burn severity within plantations
  plantBurnSevPC[[i]] <- plantBurnSev
  # Variance burn severity within plantations
  plantBurnSevVar[[i]] <- plantBurnSev %>%
    group_by(ID) %>%
    summarise_at(vars(PC), list(var)) # not sure if this calculated variance how I wanted or if variance is 
  # the stat we want to go with

}

#-- 2. final table for Percent burn severity class per fire
BurnSevPCtable <- BurnSevPC %>% 
  reduce(full_join) %>% # make into one table
  dplyr::select("Fire", everything()) %>% # move "Fire" to first column
  na.omit() # remove na rows


# G41607 = Chutanli
# G51632 = Tezzeron
# R11498 = Shovel
# R11796 = Verdun
# R11921 = Island
# R21721 = Nadina

#------------------------------------------------------------------------------#
#### IF: Not sure what data is being used in scripts below #####

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









