#Author: A.Clason
#Date: Updated January 2022

# This code intersects the VRI database with the fire perimeters and exports a raster of pre-fire basal area

library(data.table)
library(dplyr)
library(plyr)
library(raster)
library(fasterize)
library(sf)
library(stringr)
library(readr)

SpatialFilesPath <- "D:/"
study_fireTable <- fread("./Inputs/StudyFireList.csv")
dNBR_imageryDates <- fread("./Inputs/dNBR_dates.csv")
SitePrepGroups <- fread("./Inputs/SitePrep_TypeMethods.csv")

FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
VRI_rasts <- c("BASAL_AREA","CROWN_CLOS","decidCov","conifCov","PineCov","FirCov",
                   "SpruceCov", "DFirCov")

#fire perimeters
fire_perimeters <- read_sf(paste0(SpatialFilesPath,
                                  "Spatial Data/Fire/Historical wildfire perimeters/BC Wildfire Historical Fire Perimeters.shp"),quiet=TRUE)
fire_per_sel <- fire_perimeters %>%
  dplyr::select(FIRE_NUMBE,FIRE_YEAR,FIRE_CAUSE)
StudyFirePerims <- fire_per_sel %>% dplyr::filter(.,FIRE_NUMBE %in% FiresOfInterest)

#### VRI:
VRI_study <- read_sf(paste0(SpatialFilesPath,"/Spatial Data/VRI/2016/BVRCfire_VRI2016.shp"))
VRI_study_sel <- VRI_study %>%
  dplyr::select(FEATURE_ID,  MAP_ID, POLYGON_ID, OPENING_IN, OPENING_SO, OPENING_NU, OPENING_ID, BASAL_AREA, 
                CROWN_CLOS, CROWN_CL_1,FREE_TO_GR,HARVEST_DA,PROJ_AGE_1,PROJ_AGE_C,PROJ_AGE_2,PROJ_AGE_3,
                PROJ_HEIGH, PROJ_HEI_1, PROJ_HEI_2, PROJ_HEI_3,
                SPECIES_CD, SPECIES_PC, SPECIES__1, SPECIES__2, SPECIES__3, SPECIES__4, SPECIES__5,
                SPECIES__6, SPECIES__7,SPECIES__8, SPECIES__9,SPECIES_10)
setnames(VRI_study_sel,"OPENING_ID","OPEN_ID_VRI")
VRI_study_selDT <- as.data.table(VRI_study_sel)

#crown closure classes: 0 0 - 5 % crown closure: 1 6 - 15 % crown closure, 2 16 - 25 % crown closure,
#3 26 - 35 % crown closure, 4 36 - 45 % crown closure, 5 46 - 55 % crown closure, 6 56 - 65 % crown closure
#7 66 - 75 % crown closure, 8 76 - 85 % crown closure, 9 86 - 95 % crown closure,10 96 - 100 % crown closure
VRI_meltSp <- melt(VRI_study_selDT, id.vars = c("FEATURE_ID"),
                   measure.vars = c("SPECIES_CD","SPECIES__1","SPECIES__3","SPECIES__5","SPECIES__7","SPECIES__9"),
                   variable.name = "Species_Rank1",
                   value.name = "Species")
VRI_meltCov <- melt(VRI_study_selDT, id.vars = c("FEATURE_ID"),
                    measure.vars = c("SPECIES_PC","SPECIES__2","SPECIES__4", "SPECIES__6","SPECIES__8","SPECIES_10"),
                    variable.name = "Species_Rank2",
                    value.name = "PerCover")
VRIsp_cov <- VRI_meltSp[,.(FEATURE_ID, Species_Rank1, Species,
                           Species_Rank2 =VRI_meltCov[,Species_Rank2], PerCover=VRI_meltCov[,PerCover])]

VRI_conif <- VRIsp_cov[Species=="BL"|Species=="SX"|Species=="PL"|Species=="B"|Species=="BA"|Species=="FD"|Species=="SB"|
                         Species=="S"|Species=="HW"|Species=="PLC"|Species=="PLI"|Species=="SE"|Species=="FDI"|
                         Species=="PA"|Species=="SW"|Species=="PY"|Species=="JR"|Species=="L"|Species=="SS"|
                         Species=="HM"|Species=="H"|Species=="CW"|Species=="BB"|Species=="LW"|Species=="BM"|
                         Species=="SXW"|Species=="YC"|Species=="LT"|Species=="FDC"|Species=="PW",
                       .(conifCov=sum(PerCover)), 
                       by= "FEATURE_ID"]
VRI_Decid <- VRIsp_cov[Species=="AC"|Species=="AT"|Species=="EP"|Species=="E"|Species=="ACT",
                       .(decidCov=sum(PerCover)), 
                       by= "FEATURE_ID"]
VRI_Pine <- VRIsp_cov[Species=="PL"|Species=="PLC"|Species=="PLI"|Species=="PA"|Species=="PY"|Species=="PW",
                      .(PineCov=sum(PerCover)), 
                      by= "FEATURE_ID"]
VRI_Fir <- VRIsp_cov[Species=="BL"|Species=="B"|Species=="BB"|Species=="BM",
                     .(FirCov=sum(PerCover)), 
                     by= "FEATURE_ID"]
VRI_Spruce <- VRIsp_cov[Species=="S"|Species=="SW"|Species=="PLI"|Species=="SE"|Species=="SX"|Species=="SB"|
                          Species=="SS"|Species=="SXW",
                        .(SpruceCov=sum(PerCover)), 
                        by= "FEATURE_ID"]
VRI_DFir <- VRIsp_cov[Species=="FD"|Species=="FDI"|Species=="FDC",
                      .(DFirCov=sum(PerCover)), 
                      by= "FEATURE_ID"]

VRI_ConDec <- merge(VRI_conif,VRI_Decid, by=c("FEATURE_ID"), all=TRUE)
VRI_ConDecP <- merge(VRI_ConDec,VRI_Pine, by=c("FEATURE_ID"), all=TRUE)
VRI_ConDecPF <- merge(VRI_ConDecP,VRI_Fir, by=c("FEATURE_ID"), all=TRUE)
VRI_ConDecPFS <- merge(VRI_ConDecPF,VRI_Spruce, by=c("FEATURE_ID"), all=TRUE)
VRI_sum <- merge(VRI_ConDecPFS,VRI_DFir, by=c("FEATURE_ID"), all=TRUE)

#this will populate the full VRI dataset, but there are features with n/a for most of the classes
VRI_sum_all <- merge(VRI_study_selDT[,.(FEATURE_ID)],VRI_sum,by="FEATURE_ID",all.x=TRUE)
#convert n/a to 0 for conifer/deciduous cover?
for (i in seq_along(VRI_sum_all)) set(VRI_sum_all, i=which(is.na(VRI_sum_all[[i]])), j=i, value=0)

##### PREVIOUS FOREST COVER PLANTATIONS ######
for(ix in 1:length(FiresOfInterest)){
  Fire <- st_as_sf(StudyFirePerims %>% 
                     filter(FIRE_NUMBE == FiresOfInterest[ix]))
  #raster of fire perimeter
  bb <- st_bbox(st_buffer(Fire, dist = 3000))
  PlotSize <- raster(xmn=bb[1], xmx=bb[3], ymn=bb[2],ymx=bb[4],res=30, crs=crs(Fire))
  Fire$FirePerim <- 1
  FireRast <- fasterize(Fire,PlotSize, field="FirePerim", background=0)
  plot(FireRast)
  Fire_VRI <- VRI_study_sel %>%
    dplyr::select(FEATURE_ID,POLYGON_ID,BASAL_AREA, CROWN_CLOS, CROWN_CL_1, FREE_TO_GR,
                  HARVEST_DA,PROJ_AGE_1, PROJ_AGE_C, PROJ_AGE_2,PROJ_AGE_3,
                  PROJ_HEIGH, PROJ_HEI_1, PROJ_HEI_2, PROJ_HEI_3) %>%
    filter(st_intersects(Fire,., sparse = FALSE))
  Fire_VRI_dt <- as.data.table(Fire_VRI)
  Fire_VRI_dt_comp <- merge(Fire_VRI_dt,VRI_sum_all[,.(FEATURE_ID,decidCov,conifCov,
                                               PineCov, FirCov, SpruceCov, DFirCov)],by="FEATURE_ID")
  VRI_rasts_g <- c(VRI_rasts,"geometry")
  Fire_VRI_dt_comp_sf <- st_as_sf(Fire_VRI_dt_comp[,..VRI_rasts_g])
  for(iix in 1:length(VRI_rasts)){
    writeRaster(fasterize(Fire_VRI_dt_comp_sf,FireRast, field=VRI_rasts[iix]),
                paste0("./Inputs/Rasters/VRIpreds/",Fire$FIRE_NUMBE,"_",VRI_rasts[iix],".tif"),
                overwrite=TRUE)
  }
}


