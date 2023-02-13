#Author: A.Clason
#Date: Updated January 2022

# This code intersects the RESULTS spatial database with the fire perimeters and detailed RESULTS site prep data
# to create rasters for each fire of interest with plantation
# There is also code at the end to create csvs to analyse silviculture treatments based on 270m analysis grid

library(data.table)
library(plyr)
library(raster)
library(fasterize)
library(sf)
library(dplyr)
library(stringr)
library(readr)

#Users to update these 
SpatialFilesPath <- "D:/"
study_fireTable <- fread("./Inputs/StudyFireList.csv")
dNBR_imageryDates <- fread("./Inputs/dNBR_dates.csv")
#SitePrepGroups <- fread("./Inputs/SitePrep_TypeMethods.csv")
SitePrepGroups <- fread("./Inputs/SitePrep_TypeMethods_Disc.csv")

FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
RESULTS_rasts <- c("OPENING_ID","PlantAge","BroadBurn","DebrisMade","DebrisPiled","MechUnk",
                   "PileBurn", "Soil", "SpotBurn","Spaced","Brushed","Disc",
                   "Fertil","Prune")

##### Fire perimeters #####
#fire_perimeters <- read_sf(paste0(SpatialFilesPath,
                                 # "Spatial Data/Fire/Historical wildfire perimeters/BC Wildfire Historical Fire Perimeters.shp"),quiet=TRUE)
#fire_per_sel <- fire_perimeters %>%
 # dplyr::select(FIRE_NUMBE,FIRE_YEAR,FIRE_CAUSE)
#StudyFirePerims <- fire_per_sel %>% dplyr::filter(.,FIRE_NUMBE %in% study_fireTable$FireID)
# or just read in study fires:
StudyFirePerims <- read_sf("./Inputs/Shapefiles/Study_fire_perimeters.shp")

#### Results:
Results_All <- read_sf(paste0(SpatialFilesPath,
                              "Spatial Data/RESULTS/RESULTS_FirePerimeter_Intersect.shp"),quiet=T)
Results_sel <- Results_All %>%
  dplyr::select(OPENING_ID,OPENING_ST,APPROVE_DA,DISTURBANC,DISTURBA_1,DENUDATION, DENUDATI_1, 
                DENUDATI_2, DENUDATI_3,
                DENUDATI_4,DENUDATI_5,DENUDATI_6,DENUDATI_7, DENUDATI_8, DENUDATI_9, DENUDAT_10, SITE_PREP_,
                SITE_PREP1, SITE_PRE_1, SITE_PRE_2, SITE_PRE_3, SITE_PRE_4 ,SITE_PRE_5, PLANTING_1,PLANTING_2,
                PLANTING_3, PLANTING_4, PLANTING_5, PLANTING_6, PLANTING_C ,BRUSHING_T,BRUSHING_1, BRUSHING_C,
                BRUSHING_2 ,SPACING_TR, SPACING_CO ,SPACING__1, FERTILIZAT,FERTILIZ_1, FERTILIZ_2, PRUNING_TR,
                PRUNING_CO ,PRUNING__1,SLOPE,ASPECT)

rm(Results_All)
gc()

#split out by fire to reduce data in memory
for(ix in 1:length(FiresOfInterest)){
  Fire <- st_as_sf(StudyFirePerims %>% dplyr::filter(FIRE_NUMBE == FiresOfInterest[ix]))
  #raster of fire perimeter
  bb <- st_bbox(st_buffer(Fire, dist = 3000))
  PlotSize <- raster(xmn=bb[1], xmx=bb[3], ymn=bb[2],ymx=bb[4],res=30, crs=crs(Fire))
  Fire$FirePerim <- 1
  FireRast <- fasterize(Fire,PlotSize, field="FirePerim", background=0)
  plot(FireRast)
  ####### RESULTS ########
  Fire_Results <- Results_sel %>%
    #filter doesn't work anymore without explicitly passing a vector
    dplyr::filter(st_contains(Fire, ., sparse = FALSE)[1,]) #the fire had to fully contain the opening
  #Define plantations
  Fire_Results_dat <- as.data.table(Fire_Results)
  #1. To be a plantation, there had to be harvest
  Plant1 <- Fire_Results_dat[DENUDATI_5 =="L"| DENUDATION=="L"]
  #2. OrSalvage logging
  Plant2 <- Fire_Results_dat[DENUDATI_5 =="S" & DENUDATION !="L"| DENUDATION=="S" & DENUDATI_5 !="L"|
                    DENUDATION=="S" & is.na(DENUDATI_5)]
  #3. Or A disturbance along with site clearing and planting
  Plant3 <- Fire_Results_dat[DENUDATION=="B"& SITE_PREP_ =="ME" & PLANTING_C > 0|
                    DENUDATION=="B" & SITE_PRE_2 =="ME"& PLANTING_C>0|
                    DENUDATION=="P" & SITE_PREP_ =="ME"& PLANTING_C>0|
                    DENUDATION=="P" & SITE_PRE_2 =="ME"& PLANTING_C>0|
                    DENUDATION=="R" & SITE_PREP_ =="ME"& PLANTING_C>0|
                    DENUDATION=="R" & SITE_PRE_2 =="ME"& PLANTING_C>0|
                    DENUDATION=="W" & SITE_PREP_ =="ME"& PLANTING_C>0|
                    DENUDATION=="W" & SITE_PRE_2 =="ME"& PLANTING_C>0|
                    DENUDATION=="B" & SITE_PREP_ =="MA"& PLANTING_C>0|
                    DENUDATION=="B" & SITE_PRE_2 =="MA"& PLANTING_C>0|
                    DENUDATION=="P" & SITE_PREP_ =="MA"& PLANTING_C>0|
                    DENUDATION=="P" & SITE_PRE_2 =="MA"& PLANTING_C>0|
                    DENUDATION=="R" & SITE_PREP_ =="MA"& PLANTING_C>0|
                    DENUDATION=="R" & SITE_PRE_2 =="MA"& PLANTING_C>0|
                    DENUDATION=="W" & SITE_PREP_ =="MA"& PLANTING_C>0|
                    DENUDATION=="W" & SITE_PRE_2 =="MA"& PLANTING_C>0]
  Plant4 <- rbind(Plant1,Plant2,Plant3)
  
  #4. The harvest date has to preceed the fire (will refine plantation age later)
  Fire_start <- as.Date(study_fireTable[FireID == FiresOfInterest[ix]]$StartDate,format="%d/%m/%Y")
  Plantations <- Plant4[as.Date(DENUDATI_4,format="%Y/%m/%D") < Fire_start |
                               as.Date(DENUDATI_9,format="%Y/%m/%D") < Fire_start|
                               as.Date(DISTURBANC,format="%Y/%m/%D") < Fire_start]
  #union repeating opening ids: 
  if(length(unique(Plantations$OPENING_ID))!= nrow(Plantations)){
    RepOI <- Plantations[which(duplicated(Plantations$OPENING_ID)),]$OPENING_ID
    Plantations_sf <- st_as_sf(Plantations)
    for(r in 1:length(RepOI)){
      RepOI_sf <- Plantations_sf %>%
        filter(OPENING_ID == RepOI[r])
      u <- st_union(RepOI_sf)
      v <- as.data.table(RepOI_sf)[,geometry:=NULL]
      x <- merge(u,v)
      Plantations_sfu <- Plantations_sf %>%
        filter(OPENING_ID !=RepOI[r])
      Plantations_sfu <- rbind(Plantations_sfu,st_as_sf(x[1,]))
      Plantations_sf <- st_cast(Plantations_sfu,to="MULTIPOLYGON")
    }
    Plantations <- as.data.table(Plantations_sf)
  } else {
    print("no repeated OI")
  }
  
  ####### PLANTATION AGE #######
  ##1 if logged, and planted
  #1a  Planting happened after 2017/2018 fire:
  Plantations1 <- Plantations[PLANTING_C>0]
  Plantations1[as.Date(PLANTING_3,format="%Y/%m/%D") > Fire_start & is.na(PLANTING_6)|
                 as.Date(PLANTING_3,format="%Y/%m/%D") > Fire_start & 
                 as.Date(PLANTING_6,format="%Y/%m/%D") > Fire_start,
               PlantAge:= pmax((as.numeric(format(as.Date(Fire_start,format="%d/%m/%Y"),"%Y")) -
                                  as.numeric(format(as.Date(DENUDATI_4,format="%Y-%m-%d"),"%Y"))),
                               (as.numeric(format(as.Date(Fire_start,format="%d/%m/%Y"),"%Y")) -
                                  as.numeric(format(as.Date(DENUDATI_9,format="%Y-%m-%d"),"%Y"))),
                               na.rm=TRUE)]
  
  #1b.if logged and planted, and at least one planting happened before 2017/2018 fire, use the oldest planting date
  Plantations1[as.Date(PLANTING_3,format="%Y/%m/%D") < Fire_start & is.na(PLANTING_6)|
                 as.Date(PLANTING_6,format="%Y/%m/%D") < Fire_start & is.na(PLANTING_3)|
                 as.Date(PLANTING_3,format="%Y/%m/%D") < Fire_start & 
                 as.Date(PLANTING_6,format="%Y/%m/%D") > Fire_start|
                 as.Date(PLANTING_3,format="%Y/%m/%D") > Fire_start & 
                 as.Date(PLANTING_6,format="%Y/%m/%D") < Fire_start|
                 as.Date(PLANTING_3,format="%Y/%m/%D") < Fire_start & 
                 as.Date(PLANTING_6,format="%Y/%m/%D") < Fire_start, PlantAge := 
                 pmax((as.numeric(format(as.Date(Fire_start,format="%d/%m/%Y"),"%Y")) -
                         as.numeric(format(as.Date(PLANTING_3,format="%Y-%m-%d"),"%Y"))),
                      (as.numeric(format(as.Date(Fire_start,format="%d/%m/%Y"),"%Y")) -
                         as.numeric(format(as.Date(PLANTING_6,format="%Y-%m-%d"),"%Y"))),
                      na.rm=TRUE)]
  #1c. sometimes results says there's a planting, but it's not populated, so use denudation date
  Plantations1[is.na(PlantAge)]
  Plantations1[is.na(PlantAge), PlantAge :=
                 pmax((as.numeric(format(as.Date(Fire_start,format="%d/%m/%Y"),"%Y")) -
                         as.numeric(format(as.Date(DENUDATI_4,format="%Y-%m-%d"),"%Y"))),
                      (as.numeric(format(as.Date(Fire_start,format="%d/%m/%Y"),"%Y")) -
                         as.numeric(format(as.Date(DENUDATI_9,format="%Y-%m-%d"),"%Y"))),
                      na.rm=TRUE)]
  ##2. if not planted, use the harvest date
  Plantations2 <- Plantations[PLANTING_C==0]
  Plantations2[, PlantAge := 
                 pmax((as.numeric(format(as.Date(Fire_start,format="%d/%m/%Y"),"%Y")) -
                         as.numeric(format(as.Date(DENUDATI_4,format="%Y-%m-%d"),"%Y"))),
                      (as.numeric(format(as.Date(Fire_start,format="%d/%m/%Y"),"%Y")) -
                         as.numeric(format(as.Date(DENUDATI_9,format="%Y-%m-%d"),"%Y"))),
                      #(as.numeric(format(as.Date(StartDate,format="%d/%m/%Y"),"%Y")) -
                      #  as.numeric(format(as.Date(DISTURBANC,format="%Y-%m-%d"),"%Y"))),
                      na.rm=TRUE)]
  Plantations2[is.na(PlantAge)]
  Plantations2[is.na(PlantAge), PlantAge:= as.numeric(format(as.Date(Fire_start,format="%d/%m/%Y"),"%Y")) -
                 as.numeric(format(as.Date(DISTURBANC,format="%Y-%m-%d"),"%Y"))]
  PlantationsAge <- rbind(Plantations1,Plantations2)
  Plantations <- merge(Plantations,PlantationsAge[,.(OPENING_ID,PlantAge)],by="OPENING_ID")
  Plantations[is.na(PlantAge)]$OPENING_ID
  
  ###### UPDATE AGE FOR WILDFIRE DISTURBANCE ######
  #put the activities in order for plantations with activity info in spatial file
  ActivNam <- melt(Plantations, id.vars = c("OPENING_ID"),
                   measure.vars = c("DENUDATION","DENUDATI_5","SITE_PREP_","SITE_PRE_2","PLANTING_1","PLANTING_4",
                                    "BRUSHING_T","SPACING_TR","FERTILIZAT","PRUNING_TR"),
                   variable.name = "ActivityCat",
                   value.name = "Activities")
  ActivDates <- melt(Plantations, id.vars = c("OPENING_ID"),
                     measure.vars = c("DENUDATI_4","DENUDATI_9","SITE_PRE_1", "SITE_PRE_4","PLANTING_3","PLANTING_6",
                                      "BRUSHING_C","SPACING_CO","FERTILIZ_1","PRUNING_CO"),
                     variable.name = "ActivityDate",
                     value.name = "Date")
  ActivNamDates <- ActivNam[,.(OPENING_ID, ActivityCat,Activities,
                               ActivCatDate =ActivDates[,ActivityDate], Date=ActivDates[,Date])]
  ActivNamDates[ActivityCat=="SPACING_TR" & !is.na(Date),Activities:="SC"]
  ActivNamDates[ActivityCat=="FERTILIZAT"& !is.na(Date),Activities:="FE"]
  ActivNamDates[ActivityCat=="PRUNING_TR" &!is.na(Date),Activities:="PN"]
  ActivNamDates[Activities==0, Activities:=NA]
  Activ <- ActivNamDates[!is.na(Activities)]
  setorderv(Activ, cols=c("OPENING_ID","Date"))
  Activ[,ActOrder:=seq(length(Date)),by="OPENING_ID"]
  
  #identify plantations with disturbances that would impact their age at time of fire
  ReBurnPlants <- Activ[Activities=="B" & Date < Fire_start] #Was there a burn before 2017/2018
  ReBurnPlantsOI <- unique(ReBurnPlants$OPENING_ID) #get those opening ids
  #update the plantation age to the most recent stand initiation event
  if(length(ReBurnPlantsOI)>0){
    for(j in 1:length(ReBurnPlantsOI)){
      ao <- Activ[OPENING_ID==ReBurnPlantsOI[j]]
      b <- as.numeric(format(Fire_start, "%Y")) - min(as.numeric(format(ao[Activities=="B"]$Date,"%Y")))
      if(Plantations[OPENING_ID==ReBurnPlantsOI[j]]$PlantAge > b){
      Plantations[OPENING_ID==ReBurnPlantsOI[j], PlantAge := as.numeric(format(Fire_start, "%Y")) -
                    pmax(as.numeric(format(ao[Activities=="B"]$Date,"%Y")),na.rm=TRUE)]
      } else{
        print(paste("keep plantAge for OI",ReBurnPlantsOI[j]))
      }
    } 
  } else {
    print(paste("no reburns in fire",Fire$FIRE_NUMBE))
  }
  #Flag the reburn openings for later - these plantations could be established after the early wildfire as well
  Plantations[,Reburn := ifelse(OPENING_ID %in% ReBurnPlantsOI,1,0)]
  
  #remove plantations that happened after the fire that weren't caught in first crieria
  Plantations[is.na(PlantAge)]
  Plantations[PlantAge <0]
  Plantations <- Plantations[PlantAge > -1]
  
  #############################################
  ##### ADD DETAILED SP AND PLANTS #####
  SitePrep <- data.table()
  dt <- fread(paste0("./Outputs/IndFiresDat/",FiresOfInterest[ix],"_Firedat_SitePrep_MethAdds.csv"),
                na.strings=c("","NA","<NA>"))
    
  cols <-c("SITE_PREP_","SITE_PRE_2",colnames(dt)[grepl("Type",colnames(dt))])
  cols <- cols[grepl("SITE",cols)]
  dt[, (cols):=lapply(.SD, as.factor),.SDcols=cols]
  SitePrepType_melt <- melt(dt, id.vars = c("OPENING_ID"),
                            measure.vars = cols,
                            variable.name = "SP",
                            value.name = "SP_type",value.factor=TRUE)
  
  #Site prep method (pburn, trail, knock down etc.)
  cols <- colnames(dt)[grepl("Meth",colnames(dt))]
  cols <- cols[grepl("SITE",cols)]
  dt[, (cols):=lapply(.SD, as.factor),.SDcols=cols]
  SitePrepMeth_melt <- melt(dt, id.vars = c("OPENING_ID"),
                            measure.vars = cols,
                            variable.name = "SP_",
                            value.name = "SP_Method",value.factor=TRUE)
  #Site prep date
  cols <-c("SITE_PRE_1","SITE_PRE_4",colnames(dt)[grepl("Date",colnames(dt))])
  cols <- cols[grepl("SITE",cols)]
  dt[, (cols):=lapply(.SD,function(x) as.numeric(format(as.Date(x,tryFormats=c("%d/%m/%Y",
                                                                               "%d-%m-%Y",
                                                                               "%Y/%m/%d",
                                                                               "%Y-%m-%d")),"%Y"))),.SDcols=cols]
  SitePrepDate_melt <- melt(dt, id.vars = c("OPENING_ID"),
                            measure.vars = cols,
                            variable.name = "SP_3",
                            value.name = "SP_Date")
  SitePr <- SitePrepType_melt[,.(OPENING_ID,SP_type,
                                 SP_Method = SitePrepMeth_melt[,SP_Method],
                                 SP_Date = SitePrepDate_melt[,SP_Date])]
  SitePr <- SitePr[, SP_type_meth:= paste0(SP_type,"_",SP_Method)]
  SitePr[, ':='(FireID = FiresOfInterest[ix])]
  SitePrep <- rbind(SitePrep,SitePr,fill=TRUE)
  
  unique(SitePrep$SP_type_meth)
  table(SitePrep$SP_type_meth) #WATCH THAT THERE ARE NO NEW COMBINATIONS WITH NEW DATA ENTERED!!
  SitePrep[SP_type_meth=="BU_LRIP"]
  
  SitePrep[,SPgrUse:=1]
  #SitePrep <- SitePrep[!is.na(SP_type)&!is.na(SP_Method)]
  SitePrep[,.N,by="SP_type_meth"]
  SitePrep[SP_type_meth=="ME_WINDP"]
  #Bring in the grouping variable - only site preps of interest are kept
  SitePrep <- merge(SitePrep,SitePrepGroups,by.x="SP_type_meth", by.y="Type_Method", all.x=TRUE)
  SitePrep[!is.na(GroupName)]
  SitePrepCast <- dcast(SitePrep[!is.na(GroupName)], OPENING_ID~GroupName, value.var ="SPgrUse",fun.aggregate=sum)
  for (i in seq_along(SitePrepCast)) set(SitePrepCast, i=which(SitePrepCast[[i]]>0), j=i, value=1)
  
  #merge with the main dataset and make sure any opening without a value has a zero.
  SitePrep2 <- merge(SitePrepCast,SitePrep[,.(OPENING_ID,FireID)], by="OPENING_ID", all.y=TRUE)
  for (i in seq_along(SitePrep2)) set(SitePrep2, i=which(is.na(SitePrep2[[i]])), j=i, value=0)
  SitePrep2<- unique(SitePrep2)
  SitePrep2[which(duplicated(SitePrep2[,OPENING_ID]))]
  
  Plant_SP <- merge(Plantations,SitePrep2, by=c("OPENING_ID"), all.x=TRUE)
  Plant_SP[,Spaced := ifelse(SPACING__1==0,0,1)]
  Plant_SP[,Brushed := ifelse(BRUSHING_1==0,0,1)]
  Plant_SP[,SitePrepped := ifelse(SITE_PRE_5==0,0,1)]
  Plant_SP[,Fertil := ifelse(FERTILIZ_2==0,0,1)]
  Plant_SP[,Prune := ifelse(PRUNING__1==0,0,1)]
  Plant_SP[,Planted := ifelse(PLANTING_C==0,0,1)]
  #figure out which treatments are available in a given fire:
  RESULTS_rasts_avail <- c(colnames(Plant_SP)[colnames(Plant_SP) %in% RESULTS_rasts],"geometry")
  Plant_SP_sf <- st_as_sf(Plant_SP[,..RESULTS_rasts_avail])
  Plant_SP_sf <- st_make_valid(Plant_SP_sf)
  Plant_SP_sf <- st_cast(Plant_SP_sf, to="MULTIPOLYGON")
  write_sf(Plant_SP_sf, paste0("./Inputs/Shapefiles/",Fire$FIRE_NUMBE,"_Plantations.shp"))
  #rastToMake <- RESULTS_rasts_avail[RESULTS_rasts_avail != "geometry"]
  rastToMake <- "Disc"
 plot(fasterize(Plant_SP_sf,FireRast, field=RESULTS_rasts[1]))
  #write out the rasters
  for(iix in 1:length(rastToMake)){
    writeRaster(fasterize(Plant_SP_sf,FireRast, field=rastToMake[iix]),
                paste0("./Inputs/Rasters/PlantationPreds/",Fire$FIRE_NUMBE,"_",rastToMake[iix],".tif"),
                overwrite=TRUE)
  }
}


#-----------------------------------------------------------------------------------------#
# Data prep for Broadcast burning, disc trenching, spacing and brushing Mann-Whitney tests
#------------------------------ 1. Load all rasters --------------------------------------#

#SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"
SpatialFilesPath <- getwd()
# Set the fires of interest - all 2018 fires with openings
FiresOfInterest <- c("G41607", "G51632", "R11498", "R11796","R11921","R21721")


# Read in the rasters
variable_list <- list.files(paste0(SpatialFilesPath, "/Inputs/Rasters/"),
                            pattern =  paste(FiresOfInterest, sep = "", collapse = "|"),
                            recursive = TRUE,
                            full.names = TRUE)
variable_list <- grep("tif", variable_list, value=TRUE)
# Drop OpenID, None and SitePrepped
variable_list <- grep("OpenID|None|SitePrepped|n83", variable_list, value = TRUE, invert = TRUE)
# For now remove this one because the raster contains only NA's -- Alana to fix and then remove this line
variable_list <- grep("R11498_SpotBurn", variable_list, value = TRUE, invert = TRUE)
variable_list <- c(variable_list,paste0(SpatialFilesPath,"/Inputs/Rasters/Topography/DEM",
                                        c("aspect","hli","slope","tpi"),".tif"))

variables <- sapply(variable_list, raster)


# Rename the variables 
variable.name <- lapply(str_split(variable_list,"/"), function(x) grep(".tif", x, value=TRUE))
variable.name <- str_split(variable.name, ".tif", simplify = TRUE)[,1]

names(variables) <- variable.name

#ID the names of the categorical rasters
silv_variables <- c("BroadBurn", "Brushed", "DebrisMade", "DebrisPiled", "Fertil", "MechUnk", 
                    "PileBurn", "Prune", "Soil", "Spaced", "SpotBurn", "WBurn","Disc","dNBR_CAT")
silRasts <- grep(paste(silv_variables,sep = "", collapse = "|"),variable.name,value=TRUE)

#--------------------------2a. Resample rasters and stack----------------------#

# Using base rasters resample response and predictor variables to same extent and resolution
for(i in 1:length(FiresOfInterest)){
  allFireRasts <- variables[c(grep(FiresOfInterest[i],variables),grep("DEM",variables))]
  baseFireRast <- allFireRasts[grep("Base",allFireRasts)][[1]] #index just makes it not a list
  allFireRasts <- allFireRasts[grep("Base",allFireRasts,invert=TRUE)]
  #just the ones we want for this analysis:
  Silv_Rasts <- allFireRasts[c(grep(paste(silv_variables,sep = "", collapse = "|"),allFireRasts),
                               grep("dNBR",allFireRasts))]
  
  # Resample categorical and continuous variables differently
  a <- list()
  for(j in 1:length(Silv_Rasts)){
    if(names(Silv_Rasts[[j]]) %in% silRasts){
      a[[j]] <- raster::resample(Silv_Rasts[[j]], baseFireRast, method = "ngb")
    } else if(names(Silv_Rasts[[j]])=="layer"){ #not a great workaround to disc not naming properly
      a[[j]] <- raster::resample(Silv_Rasts[[j]], baseFireRast, method = "ngb")
    }else{
      a[[j]] <- raster::resample(Silv_Rasts[[j]], baseFireRast, method = "bilinear") 
    }
  }
  fireID <- str_extract(names(Silv_Rasts[1]),FiresOfInterest[i])
  SimpleRastnames <- str_remove(str_remove(names(Silv_Rasts),FiresOfInterest[i]),"_")
  names(a) <- SimpleRastnames
  #stack the simplified names and assign to fire id rast name
  assign(paste0(fireID,"rasts"), stack(a))
}
# Create index of raster stacks
RastStacks <- list(G41607rasts, G51632rasts, R11498rasts, R11796rasts, R11921rasts, R21721rasts)
names(RastStacks) <- c("G41607rasts", "G51632rasts", "R11498rasts", "R11796rasts", "R11921rasts", "R21721rasts")

#--------------------------2b. Intersect rasters with 270m grids ----------------------#
#and output csvs for analysis
for(i in 1:length(FiresOfInterest)){
  #read in grids:
  dat270_sf <- read_sf(paste0("./Inputs/",FiresOfInterest[i],"_270grid.shp"))
  # intersect with rasters and create csv for each fire
  rasts_of_int <- RastStacks[[paste0(FiresOfInterest[i],"rasts")]]
  rasts270 <-  as.data.table(raster::extract(rasts_of_int, dat270_sf,sp = TRUE))
  rasts270[,dNBR := dNBR*1000]
  
  fwrite(rasts270, paste0("./Inputs/",FiresOfInterest[i],"silv_270.csv"))
  
}


