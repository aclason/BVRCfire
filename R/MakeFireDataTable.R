library(data.table)
library(dplyr)
library(raster)
library(sf)
library(stringr)
library(readr)
st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
PerResultsOverlap <- 0.25
# You can run this whole thing as a function, 
# but really I set it up this way to be very explicit about what inputs are needed

MakeFireDataTable <- function(study_fireTable,PerResultsOverlap,fire_perimeters,Results_All,VRI_study){

#### All fire perimeters:
fire_per_sel <- fire_perimeters %>%
  dplyr::select(FIRE_NUMBE,FIRE_YEAR,FIRE_CAUSE)

#### Severity mapping (all 2015-2018):
severity_sel <- severity %>%
  dplyr::select(FIRE_NUMBE,FIRE_YEAR,BURN_SEVER)

#### Results:
Results_sel <- Results_All %>%
  dplyr::select(OPENING_ID,OPENING_ST,APPROVE_DA,DISTURBANC,DISTURBA_1,DENUDATION, DENUDATI_1, 
                DENUDATI_2, DENUDATI_3,
                DENUDATI_4,DENUDATI_5,DENUDATI_6,DENUDATI_7, DENUDATI_8, DENUDATI_9, DENUDAT_10, SITE_PREP_,
                SITE_PREP1, SITE_PRE_1, SITE_PRE_2, SITE_PRE_3, SITE_PRE_4 ,SITE_PRE_5, PLANTING_1,PLANTING_2,
                PLANTING_3, PLANTING_4, PLANTING_5, PLANTING_6, PLANTING_C ,BRUSHING_T,BRUSHING_1, BRUSHING_C,
                BRUSHING_2 ,SPACING_TR, SPACING_CO ,SPACING__1, FERTILIZAT,FERTILIZ_1, FERTILIZ_2, PRUNING_TR,
                PRUNING_CO ,PRUNING__1,SLOPE,ASPECT)

#### VRI:
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

rm(severity)
rm(Results_All)
rm(VRI_study)
gc()

#Fires in the study:
Study_fire_perimeters <- as.data.table(fire_per_sel)[FIRE_NUMBE %in% c(study_fireTable[,FireID])]
r_t_dat_p_Notself <- list()
FireSevSum_PlantList <- list()
#split out by fire to reduce data in memory
c()
for(i in 1:length(study_fireTable$FireID)){
  Fire <- st_as_sf(Study_fire_perimeters %>% 
                      filter(FIRE_NUMBE == study_fireTable[i,FireID]))
  
  ####### RESULTS ########
  Fire_Results <- Results_sel %>% 
    filter(st_contains(Fire, ., sparse = FALSE)) #the fire had to fully contain the opening
  if(nrow(Fire_Results)==0){
    print(paste("no RESULTS openings in fire",Fire$FIRE_NUMBE))
  } else {
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
    Fire_start <- as.Date(study_fireTable[FireID == Fire$FIRE_NUMBE]$StartDate,format="%d/%m/%Y")
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
    #length(unique(Activ$OPENING_ID))
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
    Plantations_sf <- st_as_sf(Plantations)
    
    ######## OVERLAPS ##########
    #Calculating the degree of overlap of results polygons
    #Fire_Res_O <-  filter(st_intersection(st_buffer(Plantations_sf,0),
                                          #st_buffer(Plantations_sf,0),sparse=FALSE))
    #Fire_Res_O$Overlap_area <- st_area(Fire_Res_O) #area of overlapping polygon
    #Plantations_sf$OpeningArea <- st_area(Plantations_sf) #area of the total results opening
    #t_dat1 <- as.data.table(Fire_Res_O)[,.(OPENING_ID,OPENING_ID.1, OverlapResArea=unclass(Overlap_area))] 
    #r_dat1 <- as.data.table(Plantations_sf)
    #r_t_dat1 <- merge(r_dat1[,.(OPENING_ID, OpeningArea=unclass(OpeningArea))],t_dat1, by.x="OPENING_ID",by.y="OPENING_ID")
    #r_t_dat_p1 <- r_t_dat1[, .(OPENING_ID.1=as.numeric(OPENING_ID.1),
                              # PropResOverlap= OverlapResArea/OpeningArea), by="OPENING_ID"]
    #setting the threshold for the minimum overlap in polygons at 25%, but this can be changed (see top of function)
    #r_t_dat_p_Notself[[i]] <- r_t_dat_p1[OPENING_ID!=OPENING_ID.1 & PropResOverlap > PerResultsOverlap]
    
    #length(unique(Plantations$OPENING_ID))
    #unioning the area to make one giant plantation polygon that won't double count overlapping area
    #PlantationsArea <- st_cast(st_union(st_as_sf(Plantations)),"MULTIPOLYGON")

    ########################
    ####### SEVERITY ########
    Fire_Sev <- severity_sel %>% filter(FIRE_NUMBE == study_fireTable[i,FireID])
    #For every fire, calculate the area of each severity polygon
    Fire_Sev$FireSevArea <- st_area(Fire_Sev)
    Fire_SevDT <- as.data.table(Fire_Sev)
    FireSevSum <- Fire_SevDT[,.(FireSevArea= round(sum(unclass(FireSevArea)/10000),0)),by=c("FIRE_NUMBE","BURN_SEVER")]
    
    #For every plantation, we can calculate the proportion burned in each category
    Fire_sev_res <- st_intersection(st_buffer(Plantations_sf,0),st_buffer(Fire_Sev,0))
    Fire_sev_res$ResSevArea <- st_area(Fire_sev_res)
    
    #Make burn severity classes columns
    Fire_sev_res_dat <- as.data.table(Fire_sev_res)
    BurnSev_prop_calcl <- Fire_sev_res_dat[,sum(unclass(ResSevArea)),
                                            by=.(OPENING_ID,BURN_SEVER)][,.(BURN_SEVER,BU_SV_P=prop.table(V1)),
                                                                         OPENING_ID]
    BurnSev_prop_calcl_d <- dcast(BurnSev_prop_calcl,OPENING_ID~BURN_SEVER,value.var="BU_SV_P")
    #If any of the burn severity levels are not present, add the missing ones here
    BurnLevels <- c("Unburned","Low","Medium","High")
    if(all(BurnLevels %in% unique(BurnSev_prop_calcl[,BURN_SEVER]))){
      BurnSev_prop_calcl_d <- BurnSev_prop_calcl_d
    }else{
      BurnSev_prop_calcl_d <- BurnSev_prop_calcl_d[,BurnLevels[which(!BurnLevels %in% 
                                                                      unique(BurnSev_prop_calcl[,BURN_SEVER]))]:=0]
    }
    Fire_Results_PropSev <- merge(Plantations_sf,BurnSev_prop_calcl_d, by="OPENING_ID")
    Plantations <- as.data.table(Fire_Results_PropSev)
    
    ####### Making sure the severity is 0 if there was none (not NA)
    Plantations[,High:= ifelse(is.na(High),0,High)]
    Plantations[,Medium:= ifelse(is.na(Medium),0,Medium)]
    Plantations[,Low:= ifelse(is.na(Low),0,Low)]
    Plantations[,Unburned:= ifelse(is.na(Unburned),0,Unburned)]
    
    Plantations_sf <- st_as_sf(Plantations)
    # Severity by class across total plantation area (unioned together to account for spatial overlaps)
   # Fire_sev_resComb <- st_intersection(st_buffer(st_as_sf(PlantationsArea),0),st_buffer(Fire_Sev,0))
  #  Fire_sev_resComb$SevOpenings <- st_area(Fire_sev_resComb)
  #  Fire_sev_resComb_DT <- as.data.table(Fire_sev_resComb)
  #  FireSevPlantSum <- Fire_sev_resComb_DT[,.(PlantAreaSev =round(sum(unclass(SevOpenings)/10000),0)),
   #                                        by=c("FIRE_NUMBE","BURN_SEVER")]
    #FireSevSum_PlantList[[i]] <- merge(FireSevSum,FireSevPlantSum,by=c("FIRE_NUMBE","BURN_SEVER"), all=TRUE)
    
    ###############################################
    ##### PREVIOUS FOREST COVER PLANTATIONS ######
    Fire_VRI <- VRI_study_sel %>%
      dplyr::select(FEATURE_ID,POLYGON_ID,BASAL_AREA, CROWN_CLOS, CROWN_CL_1, FREE_TO_GR,
                    HARVEST_DA,PROJ_AGE_1, PROJ_AGE_C, PROJ_AGE_2,PROJ_AGE_3,
                    PROJ_HEIGH, PROJ_HEI_1, PROJ_HEI_2, PROJ_HEI_3) %>%
      filter(st_intersects(Fire,., sparse = FALSE))
    Fire_sev_res_vri <- st_intersection(st_buffer(Plantations_sf,0),st_buffer(Fire_VRI,0))
    Fire_sev_res_vriDT <- as.data.table(Fire_sev_res_vri) #fire, results, severity, vri combination
    g <- merge(Fire_sev_res_vriDT,VRI_sum_all[,.(FEATURE_ID,decidCov,conifCov,
                                                 PineCov, FirCov, SpruceCov, DFirCov)],by="FEATURE_ID")
    VRI_Avg <- g[,.(DecVRICov=mean(na.omit(decidCov)),ConVRICov=mean(na.omit(conifCov)),
                    PineVRICov=mean(na.omit(PineCov)),FirVRICov=mean(na.omit(FirCov)),
                    SpruVRICov=mean(na.omit(SpruceCov)), DFirVRICov=mean(na.omit(DFirCov)),
                    CanopCloVRI = mean(na.omit(CROWN_CLOS)), AgeVRI =mean(na.omit(PROJ_AGE_1)),
                    HeightVRI = mean(na.omit(PROJ_HEIGH)),BA_VRI = mean(na.omit(BASAL_AREA))),by="OPENING_ID"]
    Plantations_sf <- merge(Plantations_sf,VRI_Avg, by="OPENING_ID")
    
    #############################  
    ############# dNBR ##########
    dNBR <- raster(paste0("./Inputs/Rasters/dNBR_",study_fireTable[i,FireID],".tif"))
    dNBR_plant <- raster::extract(dNBR, Plantations_sf, fun=median, na.rm=TRUE)
    Plantations_sf$Mn_dNBR <- dNBR_plant
    Plantations_sf <- st_cast(Plantations_sf, to="MULTIPOLYGON")
    Plantations <- as.data.table(Plantations_sf)
    
    ###############################
    ###### SUMMARIZING TREATMENTS (0,1) ########
    Plantations[,Spaced := ifelse(SPACING__1==0,0,1)]
    Plantations[,Brushed := ifelse(BRUSHING_1==0,0,1)]
    Plantations[,SitePrepped := ifelse(SITE_PRE_5==0,0,1)]
    Plantations[,Fertil := ifelse(FERTILIZ_2==0,0,1)]
    Plantations[,Prune := ifelse(PRUNING__1==0,0,1)]
    Plantations[,Planted := ifelse(PLANTING_C==0,0,1)]
    
    ##Define harvest treatment
    #if at least one of the denudations is a clearcut, or it doesn't specify, it counts as a clearcut
    Plantations[,NumDenuda := DENUDAT_10]
    Plantations[,HarvType:= ifelse(is.na(DENUDATI_1) & is.na(DENUDATI_1),"CC",
                                   ifelse(DENUDATI_1=="CLEAR"|DENUDATI_1=="CCRES"|
                                            DENUDATI_6=="CLEAR"|DENUDATI_6=="CCRES", "CC","PC"))]
    Plantations[is.na(HarvType)]$HarvType <- "PC" #i can't figure out why the else above isn't working
    ############################
    ##### Clean up the plantations dataset and drop columns no longer needed
    
    Plantations <- Plantations[,.(OPENING_ID,PlantAge, Reburn, High, Low,Medium,Unburned,
                                  DecVRICov, ConVRICov, PineVRICov, FirVRICov, SpruVRICov, DFirVRICov,
                                  CanopCloVRI, AgeVRI, HeightVRI, BA_VRI, Mn_dNBR, Spaced, Brushed,
                                  SitePrepped, Fertil, Prune, Planted, NumDenuda, HarvType, geometry)]
    Plantations_sf <- st_as_sf(Plantations)
    
    
    ########################################
    ###### SURROUNDING FOREST CONTEXT ######
    # For every plantation, what is the severity, and forest composition including plantations surrounding it
    OpeningBuff <- st_buffer(Plantations_sf, 100) #this is the plantation buffer
    SurForestList <- list()
    for(k in 1:nrow(OpeningBuff)){
      OpenDonut <- st_erase(st_buffer(OpeningBuff[k,],0),st_buffer(Plantations_sf[k,],0))
      #intersect with VRI
      SurVRI <- merge(st_intersection(st_buffer(OpenDonut[,c("OPENING_ID","geometry")],0),
                                      st_buffer(Fire_VRI,0)),VRI_sum_all, by="FEATURE_ID")
      SurVRI$VRIarea <- st_area(SurVRI)
      SurVRIDT <- as.data.table(SurVRI[,c("OPENING_ID","FEATURE_ID","BASAL_AREA","CROWN_CLOS","PROJ_AGE_C",
                                          "conifCov","decidCov", "PineCov","VRIarea")])
      SurVRIDT <- SurVRIDT[,.(OPENING_ID,SurFID=FEATURE_ID, SurBA=BASAL_AREA, SurCrClos=CROWN_CLOS,
                              SurAge=PROJ_AGE_C,SurConCov = conifCov, SurDecCov= decidCov,
                              SurPineCov=PineCov, SurVRIarea=unclass(VRIarea))][,PrSev:=SurVRIarea/sum(SurVRIarea)]
      SurVRIDT[,SurAgeCl:=ifelse(SurAge<2,"Y",ifelse(SurAge<6,"IM","M"))] #making age classes
      SurVRIDT[,SurConCovCl:=ifelse(SurConCov==0 & is.na(SurAge),"NF",
                                    ifelse(SurConCov==0,"Dec",
                                    ifelse(SurConCov<51,"50_50",
                                           ifelse(SurConCov<76,"75Con","Con"))))]
      SurVRIDT[,SurPineCl:=ifelse(SurPineCov==0 & is.na(SurAge),"NF",
                                    ifelse(SurPineCov==0,"NotPine",
                                           ifelse(SurPineCov<51,"PineMix",
                                                  ifelse(SurPineCov<76,"75Pine","Pine"))))]#I might swith this to mean Pine cover
      SurForType <-SurVRIDT[,.(PrAgeFT=sum(PrSev)),by=c("OPENING_ID","SurAgeCl","SurConCovCl")]
      SurForType[,SurAgeFT:=paste0("Sur",SurAgeCl,"-",SurConCovCl)]
      SurForest1 <- dcast(SurForType, OPENING_ID~SurAgeFT, value.var = "PrAgeFT")
      SurForest1[, SurCrClos := mean(SurVRIDT$SurCrClos)][,SurBA := mean(SurVRIDT$SurCrClos)]
      
      SurPineFor <- SurVRIDT[,.(PrAgePine=sum(PrSev)),by=c("OPENING_ID","SurAgeCl","SurPineCl")]
      SurPineMat <- SurPineFor[SurAgeCl=="M" & SurPineCl != "NotPine" & SurPineCl != "NF"]
      SurPineMat[,SurMatPine:=paste0("Sur",SurAgeCl,"-",SurPineCl)]
      #there could be no pine, so need to put condition on dcast
      if(nrow(SurPineMat)==0){
        SurForest2 <- data.table(OPENING_ID=OpenDonut$OPENING_ID)
      } else{
        SurForest2 <- dcast(SurPineMat, OPENING_ID~SurMatPine, value.var = "PrAgePine")
      }
      SurForest <- merge(SurForest1, SurForest2, by="OPENING_ID")
      #intersect with severity
      SurSev <- st_intersection(st_buffer(OpenDonut[,c("OPENING_ID","geometry")],0),st_buffer(Fire_Sev,0))
      SurSev$SevArea <- st_area(SurSev)
      SurSevDT <- as.data.table(SurSev[,c("OPENING_ID","BURN_SEVER","SevArea")])
      SurSevDT <- SurSevDT[,.(OPENING_ID,SurBurnSev = BURN_SEVER, SurSevArea=unclass(SevArea))]
      SurSevDT <- SurSevDT[,.(TotSurSev = sum(SurSevArea)),
                           by=c("OPENING_ID","SurBurnSev")][,.(OPENING_ID,
                                                               SurBurnSev,
                                                               PrSurSev=TotSurSev/sum(TotSurSev))]
      SurSevDT[,SurBurnSev:=paste0("Sur-",SurBurnSev)]
      SurForest3 <- dcast(SurSevDT, OPENING_ID~SurBurnSev, value.var = "PrSurSev")
      SurForest <- merge(SurForest, SurForest3, by="OPENING_ID")
      #intersect with dNBR
      SurDNBR <- raster::extract(dNBR,OpenDonut, fun=median)
      SurForest$sur_dNBR <- SurDNBR
      
      #intersect with results
      Fire_ResNotSelf <- Fire_Results %>%
        filter(OPENING_ID!= OpenDonut$OPENING_ID)
      SurRes <- st_intersection(st_buffer(OpenDonut[,c("OPENING_ID","geometry")],0),st_buffer(Fire_ResNotSelf,0))
      #there could be no plantations surrounding the opening
      if(nrow(SurRes)==0){
        SurForest4 <- data.table(OPENING_ID=OpenDonut$OPENING_ID)
      } else {
        SurRes$SurOpenArea <- st_area(SurRes)
        SurRes$SurTotalArea <- st_area(OpenDonut)
        SurResDT <- as.data.table(SurRes[,c("OPENING_ID","OPENING_ID.1","SurOpenArea","SurTotalArea")])
        SurResDT<- SurResDT[,.(OPENING_ID,SurOpenID=OPENING_ID.1, SurOpenArea=unclass(SurOpenArea), 
                               SurTotalArea=unclass(SurTotalArea))]
        SurResDT[,PrSurOpen:=SurOpenArea/SurTotalArea]
        #There can also be plantations from after the fire in the Fire_Results layer
        if(nrow(Plantations[OPENING_ID %in% SurResDT$SurOpenID])==0){ 
          SurForest4 <- data.table(OPENING_ID=OpenDonut$OPENING_ID) 
        } else {
          SurOpenDetails <- merge(Plantations[OPENING_ID %in% SurResDT$SurOpenID,.(OPENING_ID, 
                                                           PlAgeCl = ifelse(PlantAge<=10,1,
                                                                           ifelse(PlantAge<21,2,
                                                                                  ifelse(PlantAge<31,3,
                                                                                         ifelse(PlantAge<41,4,5)))),
                                                           HarvType,SitePrepped)],SurResDT, by.x="OPENING_ID",
                                  by.y="SurOpenID")
          z <- SurOpenDetails[,.(PrSurPlant=sum(PrSurOpen)), by=c("OPENING_ID.y","PlAgeCl")][,.(OPENING_ID=OPENING_ID.y,
                                                                                           PlAgeCl,PrSurPlant)]
          z[,PlAgeCl:=paste0("Sur-AgeCl-",PlAgeCl)]
          SurForest4 <- dcast(z, OPENING_ID~PlAgeCl, value.var = "PrSurPlant") #proportion of surrounding in plantation by age
        }
      }
      SurForest <- merge(SurForest, SurForest4,by="OPENING_ID")
      SurForestList[[k]] <- SurForest
    }
    SurForestAll <- rbindlist(SurForestList, fill=TRUE)
    Plantations <- merge(Plantations,SurForestAll, by="OPENING_ID") #check that we don't lose any here.
    Plantations_sf <- st_as_sf(Plantations)
    
    ##### NOT-MANAGED YOUNG STANDS #######
    #Plant2Erase <-st_union(st_combine(Fire_Results_PropSev))
    #YoungVRI_NoPlant <- st_difference(st_buffer(Fire_VRI,0), st_buffer(YoungVRI_NoPlant,0))
    #write_sf(YoungVRI_NoPlant,"YoungVRI_NoPlant.shp")
    #YoungVRi <- YoungVRI_NoPlant %>%
      #filter(PROJ_AGE_1<80)
    
    #############################      
    ##### PREVIOUS FIRES ######
    #Clip previous fires by current fire boundary
    Fire_Pfire <- fire_perimeters %>% 
      filter(st_contains(Fire, ., sparse = FALSE)) %>% 
      filter(FIRE_NUMBE != study_fireTable[i,FireID]) 
    Fire_Pfire_res <-  filter(st_intersection(st_buffer(Plantations_sf,0),st_buffer(Fire_Pfire,0),sparse=FALSE))
    Fire_Pfire_res$area <- st_area(Fire_Pfire_res) #area of the previous fire
    Plantations_sf$TotArea <- st_area(Plantations_sf) #area of the total results opening
    t_dat <- as.data.table(Fire_Pfire_res)[,.(OPENING_ID,FIRE_YEAR,FIRE_CAUSE, PFireArea=unclass(area))] 
    r_dat <- as.data.table(Plantations_sf)
    r_t_dat <- merge(r_dat[,.(OPENING_ID, TotArea=unclass(TotArea))],t_dat, by.x="OPENING_ID",by.y="OPENING_ID")
    r_t_dat_p <- r_t_dat[, .(FIRE_YEAR,FIRE_CAUSE, PropPfire= PFireArea/TotArea), by="OPENING_ID"]
    if(nrow(r_t_dat_p)>0){
      r_t_dat_q <- dcast(r_t_dat_p,OPENING_ID~FIRE_YEAR,value.var="PropPfire",fun =mean)
      Plantations_sf <- merge(Plantations_sf,r_t_dat_q,by="OPENING_ID", all.x=TRUE)
    } else {
      Plantations_sf <- Plantations_sf
    }
    Plantations <- as.data.table(Plantations_sf)
    
    #############################  
    ##### DAY OF BURN #####
    DOB <- raster(paste0("./Inputs/Rasters/DOB/",study_fireTable[i,FireID],"/dob.tif"))
    #DOB_proj <- projectRaster(from=DOB,crs=crs(Plantations_sf)) #what if we don't scale to 30m resolution
    DOB_plant <- raster::extract(DOB, Plantations_sf, fun=median, na.rm=TRUE)
    Plantations_sf$Md_DOB <- DOB_plant
    
    #############################  
    ##### TOPOGRAPHY #####
    DEMslope_ex <- raster::extract(DEMslope, Plantations_sf, fun=mean, na.rm=TRUE)
    Plantations_sf$DEM_Slop <- DEMslope_ex
    DEMaspect_ex <- raster::extract(DEMaspect, Plantations_sf, fun=mean, na.rm=TRUE)
    Plantations_sf$DEM_Asp <- DEMaspect_ex
    DEMtpi_ex <- raster::extract(DEMtpi, Plantations_sf, fun=mean, na.rm=TRUE)
    Plantations_sf$DEM_tpi <- DEMtpi_ex
    DEMhli_ex <- raster::extract(DEMhli, Plantations_sf, fun=mean, na.rm=TRUE)
    Plantations_sf$DEM_hli <- DEMhli_ex
    Plantations_sf <- st_cast(Plantations_sf, to="MULTIPOLYGON")
    Plantations <- as.data.table(Plantations_sf)
    
    #############################  
    write_sf(Plantations_sf, paste0("./Outputs/IndividualFires/",study_fireTable$FireID[i],"_Fire.shp"))
    Plantations[,geometry:=NULL]
    write.csv(Plantations,paste0("./Outputs/IndividualFires/",study_fireTable$FireID[i],"_Firedat.csv"),row.names = FALSE)
    #write.csv(Firedat[,.(OPENING_ID, SITEPr_1_Type = NA,SITE_PREP_, SITEPr_1_Meth = NA,SITE_PREP1, SITE_PRE_1,
     #                   SITEPr_2_Type = NA, SITE_PRE_2, SITEPr_2_Meth = NA,SITE_PRE_3, SITE_PRE_4, SITE_PRE_5,
      #                  SITEPr_3_Type=NA, SITEPr_3_Meth=NA, SITEPr_3_Area=NA,
       #                 SITEPr_3_Date=NA, SITEPr_4_Type=NA, SITEPr_4_Meth=NA,  
        #                SITEPr_4_Area=NA, SITEPr_4_Date=NA,PLANTING_1, PLANTING_2, PLANTING_3, 
         #               PLANT_1_SP1 = NA, PLANT_1_NUM2 = NA,PLANT_1_SP2 = NA, PLANT_1_NUM2 = NA,
          #              PLANTING_4, PLANTING_5, PLANTING_6, PLANT_2_SP1 = NA, PLANT_2_NUM1 = NA, 
           #             PLANT_2_SP2 = NA, PLANT_2_NUM2 = NA, PLANTING_C, PLANT_3_Type=NA, 
            #            PLANT_3_Meth=NA, PLANT_3_Area=NA, PLANT_3_Date =NA, PLANT_3_SP1 = NA,
             #           PLANT_3_NUM1 = NA, PLANT_3_SP2 = NA, PLANT_3_NUM2 = NA)],
              #paste0("./Outputs/IndividualFires/",study_fireTable$FireID[i],"_Firedat_SitePrep.csv"),row.names = FALSE)
    }
  }
  #FireSevSum_Plant <- rbindlist(FireSevSum_PlantList)
 # write.csv(FireSevSum_Plant,paste0("./Outputs/IndividualFires/PropBurnDat.csv"),row.names = FALSE)
  #r_t_dat_p_Notself_ <- rbindlist(r_t_dat_p_Notself)
 # write.csv(r_t_dat_p_Notself_,paste0("./Outputs/IndividualFires/ResultsPolyOverlap.csv"),row.names = FALSE)
}