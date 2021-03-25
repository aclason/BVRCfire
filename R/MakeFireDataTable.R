library(data.table)
library(dplyr)
library(raster)
library(sf)
library(stringr)
library(readr)

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
VRI_sum <- merge(VRI_conif,VRI_Decid, by=c("FEATURE_ID"), all=TRUE)
VRI_sum[is.na(decidCov)]$decidCov<-0
VRI_sum[is.na(conifCov)]$conifCov<-0

#this will populate the full VRI dataset, but there are features with n/a for most of the classes
VRI_sum_all <- merge(VRI_study_selDT[,.(FEATURE_ID)],VRI_sum,by="FEATURE_ID",all.x=TRUE)
rm(severity)
rm(Results_All)
rm(VRI_study)
gc()

#Fires in the study:
Study_fire_perimeters <- as.data.table(fire_per_sel)[FIRE_NUMBE %in% c(study_fireTable[,FireID])]
r_t_dat_p_Notself <- list()
FireSevSum_PlantList <- list()
#split out by fire to reduce data in memory
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
    
    #4. The date has to preceed the fire
    Fire_start <- as.Date(study_fireTable[FireID == Fire$FIRE_NUMBE]$StartDate,format="%d/%m/%Y")
    Plantations <- Plant4[as.Date(DENUDATI_4,format="%Y/%m/%D") < Fire_start |
                                 as.Date(DENUDATI_9,format="%Y/%m/%D") < Fire_start|
                                 as.Date(DISTURBANC,format="%Y/%m/%D") < Fire_start]
    Plantations_sf <- st_as_sf(Plantations)
    
    #Calculating the degree of overlap of results polygons
    Fire_Res_O <-  filter(st_intersection(st_buffer(Plantations_sf,0),
                                          st_buffer(Plantations_sf,0),sparse=FALSE))
    Fire_Res_O$Overlap_area <- st_area(Fire_Res_O) #area of overlapping polygon
    Plantations_sf$OpeningArea <- st_area(Plantations_sf) #area of the total results opening
    t_dat1 <- as.data.table(Fire_Res_O)[,.(OPENING_ID,OPENING_ID.1, OverlapResArea=unclass(Overlap_area))] 
    r_dat1 <- as.data.table(Plantations_sf)
    r_t_dat1 <- merge(r_dat1[,.(OPENING_ID, OpeningArea=unclass(OpeningArea))],t_dat1, by.x="OPENING_ID",by.y="OPENING_ID")
    r_t_dat_p1 <- r_t_dat1[, .(OPENING_ID.1=as.numeric(OPENING_ID.1),
                               PropResOverlap= OverlapResArea/OpeningArea), by="OPENING_ID"]
    #setting the threshold for the minimum overlap in polygons at 25%, but this can be changed (see top of function)
    r_t_dat_p_Notself[[i]] <- r_t_dat_p1[OPENING_ID!=OPENING_ID.1 & PropResOverlap > PerResultsOverlap]
    
    length(unique(Plantations$OPENING_ID))
    #unioning the area to make one giant plantation polygon that won't double count overlapping area
    PlantationsArea <- st_cast(st_union(st_as_sf(Plantations)),"MULTIPOLYGON")

    ########################
    ####### SEVERITY ########
    Fire_Sev <- severity_sel %>% filter(FIRE_NUMBE == study_fireTable[i,FireID])
    #For every fire, calculate the area of each severity polygon
    Fire_Sev$FireSevArea <- st_area(Fire_Sev)
    Fire_SevDT <- as.data.table(Fire_Sev)
    FireSevSum <- Fire_SevDT[,.(FireSevArea= round(sum(unclass(FireSevArea)/10000),0)),by=c("FIRE_NUMBE","BURN_SEVER")]
    
    #For every plantation, we can calculate the proportion burned in each category
    Fire_sev_res <- st_intersection(st_buffer(st_as_sf(Plantations),0),st_buffer(Fire_Sev,0))
    Fire_sev_res$ResSevArea <- st_area(Fire_sev_res)
    Fire_sev_res_dat <- as.data.table(Fire_sev_res)
    BurnSev_prop_calcl <- Fire_sev_res_dat[,sum(unclass(ResSevArea)),
                                            by=.(OPENING_ID,BURN_SEVER)][,.(BURN_SEVER,BU_SV_P=prop.table(V1)),
                                                                         OPENING_ID]
    BurnSev_prop_calcl_d <- dcast(BurnSev_prop_calcl,OPENING_ID~BURN_SEVER,value.var="BU_SV_P")
    Fire_Results_PropSev <- merge(st_as_sf(Plantations),BurnSev_prop_calcl_d, by="OPENING_ID")
    Fire_Results_PropSev_dat <- as.data.table(Fire_Results_PropSev)
    
    # Severity by class across total plantation area (unioned together to account for spatial overlaps)
    Fire_sev_resComb <- st_intersection(st_buffer(st_as_sf(PlantationsArea),0),st_buffer(Fire_Sev,0))
    Fire_sev_resComb$SevOpenings <- st_area(Fire_sev_resComb)
    Fire_sev_resComb_DT <- as.data.table(Fire_sev_resComb)
    FireSevPlantSum <- Fire_sev_resComb_DT[,.(PlantAreaSev =round(sum(unclass(SevOpenings)/10000),0)),
                                           by=c("FIRE_NUMBE","BURN_SEVER")]
    FireSevSum_PlantList[[i]] <- merge(FireSevSum,FireSevPlantSum,by=c("FIRE_NUMBE","BURN_SEVER"), all=TRUE)
    
    ###########################
    ##### PREVIOUS FOREST COVER PLANTATIONS ######
    Fire_VRI <- VRI_study_sel %>%
      dplyr::select(FEATURE_ID,POLYGON_ID,BASAL_AREA, CROWN_CLOS, CROWN_CL_1, FREE_TO_GR,
                    HARVEST_DA,PROJ_AGE_1, PROJ_AGE_C, PROJ_AGE_2,PROJ_AGE_3) %>%
      filter(st_intersects(Fire,., sparse = FALSE))
    Fire_sev_res_vri <- st_intersection(st_buffer(Fire_Results_PropSev,0),st_buffer(Fire_VRI,0))
    Fire_sev_res_vriDT <- as.data.table(Fire_sev_res_vri) #fire, results, severity, vri combination
    
    #when the VRI calls the harvest correctly, then use this VRI for species & canopy closure at time of fire
    #could try something like %m+% months(6) to get plus or %m-% months(6) minus 6 months within harvest date
    f <- merge(Fire_sev_res_vriDT[as.Date(DISTURBANC,format="%Y/%m/%D")  == as.Date(HARVEST_DA,format="%Y/%m/%D")|
                                    as.Date(DENUDATI_4,format="%Y/%m/%D") == as.Date(HARVEST_DA,format="%Y/%m/%D")],
               VRI_sum_all[,.(FEATURE_ID,decidCov,conifCov)],by="FEATURE_ID")
    harvestVRI <- f[,.(decidVRICov=mean(na.omit(decidCov)),conifVRICov=mean(na.omit(conifCov)),
                       Mn_vriCrClos = mean(na.omit(CROWN_CLOS)), Mn_vriPrAge =mean(na.omit(PROJ_AGE_1)),
                       Mn_vriBA = mean(na.omit(BASAL_AREA)),HarVRI=1),by="OPENING_ID"]
    #For the young stands, we can populate with average conditions from similar VRI
    harvestVRI[is.na(decidVRICov)]$conifVRICov <- mean(na.omit(harvestVRI[Mn_vriPrAge<10]$conifVRICov))
    harvestVRI[is.na(decidVRICov)]$Mn_vriCrClos <- mean(na.omit(harvestVRI[Mn_vriPrAge<10]$Mn_vriCrClos))
    harvestVRI[is.na(decidVRICov)]$Mn_vriBA <- mean(na.omit(harvestVRI[Mn_vriPrAge<10]$Mn_vriBA))
    harvestVRI[is.na(decidVRICov)]$decidVRICov <- mean(na.omit(harvestVRI[Mn_vriPrAge<10]$decidVRICov))
    
    #when there is no harvest call in the VRI, and the stands are >10yrs old, use averages to estimate sp, age, etc.
    g <- merge(Fire_sev_res_vriDT[!(OPENING_ID %in% harvestVRI$OPENING_ID)],
               VRI_sum_all[,.(FEATURE_ID,decidCov,conifCov)],by="FEATURE_ID")
    harvestVRI_Avg <- g[!(OPENING_ID %in% harvestVRI$OPENING_ID),
                        .(decidVRICov=mean(na.omit(decidCov)),conifVRICov=mean(na.omit(conifCov)),
                          Mn_vriCrClos = mean(na.omit(CROWN_CLOS)), Mn_vriPrAge =mean(na.omit(PROJ_AGE_1)),
                          Mn_vriBA = mean(na.omit(BASAL_AREA)), HarVRI=0),by="OPENING_ID"]

    harvestVRI_ <- rbind(harvestVRI_Avg,harvestVRI)
    #combine with Results and proportion fire severity
    Fire_Results_PropSev_PreFor <- merge(Fire_Results_PropSev,harvestVRI_, by="OPENING_ID")
    ##### NOT-MANAGED YOUNG STANDS #######
    st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))
    Plant2Erase <-st_union(st_combine(Fire_Results_PropSev))
    YoungVRI_NoPlant <- st_difference(st_buffer(Fire_VRI,0), st_buffer(YoungVRI_NoPlant,0))
    write_sf(YoungVRI_NoPlant,"YoungVRI_NoPlant.shp")
    YoungVRi <- YoungVRI_NoPlant %>%
      filter(PROJ_AGE_1<80)
    
    #############################      
    ##### PREVIOUS FIRES ######
    #Clip previous fires by current fire boundary
    Fire_Pfire <- fire_perimeters %>% 
      filter(st_contains(Fire, ., sparse = FALSE)) %>% 
      filter(FIRE_NUMBE != study_fireTable[i,FireID]) 
    Fire_Pfire_res <-  filter(st_intersection(st_buffer(Fire_Results_PropSev_PreFor,0),st_buffer(Fire_Pfire,0),sparse=FALSE))
    Fire_Pfire_res$area <- st_area(Fire_Pfire_res) #area of the previous fire
    Fire_Results_PropSev_PreFor$TotArea <- st_area(Fire_Results_PropSev_PreFor) #area of the total results opening
    t_dat <- as.data.table(Fire_Pfire_res)[,.(OPENING_ID,FIRE_YEAR,FIRE_CAUSE, PFireArea=unclass(area))] 
    r_dat <- as.data.table(Fire_Results_PropSev_PreFor)
    r_t_dat <- merge(r_dat[,.(OPENING_ID, TotArea=unclass(TotArea))],t_dat, by.x="OPENING_ID",by.y="OPENING_ID")
    r_t_dat_p <- r_t_dat[, .(FIRE_YEAR,FIRE_CAUSE, PropPfire= PFireArea/TotArea), by="OPENING_ID"]
    if(nrow(r_t_dat_p)>0){
      r_t_dat_q <- dcast(r_t_dat_p,OPENING_ID~FIRE_YEAR,value.var="PropPfire",fun =mean)
      Fire_Results_PropSev_PreFor_Pfire <- merge(Fire_Results_PropSev_PreFor,r_t_dat_q,by="OPENING_ID", all.x=TRUE)
    } else {
      Fire_Results_PropSev_PreFor_Pfire <- Fire_Results_PropSev_PreFor
    }
    #############################  


    #############################  
    write_sf(Fire_Results_PropSev_PreFor_Pfire, paste0("./Outputs/IndividualFires/",study_fireTable$FireID[i],"_Fire.shp"))
    Firedat <- as.data.table(Fire_Results_PropSev_PreFor_Pfire)[,geometry:=NULL]
    write.csv(Firedat,paste0("./Outputs/IndividualFires/",study_fireTable$FireID[i],"_Firedat.csv"),row.names = FALSE)
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
  FireSevSum_Plant <- rbindlist(FireSevSum_PlantList)
  write.csv(FireSevSum_Plant,paste0("./Outputs/IndividualFires/PropBurnDat.csv"),row.names = FALSE)
  r_t_dat_p_Notself_ <- rbindlist(r_t_dat_p_Notself)
  write.csv(r_t_dat_p_Notself_,paste0("./Outputs/IndividualFires/ResultsPolyOverlap.csv"),row.names = FALSE)
}