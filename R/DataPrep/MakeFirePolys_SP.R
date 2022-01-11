library(data.table)
library(dplyr)
library(raster)
library(sf)
#library(stringr)
#library(readr)

############## Data to read ####################
#Study fires
study_fireTable <- fread("./Inputs/StudyFireList.csv")
Fire_shortList <- c("K20637","C20735","C50647","C50744","C20729","C10784","C10970", "R11796",
                    "R11498","G41607", "G51632", "R21721", "C11937",  "R11921")
#ResultsPolyOverlap <- fread("./Outputs/IndividualFires/ResultsPolyOverlap.csv")
dNBR_imageryDates <- fread("./Inputs/dNBR_dates.csv")
SitePrepGroups <- fread("./Inputs/SitePrep_TypeMethods.csv")

##### do you want to add x y coordinate to the data? If yes, 
AddXY2DT <- "no"
if(AddXY2DT=="yes"){
  Results_All <- read_sf("E:/Spatial Data/RESULTS/RESULTS_FirePerimeter_Intersect.shp",
                         quiet=T)
  Results_sel <- Results_All %>%
    dplyr::select(OPENING_ID,OPENING_ST,APPROVE_DA,DISTURBANC,DISTURBA_1,DENUDATION, DENUDATI_1, 
                  DENUDATI_2, DENUDATI_3,
                  DENUDATI_4,DENUDATI_5,DENUDATI_6,DENUDATI_7, DENUDATI_8, DENUDATI_9, DENUDAT_10, SITE_PREP_,
                  SITE_PREP1, SITE_PRE_1, SITE_PRE_2, SITE_PRE_3, SITE_PRE_4 ,SITE_PRE_5, PLANTING_1,PLANTING_2,
                  PLANTING_3, PLANTING_4, PLANTING_5, PLANTING_6, PLANTING_C ,BRUSHING_T,BRUSHING_1, BRUSHING_C,
                  BRUSHING_2 ,SPACING_TR, SPACING_CO ,SPACING__1, FERTILIZAT,FERTILIZ_1, FERTILIZ_2, PRUNING_TR,
                  PRUNING_CO ,PRUNING__1,SLOPE,ASPECT)
}

############# Bring in Spatial plantation files ##########################################
dt <- data.table()
Plantations <- data.table()
for(j in 2:length(Fire_shortList)){
  dt <- fread(paste0("./Outputs/IndividualFires/",Fire_shortList[j],"_Firedat.csv"))
  dt[, ':='(FireID = Fire_shortList[j])]
  Plantations <- rbind(Plantations,dt,fill=TRUE)
}
Plantations <- merge(Plantations,study_fireTable, by="FireID")
###########################################################################################

############# Process spatial plantation files to add extra infomation ###########
##### scale dNBR ######
Plantations[,dNBR_sc:= Mn_dNBR*1000]
Plantations[,sur_dNBR_sc:= sur_dNBR*1000]

########## If you want the x-ys added as columns, do that here
if(AddXY2DT=="yes"){
  #Get the openings that are part of the plantations dataset
  Open_geo <- Results_sel %>%
    dplyr::select(OPENING_ID, geometry) %>%
    filter(OPENING_ID %in% Plantations$OPENING_ID)
  #Get the centroid of polygon
  Open_cent <- st_centroid(Open_geo)
  #transform to unprojected lat/long
  Open_cent <- st_transform(Open_cent, crs="+proj=longlat +datum=WGS84 +no_defs")
  #create data table
  Open_centDT <- as.data.table(Open_cent)
  Open_centDT_latLong <- as.data.table(st_coordinates(Open_cent))
  Open_centDT <- Open_centDT[,.(OPENING_ID,X=Open_centDT_latLong$X, Y=Open_centDT_latLong$Y)]
  
  Plantations <- merge(Plantations, Open_centDT, by="OPENING_ID")
}

####### remove plantations that are too young 
preDNBR <- dNBR_imageryDates[PrePost_Fire=="Pre-fire"]
postDNBR <- dNBR_imageryDates[PrePost_Fire=="Post-fire"]
preDNBR[, PreDate:=as.Date(ImageDate2,format="%d/%m/%Y")]
postDNBR[, PostDate:=as.Date(ImageDate2,format="%d/%m/%Y")]
#use the oldest date for pre-fire imagery for any fire to set the minimum plantation age
## This could easily be changed to use the predNBR date for each fire, not one year for the whole dataset
FireMinDate <- preDNBR[,.(OldestDNBR = min(PreDate)),by="FireNumber"] 
range(format(FireMinDate[,OldestDNBR],"%m")) #when was month the preimagery taken
range(format(postDNBR[,PostDate],"%m")) #when was month the postimagery taken
FireStartdNBR <- merge(study_fireTable[FireID %in% Fire_shortList,.(FireID,FireName,StartDate)],
                       FireMinDate, by.x="FireID", by.y="FireNumber")
FireStartdNBR[,MinPlantAge := as.numeric(format(as.Date(StartDate,format="%d/%m/%Y"),"%Y"))-
                as.numeric(format(OldestDNBR,"%Y"))]
Plantations <- Plantations[PlantAge>=max(FireStartdNBR[,MinPlantAge])] #using 3 myear minimum for now
##################################################################################################

############################# Aadd detailed site prep and plantings ########################################
##### Bring in data from manually entered RESULTS data:
dt <- data.table()
SitePrep <- data.table()
#I'm skipping first fire and 50647 is missing 49 entries, and need to get proofed K20637 from Ingrid
for(i in 2:14){ 
  dt <- fread(paste0("./Outputs/IndividualFires/",Fire_shortList[i],"_Firedat_SitePrep_MethAdds.csv"),
              na.strings=c("","NA","<NA>"))
  
  cols <-c("SITE_PREP_","SITE_PRE_2",colnames(dt)[grepl("Type",colnames(dt))]) #get type and area
  AreaCols <- c("SITE_PREP1","SITE_PRE_3",colnames(dt)[grepl("Area",colnames(dt))])
  cols <- cols[grepl("SITE",cols)]
  AreaCols <- AreaCols[grepl("SITE",AreaCols)]
  dt[, (cols):=lapply(.SD, as.factor),.SDcols=cols]
  dt[, (AreaCols):=lapply(.SD, as.numeric),.SDcols=AreaCols]
  #column of SP type
  SitePrepType_melt <- melt(dt, id.vars = c("OPENING_ID"),
                            measure.vars = cols,
                            variable.name = "SP",
                            value.name = c("SP_type"),value.factor=TRUE)
  #Column of SP area
  SitePrepArea_melt <- melt(dt, id.vars = c("OPENING_ID"),
                            measure.vars = AreaCols,
                            variable.name = "SP",
                            value.name = c("SP_Area"),value.factor=FALSE)
  
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
                                 SP_Date = SitePrepDate_melt[,SP_Date],
                                 SP_Area=SitePrepArea_melt[,SP_Area])]
  SitePr <- SitePr[, SP_type_meth:= paste0(SP_type,"_",SP_Method)]
  SitePr[, ':='(FireID = Fire_shortList[i])]
  SitePrep <- rbind(SitePrep,SitePr,fill=TRUE)
}
unique(SitePrep$SP_type_meth)
table(SitePrep$SP_type_meth) #WATCH THAT THERE ARE NO NEW COMBINATIONS WITH NEW DATA ENTERED!!
SitePrep[SP_type_meth=="BU_LRIP"] #checking weird combinations. Could put a better test here.

SitePrep[,SPgrUse:=1]
#SitePrep <- SitePrep[!is.na(SP_type)&!is.na(SP_Method)]
SitePrep[,.N,by="SP_type_meth"]
merge(SitePrep[,SP_type_meth],SitePrepGroups)
SitePrep[SP_type_meth=="ME_WINDP"]

##### Use grouping variable names to assign SP_meth calls to a type ####


#Bring in the grouping variable
SitePrep <- merge(SitePrep,SitePrepGroups,by.x="SP_type_meth", by.y="Type_Method", all.x=TRUE) #51236
SitePrepCast <- dcast(SitePrep, OPENING_ID~GroupName, value.var ="SPgrUse",fun.aggregate=sum)
#change all values > 0 to 1s - presence/absence of a given treatment. i.e. you can have multiple of the same treatment still ==1
for (i in seq_along(SitePrepCast)) set(SitePrepCast, i=which(SitePrepCast[[i]]>0), j=i, value=1)

#Add the treated area as new columns
AreaTreated <- SitePrep[,.(TreatArea=sum(na.omit(SP_Area))),by=c("OPENING_ID","GroupAreaNm")]
dAreaTreated <- dcast(AreaTreated, OPENING_ID~GroupAreaNm, value.var="TreatArea",fun.aggregate = sum)
SitePrepCast <- merge(SitePrepCast,dAreaTreated,by="OPENING_ID",all.x=TRUE)

#merge the casted dataset to full dataset (will make duplicates of openings)
SitePrep2 <- merge(SitePrepCast,SitePrep[,.(OPENING_ID,FireID)], by="OPENING_ID", all.y=TRUE)
#change nas to 0s (absence of treatment == 0 not n/a)
for (i in seq_along(SitePrep2)) set(SitePrep2, i=which(is.na(SitePrep2[[i]])), j=i, value=0)
SitePrep2<- unique(SitePrep2)
SitePrep2[which(duplicated(SitePrep2[,OPENING_ID]))]
Plant_SP <- merge(Plantations,SitePrep2, by=c("OPENING_ID","FireID"), all.x=TRUE)


#missing results data from 50 openings in C50647
Plant_SP <- Plant_SP[!is.na(BroadBurn),] #don't need this line when add those openings
cols <- c("BroadBurn","PileBurn","WBurn","Chemical","DebrisMade","DebrisPiled", "GrassSeed",
          "Landings","Layout","MechUnk","None","Soil","SpotBurn","Trail")
#change the site prep method columns to factors
Plant_SP[,(cols):=lapply(.SD, as.factor),.SDcols=cols] #make sure site prep methods are factors
hist(Plant_SP[SpotBurn==1]$SpotBurn_Area)
###### OPTION 2: USE THE RESULTS NAMES #####
#SitePrepCast2 <- dcast(SitePrep, OPENING_ID~SP_type_meth, value.var ="SPgrUse",fun.aggregate=sum)
######

##### write out the full csvs with site prep methods added
FiresOfInterest <- c("C20735","C50744","C20729","C10784","C10970", "R11796",
                     "R11498","G41607", "G51632", "R21721", "C11937",  "R11921")
for(ii in 1:length(FiresOfInterest)){
  write.csv(Plant_SP[FireID==FiresOfInterest[ii]],
            paste0("./Outputs/IndividualFires/",FiresOfInterest[ii],"_Firedat_SP.csv"),
            row.names = FALSE)
}
