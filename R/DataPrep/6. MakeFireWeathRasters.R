#Make fire weather and fire weather indices rasters
#based on the day of burn raster

#Alana Clason

#January 21, 2022

library(data.table)
library(raster)
library(dplyr)
library(ggplot2)
# Using BCWS fire weather data now:

#Nadina fire: Nadina, Parrott, Houston, Peden
#Verdun fire: Grassy Plains, Parrott
#Island fire: Holy Cross, East Ootsa
#Shovel fire: Augier, Holy Cross, Vanderhoof
#Chutanli fire: Kluskus, Moose Lake
#Tezzeron fire:Fort st james, north chilko
#Shag Creek: Baldface, moose lake, nazko
#North Baezaeko: Nazko, Tautri
#Baldface Mountain: Baldface, Anahim Lake
#Remianing 2018 fires: R12068 (Pondosy), R12315 (Tesla), VA1964 (Dean River) and VA1787 (Ramsey Creek) not included
#as they are in the park

FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
StudyFireList <- fread("./Inputs/StudyFireList.csv")
sfl <- StudyFireList[FireID %in% FiresOfInterest]
Stations <- c("Nadina","Parrott","Houston","Peden","GrassyPlains","HolyCross2",
              "EastOotsa","AugierLake","Vanderhoof","Kluskus","FortStJames","NorthChilco")
fw_sts <- data.table()
for(ii in 1:length(Stations)){
  fw_st <- fread(paste0("./Inputs/Fireweather/2018_",Stations[ii],".csv"),header=TRUE)
  fw_st[, ':='(stationID = Stations[ii])]
  fw_sts <- rbind(fw_sts,fw_st)
}
sf_sts <- data.table(FireName=c("Nadina Lake","Nadina Lake","Nadina Lake","Nadina Lake",
                                "Verdun Mountain","Verdun Mountain",
                                "Island Lake","Island Lake",
                                "Shovel Lake","Shovel Lake","Shovel Lake",
                                "Chutanli Lake",
                                "Tezzeron Lake","Tezzeron Lake"),
                     stationID=c("Nadina","Parrott","Houston","Peden",
                                 "GrassyPlains","Parrott",
                                 "HolyCross2","EastOotsa",
                                 "AugierLake","HolyCross2","Vanderhoof",
                                 "Kluskus",
                                 "FortStJames","NorthChilco"))
sf_sts <- merge(sfl[,.(FireID,FireName)],sf_sts,by="FireName", all.y=TRUE)
sf_sts <- merge(sf_sts, fw_sts, by=c("stationID"), allow.cartesian=TRUE)
sf_sts[,dateNoHr:= as.Date(as.character(weather_date),format="%Y%m%d")]
sf_sts[,jDay:=yday(dateNoHr)] #julian day
min(sf_sts[,dateNoHr])
#calculate the daily max temp and winds, and min Rh for each station for each fire
sts_Dailies <- sf_sts[,.(maxTemp=max(na.omit(temperature)),minRH=min(na.omit(relative_humidity)),
                         maxWind=max(na.omit(wind_speed))), 
                      by=c("FireID","stationID","dateNoHr","jDay")]
fwi_sts <- data.table()
for(ii in 1:length(Stations)){
  fwi_st <- fread(paste0("./Inputs/Fireweather/2018_",Stations[ii],"_Daily.csv"),header=TRUE)
  fwi_st[, ':='(stationID = Stations[ii])]
  fwi_sts <- rbind(fwi_sts,fwi_st)
}
fwi_sts[,dateNoHr:= as.Date(as.character(weather_date),format="%Y%m%d")][,jDay:=yday(dateNoHr)] #julian day
fwi_dailies <- merge(fwi_sts[,.(stationID,dateNoHr,jDay,gc,ffmc,dmc,dc,isi,bui,fwi)],sts_Dailies,
                     by=c("stationID","dateNoHr","jDay"), all.y=TRUE)

#calculate average daily weather for each fire
sts_Mns <- fwi_dailies[,.(MnMaxTemp=mean(maxTemp),MnMinRH=mean(minRH),MnMaxWind=mean(maxWind),
                          MnBUI=mean(bui),MnISI=mean(isi),MnDMC=mean(dmc),MnDC=mean(dc),MnFWI=mean(fwi)),
                       by=c("FireID","dateNoHr","jDay")]
ggplot()+
  geom_point(data = fwi_dailies[FireID=="R11498"], aes(x=jDay,y=dc))+
  geom_point(data = sts_Mns[FireID=="R11498"], aes(x=jDay,y=MnDC, colour="red"))
  facet_wrap(~FireID)
#mean chutlanli is the same as raw data because there's only one weather station
ggplot()+
  geom_point(data = fwi_dailies[FireID=="R11498"], aes(x=jDay,y=maxTemp))+
  geom_point(data = sts_Mns[FireID=="R11498"], aes(x=jDay,y=MnMaxTemp, colour="red"))
  
for(ii in 1:length(FiresOfInterest)){
  #read in DOB rasters
  dob <- raster(paste0("./Inputs/Rasters/DOB/DOBrounded_",FiresOfInterest[ii],".tif"))
  DaysOfInterest <- seq(min(na.omit(dob[])), max(na.omit(dob[])))
  bui_rast <- dob
  isi_rast <- dob
  dmc_rast <- dob
  dc_rast <- dob
  fwi_rast <- dob
  maxT_rast <- dob
  minRH_rast <- dob
  maxW_rast <- dob
  for(i in 1:length(DaysOfInterest)){
    bui_rast[bui_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnBUI
    isi_rast[isi_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnISI
    dmc_rast[dmc_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnDMC
    dc_rast[dc_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnDC
    fwi_rast[fwi_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnFWI
    maxT_rast[maxT_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnMaxTemp
    minRH_rast[minRH_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnMinRH
    maxW_rast[maxW_rast==DaysOfInterest[i]] <- sts_Mns[FireID== FiresOfInterest[ii] & jDay==DaysOfInterest[i]]$MnMaxWind
  }
  writeRaster(bui_rast,paste0("./Inputs/Rasters/FireWeather/bui_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(isi_rast,paste0("./Inputs/Rasters/FireWeather/isi_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(dmc_rast,paste0("./Inputs/Rasters/FireWeather/dmc_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(dc_rast,paste0("./Inputs/Rasters/FireWeather/dc_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(fwi_rast,paste0("./Inputs/Rasters/FireWeather/fwi_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(maxT_rast,paste0("./Inputs/Rasters/FireWeather/maxT_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(minRH_rast,paste0("./Inputs/Rasters/FireWeather/minRH_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
  writeRaster(maxW_rast,paste0("./Inputs/Rasters/FireWeather/maxW_",FiresOfInterest[ii],".tif"),overwrite=TRUE)
}

### Kurram's data
#fw[,FW_Year:=as.numeric(format(Date,format="%Y"))] #which year
#fw[,jDay:=yday(Date)] #Add julian date using yday because that's what S.Parks does in DOB code

#take the mean of four corners and centre for each fire
#fw[Fire_ID=="C10784"&jDay==208 & FW_Year==2016]
#cols <- c("fwi","dc","dmc","dsr","ffmc","fwi","humidity","isi","precipitation","sdmc","temperature","wind")
#fw_mns <- fw[, lapply(.SD,mean),by=.(Fire_ID,FW_Year,jDay,Date),.SDcols=cols]
#fw_mns[Fire_ID=="C10784"&jDay==208 & FW_Year==2016]

### Kira's weather station data
#WS_FW <- fread("./Inputs/Fireweather/REVISED fire weather_KMH.csv")
#setnames(WS_FW,c("MaxTemp","Precip","MinRH","Wind"),paste0("WS_",c("MaxTemp","Precip","MinRH","Wind")))
#WS_FW[FireName=="Nadina"& Year==2018 & DOB==211]
#fw_ws <- merge(fw_mns, WS_FW[,.(FireNumber,DOB,Year,WS_MaxTemp,WS_Precip,WS_MinRH,WS_Wind)],
 #              by.x=c("Fire_ID","FW_Year","jDay"),by.y=c("FireNumber","Year","DOB"), all.x=TRUE)
