library(data.table)
library(raster)

fw <- fread("./Inputs/Fireweather/FireWeather.csv")
str(fw)
fw[,FW_Year:=as.numeric(format(Date,format="%Y"))] #which year
fw[,jDay:=yday(Date)] #Add julian date using yday because that's what S.Parks does in DOB code

#take the mean of four corners and centre for each fire
fw[Fire_ID=="C10784"&jDay==208 & FW_Year==2016]
cols <- c("fwi","dc","dmc","dsr","ffmc","fwi","humidity","isi","precipitation","sdmc","temperature","wind")
fw_mns <- fw[, lapply(.SD,mean),by=.(Fire_ID,FW_Year,jDay,Date),.SDcols=cols]
fw_mns[Fire_ID=="C10784"&jDay==208 & FW_Year==2016]
#add Kira's weather station data
WS_FW <- fread("./Inputs/Fireweather/REVISED fire weather_KMH.csv")
setnames(WS_FW,c("MaxTemp","Precip","MinRH","Wind"),paste0("WS_",c("MaxTemp","Precip","MinRH","Wind")))
WS_FW[FireName=="Nadina"& Year==2018 & DOB==211]

fw_ws <- merge(fw_mns, WS_FW[,.(FireNumber,DOB,Year,WS_MaxTemp,WS_Precip,WS_MinRH,WS_Wind)],
               by.x=c("Fire_ID","FW_Year","jDay"),by.y=c("FireNumber","Year","DOB"), all.x=TRUE)

###### Nadina Lake Fire ######
#read in DOB rasters
dob_R21721 <- raster(paste0("./Inputs/Rasters/DOB/dob_R21721.tif"))
DaysOfInterest <- seq(min(na.omit(dob_R21721[])), max(na.omit(dob_R21721[])))

#fwi
bui_R21721 <- dob_R21721
bui_R21721[] <- round(bui_R21721[],0)
for(i in 1:length(DaysOfInterest)){
  bui_R21721[bui_R21721==DaysOfInterest[i]] <- fw_ws[Fire_ID=="R21721" & 
                                                       FW_Year==2018 & jDay==DaysOfInterest[i]]$bui
}
writeRaster(bui_R21721,"./Inputs/Rasters/FireWeather/bui_R21721.tif",overwrite=TRUE)

#FWI
fwi_R21721 <- dob_R21721
fwi_R21721[] <- round(fwi_R21721[],0)
for(i in 1:length(DaysOfInterest)){
  fwi_R21721[fwi_R21721==DaysOfInterest[i]] <- fw_ws[Fire_ID=="R21721" & 
                                                       FW_Year==2018 & jDay==DaysOfInterest[i]]$fwi
}
writeRaster(fwi_R21721,"./Inputs/Rasters/FireWeather/fwi_R21721.tif",overwrite=TRUE)

#Temp
temperature_R21721 <- dob_R21721
temperature_R21721[] <- round(temperature_R21721[],0)
for(i in 1:length(DaysOfInterest)){
  temperature_R21721[temperature_R21721==DaysOfInterest[i]] <- fw_ws[Fire_ID=="R21721" & 
                                                                     FW_Year==2018 & jDay==DaysOfInterest[i]]$temperature
}
writeRaster(temperature_R21721,"./Inputs/Rasters/FireWeather/temperature_R21721.tif",overwrite=TRUE)

#MaxTemp
WS_MaxTemp_R21721 <- dob_R21721
WS_MaxTemp_R21721[] <- round(WS_MaxTemp_R21721[],0)
for(i in 1:length(DaysOfInterest)){
  WS_MaxTemp_R21721[WS_MaxTemp_R21721==DaysOfInterest[i]] <- fw_ws[Fire_ID=="R21721" & 
                                                       FW_Year==2018 & jDay==DaysOfInterest[i]]$WS_MaxTemp
}
writeRaster(WS_MaxTemp_R21721,"./Inputs/Rasters/FireWeather/WS_MaxTemp_R21721.tif",overwrite=TRUE)
