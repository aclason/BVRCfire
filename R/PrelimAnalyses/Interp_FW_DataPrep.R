#script by Alana Clason, Nov 3, 2021
library(data.table)

#####Set data paths
#Fire weather data cleaning:
DatPath <- "./Inputs/Fireweather at Fire Locations/"

#####Import data
#BUI
bui <- fread(paste0(DatPath,"bui_at_bc.fires_ALL.csv"), h=T)
bui <- bui[,which(unlist(lapply(bui, function(x)!all(is.na(x))))),with=F]
bui_m <- melt(bui,id.vars = c("Fire_ID","x_y","X","Y"),variable.name="Date",value.name = "bui")
#DC
dc <- fread(paste0(DatPath,"dc_at_bc.fires_ALL.csv"), h=T)
dc <- dc[,which(unlist(lapply(dc, function(x)!all(is.na(x))))),with=F]
dc_m <- melt(dc,id.vars = c("Fire_ID","x_y","X","Y"),variable.name="Date",value.name = "dc")
#DMC
dmc <- fread(paste0(DatPath,"dmc_at_bc.fires_ALL.csv"), h=T)
dmc <- dmc[,which(unlist(lapply(dmc, function(x)!all(is.na(x))))),with=F]
dmc_m <- melt(dmc,id.vars = c("Fire_ID","x_y","X","Y"),variable.name="Date",value.name = "dmc")
#dsr
dsr <- fread(paste0(DatPath,"dsr_at_bc.fires_ALL.csv"), h=T)
dsr <- dsr[,which(unlist(lapply(dsr, function(x)!all(is.na(x))))),with=F]
dsr_m <- melt(dsr,id.vars = c("Fire_ID","x_y","X","Y"),variable.name="Date",value.name = "dsr")
#ffmc
ffmc <- fread(paste0(DatPath,"ffmc_at_bc.fires_ALL.csv"), h=T)
ffmc <- ffmc[,which(unlist(lapply(ffmc, function(x)!all(is.na(x))))),with=F]
ffmc_m <- melt(ffmc,id.vars = c("Fire_ID","x_y","X","Y"),variable.name="Date",value.name = "ffmc")
#fwi
fwi <- fread(paste0(DatPath,"fwi_at_bc.fires_ALL.csv"), h=T)
fwi <- fwi[,which(unlist(lapply(fwi, function(x)!all(is.na(x))))),with=F]
fwi_m <- melt(fwi,id.vars = c("Fire_ID","x_y","X","Y"),variable.name="Date",value.name = "fwi")
#humidity
humidity <- fread(paste0(DatPath,"humidity_at_bc.fires_ALL.csv"), h=T)
humidity <- humidity[,which(unlist(lapply(humidity, function(x)!all(is.na(x))))),with=F]
humidity_m <- melt(humidity,id.vars = c("Fire_ID","x_y","X","Y"),variable.name="Date",value.name = "humidity")
#isi
isi <- fread(paste0(DatPath,"isi_at_bc.fires_ALL.csv"), h=T)
isi <- isi[,which(unlist(lapply(isi, function(x)!all(is.na(x))))),with=F]
isi_m <- melt(isi,id.vars = c("Fire_ID","x_y","X","Y"),variable.name="Date",value.name = "isi")
#precipitation
precipitation <- fread(paste0(DatPath,"precipitation_at_bc.fires_ALL.csv"), h=T)
precipitation <- precipitation[,which(unlist(lapply(precipitation, function(x)!all(is.na(x))))),with=F]
precipitation_m <- melt(precipitation,id.vars = c("Fire_ID","x_y","X","Y"),
                        variable.name="Date",value.name = "precipitation")
#sdmc
sdmc <- fread(paste0(DatPath,"sdmc_at_bc.fires_ALL.csv"), h=T)
sdmc <- sdmc[,which(unlist(lapply(sdmc, function(x)!all(is.na(x))))),with=F]
sdmc_m <- melt(sdmc,id.vars = c("Fire_ID","x_y","X","Y"),variable.name="Date",value.name = "sdmc")
#temperautre
temperature <- fread(paste0(DatPath,"temprature_at_bc.fires_ALL.csv"), h=T)
temperature <- temperature[,which(unlist(lapply(temperature, function(x)!all(is.na(x))))),with=F]
temperature_m <- melt(temperature,id.vars = c("Fire_ID","x_y","X","Y"),variable.name="Date",value.name = "temperature")
#wind
wind <- fread(paste0(DatPath,"wind_at_bc.fires_ALL.csv"), h=T)
wind <- wind[,which(unlist(lapply(wind, function(x)!all(is.na(x))))),with=F]
wind_m <- melt(wind,id.vars = c("Fire_ID","x_y","X","Y"),variable.name="Date",value.name = "wind")

fw <- Reduce(merge, list(bui_m,dc_m,dmc_m,dsr_m,ffmc_m,fwi_m,humidity_m,
                         isi_m,precipitation_m,sdmc_m,temperature_m,wind_m))
fw[,Date:=as.Date(Date,format="%Y%m%d")]
write.csv(fw,"./Inputs/Fireweather at Fire Locations/FireWeather.csv", row.names = FALSE)
