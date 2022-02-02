# Make BEC subzone rasters
# Ingrid Farnell
# Jan 31, 2022

# This script makes BEC subzone rasters for each of the fires

#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster", "sf","fasterize")) # geo comp.

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)



#------------ Load data--------------------#
SpatialFilesPath <- "D:/Github/BVRCfire/" #"E:/Ingrid/Borealis/BVRCfire"

# BEC - all BC
#BEC_all <- read_sf(paste0(SpatialFilesPath, "./Inputs/BEC_shp/BEC_POLY_polygon.shp"))
BEC_all <- read_sf("E:/Spatial Data/BEC/BEC_POLY_polygon.shp")
# processes that happen on all the data and you only need to do it once, do it outside of the loop
BEC_dt <- data.table(BEC_all)
BEC_dt[,MapLabFact:=as.factor(MAP_LABEL)]
BEC_dt[,MapLabInt:=as.numeric(MapLabFact)]

BEC_all <- st_as_sf(BEC_dt)

# Study fire perimeters
#study_fires <- read_sf(paste0(SpatialFilesPath, "./Inputs/Study_fire_perimeters/Study_fire_perimeters.shp"))
study_fires <- read_sf(paste0(SpatialFilesPath, "./Inputs/Shapefiles/Study_fire_perimeters.shp"))

# Base rasters
base_list <- list.files("./Inputs/Rasters/BaseRasters/",
                        pattern = "*.tif", 
                        recursive = FALSE, 
                        full.names=TRUE)
print(paste("there are", length(base_list), "files"))
#getting the fire name from the basefile list
baseSplit <- str_split(base_list,"_", simplify=TRUE)
file.name <- str_split(baseSplit[,2],".tif",simplify=TRUE)[,1]

#---------------- Create BEC subzone rasters---------------#
for (i in 1:length(base_list)){
  base <- raster(base_list[i])
  # Separate study fires into individual fires
  unique_fires <- study_fires[study_fires$FIRE_NUMBE == file.name[i], ]
  BEC_ras <- fasterize::fasterize(BEC_all, base, field = "MapLabInt")  
  # Mask BEC shapefile with individual study fire
  BEC_IndFire <- mask(BEC_ras, mask = unique_fires)
  # Write rasters
  writeRaster(BEC_IndFire, paste0("./Inputs/Rasters/BECrasters/", "BEC_", file.name[i]),
              format = "GTiff", overwrite = TRUE)
}





