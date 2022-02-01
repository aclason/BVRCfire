# Make BEC subzone rasters
# Ingrid Farnell
# Jan 31, 2022

# This script makes BEC subzone rasters for each of the fires

#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster", "sf")) # geo comp.

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)



#------------ Load data--------------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"

# BEC - all BC
BEC_all <- read_sf(paste0(SpatialFilesPath, "./Inputs/BEC_shp/BEC_POLY_polygon.shp"))

# Study fire perimeters
study_fires <- read_sf(paste0(SpatialFilesPath, "./Inputs/Study_fire_perimeters/Study_fire_perimeters.shp"))

# Base rasters
base_list <- list.files(paste0(SpatialFilesPath,"./Inputs/BaseRasters/"),
                        pattern = "*.tif", 
                        recursive = FALSE, 
                        full.names=TRUE)
print(paste("there are", length(base_list), "covariates in the list"))
base_list <- base_list[!grepl("xml", base_list)]

file.name <- c("C10784", "C10970", "C11837", "C11937", "C12594", "C20729", "C20735", "C50647",
               "C50744","G41607", "G51632", "K20637", "R11498", "R11796", "R11921", "R12068", 
               "R12315", "R21721", "VA1787", "VA1964")


#---------------- Create BEC subzone rasters---------------#
for (i in 1:length(base_list)){
  base <- raster(base_list[i])
  # Separate study fires into individual fires
  unique_fires <- study_fires[study_fires$FIRE_NUMBE == file.name[i], ]
  # Rasterize
  BEC_all$MAP_LABEL <- as.integer(BEC_all$MAP_LABEL)
  BEC_ras <- rasterize(BEC_all, base, field = "MAP_LABEL")  
  # Mask BEC shapefile with individual study fire
  BEC_IndFire <- mask(BEC_ras, mask = unique_fires)
  # Write rasters
  writeRaster(BEC_IndFire, paste0(SpatialFilesPath, "./Inputs/BECrasters/", "BEC_", file.name[i]),
              format = "GTiff", overwrite = TRUE)
}
