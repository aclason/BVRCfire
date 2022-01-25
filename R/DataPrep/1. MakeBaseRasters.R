# Make base rasters
# Ingrid Farnell
# Jan 18, 2021

# This script makes the base raster for each fire that has the extent and resolution (30m x 30m) to be used to 
# stack predictor and response rasters. 


#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster")) # geo comp.

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#---------------- Load data --------------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"

# DOB - use for extent 
DOB_list <- list.files(paste0(SpatialFilesPath,"./Inputs/DOB/DOBrounded/"),
                       pattern = "*.tif", 
                       recursive = FALSE, 
                       full.names=TRUE)
print(paste("there are", length(DOB_list), "covariates in the list"))
DOB_list <- DOB_list[!grepl("xml", DOB_list)]

# dNBR - use for resolution
dNBR_list <- list.files(paste0(SpatialFilesPath,"./Inputs/dNBR/"),
                       pattern = "*.tif", 
                       recursive = FALSE, 
                       full.names=TRUE)
print(paste("there are", length(dNBR_list), "covariates in the list"))
dNBR_list <- dNBR_list[!grepl("xml", dNBR_list)]

file.name <- c("C10784", "C10970", "C11837", "C11937", "C12594", "C20729", "C20735", "C50647",
               "C50744","G41607", "G51632", "K20637", "R11498", "R11796", "R11921", "R12068", 
               "R12315", "R21721", "VA1787", "VA1964")


#------------------ Create base rasters -----------------#
for (i in 1:length(DOB_list)){
  DOB <- raster(DOB_list[i])
  dNBR <- raster(dNBR_list[i])
  # Clip extent to DOB rasters
  base_ras <- crop(dNBR, DOB)
  # Make raster blank
  base_ras[base_ras] <- NA
  # Write rasters
  writeRaster(base_ras, paste0(SpatialFilesPath, "./Inputs/BaseRasters/", "BaseRaster_", file.name[i]), format = "GTiff", 
              overwrite = TRUE)
}
