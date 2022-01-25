# Make historic fires rasters
# Ingrid Farnell
# Jan 19, 2021

# This script clips historic fires to our study fires and makes a raster for each fire year 


#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster", "sf", "rgdal")) # geo comp.

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#------------ Load data--------------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"

# Historic fires - all BC
hist_fires <- read_sf(paste0(SpatialFilesPath, "./Inputs/HistoricWildfires_shp/BC Wildfire Historical Fire Perimeters.shp"))

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


#---------------- Create historic fires rasters---------------#
for (i in 1:length(base_list)){
  base <- raster(base_list[i])
  # Get historic fire years > 1960
  fire_years <- unique(hist_fires$FIRE_YEAR)
  # Only keep fires older than 1960
  fire_years <- fire_years[fire_years >= 1960 & fire_years < 2017]
  for (j in 1:length(fire_years)){
    hist_years <- hist_fires[hist_fires$FIRE_YEAR == fire_years[j], ]
    # Rasterize 
    hist_year_ras <- rasterize(hist_years, base, field = "FIRE_YEAR")
    # Separate study fires into individual fires
    unique_fires <- study_fires[study_fires$FIRE_NUMBE == file.name[i], ]  
    # Mask historic fires with individual study fire
    hist_ras_mask <- mask(hist_year_ras, mask = unique_fires)
    # Write rasters
    writeRaster(hist_ras_mask, paste0(SpatialFilesPath, "./Inputs/HistoricFires/", "HistoricFires_", 
                                    file.name[i], sep = "_", fire_years[j]), format = "GTiff", overwrite = TRUE)
  }
}
