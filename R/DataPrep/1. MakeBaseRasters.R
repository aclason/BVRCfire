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

# DOB - use for extent (but has wrong res)
DOB_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/DOB/dob_R21721.tif"))

# dNBR - use for resolution
dNBR_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/dNBR/dNBR_R21721.tif"))
dNBR_R21721

# Plantation - use to check
broadburn_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/PlantationPreds/R21721/R21721_BroadBurn.tif"))

# Raster stack - use to check
plantation_list_R21721 <- list.files(paste0(SpatialFilesPath,"./Inputs/PlantationPreds/R21721/"),
                                     pattern = "*.tif", 
                                     recursive = FALSE, 
                                     full.names=TRUE)

print(paste("there are", length(plantation_list_R21721), "covariates in the list"))
plantation_list_R21721 <- plantation_list_R21721[!grepl("xml", plantation_list_R21721)]
plantation_R21721 <- stack(plantation_list_R21721)


#------------------ Create base raster -----------------#
# Clip extent to DOB rasters
base_ras_R21721 <- crop(dNBR_R21721, DOB_R21721)
plot(base_ras_R21721)
res(base_ras_R21721)

# Make raster blank
base_ras_R21721[base_ras_R21721] <- NA
res(base_ras_R21721)
extent(base_ras_R21721)


#--------------- Check it worked -----------------#
# Resample raster with different resolution
DOB_R21721_RS <- resample(DOB_R21721, base_ras_R21721, method = "ngb") # DOB has 91 x 99.4 m res, make 30x30
res(DOB_R21721_RS)

# Resample raster with different extent
dNBR_R21721_RS <- resample(dNBR_R21721, base_ras_R21721, method = "ngb") # dNBR has huge extent, make smaller
plot(dNBR_R21721_RS)

# Resample raster stack with differnet extents
plantation_R21721_RS <- resample(plantation_R21721, base_ras_R21721, method = "ngb") # check that it works on a raster stack with different extent

# Check that resampled dNBR and DOB can be added to the resampled stack
stack_R21721 <- addLayer(plantation_R21721_RS, DOB_R21721_RS, dNBR_R21721_RS)
dim(stack_R21721) # yep worked


#------------ Write raster ----------------#
writeRaster(base_ras_R21721, paste0(SpatialFilesPath,"./Inputs/BaseRasters/BaseRaster_R21721.tif"), overwrite = TRUE)
