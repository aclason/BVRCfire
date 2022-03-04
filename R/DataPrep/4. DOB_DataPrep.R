# DOB data prep
# Ingrid Farnell and Alana Clason
# Jan 21, 2022

library(raster)

# This script copies the dob tiffs created by Sean Parkes code (3. DOB_interpolation.R), renames into the main
#folder in order to be re-projected below.

FiresOfInterest <- c("C10784", "C10970", "C11837", "C11937", "C12594", "C20729", "C20735", "C50647",
               "C50744","G41607", "G51632", "K20637", "R11498", "R11796", "R11921", "R12068", 
               "R12315", "R21721", "VA1787", "VA1964")
for(i in 1:length(FiresOfInterest)){
  r <- raster(paste0("./Inputs/Rasters/DOB/",FiresOfInterest[i],"/dob.tif"))
  writeRaster(r,paste0("./Inputs/Rasters/DOB/dob_",FiresOfInterest[i],"n83.tif"))
}


# This script reprojects the DOB rasters from NAD83 conus Albers to NAD83 BC Albers

#---------------- Load data --------------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"

# DOB rasters
DOB_list <- list.files(paste0(SpatialFilesPath,"./Inputs/DOB/"),
                       pattern = "n83", 
                       recursive = FALSE, 
                       full.names=TRUE)


#-----------------Reproject-----------------#
for (i in 1:length(DOB_list)){
  DOB <- raster(DOB_list[i])
  # Reproject rasters to NAD83 BC Albers
  DOB_rpjct <- projectRaster(DOB, crs="+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs",
                             method="ngb")
  # Write rasters
  writeRaster(DOB_rpjct, paste0(SpatialFilesPath, "./Inputs/Rasters/DOB/", "dob_", FiresOfInterest[i]), format = "GTiff", overwrite = TRUE)
}
