# DOB data prep
# Ingrid Farnell
# Jan 21, 2022

# This script rounds the DOB rasters to a whole number. 


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

# DOB rasters (can't stack because different extents)
DOB_list <- list.files(paste0(SpatialFilesPath,"./Inputs/DOB/"),
                       pattern = "*.tif", 
                       recursive = FALSE, 
                       full.names=TRUE)
print(paste("there are", length(DOB_list), "covariates in the list"))
DOB_list <- DOB_list[!grepl("xml", DOB_list)] #AC: I don't think you need this line as you don't have any .xml files??

#-----------------Round DOB pixels-----------------#
DOB <- list()
DOB_rounded <- list()

file.name <- c("C10784", "C10970", "C11837", "C11937", "C12594", "C20729", "C20735", "C50647",
               "C50744","G41607", "G51632", "K20637", "R11498", "R11796", "R11921", "R12068", 
               "R12315", "R21721", "VA1787", "VA1964")

for (i in 1:length(DOB_list)){
  DOB <- raster(DOB_list[[i]])
  # Round DOB values to whole numbers
  DOB_rounded <- round(DOB, digits = 0)
  # Write rasters
  writeRaster(DOB_rounded, paste0(SpatialFilesPath, "./Inputs/DOB/DOBrounded/", "DOBrounded_", file.name[[i]]), format = "GTiff", overwrite = TRUE)
  #writeRaster(DOB_rounded, paste0("./Inputs/Rasters/DOB/", "DOBrounded_", file.name[[i]]), format = "GTiff", overwrite = TRUE)
}
