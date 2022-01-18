# DOB Fire runs
# Ingrid Farnell
# Jan 18, 2021

# This script counts how big each fire run (# pixels/day) was and wheather it was an early or late progression

# Identify the runs - across the whole fire
#### number of pixels that burned

#at what severity

#early progression vs late progression


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

# DOB 
DOB_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/DOB/dob_R21721.tif"))
DOB_R21721 

# Get count for each day
count_R21721 <- freq(DOB_R21721)
