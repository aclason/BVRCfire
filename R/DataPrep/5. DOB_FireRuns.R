# DOB Fire runs
# Ingrid Farnell
# Jan 18, 2021

# This script calculates the proportion of the fire that burned each day (#of pixels in dob[i]/total # fire pixels)



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
DOB_list <- list.files(paste0(SpatialFilesPath,"./Inputs/DOB/DOBrounded/"),
                       pattern = "*.tif", 
                       recursive = FALSE, 
                       full.names=TRUE)
print(paste("there are", length(DOB_list), "covariates in the list"))
DOB_list <- DOB_list[!grepl("xml", DOB_list)]

file.name <- c("C10784", "C10970", "C11837", "C11937", "C12594", "C20729", "C20735", "C50647",
               "C50744","G41607", "G51632", "K20637", "R11498", "R11796", "R11921", "R12068", 
               "R12315", "R21721", "VA1787", "VA1964")


#-----------------DOB to fire runs-----------------#
for (i in 1:length(DOB_list)){
  DOB <- raster(DOB_list[i])
  # Round DOB values to whole numbers
  DOB <- round(DOB, digits = 0)
  # Get count for each day
  count <- freq(DOB)
  count <- as.data.table(count)
  # Make sure pixels that are NA have NA for count too
  count <- count %>% mutate(count = replace(count, is.na(value), NA))
  # Calculate proportion of the fire that burned each day (# pixels burned in a day / total pixels of the fire (*subract NAs))
  prop_burned <- count %>% mutate(prop_burned = ((count/(ncell(DOB) - (freq(DOB, value = NA))))*100))
  # Drop count column
  prop_burned <-subset(prop_burned, select = -count)
  # Round to 2 decimals
  prop_burned <- prop_burned %>% mutate_if(is.numeric,
                                           round,
                                           digits = 0)
  # Substitute DOB values for prop burned values
  runs <- subs(DOB, prop_burned, by = "value", subsWithNA = FALSE)
  
  # Write rasters
  writeRaster(runs, paste0(SpatialFilesPath, "./Inputs/FireRuns/", "FireRun_", file.name[i]), format = "GTiff", 
              overwrite = TRUE)
}

