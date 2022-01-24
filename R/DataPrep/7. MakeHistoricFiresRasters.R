# Make historic fires rasters
# Ingrid Farnell
# Jan 19, 2021

# This script clips historic fires to our study fires and rasterizes (pixel = year of historic fire)

#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster", "sf")) # geo comp.


#------------ Load data--------------------#
# Historic fires - all BC

# Study fire perimeters


