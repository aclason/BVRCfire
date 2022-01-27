#Landscape topography data
library(raster)
library(spatialEco)
library(sf)
##### DEM #####
# Read in reprojected DEM - 30m resolution resampling.
dem <- raster("E:/Spatial Data/DEM/DEM_BC_albers.tif")

### mask the study area
Study_fires <- read_sf("./Inputs/Shapefiles/Study_fire_perimeters.shp")

#mask dem by study fires:
dem_studyFires <- raster::mask(dem, Study_fires)

#writeRaster(dem_studyFires,"./Inputs/Rasters/dem_studyFires.tif")
########################################################
#### Start here with reprojected DEM maksed for study area ####
dem_studyFires <- raster("./Inputs/Rasters/dem_studyFires.tif")

### topographic metrics
#slope
DEMslope <- raster::terrain(dem_studyFires, opt=c("slope"))
writeRaster(DEMslope,"./Inputs/Rasters/DEMslope.tif")
#aspect
DEMaspect <- raster::terrain(dem_studyFires, opt=c("aspect"))
writeRaster(DEMaspect,"./Inputs/Rasters/DEMaspect.tif")

#Topographic position index
DEMtpi <- tpi(dem_studyFires, win="circle", scale=100) #not sure what the scale is
writeRaster(DEMtpi,"DEMtpi.tif")

#heat load index (based on McCune and Keon 2002)
DEMhli <- hli(dem_studyFires)
writeRaster(DEMhli,"./Inputs/Rasters/DEMhli.tif")
