#Landscape topography data
library(raster)
library(spatialEco)

##### DEM #####
####### read in DEM - not projected #######
#dem <- raster("F:/Spatial Data/Fire/ForestManage_Fire/dem_fireper.asc")
#BCbound <- read_sf("F:/Spatial Data/Administrative boundaries/BC_boundary_poly2.shp")
#t <- raster(extent(BCbound), res=c(30,30), crs=crs(BCbound))
#dem83 <- projectRaster(from=dem,to=t, method="bilinear")
#writeRaster(dem83,"F:/Spatial Data/DEM/DEM_BC_albers.tif")
############## 

####### Start here with reprojected raster #######
# Read in reprojected DEM - 30m resolution resampling.
dem <- raster("F:/Spatial Data/DEM/DEM_BC_albers.tif")

### mask the study area
study_fireTable <- fread("C:/Users/Alana2012/ALANA_FILES/GitHub/BVRCfire/inputs/StudyFireList.csv")
fire_perimeters <- read_sf("F:/Spatial Data/FCI_Fire_Regen_spatialfiles/Historical wildfire perimeters/BC Wildfire Historical Fire Perimeters.shp",quiet=TRUE)
Fire_shortList <- c("K20637","C20735","C50647","C50744","C20729","C10784","C10970", "R11796",
                    "R11498","G41607", "G51632", "R21721", "C11937",  "R11921")
Study_fires <- st_as_sf(as.data.table(fire_perimeters)[FIRE_NUMBE %in% Fire_shortList])

#mask dem by study fires:
dem_studyFires <- raster::mask(dem, Study_fires)
#writeRaster(dem_studyFires,"./Inputs/Rasters/dem_studyFires.asc")
########################################################
#### Start here with reprojected DEM for study area ####
dem_studyFires <- raster("./Inputs/Rasters/dem_studyFires.asc")
dem_studyFires300 <- aggregate(dem_studyFires, fact=10, fun=mean)
dem_list <- list()
for(j in 1:length(Fire_shortList)){
  dem_list[[j]] <- raster::mask(dem_studyFires, Study_fires %>% filter(FIRE_NUMBE==Fire_shortList[j]))
}

### fire-level metrics

### characterizing openings
dem_studyFires <- raster("./Inputs/Rasters/dem_studyFires.tif")
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
