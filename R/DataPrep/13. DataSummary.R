

library(sf)
library(dplyr)



VRI_StudyFires <- read_sf("D:/Spatial Data/VRI/2016/BVRCfire_VRI2016.shp")
FiresOfInterest <- c("R11796","R11498","R21721","R11921","G41607","G51632")
StudyFirePerims <- read_sf("./Inputs/Shapefiles/Study_fire_perimeters.shp")




PlantPolys <- read_sf(paste0("./Inputs/Shapefiles/R11498_Plantations.shp"))

Fire <- st_as_sf(StudyFirePerims %>% dplyr::filter(FIRE_NUMBE == FiresOfInterest[ix]))
Fire_VRI <- VRI_StudyFires %>%
  dplyr::filter(st_contains(Fire, ., sparse = FALSE)[1,]) #%>%
  
VRI_Noplants <- st_difference(Fire_VRI, PlantPolys)
write_sf(VRI_Noplants, "VRI_Noplants_Shovel.shp")  
#filter doesn't work anymore without explicitly passing a vector
  dplyr::filter(st_difference(Fire, ., sparse = FALSE)[1,]) 