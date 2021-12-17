


#project rasters 
all.fires <- st_read("./Inputs/Shapefiles/Study_fire_perimeters.shp")
for(ix in 1:nrow(all.fires)){
  fire <- all.fires[ix,]
  dob_rast <- raster(paste0("./Inputs/Rasters/DOB/",fire$FIRE_NUMBE,"/dob.tif"))
  dob_rast_pj <- projectRaster(dob_rast, crs =" +proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83
+units=m +no_defs")
  writeRaster(dob_rast_pj,paste0("./Inputs/Rasters/DOB/dob_",fire$FIRE_NUMBE,".tif"), overwrite=TRUE)
}

# Identify the runs - across the whole fire
#### number of pixels that burned

#at what severity

#early progression vs late progression
