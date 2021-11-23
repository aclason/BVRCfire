## Ingrid clipped the RESUTLS_2021 openings layer with the 2021_fire_polys
Results_all <- read_sf("E:/Ingrid/Borealis/BVRC_21-01_CFS/RESULTS/RSLT_OPENING_SVW/RSLT_OPNGS_polygon.shp")

fires_2021 <- read_sf("E:/Ingrid/Borealis/BVRC_21-01_CFS/2021_fire_polys/2021_fire_polys.shp")

# Check that both shapefiles are in the same projection
head(Results_all) # in NAD83
head(fires_2021) #in NAD83

# Exctract the RESULTS polygons that overlap with the fires
RESULTS_2021fires <- st_intersection(fires_2021, st_make_valid(Results_all))

head(RESULTS_2021fires)

# Export shapefile
write_sf(st_as_sf(RESULTS_2021fires), "RESULTS_2021fires.shp", overwrite=TRUE)

# Export as .csv
st_write(RESULTS_2021fires, "RESULTS_2021fires.csv")

