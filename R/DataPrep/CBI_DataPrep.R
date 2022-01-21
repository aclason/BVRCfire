# CBI data prep
# Ingrid Farnell
# Dec 22, 2021


# Intersect 2021 CBI, dNBR, VRI, and RESULTS
# Dataflow:
# 1. Load data
# 2. convert CBI UTMS to lat long and leave as spatial points dataframe
# 3. Create a column for whether or not CBI plots are within study boundary
# 4. Intersect CBI dNBR, VRI and RESULTS to create one dataset.

# Intersect 2020 CBI (already has dNBR and VRI, but going to merge with VRI I used for 2021 data) with RESULTS
# Dataflow:

# 5. convert 2020 CBI UTM to lat long and leave as spatial data frame
# 6. Intersect 2020 CBI with RESULTS and VRI

# 7. Combine 2020 and 2021 CBI datasets


#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster", "sf", "spatialEco")) # geo comp.
 

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#--------------- 1. Load data -----------------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"

# Load CBI 2021
CBI_2021 <- fread("./Inputs/CBI/2021_CBI_plots_5_fires_cleaned_17Dec2021_CondensedData.csv")
str(CBI_2021)


# Load CBI 2020
CBI_2020 <- fread("./Inputs/CBI/cleaned_2020_CBI_plots_with_dNBR_and_VRI_simplified.csv")
str(CBI_2020)


# Load dNBR rasters in raster stack
dNBR_list <- list.files(paste0(SpatialFilesPath, "./Inputs/dNBR"),
                        pattern = "*.tif", 
                        recursive = FALSE, 
                        full.names=TRUE)

print(paste("there are", length(dNBR_list), "covariates in the list")) # make sure there are 20

dNBR_list <- dNBR_list[!grepl("xml",dNBR_list)]
dNBR_list <- dNBR_list[grepl("(R21721|R11796|R11498|C10784|R11921)", dNBR_list)] # select fires of interest
dNBR_list

# Create raster stack
dNBR <- stack(dNBR_list)
dim(dNBR)


# Load VRI (2016) for study fire boundary
VRI_study <- read_sf("E:/Ingrid/Borealis/BVRC_21-01_CFS/VRI/VRI2016_StudyFires/VRI2016_StudyFires.shp")
# Dissolve VRI internal boundaries to get simple fire study polygon
study_boundary <- st_union(VRI_study)


# Load VRI (2016) that extends beyond fire boundary
VRI <- read_sf("E:/Ingrid/Borealis/BVRC_21-01_CFS/VRI/VRI_2016/VRI_2016_greater_study_area.shp")
VRI_sel <- VRI %>%
  dplyr::select(FEATURE_ID,  MAP_ID, POLYGON_ID, OPENING_IN, OPENING_SO, OPENING_NU, OPENING_ID, BASAL_AREA, 
                CROWN_CLOS, CROWN_CL_1,FREE_TO_GR,HARVEST_DA,PROJ_AGE_1,PROJ_AGE_C,PROJ_AGE_2,PROJ_AGE_3,
                PROJ_HEIGH, PROJ_HEI_1, PROJ_HEI_2, PROJ_HEI_3,
                SPECIES_CD, SPECIES_PC, SPECIES__1, SPECIES__2, SPECIES__3, SPECIES__4, SPECIES__5,
                SPECIES__6, SPECIES__7,SPECIES__8, SPECIES__9,SPECIES_10)

# Load RESULTS data
Results_All <- read_sf("E:/Ingrid/Borealis/BVRC_21-01_CFS/RESULTS/RESULTS_FirePerimeter_Intersect/RESULTS_FirePerimeter_Intersect.shp")
Results_sel <- Results_All %>%
  dplyr::select(OPENING_ID,OPENING_ST,APPROVE_DA,DISTURBANC,DISTURBA_1,DENUDATION, DENUDATI_1, 
                DENUDATI_2, DENUDATI_3,
                DENUDATI_4,DENUDATI_5,DENUDATI_6,DENUDATI_7, DENUDATI_8, DENUDATI_9, DENUDAT_10, SITE_PREP_,
                SITE_PREP1, SITE_PRE_1, SITE_PRE_2, SITE_PRE_3, SITE_PRE_4 ,SITE_PRE_5, PLANTING_1,PLANTING_2,
                PLANTING_3, PLANTING_4, PLANTING_5, PLANTING_6, PLANTING_C ,BRUSHING_T,BRUSHING_1, BRUSHING_C,
                BRUSHING_2 ,SPACING_TR, SPACING_CO ,SPACING__1, FERTILIZAT,FERTILIZ_1, FERTILIZ_2, PRUNING_TR,
                PRUNING_CO ,PRUNING__1,SLOPE,ASPECT)


#----------------- 2. Convert UTM in 2021 CBI to lat long ----------------------#
# Data has 2 UTM zones (9 and 10) so split into two datasets then merge back together?
utm9.cbi <- CBI_2021[UTM_Zone == 9,]
utm10.cbi <- CBI_2021[UTM_Zone == 10,]

# Create spatial object for coordinate conversion
coordinates(utm9.cbi) <- c("UTM_E", "UTM_N") # fyi this removes UTM_E and UTM_N as attributes
coordinates(utm10.cbi) <- c("UTM_E", "UTM_N")

proj4string(utm9.cbi) <- CRS("+init=epsg:26909") # for UTM zone 9
proj4string(utm10.cbi) <- CRS("+init=epsg:26910") # for UTM zone 10


# Now datasets have coordinates and CRS, next convert to Longitude and Latitude
longlat9.cbi <- spTransform(utm9.cbi, CRS("+init=epsg:3005"))
longlat10.cbi <- spTransform(utm10.cbi, CRS("+init=epsg:3005"))

# Now merge datasets and add UTM E and UTM N back on as attributes
longlat.cbi <- raster::union(longlat9.cbi, longlat10.cbi)
str(longlat.cbi) # make sure there are 123 observations

CBI_2021_utms <- CBI_2021[,.(Plot_ID, UTM_E, UTM_N)]
longlat.cbi <- merge(longlat.cbi, CBI_2021_utms, by = "Plot_ID")
str(longlat.cbi) # make sure there are 48 observations i.e UTMS were added


# Check if study boundary and CBI plots intersect
plot(longlat.cbi, col = "red")
plot(study_boundary, add = TRUE)
### THERE ARE SOME PLOTS OUTSIDE OF FIRE BOUNDARIES!! ##### create column to note this



#-------------------3. Create column for whether CBI plot is within study boundary ------------#
# Convert spdf to sf 
plots_sf <- st_as_sf(longlat.cbi)
# Which plots intersect boundary
inside_study <- st_intersects(plots_sf, study_boundary, sparse = FALSE)
# Join with plot data
plots <- cbind(plots_sf, inside_study)
# Convert sf to spdf
plots_spdf<- as_Spatial(plots)



#-------------------- 4. Intersect 2021 CBI, dNBR, VRI, and RESULTS ---------------#
# Have to merge one at a time (can't find a way to merge all at the same time)
# Extract polygon info for each point
merged_2021CBI_VRI <- point.in.poly(plots_spdf, VRI_sel)
merged_2021CBI_VRI_RESULTS <- point.in.poly(merged_2021CBI_VRI, Results_sel)

# Extract raster values by points
rasValue_mean <- raster::extract(dNBR, merged_2021CBI_VRI_RESULTS, buffer = 30, fun = mean)
rasValue_median <- raster::extract(dNBR, merged_2021CBI_VRI_RESULTS, buffer = 30, fun = median)
rasValue_min <- raster::extract(dNBR, merged_2021CBI_VRI_RESULTS, buffer = 30, fun = min)
rasValue_max <- raster::extract(dNBR, merged_2021CBI_VRI_RESULTS, buffer = 30, fun = max)

# Combine raster values with point
merged_2021.1 <- cbind(merged_2021CBI_VRI_RESULTS, rasValue_mean) # mean
merged_2021.2 <- cbind(merged_2021.1, rasValue_median) # median
merged_2021.3 <- cbind(merged_2021.2, rasValue_min) # min
merged_2021.4 <- cbind(merged_2021.3, rasValue_max) # max

# Combine all means, median, min, max into single columns & rename
# Some columns have 2 dNBR's (overlaping rasters) - so drop dNBR that is not the correct fire ID
merged_2021.4 <- as.data.table(merged_2021.4)
# Means
merged_2021.4[Fire_ID != "C10784", dNBR_C10784:=paste0(dNBR_C10784, NA)]
merged_2021.4[Fire_ID != "R11498", dNBR_R11498:=paste0(dNBR_R11498, NA)]
merged_2021.4[Fire_ID != "R11796", dNBR_R11796:=paste0(dNBR_R11796, NA)]
merged_2021.4[Fire_ID != "R11921", dNBR_R11921:=paste0(dNBR_R11921, NA)]
merged_2021.4[Fire_ID != "R21721", dNBR_R21721:=paste0(dNBR_R21721, NA)]

merged_2021.4 <- merged_2021.4 %>% 
  unite(dNBR_mean, c("dNBR_C10784", "dNBR_R11498", "dNBR_R11796", "dNBR_R11921", "dNBR_R21721"), na.rm = TRUE)

# Medians
merged_2021.4[Fire_ID != "C10784", dNBR_C10784.1:=paste0(dNBR_C10784.1, NA)]
merged_2021.4[Fire_ID != "R11498", dNBR_R11498.1:=paste0(dNBR_R11498.1, NA)]
merged_2021.4[Fire_ID != "R11796", dNBR_R11796.1:=paste0(dNBR_R11796.1, NA)]
merged_2021.4[Fire_ID != "R11921", dNBR_R11921.1:=paste0(dNBR_R11921.1, NA)]
merged_2021.4[Fire_ID != "R21721", dNBR_R21721.1:=paste0(dNBR_R21721.1, NA)]

merged_2021.4 <- merged_2021.4 %>% 
  unite(dNBR_median, c("dNBR_C10784.1", "dNBR_R11498.1", "dNBR_R11796.1", "dNBR_R11921.1", "dNBR_R21721.1"), na.rm = TRUE)

# Mins
merged_2021.4[Fire_ID != "C10784", dNBR_C10784.2:=paste0(dNBR_C10784.2, NA)]
merged_2021.4[Fire_ID != "R11498", dNBR_R11498.2:=paste0(dNBR_R11498.2, NA)]
merged_2021.4[Fire_ID != "R11796", dNBR_R11796.2:=paste0(dNBR_R11796.2, NA)]
merged_2021.4[Fire_ID != "R11921", dNBR_R11921.2:=paste0(dNBR_R11921.2, NA)]
merged_2021.4[Fire_ID != "R21721", dNBR_R21721.2:=paste0(dNBR_R21721.2, NA)]

merged_2021.4 <- merged_2021.4 %>% 
  unite(dNBR_min, c("dNBR_C10784.2", "dNBR_R11498.2", "dNBR_R11796.2", "dNBR_R11921.2", "dNBR_R21721.2"), na.rm = TRUE)

# Maxs
merged_2021.4[Fire_ID != "C10784", dNBR_C10784.3:=paste0(dNBR_C10784.3, NA)]
merged_2021.4[Fire_ID != "R11498", dNBR_R11498.3:=paste0(dNBR_R11498.3, NA)]
merged_2021.4[Fire_ID != "R11796", dNBR_R11796.3:=paste0(dNBR_R11796.3, NA)]
merged_2021.4[Fire_ID != "R11921", dNBR_R11921.3:=paste0(dNBR_R11921.3, NA)]
merged_2021.4[Fire_ID != "R21721", dNBR_R21721.3:=paste0(dNBR_R21721.3, NA)]

merged_2021.4 <- merged_2021.4 %>% 
  unite(dNBR_max, c("dNBR_C10784.3", "dNBR_R11498.3", "dNBR_R11796.3", "dNBR_R11921.3", "dNBR_R21721.3"), na.rm = TRUE)


# Export table to check it out
write.csv(merged_2021.4, file="./Outputs/CBI/2021_CBI_dNBR_VRI_RESULTS.csv")



#----------------- 5. Convert UTM in 2020 CBI to lat long ----------------------#
# Data has 2 UTM zones (9 and 10) so split into two datasets then merge back together?
utm9.cbi2020 <- CBI_2020[UTM_Zone == 9,]
utm10.cbi2020 <- CBI_2020[UTM_Zone == 10,]

# Create spatial object for coordinate conversion
coordinates(utm9.cbi2020) <- c("UTM_E", "UTM_N") # fyi this removes UTM_E and UTM_N as attributes
coordinates(utm10.cbi2020) <- c("UTM_E", "UTM_N")

proj4string(utm9.cbi2020) <- CRS("+init=epsg:26909") # for UTM zone 9
proj4string(utm10.cbi2020) <- CRS("+init=epsg:26910") # for UTM zone 10


# Now datasets have coordinates and CRS, next convert to Longitude and Latitude
longlat9.cbi2020 <- spTransform(utm9.cbi2020, CRS("+init=epsg:3005"))
longlat10.cbi2020 <- spTransform(utm10.cbi2020, CRS("+init=epsg:3005"))

# Now merge datasets and add UTM E and UTM N back on as attributes
longlat.cbi2020 <- raster::union(longlat9.cbi2020, longlat10.cbi2020)
str(longlat.cbi2020) # make sure there are 214 observations

CBI_2020_utms <- CBI_2020[,.(Plot_ID, UTM_E, UTM_N)]
CBI_2020_utms <- unique(CBI_2020_utms, by = "Plot_ID") # there are duplicate plot_IDs (need to get rid of for below code to work)
longlat.cbi2020 <- merge(longlat.cbi2020, CBI_2020_utms, by = "Plot_ID")
str(longlat.cbi2020) # make sure there are 214 observations and 141 variables



#------------------- 6. Intersect 2020 CBI (already has VRI and dNBR) with RESULTS --------------#
merged_2020CBI_VRI <- point.in.poly(longlat.cbi2020, VRI_sel)
merged_2020CBI_VRI_RESULTS <- point.in.poly(merged_2020CBI_VRI, Results_sel)

# Export table to check it out
as.data.frame(merged_2020CBI_VRI_RESULTS)
write.csv(merged_2020CBI_VRI_RESULTS, file="./Outputs/CBI/2020_CBI_dNBR_VRI_RESULTS.csv")



#---------------- 7. Combine 2021 and 2020 datasets ----------------------#
# Convert spdf to data tables
dt_2020 <- as.data.table(merged_2020CBI_VRI_RESULTS)
str(dt_2020)
dt_2020[, dNBR_mean := as.character(dNBR_mean)]
dt_2020[, dNBR_median := as.character(dNBR_median)]
dt_2020[, dNBR_min := as.character(dNBR_min)]
dt_2020[, dNBR_max := as.character(dNBR_max)]

dt_2021 <- as.data.table(merged_2021.4)
str(dt_2021)
dt_2021[, Slope_. :=as.character(Slope_.)] # Slope_. is a character in 2020 data

# Merge tables
CBI_merged <- full_join(x = dt_2020, y = dt_2021)

# Export
write.csv(CBI_merged, file = "./Outputs/CBI/2020_2021_CBI_merged.csv")
