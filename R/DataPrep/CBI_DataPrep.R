# CBI data prep
# Ingrid Farnell
# Dec 22, 2021


# Intersect 2021 CBI, dNBR, VRI, and RESULTS
# Dataflow:
# 1. Load data
# 2. convert CBI UTMS to lat long and leave as spatial points dataframe
# 3. Intersect CBI dNBR, VRI and RESULTS to create one dataset.

# Intersect 2020 CBI (already has dNBR and VRI) with RESULTS.
# Dataflow:

# 4. convert 2020 CBI UTM to lat long and leave as spatial data frame
# 5. Intersect 2020 CBI with RESULTS

# 6. Combine 2020 and 2021 CBI datasets


#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("sp", "GSIF", "stars", "sf")) # geo comp.
ls <- append(ls, c("rgdal", "sf", "raster", # more geo comp.
                   "rasterVis", "tmap", "RColorBrewer", "spatialEco"))   

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#--------------- 1. Load data -----------------------#
# Load CBI 2021
CBI_2021 <- fread("./Inputs/CBI/2021_CBI_plots_5_fires_cleaned_17Dec2021_CondensedData.csv")
str(CBI_2021)


# Load CBI 2020
CBI_2020 <- fread("./Inputs/CBI/cleaned_2020_CBI_plots_with_dNBR_and_VRI_simplified.csv")
str(CBI_2020)
# Rename plot_ID to match 2021
setnames(CBI_2020, "plot_ID", "Plot_ID")


# Load dNBR rasters in raster stack
dNBR_list <- list.files("E:/Ingrid/Borealis/BVRC_21-01_CFS/Rasters/dNBR/",
                        pattern = "*.tif", 
                        recursive = FALSE, 
                        full.names=TRUE)

print(paste("there are", length(dNBR_list), "covariates in the list")) # make sure there are 20

dNBR_list <- dNBR_list[!grepl("xml",dNBR_list)]
dNBR_list

# Create raster stack
dNBR <- stack(dNBR_list)
dim(dNBR)


# Load VRI (2016)
VRI_study <- read_sf("E:/Ingrid/Borealis/BVRC_21-01_CFS/VRI/VRI2016_StudyFires/VRI2016_StudyFires.shp")
VRI_study_sel <- VRI_study %>%
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

proj4string(utm9.cbi) # at this point dataset doesn't have a CRS

proj4string(utm9.cbi) <- CRS("+init=epsg:26909") # for UTM zone 9
proj4string(utm10.cbi) <- CRS("+init=epsg:26910") # for UTM zone 10


# Now datasets have coordinates and CRS, next convert to Longitude and Latitude
longlat9.cbi <- spTransform(utm9.cbi, CRS("+init=epsg:3005"))
str(longlat9.cbi)
longlat10.cbi <- spTransform(utm10.cbi, CRS("+init=epsg:3005"))
str(longlat10.cbi)

# Now merge datasets and add UTM E and UTM N back on as attributes
longlat.cbi <- union(longlat9.cbi, longlat10.cbi)
str(longlat.cbi) # make sure there are 123 observations

CBI_2021_utms <- CBI_2021[,.(Plot_ID, UTM_E, UTM_N)]
longlat.cbi <- merge(longlat.cbi, CBI_2021_utms, by = "Plot_ID")
str(longlat.cbi) # make sure there are 48 observations i.e UTMS were added

# Make sure VRI and CBI plots intersect
plot(longlat.cbi, col = "red")
plot(VRI_study_sel, add = TRUE)
### THERE ARE 2 PLOTS OUTSIDE OF FIRE BOUNDARIES!! ##### continue on but flag to Phil


#-------------------- 3. Intersect 2021 CBI, dNBR, VRI, and RESULTS ---------------#
# Have to merge one at a time (can't find a way to merge all at the same time)
# Extract polygon info for each point
merged_2021CBI_VRI <- point.in.poly(longlat.cbi, VRI_study_sel)
merged_2021CBI_VRI_RESULTS <- point.in.poly(merged_2021CBI_VRI, Results_sel)

# Extract raster values by points
rasValue <- raster::extract(dNBR, merged_2021CBI_VRI_RESULTS)
# Combine raster values with point
all_merged_2021 <- cbind(merged_2021CBI_VRI_RESULTS, rasValue)

# Export table to check it out
write.csv(all_merged_2021, file="./Outputs/CBI/2021_CBI_dNBR_VRI_RESULTS.csv")


#----------------- 4. Convert UTM in 2020 CBI to lat long ----------------------#
# Data has 2 UTM zones (9 and 10) so split into two datasets then merge back together?
utm9.cbi2020 <- CBI_2020[UTM_zone == 9,]
utm10.cbi2020 <- CBI_2020[UTM_zone == 10,]

# Create spatial object for coordinate conversion
coordinates(utm9.cbi2020) <- c("UTM_E", "UTM_N") # fyi this removes UTM_E and UTM_N as attributes
coordinates(utm10.cbi2020) <- c("UTM_E", "UTM_N")

proj4string(utm9.cbi2020) # at this point dataset doesn't have a CRS

proj4string(utm9.cbi2020) <- CRS("+init=epsg:26909") # for UTM zone 9
proj4string(utm10.cbi2020) <- CRS("+init=epsg:26910") # for UTM zone 10


# Now datasets have coordinates and CRS, next convert to Longitude and Latitude
longlat9.cbi2020 <- spTransform(utm9.cbi2020, CRS("+init=epsg:3005"))
str(longlat9.cbi2020)
longlat10.cbi2020 <- spTransform(utm10.cbi2020, CRS("+init=epsg:3005"))
str(longlat10.cbi2020)

# Now merge datasets and add UTM E and UTM N back on as attributes
longlat.cbi2020 <- union(longlat9.cbi2020, longlat10.cbi2020)
str(longlat.cbi2020) # make sure there are 214 observations

CBI_2020_utms <- CBI_2020[,.(Plot_ID, UTM_E, UTM_N)]
CBI_2020_utms <- unique(CBI_2020_utms, by = "Plot_ID") # there are duplicate plot_IDs (need to get rid of for below code to work)
longlat.cbi2020 <- merge(longlat.cbi2020, CBI_2020_utms, by = "Plot_ID")
str(longlat.cbi2020) # make sure there are 214 observations and 141 variables

#------------------- 5. Intersect 2020 CBI (already has VRI and dNBR) with RESULTS --------------#
all_merged_2020 <- point.in.poly(longlat.cbi2020, Results_sel)
# Export table to check it out
write.csv(all_merged_2020, file="./Outputs/CBI/2020_CBI_dNBR_VRI_RESULTS.csv")


#---------------- 6. Combine 2021 and 2020 datasets ----------------------#
