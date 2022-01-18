# Title: Four fires analysis
# Author: Ingrid Farnell
# Date: 12-15-2021

# This script analyses fires from 2018 using Random Forest.
# We are analysing which variables (site prep, topography, and fire weather) are imporatant for fire severity (dNBR) 
# in plantations.

# DATA FLOW
# 1. Assess predictor variables for multicollinearity. RF can handle correlated data, but we don't want too many 
# correlated variables because it could bias the variable importance. Many paper's drop one of the variables if 
# spearman >|0.8|

# 2. Moran's Index on dNBR to choose distance between sample points. Run on one fire at varying raster resolutions to pick a sample distance
# 3. Create sample points based on other papers and our Moran's index.
# 4. Intersect predictor and response variables with sample points to create dataframes for RF
# 5. Run initial RF

#--------------- Load libraries----------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster")) # geo comp.
ls <- append(ls, c("randomForest", "SpatialML")) # analysis

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#--------- Load data -----------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"

# Set the fires of interest - all 2018 fires with openings
study_fireTable <- fread("./Inputs/StudyFireList.csv") # all potential fires
FiresOfInterest <- c( "R11796","R11498","R21721","R11921", "G41607", "G51632", "C11937")

# Fire weather data - to check for multicollinearity
all_fireweather <- fread("./Inputs/Fireweather/FireWeather.csv")
Fireweather <- all_fireweather[Fire_ID %in% FiresOfInterest] # select only fires of interest
unique(Fireweather[,Fire_ID]) # make sure it worked

# Rasters
# dNBR - response variable
#dNBR_R11796 <- raster(paste0(SpatialFilesPath, "./Inputs/dNBR/dNBR_R11796.tif")
dNBR_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/dNBR/dNBR_R21721.tif"))

# Predictor variables
# DOB
#DOB_R11796 <- raster(paste0(SpatialFilesPath, "./Inputs/DOB/dob_R11796.tif")
DOB_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/DOB/dob_R21721.tif"))

# Plantation
# R21721
plantation_list_R21721 <- list.files(paste0(SpatialFilesPath,"./Inputs/PlantationPreds/R21721/"),
                        pattern = "*.tif", 
                        recursive = FALSE, 
                        full.names=TRUE)

print(paste("there are", length(plantation_list_R21721), "covariates in the list"))
plantation_list_R21721 <- plantation_list_R21721[!grepl("xml", plantation_list_R21721)]
plantation_R21721 <- stack(plantation_list_R21721)

# Fire weather
# R21721
weather_list_R21721 <- list.files(paste0(SpatialFilesPath,"./Inputs/FireWeather/R21721/"),
                                  pattern = "*.tif", 
                                  recursive = FALSE, 
                                  full.names=TRUE)
print(paste("there are", length(weather_list_R21721), "covariates in the list"))
weather_list_R21721 <- weather_list_R21721[!grepl("xml", weather_list_R21721)]
weather_R21721 <- stack(weather_list_R21721)

# Topography
topo_list <- list.files(paste0(SpatialFilesPath,"./Inputs/Topography/"),
                                  pattern = "*.tif", 
                                  recursive = FALSE, 
                                  full.names=TRUE)
print(paste("there are", length(topo_list), "covariates in the list"))
topo_list <- topo_list[!grepl("xml", topo_list)]
topo <- stack(topo_list)

# Base rasters -- to resample rasters
base_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/BaseRasters/BaseRaster_R21721.tif"))


# ------------- Resample rasters to have same extent and resolution-----------------#
# Using base rasters resample response and predictor variables to same extent and resolution

# dNBR
dNBR_R21721 <- resample(dNBR_R21721, base_R21721, method = "ngb")

# DOB
DOB_R21721 <- resample(DOB_R21721, base_R21721, method = "ngb")

# Plantation stack
plantation_R21721 <- resample(plantation_R21721, base_R21721, method = "ngb")

# Fire weather
weather_R21721 <- resample(weather_R21721, base_R21721, method = "ngb")

# Topography
topo_R21721 <- resample(topo, base_R21721, method = "ngb") ## getting Error .. 



#----------1. Multicollinearity --------------------#
# Create a correlation matrix to see which variables in Fireweather are correlated
# Extract only weather columns to use for creating matrix
Fireweather_var <- Fireweather[ ,.(bui, dc, dmc, dsr, ffmc, fwi, humidity, isi, precipitation, sdmc, temperature, wind)]

# Correlation matrix
round(cor(Fireweather_var, use = "complete.obs", method = "spearman"), 2)

# Kira and Phil suggest keeping: FWI, BUI, ISI, DMC, DC. FWI & ISI and FWI & DMC are correlated. Keep for now in initial
# RF run and see what happens. If those are top variables pick one from each pair to keep and rerun model. 


#---------- 2. Moran's Index ----------------#
# Do Moran's on a single dNBR, initially at a 30 m distance, then increase distance to 200, 400, etc up to 12000 - similar to 
# other papers. Run RF with chosen sample distance then do Moran's on residual. 


# Calculate Moran's I
# Create weighting matrix
queen <- matrix(c(1, 1, 1, 1, 0, 1, 1, 1, 1), 3,3)

# 30 m dist - Moran's I with 3x3 queen filter
m.3.q_dNBR_R11796 <- raster::Moran(dNBR_R11796, w = queen)
m.3.q_dNBR_R11796 # 0.9393669 = high clustering of similar values

# 200 m dist 
dNBR_R11796_210m <- aggregate(dNBR_R11796, fact = 7, fun = mean)
m.3.q_dNBR_R11796_210 <- Moran(dNBR_R11796_210m, w = queen)
m.3.q_dNBR_R11796_210 # 0.8467199

# 400 m dist
dNBR_R11796_420m <- aggregate(dNBR_R11796, fact = 14, fun = mean)
m.3.q_dNBR_R11796_420 <- Moran(dNBR_R11796_420m, w = queen)
m.3.q_dNBR_R11796_420 # 0.8054098

# 800 m dist
dNBR_R11796_810m <- aggregate(dNBR_R11796, fact = 27, fun = mean)
m.3.q_dNBR_R11796_800 <- Moran(dNBR_R11796_800m, w = queen)
m.3.q_dNBR_R11796_800 # 0.7647504

# 1000 m dist
dNBR_R11796_1000m <- aggregate(dNBR_R11796, fact = 34, fun = mean)
m.3.q_dNBR_R11796_1000 <- Moran(dNBR_R11796_1000m, w = queen)
m.3.q_dNBR_R11796_1000 # 0.7419534

# 1200 m dist
dNBR_R11796_1200m <- aggregate(dNBR_R11796, fact = 40, fun = mean)
m.3.q_dNBR_R11796_1200 <- Moran(dNBR_R11796_1200m, w = queen)
m.3.q_dNBR_R11796_1200 # 0.7218777

# 2000 m dist (just to see)
dNBR_R11796_2000m <- aggregate(dNBR_R11796, fact = 67, fun = mean)
m.3.q_dNBR_R11796_2000 <- Moran(dNBR_R11796_2000m, w = queen)
m.3.q_dNBR_R11796_2000 # 0.66

# We are going to choose 800 m. 
dNBR_R21721_810m <- aggregate(dNBR_R21721, fact = 27, fun = mean)


#-----------------3. Create sample points ------------------#
# Choosing 800 m dist sample points
# R11796
# Use 800 m resolution raster to convert to point data
R11796_sample_pnts_810 <- rasterToPoints(dNBR_R11796_810m, spatial = TRUE)

# R21721- 810 m dist
R21721_sample_pnts_810 <- rasterToPoints(dNBR_R21721_810m, spatial = TRUE)



#--------------- 4. Intersect sample points with response and predictor variables --------------#
# Create raster stack of predictor variables
preds_stack_R21721 <- addLayer(plantation_R21721, DOB_R21721, weather_R21721)


# Extract predictor raster values at sample points
preds_value_R21721 <- raster::extract(preds_stack_R21721, R21721_sample_pnts_810)
# Extract response dNBR raster value
resp_dNBR_R21721 <- raster::extract(dNBR_R21721, R21721_sample_pnts_810)
# Combine data
R21721 <- cbind(resp_dNBR_R21721, preds_value_R21721)
# Convert to data frame
R21721_df <- as.data.frame(R21721)

# Check if has an opening_id dataset has planation columns with data
check <- R21721_df %>%
  filter(!is.na(R21721_OpenID) & is.na(R21721_BroadBurn))

check.1 <- R21721_df %>%
  filter(!is.na(R21721_OpenID) & is.na(dob_R21721))

# Drop rows that are NA because we only want to include plantation openings
R21721_df <- R21721_df[complete.cases(R21721_df), ]



#---------------------5. Run initial RF -----------------------------#
# R21721
# Data should only contain response (first row) and predictors
names(R21721_df)

rfmod_R21721 <- randomForest(resp_dNBR_R21721 ~ ., data = R21721_df, 
                             ntree = 500, 
                             importance = TRUE, 
                             na.action = na.omit)
rfmod_R21721

# Find best mtry value
mtry <- tuneRF(R21721_df[,-1], R21721_df[,1], ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# Rerun model with best mtry
rfmod_R21721 <- randomForest(resp_dNBR_R21721 ~ .,data = R21721_df, 
                             ntree = 500, 
                             mtry = 5,
                             importance = TRUE, 
                             na.action = na.omit)
rfmod_R21721

# Variable importance
varImpPlot(rfmod_R21721)

# Calculate R2
actual <- R21721_df[,1]
predicted <- rfmod_R21721$predicted
R2 <- 1- (sum((actual - predicted)^2)/sum((actual - mean(actual))^2))
R2 # 0.32

#### Try out Spatial ML ###
Coords_R21721 <- R21721_dt[ ,c("x", "y")]

grf_R21721 <- grf(dNBR_R21721 ~ R21721_BroadBurn+R21721_Brushed+R21721_DebrisMade+R21721_DebrisPiled+R21721_Fertil+R21721_MechUnk+R21721_None+R21721_OpenID+R21721_PileBurn+R21721_PlantAge+R21721_Prune+R21721_SitePrepped+R21721_Soil+R21721_Spaced+R21721_SpotBurn+R21721_WBurn+dob_R21721, 
                  dframe = R21721_df, 
                  bw = 800, 
                  ntree = 500, 
                  kernel = "fixed", 
                  coords = Coords_R21721, 
                  importance = TRUE)
