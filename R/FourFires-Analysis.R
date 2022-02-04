# Title: Four fires analysis
# Author: Ingrid Farnell
# Date: 12-15-2021

# This script analyses fires from 2017 & 2018 using Random Forest.
# We are analysing which variables (site prep, topography, and fire weather) are important for fire severity (dNBR) 
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
ls <- append(ls, c("terra")) # geo comp.
ls <- append(ls, c("randomForest", "pdp")) # analysis

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#--------- Load data -----------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"

# Set the fires of interest - all 2018 fires with openings
FiresOfInterest <- c( "G41607", "G51632", "R11498", "R11796","R11921","R21721")

# Rasters
# Base raster -- to resample rasters
base_list <- list.files(paste0(SpatialFilesPath,"./Inputs/BaseRasters/"),
                        pattern = paste(FiresOfInterest, sep = "", collapse = "|"),
                        recursive = FALSE,
                        full.names = TRUE)

base <- sapply(base_list, rast)

# Create names for base rasters
baseSplit <- str_split_fixed(base_list, "_", 2)
base.name <- str_split(baseSplit[,2], ".tif", simplify = TRUE)[,1]

# Give base rasters a name
names(base) <- base.name
names(base)

variable_list <- list.files(paste0(SpatialFilesPath, "./Inputs/Rasters/"),
                            pattern =  paste(FiresOfInterest, sep = "", collapse = "|"),
                            recursive = TRUE,
                            full.names = TRUE)


variables <- sapply(variable_list, rast)

# Create names for the variables
variableSplit <- str_split_fixed(variable_list, "/", 9)
variable.name <- str_split(variableSplit[,9], ".tif", simplify = TRUE)[,1]

# Give each raster variable a name
names(variables) <- variable.name
names(variables)


ctg_variables <- c("BEC", "BroadBurn", "Brushed", "DebrisMade", "DebrisPiled", "Fertil", "MechUnk", "None", 
                   "OpenID", "OPENING_ID", "PileBurn", "PlantAge", "Prune", "SitePrepped", "Soil", "Spaced", 
                   "SpotBurn", "WBurn")



# ------------- Resample rasters to have same extent and resolution-----------------#
# Using base rasters resample response and predictor variables to same extent and resolution


# Using base rasters resample response and predictor variables to same extent and resolution


# Resample categorical and continuous variables differently
for (i in 1:length(variables)) {
  for (j in 1:length(base)){

  variables_RS <- ifelse (sapply(as.list(variables), function(x) {grepl(paste(ctg_variables, sep = "", collapse = "|"), x)}),
                        
                          # I want to use base [fire] to resample variable [fire].... it doesnt work
                        resample(variables[i[grepl(paste(FiresOfInterest[j], collapse = "|"), variables[i])]] == base[j], base[j], method = "ngb"),
                        
                        resample(variables[i[grepl(paste(FiresOfInterest[j], collapse = "|"), variables[i])]] == base[j], base[j], method = "bilinear")
)

  }
}

  
  
# dNBR
dNBR <- resample(dNBR, base, method = "bilinear")
# DOB
DOB_R21721 <- resample(DOB_R21721, base_R21721, method = "bilinear")
# Plantation stack
plantation_R21721 <- resample(plantation_R21721, base_R21721, method = "ngb") # ngb for categorical
# Fire weather
weather_R21721 <- resample(weather_R21721, base_R21721, method = "bilinear")
# Topography
topo_R21721 <- resample(topo, base_R21721, method = "bilinear") 
# Fire runs
run_R21721 <- resample(run_R21721, base_R21721, method = "bilinear")
# VRI
VRI_R21721 <- resample(VRI_R21721, base_R21721, method = "bilinear")
# Historic fires were made with base raster (so should stack without resampling)



# #----------1. Multicollinearity --------------------#
# # Create a correlation matrix to see which variables in Fireweather are correlated
# # Extract only weather columns to use for creating matrix
# Fireweather_var <- Fireweather[ ,.(bui, dc, dmc, dsr, ffmc, fwi, humidity, isi, precipitation, sdmc, temperature, wind)]
# 
# # Correlation matrix
# round(cor(Fireweather_var, use = "complete.obs", method = "spearman"), 2)
# 
# # Kira and Phil suggest keeping: FWI, BUI, ISI, DMC, DC. FWI & ISI and FWI & DMC are correlated. Keep for now in initial
# # RF run and see what happens. If those are top variables pick one from each pair to keep and rerun model. 


#---------- 2. Moran's Index ----------------#
# Do Moran's on a single dNBR, initially at a 30 m distance, then increase distance to 200, 400, etc up to 12000 - similar to 
# other papers. Run RF with chosen sample distance then do Moran's on residual. 

# Calculate Moran's I
# Create weighting matrix
queen <- matrix(c(1, 1, 1, 1, 0, 1, 1, 1, 1), 3,3)

# 30 m dist - Moran's I with 3x3 queen filter
m.3.q_dNBR_R21721 <- raster::Moran(dNBR_R21721, w = queen)
m.3.q_dNBR_R21721 # 0.95 = high clustering of similar values

# 200 m dist 
dNBR_R21721_210m <- aggregate(dNBR_R21721, fact = 7, fun = mean)
m.3.q_dNBR_R21721_210 <- Moran(dNBR_R21721_210m, w = queen)
m.3.q_dNBR_R21721_210 # 0.87

# 400 m dist
dNBR_R21721_420m <- aggregate(dNBR_R21721, fact = 14, fun = mean)
m.3.q_dNBR_R21721_420 <- Moran(dNBR_R21721_420m, w = queen)
m.3.q_dNBR_R21721_420 # 0.83

# 800 m dist
dNBR_R21721_810m <- aggregate(dNBR_R21721, fact = 27, fun = mean)
m.3.q_dNBR_R21721_810 <- Moran(dNBR_R21721_810m, w = queen)
m.3.q_dNBR_R21721_810 # 0.80

# 1000 m dist
dNBR_R21721_1020m <- aggregate(dNBR_R21721, fact = 34, fun = mean)
m.3.q_dNBR_R21721_1020 <- Moran(dNBR_R21721_1020m, w = queen)
m.3.q_dNBR_R21721_1020 # 0.79

# 1200 m dist
dNBR_R21721_1200m <- aggregate(dNBR_R21721, fact = 40, fun = mean)
m.3.q_dNBR_R21721_1200 <- Moran(dNBR_R21721_1200m, w = queen)
m.3.q_dNBR_R21721_1200 # 0.77

# 2000 m dist (just to see)
dNBR_R21721_2010m <- aggregate(dNBR_R21721, fact = 67, fun = mean)
m.3.q_dNBR_R21721_2010 <- Moran(dNBR_R21721_2010m, w = queen)
m.3.q_dNBR_R21721_2010 # 0.72

# We are going to choose 810 m and 270 - then check spatial autocorrelation on residuals


#-----------------3. Create sample points ------------------#
# 810 m dist sample points
dNBR_R21721_810m <- aggregate(dNBR_R21721, fact = 27, fun = mean)
R21721_smpl_810 <- rasterToPoints(dNBR_R21721_810m, spatial = TRUE)


#--------------- 4. Intersect sample points with response and predictor variables --------------#
# Create raster stack of response and predictor variables
stack_R21721 <- addLayer(dNBR_R21721, plantation_R21721, DOB_R21721, weather_R21721, hist_R21721, 
                         run_R21721, topo_R21721, VRI_R21721)

for (i in length(base_list)){
  stack <- c(dNBR, DOB, run, hist, topo, VRI, weather)
}

# Extract response and predictor raster values at sample points
R21721_smpl_810_ras <- raster::extract(stack_R21721, R21721_smpl_810)

# Convert to data frame
R21721_810 <- as.data.frame(R21721_smpl_810_ras)

# Drop rows that don't have an opening ID because we only want to include plantation openings
R21721_810 <- R21721_810 %>%
  filter(!is.na(R21721_OpenID))
# Drop opening ID column
R21721_810 <- subset(R21721_810, select =-c(R21721_OpenID, R21721_OPENING_ID, R21721_SitePrepped, R21721_None))


#---------------------5. Run initial RF -----------------------------#
# R21721
# Data should only contain response (first row) and predictors
names(R21721_810)

rfmod_R21721_810 <- randomForest(dNBR_R21721 ~ ., data = R21721_810, 
                             ntree = 500, 
                             importance = TRUE, 
                             na.action = na.omit)
rfmod_R21721_810

# Find best mtry value
mtry <- tuneRF(R21721_810[,-1], R21721_810[,1], ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# Rerun model with best mtry
rfmod_R21721_810 <- randomForest(dNBR_R21721 ~ .,data = R21721_810,
                                 ntree = 500,
                                 mtry = 8,
                                 importance = TRUE,
                                 na.action = na.omit)
rfmod_R21721_810

# Variable importance
varImpPlot(rfmod_R21721_810, n.var = 10)

# Partial dependence plots
rfmod_R21721_810 %>%
  partial(pred.var = "R21721_PlantAge") %>%
  autoplot(train = R21721_810)

rfmod_R21721_810 %>%
  partial(pred.var = "R21721_PineCov") %>%
  autoplot(train = R21721_810)

rfmod_R21721_810 %>%
  partial(pred.var = "R21721_SitePrepped") %>%
  autoplot(train = R21721_810)

rfmod_R21721_810 %>%
  partial(pred.var = "R21721_SpruceCov") %>%
  autoplot(train = R21721_810)

rfmod_R21721_810 %>%
  partial(pred.var = "R21721_None") %>%
  autoplot(train = R21721_810)

rfmod_R21721_810 %>%
  partial(pred.var = "R21721_CROWN_CLOS") %>%
  autoplot(train = R21721_810)

rfmod_R21721_810 %>%
  partial(pred.var = "DEMtpi") %>%
  autoplot(train = R21721_810)

rfmod_R21721_810 %>%
  partial(pred.var = "FireRun_R21721") %>%
  autoplot(train = R21721_810)

rfmod_R21721_810 %>%
  partial(pred.var = "R21721_FirCov") %>%
  autoplot(train = R21721_810)

rfmod_R21721_810 %>%
  partial(pred.var = "dob_R21721") %>%
  autoplot(train = R21721_810)

# Calculate R2
actual <- R21721_810[,1]
predicted <- rfmod_R21721_810$predicted
R2 <- 1- (sum((actual - predicted)^2)/sum((actual - mean(actual))^2))
R2 # 0.27


#------------------------------------------------------------------------------#
#-------------------- 270 m sample points--------------------------------------#

#-----------------3. Create sample points ------------------#
# R21721- 270 m dist
dNBR_R21721_270m <- aggregate(dNBR_R21721, fact = 9, fun = mean)
R21721_smpl_270 <- rasterToPoints(dNBR_R21721_270m, spatial = TRUE)


#--------------- 4. Intersect sample points with response and predictor variables --------------#
# Extract response and predictor raster values at sample points
R21721_smpl_270_ras <- raster::extract(stack_R21721, R21721_smpl_270)

# Convert to data frame
R21721_270 <- as.data.frame(R21721_smpl_270_ras)

# Drop rows that don't have an opening ID because we only want to include plantation openings
R21721_270 <- R21721_270 %>%
  filter(!is.na(R21721_OpenID))
# Drop opening ID column
R21721_270 <- subset(R21721_270, select =-c(R21721_OpenID, R21721_OPENING_ID, R21721_SitePrepped, R21721_None))


#---------------------5. Run initial RF -----------------------------#
# R21721
# Data should only contain response (first row) and predictors
names(R21721_270)

rfmod_R21721_270 <- randomForest(dNBR_R21721 ~ ., data = R21721_270,
                                 ntree = 500,
                                 importance = TRUE,
                                 na.action = na.omit)
rfmod_R21721_270

# Find best mtry 
mtry <- tuneRF(R21721_270[,-1], R21721_270[,1], ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE) # NAs in predictors.. but not in 810 samples??
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# # Rerun model with best mtry
# rfmod_R21721_270 <- randomForest(dNBR_R21721 ~ .,data = R21721_270, 
#                              ntree = 500,
#                              importance = TRUE, 
#                              na.action = na.omit)
# rfmod_R21721_270

# Variable importance
varImpPlot(rfmod_R21721_270, n.var = 10)

# Partial dependence plots
rfmod_R21721_270 %>%
  partial(pred.var = "R21721_PlantAge") %>%
  autoplot(train = R21721_270)

rfmod_R21721_270 %>%
  partial(pred.var = "R21721_PineCov") %>%
  autoplot(rug = TRUE, train = R21721_270)

rfmod_R21721_270 %>%
  partial(pred.var = "R21721_SitePrepped") %>%
  autoplot(rug = TRUE, train = R21721_270)

#---------------------------------- Moran's Index on residuals---------------------------#
# Residuals
actual_270 <- R21721_270[,1]
predicted_270 <- rfmod_R21721_270$predicted

plot(actual_270, actual_270 - predicted_270)

residuals_270 <- actual_270 - predicted_270
plot(residuals_270)

# Moran's Index
residuals_270_df <- as.data.frame(residuals_270)
m.3.q_R21721_270_resid <- Moran(residuals_270_df, w = queen)
