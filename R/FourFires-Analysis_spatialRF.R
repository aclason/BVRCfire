# Four fires analysis - spatialRF
# Ingrid Farnell
# Jan 26, 2022

# This script runs spatial random forest. 
# See https://blasbenito.github.io/spatialRF/ for detailed information


#------------------------------ Load libraries---------------------------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("raster")) # geo comp.
ls <- append(ls, c("pdp", "spatialRF")) # analysis

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#------------------------------ 1. Load data ----------------------------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"

# Rasters -- R21721 Nadina
# Response variable
# dNBR
dNBR_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/dNBR/dNBR_R21721.tif"))

# Predictor variables
# DOB
DOB_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/DOB/dob_R21721.tif"))

# Plantation
plantation_list_R21721 <- list.files(paste0(SpatialFilesPath,"./Inputs/PlantationPreds/"),
                                     pattern = "R21721", 
                                     recursive = FALSE, 
                                     full.names=TRUE)

print(paste("there are", length(plantation_list_R21721), "covariates in the list"))
plantation_list_R21721 <- plantation_list_R21721[!grepl("xml", plantation_list_R21721)]
plantation_R21721 <- stack(plantation_list_R21721)

# Fire weather
weather_list_R21721 <- list.files(paste0(SpatialFilesPath,"./Inputs/FireWeather/"),
                                  pattern = "*_R21721.tif", 
                                  recursive = FALSE, 
                                  full.names=TRUE)
print(paste("there are", length(weather_list_R21721), "covariates in the list"))
weather_list_R21721 <- weather_list_R21721[!grepl("xml", weather_list_R21721)]
weather_R21721 <- stack(weather_list_R21721)

# Topography (hli currently taken out)
topo_list <- list.files(paste0(SpatialFilesPath,"./Inputs/Topography/"),
                        pattern = "*.tif", 
                        recursive = FALSE, 
                        full.names=TRUE)
print(paste("there are", length(topo_list), "covariates in the list"))
topo_list <- topo_list[!grepl("xml", topo_list)]
topo <- stack(topo_list)

# Fire runs
run_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/FireRuns/FireRun_R21721.tif"))

# Historic fires
hist_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/HistoricFires/HistoricFires_R21721.tif"))

# VRI 
VRI_list_R21721 <- list.files(paste0(SpatialFilesPath,"./Inputs/VRIpreds/"),
                              pattern = "R21721",
                              recursive = FALSE, 
                              full.names=TRUE)
print(paste("there are", length(VRI_list_R21721), "covariates in the list"))
VRI_list_R21721 <- VRI_list_R21721[!grepl("xml", VRI_list_R21721)]
VRI_R21721 <- stack(VRI_list_R21721)

# BEC (Ingrid to make)

# Base raster -- to resample rasters
base_R21721 <- raster(paste0(SpatialFilesPath, "./Inputs/BaseRasters/BaseRaster_R21721.tif"))



# ------------------------------Prepare data-----------------------------------#
#--------------------------2a. Resample rasters for stacking-------------------#
# Note - can't have a raster with no data or else it won't resample.. had to remove DEMhli

# dNBR
dNBR_R21721 <- resample(dNBR_R21721, base_R21721, method = "bilinear")
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


#-----------------------------2b. Get sample points----------------------------#
# Create sample points - 270 m dist
R21721_270m <- aggregate(dNBR_R21721, fact = 9, fun = mean)
R21721_smpl_270 <- rasterToPoints(R21721_270m, spatial = TRUE)
colnames(R21721_smpl_270@data) <- "drop" # make sure to drop this later on(it's a place holder column for points)

# Create raster stack of response and predictor variables
stack_R21721 <- addLayer(dNBR_R21721, plantation_R21721, DOB_R21721, weather_R21721, hist_R21721, 
                         run_R21721, topo_R21721, VRI_R21721)

# Extract response and predictor values at sample points
R21721_smpl_270_ras <- raster::extract(stack_R21721, R21721_smpl_270, sp = TRUE)

# Convert to data frame
R21721_270 <- as.data.frame(R21721_smpl_270_ras)
# Drop rows that don't have an opening ID because we only want to include plantation openings
R21721_270 <- R21721_270 %>%
  filter(!is.na(R21721_OpenID))
# Drop opening ID columns, site prepped and none
R21721_270 <- subset(R21721_270, select =-c(drop, R21721_OpenID, R21721_OPENING_ID, R21721_SitePrepped, R21721_None))

# Create distance matrix
dist_R21721_270 <- pointDistance(R21721_270[, c("x", "y")], lonlat = FALSE)


#--------------------2c. Meet spatialRF data requirements----------------------#
# 1. Must be free of NA
sum(apply(R21721_270, 2, is.na))
R21721_270 <- na.omit(R21721_270) # remove NAs

# 2. Columns cannot have 0 variance
apply(R21721_270, 2, var) == 0
R21721_270 <- subset(R21721_270, select =-c(R21721_MechUnk)) # remove column with 0 variance

# 3. Columns must not yield NaN or Inf when scaled
sum(apply(scale(R21721_270), 2, is.nan)) 
sum(apply(scale(R21721_270), 2, is.infinite))
# Find which columns are giving issue
#sapply(as.data.frame(scale(R21721_270)), function(x)any(is.nan(x)))


#-----------------------------Run spatialRF------------------------------------#
#----------------------------3a. Set up data-----------------------------------#
# Names of response and predictor variables
dependent.variable.name <- "dNBR_R21721"
predictor.variable.names <- colnames(R21721_270)[2:35]

# Coordinates of the cases
xy <- R21721_270[, c("x", "y")]

# Distance matrix
distance.matrix <- dist_R21721_270

# Distance thresholds (same units as distance matrix)
distance.thresholds <- c(0, 270, 381, 540, 810, 1080, 1113, 1350)

# Random seed for reproducibility
random.seed <- 1


#----------------------3b. Assess spatial autocorrelation----------------------#
moran_preds <- spatialRF::plot_training_df_moran(
  data = R21721_270, 
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  fill.color = viridis::viridis(
    100, 
    option = "F", 
    direction = -1),
  point.color = "gray40"
)


#-------------------3c. Reduce autocorrelation in predictors-------------------#
# Select preds preference order - the ones we want to keep
preference.order <- c()

# Assess autocorrelation & drop highly correlated
predictor.variable.names <- spatialRF::auto_cor(
  x = R21721_270[, predictor.variable.names],
  cor.threshold = 0.6,
  preference.order = preference.order
) %>%
  spatialRF::auto_vif(
    vif.threshold = 2.5,
    preference.order = preference.order
  )


#------------------------3d. Find variable interactions------------------------#
interactions <- spatialRF::the_feature_engineer(
  data = R21721_270,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  xy = xy,
  importance.threshold = 0.50, #uses 50% best predictors
  cor.threshold = 0.60, #max corr between interactions and predictors
  seed = random.seed,
  repetitions = 100,
  verbose = TRUE
)

# Add interaction column to training data - (be sure interactions added make sense)
R21721_270 <- () 

# Add interaction name to predictor.variable.names
predictor.variable.names <- ()


#------------------------4. Fit a non-spatial RF model-------------------------#
model.non.spatial <- spatialRF::rf(
  data = R21721_270,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance.matrix,
  distance.thresholds = distance.thresholds,
  xy = xy, #not needed by rf, but other functions read it from the model
  seed = random.seed,
  verbose = FALSE
)

# Residuals
spatialRF::plot_residuals_diagnostics(
  model.non.spatial,
  verbose = FALSE
)


#----------------------------4a. Variable importance---------------------------#
# Global variable importance
spatialRF::plot_importance(
  model.non.spatial,
  verbose = FALSE
)

# Contribution of predictors to model transferability
model.non.spatial <- spatialRF::rf_importance(
  model = model.non.spatial
)

# Local variable importance
local.importance <- spatialRF::get_importance_local(model.non.spatial)

# Partial dependence plots
spatialRF::plot_response_curves(
  model.non.spatial,
  quantiles = 0.5, # other predictors set to 0.5 of quantiles
  ncol = 3
)

#----------------------------4b. Model performance-----------------------------#
spatialRF::print_performance(model.non.spatial)


#-------------------------4c. Spatial cross-validation-------------------------#
model.non.spatial <- spatialRF::rf_evaluate(
  model = model.non.spatial,
  xy = xy,                  #data coordinates
  repetitions = 30,         #number of spatial folds
  training.fraction = 0.75, #training data fraction on each fold
  metrics = "r.squared",
  seed = random.seed,
  verbose = FALSE
)

spatialRF::plot_evaluation(model.non.spatial)
spatialRF::print_evaluation(model.non.spatial)


#---------------------------5. Fit a spatial model-----------------------------#
# Spatial autocorrelation of residuals on non-spatial model
spatialRF::plot_moran(
  model.non.spatial,
  verbose = FALSE
)

# Transform non-spatial model to a spatial model
model.spatial <- spatialRF::rf_spatial(
  model = model.non.spatial,
  method = "mem.moran.sequential", #default method
  verbose = FALSE,
  seed = random.seed
)

# Spatial autocorrelation of residuals on spatial model (what we are trying to reduce from the beginning)
spatialRF::plot_moran(
  model.spatial,
  verbose = FALSE
)

# Variable importance
p1 <- spatialRF::plot_importance(
  model.non.spatial,
  verbose = FALSE) +
  ggplot2::ggtitle("Non-spatial model")

p2 <- spatialRF::plot_importance(
  model.spatial,
  verbose = FALSE) +
  ggplot2::ggtitle("Spatial model")

p1 | p2

# Top 10 spatial predictors
head(model.spatial$importance$per.variable, n = 10)


#----------------------------------6. Tune the model---------------------------#
model.spatial <- rf_tuning(
  model = model.spatial,
  xy = xy,
  repetitions = 30,
  num.trees = c(500, 1000),
  mtry = seq(
    2,
    length(model.spatial$ranger.arguments$predictor.variable.names), #number of predictors
    by = 9),
  min.node.size = c(5, 15),
  seed = random.seed,
  verbose = FALSE
)

#-----------------------------7. Repeat a model execution----------------------#
model.spatial.repeat <- spatialRF::rf_repeat(
  model = model.spatial,
  repetitions = 30,
  seed = random.seed,
  verbose = FALSE
)

# Plot variable importance
spatialRF::plot_importance(
  model.spatial.repeat,
  verbose = FALSE
)

# Plot partial dependence curves 
spatialRF::plot_response_curves(
  model.spatial.repeat,
  quantiles = 0.5,
  ncol = 3
)

# Model performance
spatialRF::print_performance(model.spatial.repeat)



#------------------------------------------------------------------------------#
#-------------------------To run it all at once - Alana's comp-----------------#

#creating and registering the cluster
local.cluster <- parallel::makeCluster(
  parallel::detectCores() - 1,
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = local.cluster)

#fitting, tuning, evaluating, and repeating a model
model.full <- rf_spatial(
  data = plant_richness_df,
  dependent.variable.name = dependent.variable.name,
  predictor.variable.names = predictor.variable.names,
  distance.matrix = distance_matrix,
  distance.thresholds = distance.thresholds,
  xy = xy,
  cluster = local.cluster #is passed via pipe to the other functions
) %>%
  rf_tuning() %>%
  rf_evaluate() %>%
  rf_repeat()

#stopping the cluster
parallel::stopCluster(cl = local.cluster)