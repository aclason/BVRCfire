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
ls <- append(ls, c("raster")) # geo comp.
ls <- append(ls, c("randomForest", "pdp")) # analysis

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#-------------------------------- Load data -----------------------------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"
SpatialFilesPath <- getwd()
# Set the fires of interest - all 2018 fires with openings
FiresOfInterest <- c("G41607", "G51632", "R11498", "R11796","R11921","R21721")


# ----- Read in and resample rasters to have same extent and resolution--------#
# Using base rasters resample response and predictor variables to same extent and resolution
# Resample categorical and continuous variables differently

# Read in the rasters
variable_list <- list.files(paste0(SpatialFilesPath, "/Inputs/Rasters/"),
                            pattern =  paste(FiresOfInterest, sep = "", collapse = "|"), # only import the fires of interest
                            recursive = TRUE,
                            full.names = TRUE)
variable_list <- grep("tif",variable_list,value=TRUE)
# Drop OpenID, None and SitePrepped
variable_list <- grep("OpenID|None|SitePrepped",variable_list, value = TRUE, invert = TRUE)


variables <- sapply(variable_list, raster)
#variables <- sapply(variable_list, rast) #if we want to use rast instead?

# Rename the variables 
variable.name <- lapply(str_split(variable_list,"/"),function(x) grep(".tif",x, value=TRUE))
variable.name <- str_split(variable.name, ".tif", simplify = TRUE)[,1]
names(variables) <- variable.name

#ID the names of the categorical rasters
ctg_variables <- c("BEC", "BroadBurn", "Brushed", "DebrisMade", "DebrisPiled", "Fertil", "MechUnk", 
                   "OPENING_ID", "PileBurn", "PlantAge", "Prune", "Soil", "Spaced", 
                   "SpotBurn", "WBurn")
CatRasts <- grep(paste(ctg_variables,sep = "", collapse = "|"),variable.name,value=TRUE)


for(i in 1:length(FiresOfInterest)){
  allFireRasts <- variables[grep(FiresOfInterest[i],variables)]
  baseFireRast <- allFireRasts[grep("Base",allFireRasts)][[1]] #index just makes it not a list
  allFireRasts <- allFireRasts[grep("Base",allFireRasts,invert=TRUE)]

  a <- list()
  for(j in 1:length(allFireRasts)){
    if(names(allFireRasts[[j]]) %in% CatRasts){
      a[[j]] <- resample(allFireRasts[[j]], baseFireRast, method = "ngb")
    } else {
      a[[j]] <- resample(allFireRasts[[j]], baseFireRast, method = "bilinear")
    }
  }
  fireID <- str_extract(names(allFireRasts[1]),FiresOfInterest[i])
  SimpleRastnames <- str_remove(str_remove(names(allFireRasts),FiresOfInterest[i]),"_")
  names(a) <- SimpleRastnames
  #stack the simplified names and assign to fire id rast name
  assign(paste0(fireID,"rasts"), stack(a))
}




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


#-----------------3. Create sample points 270 m dist------------------#
# Create index of raster stacks
RastStacks <- list(G41607rasts, G51632rasts, R11498rasts, R11796rasts, R11921rasts, R21721rasts)

for(i in 1:length(FiresOfInterest)){
  allFireRasts <- variables[grep(FiresOfInterest[i],variables)]
  dNBRFireRast <- allFireRasts[grep("dNBR",allFireRasts)][[1]] #use a raster (doesn't matter which one)
  
  # 270 m grid distance
  b <- aggregate(dNBRFireRast, fact = 9, fun = mean)
  points270 <- rasterToPoints(b)

 
  # Extract response and predictor values at sample points
  SampledRaster <- raster::extract(RastStacks[[i]], points270)
  # Convert to data frame
  dat270 <- as.data.frame(SampledRaster) 
  
  # Drop rows that don't have an opening ID because we only want to include plantation openings
  dat270 <- dat270 %>% filter(!is.na(OPENING_ID))
  # Drop opening ID column
  dat270 <- subset(dat270, select =-c(OPENING_ID, drop))
  
  # Meet spatial RF requirements
  # 1. Must be free of NA
  dat270 <- dat270[complete.cases(dat270), ] # remove NAs
  
  # 2. Columns cannot have 0 variance
  RemoveZeroVar <- function(dat270) {
    dat270[, !sapply(dat270, function(x) min(x) == max(x))]
  }
  dat270 <- RemoveZeroVar(dat270)

  # 3. Columns must not yield NaN or Inf when scaled
  #sum(apply(scale(R11796dat270), 2, is.nan)) 
  #sum(apply(scale(R11796dat270), 2, is.infinite))
  # Find which columns are giving issue
  #sapply(as.data.frame(scale(R21721_270)), function(x)any(is.nan(x)))
  
  # Move response (dNBR) to first column
  dat270 <- dat270 %>% select(dNBR, everything())
  fireID <- str_extract(names(allFireRasts[1]),FiresOfInterest[i])
  assign(paste0(fireID,"dat270"), dat270)

}


#---------------------5. Run initial RF -----------------------------#

dat270 <- list(G41607dat270, G51632dat270, R11498dat270, R11796dat270, R11921dat270, R21721dat270)

for (i in 1:length(FiresOfInterest)) {
  rfmod<- randomForest(dNBR ~ ., data = dat270[i], 
                       ntree = 500, 
                       importance = TRUE, 
                       na.action = na.omit)
  rfmod

  # Variable importance
  varImpPlot(rfmod, n.var = 10)

  # Calculate R2
  actual <- dat270[,1]
  predicted <- dat270$predicted
  R2 <- 1- (sum((actual - predicted)^2)/sum((actual - mean(actual))^2))
  R2 
}



#------------------------------------ R11921

# Find best mtry value
mtry <- tuneRF(R11921dat270[,-1], R11921dat270[,1], ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# Rerun model with best mtry
rfmod_R11921 <- randomForest(dNBR ~ .,data = R11921dat270,
                             ntree = 500,
                             mtry = 8,
                             importance = TRUE,
                             na.action = na.omit)
rfmod_R11921

# Variable importance
varImpPlot(rfmod_R11921, n.var = 10)

# Partial dependence plots
rfmod_R11921 %>%
  partial(pred.var = "PlantAge") %>%
  autoplot(train = R11921dat270)


#-------------------------------- R21721
# Data should only contain response (first row) and predictors
names(R21721dat270)

rfmod_R21721<- randomForest(dNBR ~ ., data = R21721dat270, 
                            ntree = 500, 
                            importance = TRUE, 
                            na.action = na.omit)
rfmod_R21721

# Find best mtry value
mtry <- tuneRF(R21721dat270[,-1], R21721dat270[,1], ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

# Rerun model with best mtry
rfmod_R21721 <- randomForest(dNBR ~ .,data = R21721dat270,
                             ntree = 500,
                             mtry = 8,
                             importance = TRUE,
                             na.action = na.omit)
rfmod_R21721

# Variable importance
varImpPlot(rfmod_R21721, n.var = 10)

# Partial dependence plots
rfmod_R21721 %>%
  partial(pred.var = "PlantAge") %>%
  autoplot(train = R21721dat270)

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
actual <- R21721dat270[,1]
predicted <- R21721dat270$predicted
R2 <- 1- (sum((actual - predicted)^2)/sum((actual - mean(actual))^2))
R2 # 0.27




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
