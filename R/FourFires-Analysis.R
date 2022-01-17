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
ls <- append(ls, c("ncf")) # analysis

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#--------- Load data -----------------#
# Set the fires of interest - all 2018 fires
study_fireTable <- fread("./Inputs/StudyFireList.csv") # all potential fires
FiresOfInterest <- c( "R11796","R11498","R21721","R11921", "G41607", "G51632", "C11937")

# Plantations with site prep
dt <- data.table()
Plantations <- data.table()
for(j in 1:length(FiresOfInterest)){
  dt <- fread(paste0("./Outputs/IndividualFires/",FiresOfInterest[j],"_Firedat_SP.csv"))
  dt[, ':='(FireID = FiresOfInterest[j])]
  dt[,.N,by=OPENING_ID]
  dt[,.N]
  Plantations <- rbind(Plantations,dt,fill=TRUE)
}

# Fire weather data
all_fireweather <- fread("./Inputs/Fireweather/FireWeather.csv")
Fireweather <- all_fireweather[Fire_ID %in% FiresOfInterest] # select only fires of interest
unique(Fireweather[,Fire_ID]) # make sure it worked

# Rasters
# dNBR
dNBR_R11796 <- raster("E:/Ingrid/Borealis/BVRC_21-01_CFS/Rasters/dNBR/dNBR_R11796.tif")

# DOB
DOB_R11796 <- raster("E:/Ingrid/Borealis/BVRC_21-01_CFS/Rasters/DOB/dob_R11796.tif")


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

# dNBR R11796 (has huge extent, so need to reduce size)
# Clip extent to DOB R11796
dNBR_R11796_clip <- crop(dNBR_R11796, DOB_R11796)
res(dNBR_R11796_clip) # make sure still 30 x 30 res

# Calculate Moran's I
# Create weighting matrix
queen <- matrix(c(1, 1, 1, 1, 0, 1, 1, 1, 1), 3,3)

# 30 m dist - Moran's I with 3x3 queen filter
m.3.q_dNBR_R11796 <- raster::Moran(dNBR_R11796_clip, w = queen)
m.3.q_dNBR_R11796 # 0.9393669 = high clustering of similar values

# 200 m dist 
dNBR_R11796_210m <- aggregate(dNBR_R11796_clip, fact = 7, fun = mean)
m.3.q_dNBR_R11796_210 <- Moran(dNBR_R11796_210m, w = queen)
m.3.q_dNBR_R11796_210 # 0.8467199

# 400 m dist
dNBR_R11796_420m <- aggregate(dNBR_R11796_clip, fact = 14, fun = mean)
m.3.q_dNBR_R11796_420 <- Moran(dNBR_R11796_420m, w = queen)
m.3.q_dNBR_R11796_420 # 0.8054098

# 800 m dist
dNBR_R11796_800m <- aggregate(dNBR_R11796_clip, fact = 27, fun = mean)
m.3.q_dNBR_R11796_800 <- Moran(dNBR_R11796_800m, w = queen)
m.3.q_dNBR_R11796_800 # 0.7647504

# 1000 m dist
dNBR_R11796_1000m <- aggregate(dNBR_R11796_clip, fact = 34, fun = mean)
m.3.q_dNBR_R11796_1000 <- Moran(dNBR_R11796_1000m, w = queen)
m.3.q_dNBR_R11796_1000 # 0.7419534

# 1200 m dist
dNBR_R11796_1200m <- aggregate(dNBR_R11796_clip, fact = 40, fun = mean)
m.3.q_dNBR_R11796_1200 <- Moran(dNBR_R11796_1200m, w = queen)
m.3.q_dNBR_R11796_1200 # 0.7218777

# 2000 m dist (just to see)
dNBR_R11796_2000m <- aggregate(dNBR_R11796_clip, fact = 67, fun = mean)
m.3.q_dNBR_R11796_2000 <- Moran(dNBR_R11796_2000m, w = queen)
m.3.q_dNBR_R11796_2000 # 0.66

# We are going to choose 800 m. 


#-----------------3. Create sample points ------------------#
# Use 800 m resolution raster to convert to point data
sample_pnts_810 <- rasterToPoints(dNBR_R11796_clip, spatial = TRUE)





############## NOT USING but keep for code just in case################################
# Convert raster to points
dNBR_R11796_pnts <- rasterToPoints(dNBR_R11796_crp, spatial = TRUE)
x <- dNBR_R11796_pnts@coords[, 1]
y <- dNBR_R11796_pnts@coords[, 2]
z <- dNBR_R11796_pnts@data

ncf.dNBR_R11796 <- ncf::correlog(x = x, y = y, z= z, 
                            increment = 45, 
                            resamp = 0,
                            latlon = TRUE )
