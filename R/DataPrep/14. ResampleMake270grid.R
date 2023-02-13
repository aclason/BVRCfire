#Prepare the data:
#2a) Resample response and predictor variables to have the same extent and resolution (30 x 30 m) for each of the fires.
#- Categorical variables are resampled using nearest neighbourhood and continuous variables are resampled using bilinear resampling.

#2b) Subsample at 270-m grid spacing to reduce spatial autocorrelation in the dataset used for analyses
#- This is not a random selection process, so can be reproduced
#- Data must be free of NAs 
#- Data must not have variables with no variance


# Alana Clason & Ingrid Farnell

# The output of this script produces the csvs for each fire to be analysed in Random Forest AND
# the 270 grid to ensure reproducibility of pixel selection


#------------------------------ Load libraries---------------------------------#
ls <- c("tidyverse", "data.table", "magrittr") # Data Management and Manipulation
ls <- append(ls, c("raster","sf")) # geo comp.

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#------------------------------ 1. Load data ----------------------------------#
#SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"
SpatialFilesPath <- getwd()
# Set the fires of interest - all 2018 fires with openings
FiresOfInterest <- c("G41607", "G51632", "R11498", "R11796","R11921","R21721")


# Read in the rasters
variable_list <- list.files(paste0(SpatialFilesPath, "/Inputs/Rasters/"),
                            pattern =  paste(FiresOfInterest, sep = "", collapse = "|"),
                            recursive = TRUE,
                            full.names = TRUE)
variable_list <- grep("tif", variable_list, value=TRUE)
# Drop OpenID, None and SitePrepped
variable_list <- grep("OpenID|None|SitePrepped|n83", variable_list, value = TRUE, invert = TRUE)
# For now remove this one because the raster contains only NA's -- Alana to fix and then remove this line
variable_list <- grep("R11498_SpotBurn", variable_list, value = TRUE, invert = TRUE)
variable_list <- c(variable_list,paste0(SpatialFilesPath,"/Inputs/Rasters/Topography/DEM",
                                        c("aspect","hli","slope","tpi"),".tif"))

variables <- sapply(variable_list, raster)


# Rename the variables 
variable.name <- lapply(str_split(variable_list,"/"), function(x) grep(".tif", x, value=TRUE))
variable.name <- str_split(variable.name, ".tif", simplify = TRUE)[,1]

names(variables) <- variable.name

#ID the names of the categorical rasters
ctg_variables <- c("BEC", "BroadBurn", "Brushed", "DebrisMade", "DebrisPiled", "Fertil", "MechUnk", 
                   "OPENING_ID", "PileBurn", "Prune", "Soil", "Spaced", 
                   "SpotBurn", "WBurn")
CatRasts <- grep(paste(ctg_variables,sep = "", collapse = "|"),variable.name,value=TRUE)

# ------------------------------Prepare data-----------------------------------#
#--------------------------2a. Resample rasters and stack----------------------#

# Using base rasters resample response and predictor variables to same extent and resolution
for(i in 1:length(FiresOfInterest)){
  allFireRasts <- variables[c(grep(FiresOfInterest[i],variables),grep("DEM",variables))]
  baseFireRast <- allFireRasts[grep("Base",allFireRasts)][[1]] #index just makes it not a list
  allFireRasts <- allFireRasts[grep("Base",allFireRasts,invert=TRUE)]
  
  # Resample categorical and continuous variables differently
  a <- list()
  for(j in 1:length(allFireRasts)){
    if(names(allFireRasts[[j]]) %in% CatRasts){
      a[[j]] <- raster::resample(allFireRasts[[j]], baseFireRast, method = "ngb")
    } else {
      a[[j]] <- raster::resample(allFireRasts[[j]], baseFireRast, method = "bilinear")
    }
  }
  fireID <- str_extract(names(allFireRasts[1]),FiresOfInterest[i])
  SimpleRastnames <- str_remove(str_remove(names(allFireRasts),FiresOfInterest[i]),"_")
  names(a) <- SimpleRastnames
  #stack the simplified names and assign to fire id rast name
  assign(paste0(fireID,"rasts"), stack(a))
}

#-----------------------------2b. Get sample points----------------------------#
# Create index of raster stacks
RastStacks <- list(G41607rasts, G51632rasts, R11498rasts, R11796rasts, R11921rasts, R21721rasts)

for(i in 1:length(FiresOfInterest)){
  allFireRasts <- variables[grep(FiresOfInterest[i],variables)]
  dNBRFireRast <- allFireRasts[grep("dNBR",allFireRasts)][[1]] #use a raster (doesn't matter which one)
  
  # 270 m grid distance
  b <- aggregate(dNBRFireRast, fact = 9, fun = mean)
  points270 <- rasterToPoints(b, spatial = TRUE) # get sample grid: 1 point/270 m, spatial = TRUE so coordinates are attached
  colnames(points270@data) <- "drop" # make sure to drop this later on(it's a place holder column for points)
  
  # Extract response and predictor values at sample points
  SampledRaster <- raster::extract(RastStacks[[i]], points270, sp = TRUE)
  # Convert to data frame
  dat270 <- as.data.frame(SampledRaster) # hopefully xy = TRUE will attach coordinates, if not do sp = TRUE in above extract line
  
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
  dat270 <- dat270 %>% dplyr::select("dNBR", everything())
  fireID <- str_extract(names(allFireRasts[1]),FiresOfInterest[i])
  assign(paste0(fireID,"dat270"), dat270)
  
}
#-----------------------------3. save csvs for RF analysis ----------------------------#
#watch - order is hard coded
list_dats <- list(G41607dat270,G51632dat270,R11498dat270,R11796dat270,R11921dat270,R21721dat270)
FiresOfInterest
for(ii in 1:length(list_dats)){
  write.csv(list_dats[[ii]],paste0("./Inputs/",FiresOfInterest[ii],"dat270.csv"),row.names = FALSE)
}

#-----------------------------3b. save 270 grids as spatial objects ----------------------------#
for(iii in 1:length(FiresOfInterest)){
  #read in the csv
  dat270 <- fread(paste0("./Inputs/",FiresOfInterest[iii],"dat270.csv"))
  # convert to sf object
  dat270_sf <- st_as_sf(dat270, coords = c("x","y"))
  dat270_sf <- dat270_sf %>%
    dplyr::select(.,c("geometry"))
  write_sf(dat270_sf, paste0("./Inputs/",FiresOfInterest[iii],"_270grid.shp"))
}


