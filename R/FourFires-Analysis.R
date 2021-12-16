# Title: Four fires analysis
# Author: Ingrid Farnell
# Date: 12-15-2021

# This script analyses four fires from 2018 (Shovel Lake, Nadina, Verdun, Island Lake) using Random Forest.
# We are analysing which variables (site prep, topography, and fire weather) are imporatant for fire severity (dNBR) 
# in plantations.

# DATA FLOW
# 1. Assess predictor variables for multicollinearity. RF can handle correlated data, but we don't want too many 
# correlated variables because it could bias the variable importance. Many paper's drop one of the variables if 
# spearman >|0.8|



#---------- Load libraries ---------------#
library(data.table)



#--------- Load data -----------------#
# Set the fires of interest
study_fireTable <- fread("./Inputs/StudyFireList.csv") # all potential fires
FiresOfInterest <- c( "R11796","R11498","R21721","R11921")

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



#----------1. Multicollinearity --------------------#
# Create a correlation matrix to see which variables in Fireweather are correlated
# Extract only weather columns
Fireweather_var <- Fireweather[ ,.(bui, dc, dmc, dsr, ffmc, fwi, humidity, isi, precipitation, sdmc, temperature, wind)]
round(cor(Fireweather_var, use = "complete.obs", method = "spearman"), 2)

# Next step, decide which variables to drop
