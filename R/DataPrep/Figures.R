# Make dNBR vs plantation age figure
# Ingrid Farnell
# Feb 23, 2022


#------------------------------ Load libraries---------------------------------#
ls <- c("tidyverse", "data.table") # Data Management and Manipulation
ls <- append(ls, c("sf")) # geo comp.
ls <- append(ls, c("ggpubr")) # maps: ggarrange

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#-------------------- Import spatial layers & data prep------------------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"


Nadina <- fread("C:/Users/farne/Documents/R21721dat270.csv")
Nadina[,dNBRcat := ifelse(dNBR >0.660, "high",
                          ifelse(dNBR >0.270, "moderate", 
                                 ifelse(dNBR >0.100, "low",
                                        "unburned")))] 
# Change factor level 
Nadina$dNBRcat <- factor(Nadina$dNBRcat, levels = c("unburned", "low", "moderate", "high"))



Nadina[,dNBRscaled := dNBR*1000]

#----------------------------------Figure 1------------------------------------#
nad <- ggplot(Nadina, aes(x=PlantAge)) +
  geom_bar(aes(fill=dNBRcat), position=position_stack(reverse = TRUE)) +
  theme_minimal()
nad
