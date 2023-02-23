# Script for manuscript figures
# Ingrid Farnell
# Feb 16, 2022

# Figure of study area map. The 6 fires in the context of BC and just Nadina with the
# fire severity and satelitte imagery in the background

# Remove workspace
rm(list=ls())

#------------------------------ Load libraries---------------------------------#
ls <- c("tidyverse") # Data Management and Manipulation
ls <- append(ls, c("sf")) # geo comp.
ls <- append(ls, c("ggpubr", "bcmaps", "bcdata", "ggsn", "cowplot", "ggmap")) # maps: ggarrange, data bc layers, data bc layers, north arrow and scale, add inset to map, satellite imagery

# Install if needed -- then load. 
new.packages <- ls[!(ls %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(ls, library, character.only = TRUE)  # load the required packages
rm(ls, new.packages)


#-------------------- Import spatial layers & data prep------------------------#
SpatialFilesPath <- "E:/Ingrid/Borealis/BVRCfire"

# FIRE PERIMETERS
allFires <- read_sf(paste0(SpatialFilesPath, "./Inputs/Study_fire_perimeters/Study_fire_perimeters.shp")) # all potential fires
# Fires of interest
FiresOfInterest <- c("G41607", "G51632", "R11498", "R11796","R11921","R21721")
StudyFirePerims <- allFires %>% dplyr::filter(.,FIRE_NUMBE %in% FiresOfInterest)
# Change factor level for ggplot legend
StudyFirePerims$FIRE_NUMBE <- as.factor(StudyFirePerims$FIRE_NUMBE)
StudyFirePerims$FIRE_NUMBE <- factor(StudyFirePerims$FIRE_NUMBE, levels = c("R21721", "R11796", "R11921", "G51632", "R11498", "G41607"))

# BC BORDER
bc_border <- bc_bound()

# MAJOR WATER FEATURES
water <- read_sf("E:/Ingrid/Borealis/BVRC_20-01_WildfireResearchInitiative/Water_BC/Water_BC.shp")
roads <- read_sf("E:/Ingrid/Borealis/BVRC_20-01_WildfireResearchInitiative/Transport/Roads_BC.shp")

# CITIES (data bc)
bcdc_search("cities", res_format="shp") # finds layer
all.cities.metadata <- bcdc_get_record("b678c432-c5c1-4341-88db-0d6befa0c7f8") # get metadata first
all.cities <- bcdc_get_data(all.cities.metadata, resource = "b678c432-c5c1-4341-88db-0d6befa0c7f8") # get the layer
# filter cities for small scale map
cities <- all.cities %>%
  filter(NAME=="Smithers"| NAME=="Telkwa"| NAME=="Houston" | NAME=="Burns Lake"| NAME=="Fraser Lake" | NAME=="Vanderhoof" | NAME=="Prince George" | NAME=="Fort St. James")
# filter cities for large scale map
major.cities <- all.cities %>%
  filter(NAME=="Burns Lake"| NAME=="Prince George" | NAME=="Vancouver")

# ROADS (data bc)
bcdc_search("highways", res_format="shp") # finds layer
all.roads.metadata <- bcdc_get_record("e37cc64b-41a6-491f-be3b-9983bd5948f7") # get metadata first
all.roads <- bcdc_get_data(all.roads.metadata, resource = "e37cc64b-41a6-491f-be3b-9983bd5948f7") # get the layer
# filter to just highway 16, 35, 27 
highways <- all.roads %>%
  filter(HIGHWAY_NUMBER=="H16"| HIGHWAY_NUMBER=="H35"| HIGHWAY_NUMBER=="H27")

# WEATHER STATIONS
stations <- read_sf(paste0(SpatialFilesPath, "./Inputs/FireWeatherStations/crmp_network_geoserverPoint.shp"))
stationID <- c("NADINA", "ZZ PARROTT", "HOUSTON(DUNGATE)", "PEDEN", "GRASSY PLAINS", 
               "HOLY CROSS 2", "EAST OOTSA", "AUGIER LAKE", "Vanderhoof", "KLUSKUS", 
               "MOOSE LAKE", "FORT ST JAMES", "NORTH CHILCO")
stations <- stations %>%
  dplyr::filter(., station_na %in% stationID)
stations <- stations %>%
  dplyr::filter(!network_na == "EC" & !network_na == "ARDA") # remove the stations that are not wildfire (except Vanderhoof station)


#-------------------------------------------------------------#
#--------FIGURE 1: STUDY AREA with 4 fires ---------------#
fire.labels <- c("Nadina Lake", "Verdun Mountain", "Island Lake", "Tezzeron", "Shovel Lake", "Chutanli Lake")

# small scale - 4 fires
fires <- ggplot() +
  geom_sf(data = StudyFirePerims, mapping = aes(fill = FIRE_NUMBE)) +
  scale_fill_brewer(palette = "Dark2", # colour palette for fires
                    labels = fire.labels) + 
  geom_sf(data = water, fill = "lightblue2", colour=NA) +
  geom_sf(data = highways) +
  geom_sf(data = cities) +
  geom_sf_text(data = cities, 
               aes(label = NAME), 
               size = 2.5, 
               vjust = -0.5,
               hjust = -0.05) +
  geom_sf(data = stations, colour = "brown4", aes(shape = "Weather stations")) +
  north(StudyFirePerims, symbol = 10, 
        location = "bottomleft") +
  scalebar(StudyFirePerims, dist = 50, dist_unit = "km", st.size = 2, 
           transform = FALSE, 
           location = "bottomleft", 
           border.size = 0.1) +
  geom_rect(aes( # add a rectangle around the fire area
    xmin = 910000,
    ymin = 885000, 
    xmax = 1167000, 
    ymax = 1110000),
    fill = NA, 
    colour = "black", 
    size= 0.6
  ) +
  coord_sf(xlim = c(910000, 1167000), ylim = c(885000, 1110000), expand = FALSE) + # got from bb doing head(x)
  theme_void() +
  labs(fill = "2018 Wildfires",
       shape = NULL) +
  theme(legend.position = "bottom",
        legend.box = "vertical")

   
fires

# zoom in level 54.8N - 53.5 and 127-123

# Provincial scale 
bc <- ggplot() +
  geom_sf(data = bc_border, fill = "white") +
  geom_sf(data = major.cities) +
  geom_sf_text(data = major.cities, 
          aes(label = NAME), 
          size = 2.5, 
          vjust = -0.5,
          hjust = -0.05) +
  geom_sf(data = StudyFirePerims, fill = "black", colour = NA) +
  geom_rect(aes( # add a rectangle around the fire area
    xmin = 910000,
    ymin = 885000, 
    xmax = 1167000, 
    ymax = 1110000),
    fill = NA, 
    colour = "black", 
    size= 0.6) +
  theme_void() +
  theme(plot.background = element_rect(colour = "black", size = 1))
bc

# Draw the area of the fires on the main map for map inset
final_map <- 
  ggdraw() +
  draw_plot(fires, 
            scale = 1.45) +
  draw_plot(bc,
            x = -0.1, 
            y = 0.95, 
            width = 0.6, 
            height = 0.7) +
  theme(aspect.ratio = 0.4)

final_map

ggsave(filename = "StudyArea.svg", device = "svg", 
       plot = final_map, path = "./Outputs/Figures/", 
       bg = "white",
       height = 7.4,
       width = 7.4)


