# Script for manuscript figures
# Ingrid Farnell
# Feb 16, 2022

# Figure of study area map. The 6 fires in the context of BC and just Nadina with the
# fire severity and satelitte imagery in the background

# Remove workspace
rm(list=ls())
gc()

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
  filter(NAME=="Smithers"| NAME=="Telkwa"| NAME=="Houstson" | NAME=="Burns Lake"| NAME=="Fraser Lake" | NAME=="Vanderhoof" | NAME=="Prince George" | NAME=="Fort St. James")
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
               nudge_y = -0.1) +
  north(StudyFirePerims, symbol = 10, 
        location = "bottomleft") +
  scalebar(StudyFirePerims, dist = 50, dist_unit = "km", st.size = 2, 
           transform = FALSE, 
           location = "bottomleft", 
           border.size = 0.1) +
  geom_rect(aes( # add a rectangle around the fire area
    xmin = 910000,
    ymin = 915000, 
    xmax = 1150000, 
    ymax = 1110000),
    fill = NA, 
    colour = "black", 
    size= 0.6
  ) +
  coord_sf(xlim = c(910000, 1150000), ylim = c(915000, 1110000), expand = FALSE) + # got from bb doing head(x)
  theme_void() +
  theme(legend.position = "bottom") +
  labs(fill = "2018 Wildfires")
   
fires

# zoom in level 54.8N - 53.5 and 127-123

# Provincial scale 
bc <- ggplot() +
  geom_sf(data = bc_border, fill = "white") +
  geom_sf(data = major.cities) +
  geom_sf_text(data = major.cities, 
          aes(label = NAME), 
          size = 2.5, 
          nudge_y = -0.1) +
  geom_sf(data = StudyFirePerims, fill = "black", colour = NA) +
  geom_rect(aes( # add a rectangle around the fire area
    xmin = 910000,
    ymin = 915000, 
    xmax = 1150000, 
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
            scale = 1.5) +
  draw_plot(bc,
            x = -0.1, 
            y = 0.61, 
            width = 0.6, 
            height = 0.7) +
  theme(aspect.ratio = 0.45)

final_map

ggsave(filename = "StudyArea.svg", device = "svg", plot = final_map, path = "./BVRCfire/Outputs/Figures/", bg = "white")



#--------FIGURE 2: Nadina with fire severity and satellite imagery background-------------#

# NADINA FIRE SEVERITY
nadina.severity <- read_sf("E:/Ingrid/Borealis/BVRC_20-01_WildfireResearchInitiative/Severity/Nadina_severity_3857.shp")
# Change factor level for legend
nadina.severity$BURN_SEVER <- factor(nadina.severity$BURN_SEVER, levels = c("Unburned", "Low", "Medium", "High"))


# NADINA BOUNDING BOX
nadina.bb <- read_sf("E:/Ingrid/Borealis/BVRC_20-01_WildfireResearchInitiative/Nadina_BB/Nadina_bb.shp")
head(nadina.bb) # to get the bb to use for zoom in scale


# Upload satellite imagery 

# have to register API key to get maps (only need to do once, so commented)
# register_google(key = "AIzaSyDdWd_daj9nmODnInXXD4tkFhO2b2uShHE", 
#                 write = TRUE)  
nadina.location <- c(lon=-126.703498, lat=53.932633)
nadina.satellite <- get_map(location = nadina.location, 
                            source = "google", maptype = "satellite", 
                            zoom = 9)
ggmap(nadina.satellite) # view map


# Have to use this work around to get geom_sf to work with ggmap
# from https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(nadina.satellite) {
  if (!inherits(nadina.satellite, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(nadina.satellite, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(nadina.satellite, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(nadina.satellite, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(nadina.satellite, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(nadina.satellite, "bb")$ur.lon <- bbox_3857["xmax"]
  nadina.satellite
}

nadina.map <- ggmap_bbox(nadina.satellite)


# Nadina Burn Severity
nadina <- ggmap(nadina.map) +
  geom_sf(data = nadina.severity, mapping = aes(fill = factor(BURN_SEVER)), 
          colour = NA, # removes polygon borders
          inherit.aes = FALSE) +
  scale_fill_brewer(palette="Spectral", direction = -1,
                    name = "Burn severity", 
                    labels = c("Unburned: -0.100-0.100 \n (-100-100)", # "\n" to split into two lines
                               "Low: 0.100-0.27 \n (100-270)", 
                               "Moderate: 0.270-0.660 \n (270-660)", 
                               "High: >0.660 \n (>660)")) +
  coord_sf(crs = st_crs(3857), 
           xlim = c(-14160000, -14030000), # used the bb from nadina.bb - with adjustments
           ylim = c(7130000, 7195000), expand = FALSE) +
  north(data = nadina.severity, 
        location = "topright", 
        symbol = 4, 
        scale = 0.15) +
  scalebar(data = nadina.severity, 
           location = "bottomleft",
           dist = 25, dist_unit = "km", 
           transform = FALSE, 
           model = "WGS84", 
           st.color = "white", st.size = 2.5, border.size = 0.1, 
           st.dist = 0.05) +
  labs(title = "Nadina Burn Severity") +
  theme(axis.text.x = element_text(angle = -45, hjust = -0.3)) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_y_continuous(breaks = c(54.1, 54.0, 53.9, 53.8))



nadina

