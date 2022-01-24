#dNBR data cleaning
library(raster)
library(sf)

#create raster extent
BCbound <- read_sf("F:/Spatial Data/Administrative boundaries/BC_boundary_poly2.shp")
t <- raster(extent(BCbound), res=c(30,30), crs=crs(BCbound))

#each fire is stored in folders with names
dNBR_list <- list()
dNBR_list[[1]] <- raster("E:/Fires_dNBR/dNBR_imagery/C10784_Plateau_Complex_dNBR.tif")
dNBR_list[[2]] <- raster("E:/Fires_dNBR/dNBR_imagery/C10970_Kluskoil_Lake_dNBR.tif")
dNBR_list[[3]] <- raster("E:/Fires_dNBR/dNBR_imagery/C11837_Shag_Creek_dNBR.tif")
dNBR_list[[4]] <- raster("E:/Fires_dNBR/dNBR_imagery/C11937_North_Baezaeko_dNBR.tif")
dNBR_list[[5]] <- raster("E:/Fires_dNBR/dNBR_imagery/C12594_Baldface_Mtn_dNBR.tif")
dNBR_list[[6]] <- raster("E:/Fires_dNBR/dNBR_imagery/C20279_Wildwood_dNBR.tif")
dNBR_list[[7]] <- raster("E:/Fires_dNBR/dNBR_imagery/C20735_White_Lake_dNBR.tif")
dNBR_list[[8]] <- raster("E:/Fires_dNBR/dNBR_imagery/C50647_Hanceville_Riske_Creek_dNBR.tif")
dNBR_list[[9]] <- raster("E:/Fires_dNBR/dNBR_imagery/C50744_Kleena_Kleene_dNBR.tif")
dNBR_list[[10]] <- raster("E:/Fires_dNBR/dNBR_imagery/G41607_Chutanli_dNBR.tif")
dNBR_list[[11]] <- raster("E:/Fires_dNBR/dNBR_imagery/G51632_Tezzeron_dNBR.tif")
dNBR_list[[12]] <- raster("E:/Fires_dNBR/dNBR_imagery/K20637_Elephant_Hill_dNBR.tif")
dNBR_list[[13]] <- raster("E:/Fires_dNBR/dNBR_imagery/dNBR_imagery/R11498_Shovel_Lake_dNBRS2.tif")
dNBR_list[[14]] <- raster("E:/Fires_dNBR/dNBR_imagery/R11796_Verdun_dNBR.tif")
dNBR_list[[15]] <- raster("E:/Fires_dNBR/dNBR_imagery/R11921_Island_Lake_dNBR.tif")
dNBR_list[[16]] <- raster("E:/Fires_dNBR/dNBR_imagery/R12068_PondosyBay_dNBR.tif")
dNBR_list[[17]] <- raster("E:/Fires_dNBR/dNBR_imagery/R21721_Nadina_dNBR.tif")
dNBR_list[[18]] <- raster("E:/Fires_dNBR/dNBR_imagery/R91947_AlkaliLk_dNBR.tif")
dNBR_list[[19]] <- raster("E:/Fires_dNBR/dNBR_imagery/VA1787_RamseyCrk_dNBR.tif")
dNBR_list[[20]] <- raster("E:/Fires_dNBR/dNBR_imagery/VA1964_DeanRiver_dNBR.tif")

dNBR_list_alb <-list()
#this list needs to be in the same order as the above list!
FireListFromSam <- c("C10784","C10970","C11837","C11937","C12594","C20279","C20735", "C50647","C50744",
                     "G41607","G51632","K20637","R11498","R11796","R11921","R12068","R21721",
                     "R91947","VA1787","VA1964")
for(j in 1:length(dNBR_list)){
  dNBR_list_alb[[j]] <- projectRaster(from=dNBR_list[[j]],to=t, method="bilinear")
  writeRaster(dNBR_list_alb[[j]], paste0("C:/Users/Alana2012/ALANA_FILES/GitHub/BVRCfire/Inputs/Rasters/dNBR_",
                                           FireListFromSam[j],".tif"),overwrite=TRUE)
}




