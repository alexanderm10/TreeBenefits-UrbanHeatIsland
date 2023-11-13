# creating maps of mean temperature and vegetation cover for the NorthStar2023 Domain
library(raster)
library(sf)

path.files <- "V:/alexander/northStar2023/1km_modis/Outputs"
fileOut <- "U:/projects/NorthStar2023/1km_modis_mean_maps"

file.type <- c("lst", "vegetation")



for(i in file.type){
  fileNames <- list.files(file.path(path.files,i), pattern="\\.tif$") # getting just the .tif files
  
  pb <- txtProgressBar(min=0, max=length(fileNames), style=3)
  pb.ind=1
  
  for(j in fileNames){
    city.id <- stringr::str_split_i(j,"_",1)
    measure.id1 <- stringr::str_split_i(j,"[.]",1)
    measure.id2 <- paste(stringr::str_split_i(measure.id1,"_",2), stringr::str_split_i(measure.id1,"_",3), sep="_")
    
    temp <- brick(file.path(path.files, i, j))
    temp.mean <- mean(temp)
    
    if(!dir.exists(file.path(fileOut, i))) dir.create(file.path(fileOut, i), recursive=T)
    
    writeRaster(temp.mean, filename= file.path(fileOut, i, paste0(city.id, "_", measure.id2,"_meanMap",".tif")), format="GTiff", overwrite=T)
    setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
  }
}

#######################################
# need to save the GeoTiffs as a .png

# read in temperature files
file.geo <- "U:/projects/NorthStar2023/1km_modis_mean_maps/lst"
pngOut <- "U:/projects/NorthStar2023/1km_modis_mean_maps/png"
geo.file.names <- list.files(file.geo)
geo.file.names <- geo.file.names[!geo.file.names %in% "geotiff"]

pb <- txtProgressBar(min=0, max=length(geo.file.names), style=3)
pb.ind=1

for(i in geo.file.names){
  city.id <- stringr::str_split_i(i,"_",1)
  
  temp <- brick(file.path(file.geo, i))
  temp.mean.p <- rasterToPoints(temp)
  temp.mean.df <- data.frame(temp.mean.p)
  names(temp.mean.df) <- c("x", "y", "LST")
  
  #conversion to fahrenheit
  temp.mean.df$LST.k <- (temp.mean.df$LST - 273.15) * (9/5) + 32 
  
  temp.mean.gg <- ggplot(data=temp.mean.df[!is.na(temp.mean.df),]) +
    geom_raster(aes(x= x, y=y, fill=LST.k)) +
    scale_fill_gradient(low = "#67a9cf", high="#ef8a62") +
    labs(x = "Longitude", y = "Latitude", fill="LST (\u00B0F)", title = "Avg. July - Aug. Maximum Temperature") + 
    guides(fill = guide_colorbar(barwidth = 10, barheight = 1)) +        
    theme(panel.background = element_rect(fill = "white"),
          legend.title = element_text(size=14, face="bold"),
          panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          strip.text=element_text(face="bold", size=20),
          legend.position = "top",
          axis.title.x=element_text(color="black", size=14, face="bold"), 
          axis.title.y=element_text(color="black", size=14,face="bold"),
          panel.border = element_rect(colour = "black", fill=NA),
          plot.title = element_text(face="bold", size = 14)) 
  
  ggsave(temp.mean.gg, file=file.path(pngOut,"lst", paste0(city.id, "_", "LST","_meanMap",".png")), width = 8, height = 8, dpi=300, units = "in")
  
  # png(filename = file.path(pngOut,"lst", paste0(city.id, "_", "LST","_meanMap",".png")), height= 8, width= 8, unit="in", res=300)
  #   temp.mean.gg
  # dev.off()
  # 
  setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
}
# read in vegetation files
file.geo <- "U:/projects/NorthStar2023/1km_modis_mean_maps/vegetation"
pngOut <- "U:/projects/NorthStar2023/1km_modis_mean_maps/png"
geo.file.names <- list.files(file.geo)
geo.file.names <- geo.file.names[grep("PercentTree",geo.file.names, ignore.case = T)]

pb <- txtProgressBar(min=0, max=length(geo.file.names), style=3)
pb.ind=1

for(i in geo.file.names){
  city.id <- stringr::str_split_i(i,"_",1)
  measure.id1 <- stringr::str_split_i(i,"[.]",1)
  measure.id2 <- paste(stringr::str_split_i(measure.id1,"_",2), stringr::str_split_i(measure.id1,"_",3), sep="_")
  measure.id3 <- stringr::str_split_i(measure.id1,"_",3)
  
  temp <- brick(file.path(file.geo, i))
  temp.mean.p <- rasterToPoints(temp)
  temp.mean.df <- data.frame(temp.mean.p)
  names(temp.mean.df) <- c("x", "y", "cover")
  
  
  temp.mean.gg <- ggplot(data=temp.mean.df[!is.na(temp.mean.df),]) +
    geom_raster(aes(x= x, y=y, fill=cover)) +
    scale_fill_gradient(low = "#d8b365", high="#5ab4ac") +
    labs(x = "Longitude", y = "Latitude", fill="Tree Cover (%)", title = "Avg. July - Aug. Tree Cover") + 
    guides(fill = guide_colorbar(barwidth = 10, barheight = 1)) +        
    theme(panel.background = element_rect(fill = "white"),
          legend.title = element_text(size=14, face="bold"),
          panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
          strip.text=element_text(face="bold", size=20),
          legend.position = "top",
          axis.title.x=element_text(color="black", size=14, face="bold"), 
          axis.title.y=element_text(color="black", size=14,face="bold"),
          panel.border = element_rect(colour = "black", fill=NA),
          plot.title = element_text(face="bold", size = 14)) 
  
  ggsave(temp.mean.gg, file=file.path(pngOut,"vegetation", paste0(city.id, "_", measure.id2,"_meanMap",".png")), width = 8, height = 8, dpi=300, units = "in")
  
  # png(filename = file.path(pngOut,"lst", paste0(city.id, "_", "LST","_meanMap",".png")), height= 8, width= 8, unit="in", res=300)
  #   temp.mean.gg
  # dev.off()
  # 
  setTxtProgressBar(pb, pb.ind); pb.ind=pb.ind+1
}



# # Fairbanks
# testLST <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA1105_LST_Day_Tmean.tif")
# plot(testLST)
# 
# testTree <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA1105_Vegetation_PercentTree.tif")
# plot(testTree)
# 
# datTreeFair <- cbind(coordinates(testTree), getValues(testTree))
# summary(datTreeFair)
# 
# # Chicago: 26687
# lstChi <- brick("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA26687_LST_Day_Tmean.tif")
# lstChi
# plot(lstChi)
# 
# treeChi <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA26687_Vegetation_PercentTree.tif")
# plot(treeChi)
# datTreeChi <- cbind(coordinates(treeChi), getValues(treeChi))
# summary(datTreeChi)
# 
# 
# # SanJose/SanFran 33781
# lstSF <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA33781_LST_Day_Tmean.tif")
# plot(lstSF)
# 
# treeSF <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA33781_Vegetation_PercentTree.tif")
# plot(treeSF)
# 
# 
# 
# # Example of calculating quick mean tree Cover
# treeChi <- brick("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA26687_Vegetation_PercentTree.tif")
# plot(treeChi)
# 
# fileOut <- "~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/quickMeans"
# if(!dir.exists(fileOut)) dir.create(fileOut, recursive=T)
# setwd(fileOut)
# 
# treeChiMean <- mean(treeChi)
# 
# writeRaster(treeChiMean, filename="USA26687_Vegetation_PercentTree_Means.tif", format="GTiff")
# 
# 
# test <- raster("USA26687_Vegetation_PercentTree_Means.tif")
# test
