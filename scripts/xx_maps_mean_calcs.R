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

# Fairbanks
testLST <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA1105_LST_Day_Tmean.tif")
plot(testLST)

testTree <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA1105_Vegetation_PercentTree.tif")
plot(testTree)

datTreeFair <- cbind(coordinates(testTree), getValues(testTree))
summary(datTreeFair)

# Chicago: 26687
lstChi <- brick("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA26687_LST_Day_Tmean.tif")
lstChi
plot(lstChi)

treeChi <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA26687_Vegetation_PercentTree.tif")
plot(treeChi)
datTreeChi <- cbind(coordinates(treeChi), getValues(treeChi))
summary(datTreeChi)


# SanJose/SanFran 33781
lstSF <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA33781_LST_Day_Tmean.tif")
plot(lstSF)

treeSF <- raster("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA33781_Vegetation_PercentTree.tif")
plot(treeSF)



# Example of calculating quick mean tree Cover
treeChi <- brick("~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/geotiff_test/USA26687_Vegetation_PercentTree.tif")
plot(treeChi)

fileOut <- "~/Google Drive/Shared drives/Urban Ecological Drought/UHI_northstar2023/quickMeans"
if(!dir.exists(fileOut)) dir.create(fileOut, recursive=T)
setwd(fileOut)

treeChiMean <- mean(treeChi)

writeRaster(treeChiMean, filename="USA26687_Vegetation_PercentTree_Means.tif", format="GTiff")


test <- raster("USA26687_Vegetation_PercentTree_Means.tif")
test
