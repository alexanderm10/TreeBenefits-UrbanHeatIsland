# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- "/Volumes/GoogleDrive/My Drive"
GoogleFolderSave <- "UHI_Analysis_Output"

##################### 
# 0. Set up some choices for data quality thresholds
##################### 
# yr.analy <- 2001:2020
# thresh.sigma <- 6 # Use 6-sigma outliers for the data filtering\
# thresh.pts <- 50
# thresh.prop <- 0.5 # The proportion of data needed for a time point to be "good"; currenlty 0.5
overwrite=F
##################### 


##################### 
# 0. Set up helper functions
##################### 
# addTime <- function(image){
#   return(image$addBands(image$metadata('system:time_start')$divide(1000 * 60 * 60 * 24 * 365)))
# }
# 
# setYear <- function(img){
#   return(img$set("year", img$date()$get("year")))
# }
##################### 


##################### 
# 1. Load and select cities
#####################
sdei.df <- data.frame(vect("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"))
sdei.df <- sdei.df[sdei.df$ES00POP>=100e3 & sdei.df$SQKM_FINAL>=1e2,]
cityIDsAll <- sdei.df$ISOURBID

sdei <- ee$FeatureCollection('users/crollinson/sdei-global-uhi-2013');
# print(sdei.first())

# Right now, just set all cities with >100k people in the metro area and at least 100 sq km in size
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2)) 
# ee_print(citiesUse) # Thsi function gets the summary stats; this gives us 2,682 cities

# Use map to go ahead and create the buffer around everything
citiesUse <- citiesUse$map(function(f){f$buffer(10e3)})
# ee_print(citiesUse)


# projSDEI = citiesUse$geometry()$getInfo()
# projCRS = citiesUse$geometry()$crs()
# projTransform <- unlist(citiesUse$geometry()$crs())

##################### 

##################### 
# 2. Load in data layers  -- we did all the reprojeciton etc. in step 1, so this should be faster now, 
####################
modTree <- ee$Image('users/crollinson/MOD44b_1km_Reproj_Percent_Tree_Cover')
modVeg <- ee$Image('users/crollinson/MOD44b_1km_Reproj_Percent_NonTree_Vegetation')
modBare <- ee$Image('users/crollinson/MOD44b_1km_Reproj_Percent_NonVegetated')
##################### 


extractVeg <- function(CITIES, TREE, VEG, BARE, GoogleFolderSave, overwrite=F, ...){
  # CITIES needs to be a list
  # Vegetation should be the reprojected MODIS44b product with year added in
  cityseq <- seq_len(CITIES$length()$getInfo())
  pb <- txtProgressBar(min=0, max=max(cityseq), style=3)
  for(i in (cityseq - 1)){
    setTxtProgressBar(pb, i)
    # cityNow <- citiesUse$filter('NAME=="Chicago"')$first()
    cityNow <- ee$Feature(CITIES$get(i))
    # cityNow$first()$propertyNames()$getInfo()
    cityID <- cityNow$get("ISOURBID")$getInfo()
    # cityName <- cityNow$get("NAME")$getInfo()
    # print(cityName)
    # Map$centerObject(cityNow) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
    # Map$addLayer(cityNow)
    #-------
    
    
    #-------
    # Extracting vegetation cover -- we've already masked places where veg/non-veg cover doesn't add up
    #-------
    # Start Tree Cover Layer
    if(overwrite | !any(grepl(cityID, tree.done))){
      treeCity <- TREE$clip(cityNow)
      # ee_print(treeCity)
      # Map$addLayer(treeCity$select('2020_Percent_Tree_Cover'), vizTree, 'Percent Tree Cover')
      
      exportTree <- ee_image_to_drive(image=treeCity, description=paste0(cityID, "_Vegetation_PercentTree"), fileNamePrefix=paste0(cityID, "_Vegetation_PercentTree"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e6, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
      exportTree$start()
    } # End Tree Cover Layer
    
    # Start Other Veg Cover Layer
    if(overwrite | !any(grepl(cityID, tree.done))){
      vegCity <- VEG$clip(cityNow)
      # ee_print(vegCity)
      exportVeg <- ee_image_to_drive(image=vegCity, description=paste0(cityID, "_Vegetation_PercentOtherVeg"), fileNamePrefix=paste0(cityID, "_Vegetation_PercentOtherVeg"), folder="UHI_Analysis_Output", timePrefix=F, region=cityNow$geometry(), maxPixels=5e6, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
      exportVeg$start()
    } # End Other Veg Cover Layer
    
    # Start No Veg Cover layer
    if(overwrite | !any(grepl(cityID, tree.done))){
      bareCity <- VEGETATION$select("Percent_NonVegetated")$map(function(img){
        return(img$clip(cityNow))
      })
      # ee_print(treeCity)
      bareCity <- ee$ImageCollection$toBands(bareCity)$rename(yrString)
      # ee_print(bareCity)
      
      export.bare <- ee_image_to_drive(image=bareCity, description=paste0(cityID, "_Vegetation_PercentNoVeg"), fileNamePrefix=paste0(cityID, "_Vegetation_PercentNoVeg"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e6, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
      export.bare$start()
    } # End Write No Veg
    #-------
    
  }  
}

##################### 
# 3 . Start extracting data for each city
# NOTE: This will need to become a loop, but lets get it working first
# https://r-spatial.github.io/rgee/articles/rgee03.html
##################### 
# 3.1 select the city
print(citiesUse$first()$propertyNames()$getInfo())

# Figuring out how many cities we have (2682 in all)
ncities <- citiesUse$size()$getInfo()

# To co all of them
# citiesList <- citiesUse$toList(250)
# citiesList <- citiesUse$toList(ncities)
# print(citiesList$size()$getInfo())

### FOR LOOP STARTS HERE

# If we're not trying to overwrite our files, remove files that were already done
if(!overwrite){
  ### Filter out sites that have been done!
  tree.done <- dir(file.path(path.google, GoogleFolderSave), "PercentTree.tif")
  other.done <- dir(file.path(path.google, GoogleFolderSave), "PercentOtherVeg.tif")
  bare.done <- dir(file.path(path.google, GoogleFolderSave), "PercentNoVeg.tif")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  citiesDone <- unlist(lapply(strsplit(tree.done, "_"), function(x){x[1]}))
  if(length(citiesDone)>0){
    cityRemove <- vector()
    for(i in 1:length(citiesDone)){
      cityCheck <- citiesDone[i] # Check by name because it's going to change number
      cityDONE <- any(grepl(cityCheck, tree.done)) & any(grepl(cityCheck, other.done)) & any(grepl(cityCheck, bare.done))
      if(!cityDONE) next 
      cityRemove <- c(cityRemove, cityCheck)
    }
    
    cities.keep <- cityIDsAll[!cityIDsAll %in% cityRemove]
    
    citiesUse <- citiesUse$filter(ee$Filter$inList('ISOURBID', ee$List(cities.keep)))
  }
  # length(citiesDone)
  ncitiesAll <- citiesUse$size()$getInfo()
} # End remove cities loop


citiesSouth <- citiesUse$filter(ee$Filter$lt('LATITUDE', 0))
citiesNorthW <- citiesUse$filter(ee$Filter$gte('LATITUDE', 0))$filter(ee$Filter$lte('LONGITUDE', 0))
citiesNorthE1 <- citiesUse$filter(ee$Filter$gte('LATITUDE', 0))$filter(ee$Filter$gt('LONGITUDE', 0))$filter(ee$Filter$lte('LONGITUDE', 75))
citiesNorthE2 <- citiesUse$filter(ee$Filter$gte('LATITUDE', 0))$filter(ee$Filter$gt('LONGITUDE', 75))



# Figuring out how many cities we have (2682 in all)
ncitiesSouth <- citiesSouth$size()$getInfo() # 336 cities
ncitiesNorthW <- citiesNorthW$size()$getInfo() # 484 cities 
ncitiesNorthE1 <- citiesNorthE1$size()$getInfo() # 982 cities
ncitiesNorthE2 <- citiesNorthE2$size()$getInfo() # 880 cities

# To co all of them
# citiesList <- citiesUse$toList(3)
# North: 2346 total
# print(citiesSouthList$size()$getInfo())
# Map$addLayer(citiesSouth)

# lstSHFinal$first()$get("system:id")$getInfo()
# lstNHFinal$first()$get("system:id")$getInfo()

# Cities South is still running
if(ncitiesSouth>0){
  citiesSouthList <- citiesSouth$toList(ncitiesSouth)
  extractVeg(CITIES=citiesSouthList, TREE=modTree, VEG = modVeg, BARE=modBare, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

# if(ncitiesNorthW>0){
#   citiesNorthWList <- citiesNorthW$toList(ncitiesNorthW) #  total
#   extractVeg(CITIES=citiesNorthWList, VEGETATION=mod44bReproj, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
# }

if(ncitiesNorthE1>0){
  citiesNorthE1List <- citiesNorthE1$toList(ncitiesNorthE1) #  total
  extractVeg(CITIES=citiesNorthE1List, VEGETATION=mod44bReproj, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

if(ncitiesNorthE2>0){
  citiesNorthE2List <- citiesNorthE2$toList(ncitiesNorthE2) #  total
  extractVeg(CITIES=citiesNorthE2List, VEGETATION=mod44bReproj, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
  
}


### FOR LOOP ENDS HERE
##################### 

