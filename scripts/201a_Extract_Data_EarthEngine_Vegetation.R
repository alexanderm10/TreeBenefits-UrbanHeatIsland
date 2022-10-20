# Migrating the Trees & Urban Heat Island workflow to using Google Earht Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initalizing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- "/Volumes/GoogleDrive/My Drive"
GoogleFolderSave <- "UHI_Analysis_Output"

##################### 
# 0. Set up some choices for data quality thresholds
##################### 
yr.analy <- 2001:2020
thresh.sigma <- 6 # Use 6-sigma outliers for the data filtering\
thresh.pts <- 50
thresh.prop <- 0.5 # The proportion of data needed for a time point to be "good"; currenlty 0.5
overwrite=F
##################### 


##################### 
# 0. Set up helper functions
##################### 
addTime <- function(image){
  return(image$addBands(image$metadata('system:time_start')$divide(1000 * 60 * 60 * 24 * 365)))
}

setYear <- function(img){
  return(img$set("year", img$date()$get("year")))
}
##################### 


##################### 
# 1. Load and select cities
#####################
sdei <- ee$FeatureCollection('users/crollinson/sdei-global-uhi-2013');
# print(sdei.first())

# Right now, just set all cities with >100k people in the metro area and at least 100 sq km in size
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2)) 
# ee_print(citiesUse) # Thsi function gets the summary stats; this gives us 2,682 cities

# Use map to go ahead and create the buffer around everything
citiesUse <- citiesUse$map(function(f){f$buffer(10e3)})

## Just testing to make sure it works
# popLarge <- citiesUse$filter(ee$Filter$gte('ES00POP', 1e6))$filter(ee$Filter$gte('SQKM_FINAL', 1e2))
# ee_print(popLarge) # 389 cities
# Map$addLayer(popLarge)
# citiesBuff <- popLarge

##################### 

##################### 
# 2. Load in data layers 
####################
# -----------
# 2.a - Land Surface Temperature
# -----------
# 2.a.1 - Northern Hemisphere: July/August
tempJulAug <- ee$ImageCollection('MODIS/006/MOD11A2')$filter(ee$Filter$dayOfYear(181, 240))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime);
tempJulAug <- tempJulAug$map(setYear)
# ee_print(tempJulAug)
# tempJulAug$first()$propertyNames()$getInfo()
# ee_print(tempJulAug$first())
# Map$addLayer(tempJulAug$first()$select('LST_Day_1km'), vizTempK, "Jul/Aug Temperature")

projLST = tempJulAug$select("LST_Day_1km")$first()$projection()
projCRS = projLST$crs()
projTransform <- unlist(projLST$getInfo()$transform)

# ee_print(projLST)
# -----------

# -----------
# 2.b MODIS Tree Data
# -----------
mod44b <- ee$ImageCollection('MODIS/006/MOD44B')$filter(ee$Filter$date("2001-01-01", "2020-12-31"))
mod44b <- mod44b$map(setYear)
# ee_print(mod44b)
# Map$addLayer(mod44b$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')


# This seems to work, but seems to be very slow
mod44bReproj = mod44b$map(function(img){
  return(img$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST))
})$map(addTime); # add year here!

# Create a noVeg Mask
vegMask <- mod44bReproj$first()$select("Percent_Tree_Cover", "Percent_NonTree_Vegetation", "Percent_NonVegetated")$reduce('sum')$gt(50)$mask()
# ee_print(vegMask)
# Map$addLayer(vegMask)

# ee_print(mod44bReproj)
# ee_print(mod44bReproj$first())
# Map$addLayer(mod44bReproj$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')
# -----------
##################### 


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
citiesList <- citiesUse$toList(ncities)
print(citiesList$size()$getInfo())

### FOR LOOP STARTS HERE
tree.done <- dir(file.path(path.google, GoogleFolderSave), "PercentTree.tif")
other.done <- dir(file.path(path.google, GoogleFolderSave), "PercentOtherVeg.tif")
bare.done <- dir(file.path(path.google, GoogleFolderSave), "PercentNoVeg.tif")

# i=0
pb <- txtProgressBar(min=0, max=ncities, style=3)
for(i in (seq_len(citiesList$length()$getInfo()) - 1)){
  setTxtProgressBar(pb, i)
  # cityNow <- citiesUse$filter('NAME=="Chicago"')$first()
  cityNow <- ee$Feature(citiesList$get(i))
  # cityNow$first()$propertyNames()$getInfo()
  cityID <- cityNow$get("ISOURBID")$getInfo()
  # cityName <- cityNow$get("NAME")$getInfo()
  # print(cityName)
  # Map$centerObject(cityNow) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
  # Map$addLayer(cityNow)
  
  if(!overwrite & all(any(grepl(cityID, tree.done)), any(grepl(cityID, other.done)), any(grepl(cityID, bare.done)))) next
  #-------
  
  
  #-------
  # Extracting vegetation cover -- we've already masked places where veg/non-veg cover doesn't add up
  #-------
  yrMod <- ee$List(mod44bReproj$aggregate_array("year"))$distinct()
  yrString <- ee$List(paste(yrMod$getInfo()))
  # yrMod$getInfo()
  
  # Start Tree Cover Layer
  if(overwrite | !any(grepl(cityID, tree.done))){
    treeCity <- mod44bReproj$select("Percent_Tree_Cover")$map(function(img){
      return(img$clip(cityNow))
    })
    # ee_print(treeCity)
    treeCity <- ee$ImageCollection$toBands(treeCity)$rename(yrString)
    # ee_print(treeCity)
    # Map$addLayer(treeCity$select('2020_Percent_Tree_Cover'), vizTree, 'Percent Tree Cover')
    export.tree <- ee_image_to_drive(image=treeCity, description=paste0(cityID, "_Vegetation_PercentTree"), fileNamePrefix=paste0(cityID, "_Vegetation_PercentTree"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e6, crs=projCRS, crsTransform=projTransform)
    export.tree$start()
  } # End Tree Cover Layer
  
  # Start Other Veg Cover Layer
  if(overwrite | !any(grepl(cityID, tree.done))){
    vegCity <- mod44bReproj$select("Percent_NonTree_Vegetation")$map(function(img){
      return(img$clip(cityNow))
    })
    # ee_print(treeCity)
    vegCity <- ee$ImageCollection$toBands(vegCity)$rename(yrString)
    # ee_print(vegCity)
    export.veg <- ee_image_to_drive(image=vegCity, description=paste0(cityID, "_Vegetation_PercentOtherVeg"), fileNamePrefix=paste0(cityID, "_Vegetation_PercentOtherVeg"), folder="UHI_Analysis_Output", timePrefix=F, region=cityNow$geometry(), maxPixels=5e6, crs=projCRS, crsTransform=projTransform)
    export.veg$start()
  } # End Other Veg Cover Layer

  # Start No Veg Cover layer
  if(overwrite | !any(grepl(cityID, tree.done))){
    bareCity <- mod44bReproj$select("Percent_NonVegetated")$map(function(img){
      return(img$clip(cityNow))
    })
    # ee_print(treeCity)
    bareCity <- ee$ImageCollection$toBands(bareCity)$rename(yrString)
    # ee_print(bareCity)
    
    export.bare <- ee_image_to_drive(image=bareCity, description=paste0(cityID, "_Vegetation_PercentNoVeg"), fileNamePrefix=paste0(cityID, "_Vegetation_PercentNoVeg"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e6, crs=projCRS, crsTransform=projTransform)
    export.bare$start()
  } # End Write No Veg
  #-------

}

### FOR LOOP ENDS HERE
##################### 

