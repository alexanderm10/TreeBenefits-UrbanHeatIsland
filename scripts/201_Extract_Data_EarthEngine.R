# Migrating the Trees & Urban Heat Island workflow to using Google Earht Engine

library(rgee); library(raster); library(rgdal); library(terra)
ee_check() # For some reason, it's important to run this before initalizing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)

# chi <- readOGR("Chicago.shp")

##################### 
# 0. Set up some choices for data quality thresholds
##################### 
thresh.sigma <- 6 # Use 6-sigma outliers for the data filtering\
thresh.pts <- 50
thresh.prop <- 0.5 # The proportion of data needed for a time point to be "good"; currenlty 0.5
##################### 


##################### 
# 0. Set up helper functions
##################### 
addTime <- function(image){
  return(image$addBands(image$metadata('system:time_start')$divide(1000 * 60 * 60 * 24 * 365)))
}

setNPts <- function(img){
  npts.now <- img$select("LST_Day_1km")$reduceRegion(reducer=ee$Reducer$count(), geometry=cityNow$geometry(), scale=1e3)
  p.now <- list(p_Pts=ee$Number(npts.now$get("LST_Day_1km"))$divide(npts.elev))
  p.now <- ee$Dictionary(p.now)
  
  # test <- img$set("n_Pts", npts.now$get("LST_Day_1km"))$set("p_Pts", p.now$get("p_Pts"))
  # test$get("p_Pts")$getInfo()
  return(img$set("n_Pts", npts.now$get("LST_Day_1km"))$set("p_Pts", p.now$get("p_Pts")))
}

lstConvert <- function(img){
  lstDay <- img$select('LST_Day_1km')$multiply(0.02)
  lstNight <- img$select('LST_Night_1km')$multiply(0.02)
  lstK <- ee$Image(c(lstDay, lstNight));
  img <- img$addBands(srcImg=lstK, overwrite=TRUE);
  return(img)
}

# // Cleaning Up and getting just Good LST data
# // Code adapted from https://gis.stackexchange.com/a/349401/5160
# // Let's extract all pixels from the input image where
# // Bits 0-1 <= 1 (LST produced of both good and other quality)
# // Bits 2-3 = 0 (Good data quality)
# // Bits 4-5 Ignore, any value is ok
# // Bits 6-7 = 0 (Average LST error ≤ 1K)
# // var lstMask = function(qcDay, lstDay){
# //   var qaMask = bitwiseExtract(qcDay, 0, 1).lte(1)
# //   var dataQualityMask = bitwiseExtract(qcDay, 2, 3).eq(0)
# //   var lstErrorMask = bitwiseExtract(qcDay, 6, 7).eq(0)
# //   var mask = qaMask.and(dataQualityMask).and(lstErrorMask)
# //   var lstDayMasked = lstDay.updateMask(mask)  
# // }
bitwiseExtract <- function(input, fromBit, toBit) {
  maskSize <- ee$Number(1)$add(toBit)$subtract(fromBit)
  mask <- ee$Number(1)$leftShift(maskSize)$subtract(1)
  return(input$rightShift(fromBit)$bitwiseAnd(mask))
}

lstMask <- function(img){
  qcDay <- img$select('QC_Day')
  qaMask <- bitwiseExtract(qcDay, 0, 1)$lte(1);
  dataQualityMask <- bitwiseExtract(qcDay, 2, 3)$eq(0)
  lstErrorMask <- bitwiseExtract(qcDay, 6, 7)$lte(2) # setting up error <=2 ˚K
  datVal <- img$select('LST_Day_1km')$gt(0)
  maskT <- qaMask$And(dataQualityMask)$And(lstErrorMask)$And(datVal)
  lstDayMasked <- img$updateMask(maskT)
  return(lstDayMasked)
}


# A bunch of visualization palettes
vizTree <- list(
  # bands: ['Percent_Tree_Cover'],
  min=0.0,
  max=100.0,
  palette=c('bbe029', '0a9501', '074b03')
);

tempColors <- c(
  '040274', '040281', '0502a3', '0502b8', '0502ce', '0502e6',
  '0602ff', '235cb1', '307ef3', '269db1', '30c8e2', '32d3ef',
  '3be285', '3ff38f', '86e26f', '3ae237', 'b5e22e', 'd6e21f',
  'fff705', 'ffd611', 'ffb613', 'ff8b13', 'ff6e08', 'ff500d',
  'ff0000', 'de0101', 'c21301', 'a71001', '911003'
)
vizTemp <- list(
  min=10.0,
  max=47.0,
  palette=tempColors
);

vizTempK <- list( 
  min=10.0+273.15,
  max=47.0+273.15,
  palette=tempColors
);

vizTempAnom <- list(
  min=-15,
  max=15,
  palette=tempColors
);

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
citiesBuff <- citiesUse$map(function(f){f$buffer(10e3)})

## Just testing to make sure it works
# popLarge <- citiesBuff$filter(ee$Filter$gte('ES00POP', 1e6))$filter(ee$Filter$gte('SQKM_FINAL', 1e2))
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
tempJulAug <- tempJulAug$map(lstConvert)
# ee_print(tempJulAug$first())
# Map$addLayer(tempJulAug$first()$select('LST_Day_1km'), vizTempK, "Jul/Aug Temperature")

# 2.a.2 - Souther Hemisphere: Jan/Feb
tempJanFeb <- ee$ImageCollection('MODIS/006/MOD11A2')$filter(ee$Filter$dayOfYear(1, 60))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime);
tempJanFeb <- tempJanFeb$map(lstConvert)
# ee_print(tempJanFeb$first())

# Filtering good LST Data --> note: we'll still do some outlier remover from each city
lstDayGoodNH <- tempJulAug$map(lstMask)
lstDayGoodSH <- tempJanFeb$map(lstMask)

# ee_print(lstDayGoodNH$first())
# Map$addLayer(lstDayGoodNH$first()$select('LST_Day_1km'), vizTempK, "GOOD Jul/Aug Temperature")
# Map$addLayer(lstDayGoodSH$first()$select('LST_Day_1km'), vizTempK, "GOOD Jan/Feb Temperature")
projLST = lstDayGoodNH$select("LST_Day_1km")$first()$projection()
# ee_print(projLST)
# -----------

# -----------
# 2.b MODIS Tree Data
# -----------
mod44b = ee$ImageCollection('MODIS/006/MOD44B')$filter(ee$Filter$date("2001-01-01", "2020-12-31"))
# ee_print(mod44b)
# Map$addLayer(mod44b$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')

## This worked yesterday, but not today!  :shrug:
# mod44bReproj = mod44b$map(function(img){
#   return(img$reduceResolution({ 
#     reducer=ee$Reducer$mean()
#   }))$reproject(projLST)
# })$map(addTime); # add year here!

# This isn't working today.  No f*ng clue hwy
mod44bReproj = mod44b$map(function(img){
  return(img$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST))
})$map(addTime); # add year here!

# mod44bReproj = mod44b$map(function(img){
#   return(img$reproject(projLST))
# })$map(addTime); # add year here!


# Create a noVeg Mask
vegMask <- mod44bReproj$first()$select("Percent_Tree_Cover", "Percent_NonTree_Vegetation", "Percent_NonVegetated")$reduce('sum')$gt(50)$mask()
ee_print(vegMask)
Map$addLayer(vegMask)

# ee_print(mod44bReproj)
ee_print(mod44bReproj$first())
# Map$addLayer(mod44bReproj$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')
# -----------

# -----------
# Update LST Data with the MODIS veg mask
# -----------
# Map$addLayer(lstDayGoodNH$first()$select('LST_Day_1km'), vizTempK, "GOOD Jul/Aug Temperature")
lstNHmask <- lstDayGoodNH$map(function(IMG){IMG$updateMask(vegMask)})
lstSHmask <- lstDayGoodSH$map(function(IMG){IMG$updateMask(vegMask)})

# ee_print(lstNHmask$first()$select("LST_Day_1km"))
lstNHFinal <- lstNHmask$map(function(IMG){
  dat <- IMG$select("LST_Day_1km")$gt(0)
  return(IMG$updateMask(dat))
})

lstSHFinal <- lstSHmask$map(function(IMG){
  dat <- IMG$select("LST_Day_1km")$gt(0)
  return(IMG$updateMask(dat))
})

# test$getInfo()
# lstNHmasl2 <- lstNHmask
# Map$addLayer(lstNHFinal$first()$select('LST_Day_1km'), vizTempK, "GOOD MASKED Jul/Aug Temperature")
# -----------


# -----------
# 2.c  - Elevation (static = easy!)
# -----------
elev <- ee$Image('USGS/SRTMGL1_003')$select('elevation')
# ee_print(elev)
# Map$addLayer(elev, list(min=-10, max=5e3))


# # I don't know why this is causing issues, but it is
elevReprojA <- elev$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST)
ee_print(elevReprojA)
# 
elevReproj <- elev$reproject(projLST)
elevReproj <- elevReproj$updateMask(vegMask)
ee_print(elevReproj)
# Map$addLayer(elevReproj$select("elevation"), list(min=-10, max=5e3))

# Map$addLayer(elevReprojA$select("elevation"), list(min=-10, max=5e3))

# -----------
##################### 


##################### 
# 3 . Start extracting data for each city
# NOTE: This will need to become a loop, but lets get it working first
# https://r-spatial.github.io/rgee/articles/rgee03.html
##################### 
# 3.1 select the city
print(citiesUse$first()$propertyNames()$getInfo())

# Trying the buffer outside of a loop
citiesTest <- citiesBuff$limit(10)
ee_print(citiesTest)
# Map$addLayer(citiesTest)

# cityMask <- ee$Image$constant(1)$clip(citiesTest$geometry())$mask()
# ee_print(cityMask)
# Map$addLayer(cityMask$first())

# Figuring otu how many cities we have (2682 in all)
ncities <- citiesTest$size()$getInfo()
# ncities <- citiesUse$size()$getInfo()

# To co all of them
# citiesList <- citiesUse$toList(3)
citiesList <- citiesUse$toList(ncities)
print(citiesList$size()$getInfo())

### FOR LOOP STARTS HERE
# for(i in (seq_len(citiesList$length()$getInfo()) - 1)){
  i=0
  cityNow <- citiesBuff$filter('NAME=="Chicago"')
  # cityNow <- ee$Feature(citiesList$get(i))
  Map$centerObject(cityNow)
  Map$addLayer(cityNow)

  # cityMask = ee$Image$constant(1)$clip(cityNow$geometry())$mask()
  # cityMask = ee$Image$constant(1)$clip(cityNow$geometry())$mask()
  # ee_print(cityMask)
  
  #-------
  # extracting elevation 
  #-------
  # elevCity <- elevReproj$updateMask(cityMask)
  elevCity <- elevReproj$clip(cityNow)
  # Map$addLayer(elevCity, list(min=-10, max=500))
  
  # elevNow <- elevCity$reduceRegion(reducer=ee$Reducer$mean(), geometry=cityNow$geometry(), scale=1e3)
  # elevNow$getInfo()
  npts.elev <- elevCity$reduceRegion(reducer=ee$Reducer$count(), geometry=cityNow$geometry(), scale=1e3)
  npts.elev <- npts.elev$getInfo()$elevation
  
  ### *** ###  If we don't have many points in the elevation file, skip the city
  #-------
  
  
  #-------
  # Extracting vegetation cover -- we've already masked places where veg/non-veg cover doesn't add up
  #-------
  modCity <- mod44bReproj$map(function(img){
    # First masking to Chicago
    tmp <- img$clip(cityNow);
    return(tmp$select("system:time_start", "Percent_Tree_Cover", "Percent_NonTree_Vegetation", "Percent_NonVegetated"))
  })
  # ee_print(modCity);
  # Map$addLayer(modCity$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover');
  #-------
  
  
  
  #-------
  # Now doing Land Surface Temperature
  #-------
  cityLat <- cityNow$first()$get("LATITUDE")$getInfo()
  
  if(cityLat>0) { tempHemi <- lstNHFinal} else { tempHemi <- lstSHFinal}
  # tempHemi
  
  # Map$addLayer(tempHemi$first()$select('LST_Day_1km'), vizTempK, "Raw Surface Temperature")
  
  # JUST GET AN MASK THE RAW DATA FIRST
  tempCityAll <- tempHemi$map(function(img){
    tempNow <- img$clip(cityNow)
    # dat <- tempNow$gt(0)
    # tempNow <- tempNow$updateMask(dat)
    return(tempNow)
  })
  # ee_print(tempCityAll)
  # Map$addLayer(tempCityAll$first()$select('LST_Day_1km'), vizTempK, "Raw Surface Temperature")
  
  ## ----------------
  ## Need to remove layers that don't meet our requirements right off the bat
  ##   -- because we can't compute means etc when there are no good values
  ##   -- resource from Ren https://gis.stackexchange.com/questions/276791/removing-images-based-on-band-threshold-using-google-earth-engine
  ## ----------------
  tempCityAll <- tempCityAll$map(setNPts)
  # ee_print(tempCityAll$first())
  # print(tempCityAll$first()$get("n_Pts")$getInfo())
  # print(tempCityAll$first()$get("p_Pts")$getInfo())

  tempCityAll <- tempCityAll$filter(ee$Filter$gte("n_Pts", thresh.pts)) # have at least 50 points (see top of script)
  tempCityAll <- tempCityAll$filter(ee$Filter$gte("p_Pts", thresh.prop)) # Have at least 50% of the data
  # ee_print(tempCityAll)
  # Map$addLayer(tempCityAll$first()$select('LST_Day_1km'), vizTempK, "Raw Surface Temperature")
  ## ----------------
  
  ## ----------------
  # Now remove outliers
  # #  NOTE : THIS DOESN"T WORK YET!
  # Once it DOES work, we can re-run the the setNPts
  ## ----------------
  lstOutliers <- function(img){
    # Calculate the means & sds for the region
    tempStats <- img$select("LST_Day_1km")$reduceRegion(reducer=ee$Reducer$mean()$combine(
      reducer2=ee$Reducer$stdDev(), sharedInputs=T),
      geometry=cityNow$geometry(), scale=1e3)
    
    # Cacluate the key numbers for our sanity
    tmean <- ee$Number(tempStats$get("LST_Day_1km_mean"))
    tsd <- ee$Number(tempStats$get("LST_Day_1km_stdDev"))
    thresh <- tsd$multiply(thresh.sigma)
    
    # Do the filtering
    dat.low <- img$gte(tmean$subtract(thresh))
    dat.hi <- img$lte(tmean$add(thresh))
    img <- img$updateMask(dat.low)
    img <- img$updateMask(dat.hi)
    
    # Map$addLayer(tempNow$select('LST_Day_1km'), vizTempK, "Raw Surface Temperature")
    return(img)
  }
  
  tempCityAll <- tempCityAll$map(lstOutliers)
  # ee_print(tempCityAll)
  # Map$addLayer(tempCityAll$first()$select('LST_Day_1km'), vizTempK, "Raw Surface Temperature")
  
  tempCityAll <- tempCityAll$map(setNPts)
  # print(tempCityAll$first()$get("n_Pts")$getInfo())
  # print(tempCityAll$first()$get("p_Pts")$getInfo())
  
  tempCityAll <- tempCityAll$filter(ee$Filter$gte("n_Pts", thresh.pts)) # have at least 50 points (see top of script)
  tempCityAll <- tempCityAll$filter(ee$Filter$gte("p_Pts", thresh.prop)) # Have at least 50% of the data
  # ee_print(tempCityAll)
  ## ----------------
  
  ## ----------------
  # Now calculate Temperature Deviations
  ## ----------------
  
  ## ----------------
  ## ----------------
  
# }

### FOR LOOP ENDS HERE
##################### 

