# Migrating the Trees & Urban Heat Island workflow to using Google Earht Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initalizing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)

# chi <- readOGR("Chicago.shp")
path.out <- "../data_raw/GoogleEarthEngine"
if(!dir.exists(path.out)) dir.create(path.out, recursive=T)

##################### 
# 0. Set up some choices for data quality thresholds
##################### 
yr.analy <- 2001:2020
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

setYear <- function(img){
  return(img$set("year", img$date()$get("year")))
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

trendviz = list(
  bands='scale',
  min=-3,
  max=3,
  palette=c('red', 'yellow', 'gray','cyan', 'blue'))

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
tempJulAug <- tempJulAug$map(setYear)
# ee_print(tempJulAug)
# tempJulAug$first()$propertyNames()$getInfo()
# ee_print(tempJulAug$first())
# Map$addLayer(tempJulAug$first()$select('LST_Day_1km'), vizTempK, "Jul/Aug Temperature")

# 2.a.2 - Souther Hemisphere: Jan/Feb
tempJanFeb <- ee$ImageCollection('MODIS/006/MOD11A2')$filter(ee$Filter$dayOfYear(1, 60))$filter(ee$Filter$date("2001-01-01", "2020-12-31"))$map(addTime);
tempJanFeb <- tempJanFeb$map(lstConvert)
tempJanFeb <- tempJulAug$map(setYear)
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
mod44b <- ee$ImageCollection('MODIS/006/MOD44B')$filter(ee$Filter$date("2001-01-01", "2020-12-31"))
mod44b <- mod44b$map(setYear)
# ee_print(mod44b)
# Map$addLayer(mod44b$select('Percent_Tree_Cover')$first(), vizTree, 'Percent Tree Cover')


# This seems to work, but seems to be very slow
mod44bReproj = mod44b$map(function(img){
  return(img$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST))
})$map(addTime); # add year here!

# mod44bReproj = mod44b$map(function(img){
#   return(img$reproject(projLST))
# })$map(addTime); # add year here!


# Create a noVeg Mask
vegMask <- mod44bReproj$first()$select("Percent_Tree_Cover", "Percent_NonTree_Vegetation", "Percent_NonVegetated")$reduce('sum')$gt(50)$mask()
# ee_print(vegMask)
# Map$addLayer(vegMask)

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
# elevReproj <- elev$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST)
# ee_print(elevReprojA)

# Need to use this version of reproject :shrug:
elevReproj <- elev$reproject(projLST)
elevReproj <- elevReproj$updateMask(vegMask)
# ee_print(elevReproj)
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


# Figuring out how many cities we have (2682 in all)
ncities <- citiesTest$size()$getInfo()
# ncities <- citiesUse$size()$getInfo()

# To co all of them
# citiesList <- citiesUse$toList(3)
citiesList <- citiesUse$toList(ncities)
print(citiesList$size()$getInfo())

elevCities <- elevReproj$clipToCollection(citiesTest)
Map$addLayer(elevCities, list(min=0, max=5000))
# elevCities <- citiesTest$map(function(feat){
#   return(elevReproj$clip(feat))
# })
# elevCities
# Map$addLayer(elevCities$first(), list(min=-5, max=5000))

### FOR LOOP STARTS HERE
# i=0
pb <- txtProgressBar(min=0, max=ncities, style=3)
for(i in (seq_len(citiesList$length()$getInfo()) - 1)){
  setTxtProgressBar(pb, i)
  # cityNow <- citiesBuff$filter('NAME=="Chicago"')$first()
  cityNow <- ee$Feature(citiesList$get(i))
  # cityNow$first()$propertyNames()$getInfo()
  cityID <- cityNow$get("ISOURBID")$getInfo()
  cityName <- cityNow$get("NAME")$getInfo()
  print(cityName)
  # Map$centerObject(cityNow)
  # Map$addLayer(cityNow)

  pathCity <- file.path(path.out, cityID)
  dir.create(file.path(pathCity, "elev"), recursive=T, showWarnings=F)
  dir.create(file.path(pathCity, "VegCover"), recursive=T, showWarnings=F)
  dir.create(file.path(pathCity, "LST_1km_Day"), recursive=T, showWarnings=F)
  dir.create(file.path(pathCity, "LST_1km_Day_Dev"), recursive=T, showWarnings=F)
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
  if(npts.elev<thresh.pts) next
  # ee_imagecollection_to_local(ic=elevCity, region=cityNow$geometry(), scale=1e3, dsn=file.path(pathCity, "elev", paste0(cityID, "_elevation")))
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
  # modCity$first()$get("year")$getInfo()
  #-------
  
  
  
  #-------
  # Now doing Land Surface Temperature
  #-------
  cityLat <- cityNow$get("LATITUDE")$getInfo()
  
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
  # tempCityAll$first()$get("year")$getInfo()
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
  tempCityAll <- tempCityAll$map(function(img){
    tempMed <- img$select('LST_Day_1km')$reduceRegion(reducer=ee$Reducer$median(), geometry=cityNow$geometry(), scale=1e3)
    tempDev <- img$select('LST_Day_1km')$subtract(ee$Number(tempMed$get('LST_Day_1km')))$rename('LST_Day_Dev')$toFloat()
    return(img$addBands(tempDev))
  })
  # print("Temperature Deviation (Raw)", tempCityAll);
  # Map$addLayer(tempCityAll$first()$select('LST_Day_Dev'), vizTempAnom, 'Surface Temperature - Anomaly');
  # devList = tempCityAll.toList(tempCityAll.size())
  # Map.addLayer(ee.Image(devList.get(12)).select('LST_Day_Dev'), vizTempAnom, 'Surface Temperature - Anomaly');
  ## ----------------
  
  
  ## ----------------
  # Now lets do our annual means
  ## ----------------
  # Only iterate through years with some data! 
  yrList <- ee$List(tempCityAll$aggregate_array("year"))$distinct()
  
  tempYrMean <- yrList$map(ee_utils_pyfunc(function(j){
    YR <- ee$Number(j);
    START <- ee$Date$fromYMD(YR,1,1);
    END <- ee$Date$fromYMD(YR,12,31);
    lstYR <- tempCityAll$filter(ee$Filter$date(START, END))
    # // var lstDev =  // make each layer an anomaly map
    tempMean <- lstYR$select('LST_Day_1km')$reduce(ee$Reducer$mean())
    # tempDev <- lstYR$select('LST_Day_Dev')$reduce(ee$Reducer$mean())
    tempAgg <- ee$Image(tempMean)

     ## ADD YEAR AS A PROPERTY!!
    tempAgg <- tempAgg$set(ee$Dictionary(list(year=YR)))
    tempAgg <- tempAgg$set(ee$Dictionary(list(`system:index`=YR$format("%03d"))))
    # ee_print(tempAgg)
    # Map$addLayer(tempAgg$select('LST_Day_1km_mean'), vizTempK, 'Mean Surface Temperature (K)');
    # Map$addLayer(tempAgg$select('LST_Day_Dev_mean'), vizTempAnom, 'Mean Surface Temperature - Anomaly');
    
    return (tempAgg); # update to standardized once read
    
  }))
  tempYrMean <- ee$ImageCollection$fromImages(tempYrMean) # go ahead and overwrite it since we're just changing form
  # ee_print(tempYrMean)
  # tempYrMean$first()$id()$getInfo()
  # ee_print(tempYrMean$first()$id()$getInfo())
  # Map$addLayer(tempYrMean$select('LST_Day_1km_mean')$first(), vizTempK, 'Mean Surface Temperature (K)');
  
  
  tempYrDev <- yrList$map(ee_utils_pyfunc(function(j){
    YR <- ee$Number(j);
    START <- ee$Date$fromYMD(YR,1,1);
    END <- ee$Date$fromYMD(YR,12,31);
    lstYR <- tempCityAll$filter(ee$Filter$date(START, END))
    tempDev <- lstYR$select('LST_Day_Dev')$reduce(ee$Reducer$mean())
    tempAgg <- ee$Image(tempDev)
    
    ## ADD YEAR AS A PROPERTY!!
    tempAgg <- tempAgg$set(ee$Dictionary(list(year=YR)))
    tempAgg <- tempAgg$set(ee$Dictionary(list(`system:index`=YR$format("%03d"))))
    # ee_print(tempAgg)
    # Map$addLayer(tempAgg$select('LST_Day_1km_mean'), vizTempK, 'Mean Surface Temperature (K)');
    # Map$addLayer(tempAgg$select('LST_Day_Dev_mean'), vizTempAnom, 'Mean Surface Temperature - Anomaly');
    
    return (tempAgg); # update to standardized once read
    
  }))
  tempYrDev <- ee$ImageCollection$fromImages(tempYrDev) # go ahead and overwrite it since we're just changing form
  # Map$addLayer(tempYr$select('LST_Day_Dev_mean')$first(), vizTempAnom, 'Mean Surface Temperature - Anomaly');

    # ee_imagecollection_to_local(ic=tempYrMean, region=cityNow$geometry(), scale=1e3, dsn="~/Desktop/EarthEngine_TEST_")
  # testRast <- ee_as_raster(tempYrMean$first())
  # Map$addLayer(tempYr$select('LST_Day_1km_mean')$first(), vizTempK, 'Mean Surface Temperature (K)');
  ## ----------------

  
  
  ## ----------------
  ## Write everythign out here
  ## ----------------
  # ee_as_raster(image=elevCity, region=cityNow$geometry(), scale=1e3, dsn=file.path(pathCity, "elev", paste0(cityID, "_elevation")))
  export.elev <- ee_image_to_drive(image=elevCity, fileNamePrefix =paste0(cityID, "_elevation"), folder="rgee_test")
  export.elev$start()
  ee_monitoring(export.elev)
  
  
  ee_imagecollection_to_local(ic=modCity, region=cityNow$geometry(), scale=1e3, dsn=file.path(pathCity, "VegCover", paste0(cityID, "_vegetation_")))
  ee_imagecollection_to_local(ic=tempYrMean, region=cityNow$geometry(), scale=1e3, dsn=file.path(pathCity, "LST_1km_Day", paste0(cityID, "_LSTday_")))
  ee_imagecollection_to_local(ic=tempYrDev, region=cityNow$geometry(), scale=1e3, dsn=file.path(pathCity, "LST_1km_Day_Dev", paste0(cityID, "_LSTdev_")))
  ## ----------------
  
  ## ----------------
  ## Calculate some of the simple stats & trends; store it in a data frame
  ## ----------------
  # # meanTree <- modCity$select("Percent_Tree_Cover")$reduce('mean')$reduceRegions(ee$Reducer$mean(), geometry=cityNow$geometry())
  # trendTree <- modCity$select("system:time_start", "Percent_Tree_Cover")$reduce(ee$Reducer$linearFit())
  # ee_print(trendTree)
  
  # Calcualte: 1) Mean Trend w/ SD; 2) % area increasing, decreasing
  
  #  Reducing the temporal trend to get regional averages, etc.
  # minMaxDictionary <- trendTree$reduceRegion(
  #   reducer=ee$Reducer$minMax(),
  #   geometry=cityNow$geometry(),
  #   scale=1e3
  # );
  # minMaxDictionary$getInfo()
  # 
  # meanDictionary <- trendTree$reduceRegion(
  #   reducer=ee$Reducer$mean(),
  #   geometry=cityNow$geometry(),
  #   scale=1e3
  # );
  # meanDictionary$getInfo()
  
  # // var SlopeMinMax = trendTree.select('scale').reduceRegion(ee.Reducer.minMax())
  # // print("slopeVals", SlopeMinMax)
  # // print(trendTree.abs().select('scale').reducer(ee.Reducer.max()))
  
  # // visualizing the trend
  # Map$addLayer(trendTree, trendviz, "Tree Trend")
  
  
  # modallchi.select('system:time_start', "Percent_Tree_Cover").reduce(ee.Reducer.linearFit())
  ## ----------------
  
  
}

### FOR LOOP ENDS HERE
##################### 

