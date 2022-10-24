## 
## NEED TO GO AHEAD AND SPLIT INTO N/S Cities since that matters here!!
## 
# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
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
projCRS = projLST$crs()
projTransform <- unlist(projLST$getInfo()$transform)
# ee_print(projLST)
# -----------

# -----------
# 2.b MODIS Tree Data -- Still need this to get the veg mask!
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
# ee_print(mod44bReproj$first())
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
# -----------
##################### 

## Making the workflow a function that we can then feed N/S data to
# Cities needs to be an EarthEngine Feature List
extractTempEE <- function(CITIES, TEMPERATURE, GoogleFolderSave, overwrite=F, ...){
  cityseq <- seq_len(CITIES$length()$getInfo())
  pb <- txtProgressBar(min=0, max=max(cityseq), style=3)
  for(i in (cityseq - 1)){
    setTxtProgressBar(pb, i)
    # cityNow <- citiesBuff$filter('NAME=="Chicago"')$first()
    cityNow <- ee$Feature(CITIES$get(i))
    # cityNow$first()$propertyNames()$getInfo()
    cityID <- cityNow$get("ISOURBID")$getInfo()
    # Map$centerObject(cityNow) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
    
    # cityName <- cityNow$get("NAME")$getInfo()
    # print(cityName)
    # Map$addLayer(cityNow)
    # if(!overwrite & all(any(grepl(cityID, elev.done)), any(grepl(cityID, tmean.done)), any(grepl(cityID, tdev.done)))) next
    
    
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
    
    # Save elevation only if it's worth our while -- Note: Still doing the extraction & computation first since we use it as our base
    if(overwrite | !any(grepl(cityID, elev.done))){
      export.elev <- ee_image_to_drive(image=elevCity, description=paste0(cityID, "_elevation"), fileNamePrefix=paste0(cityID, "_elevation"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e6, crs=projCRS, crsTransform=projTransform)
      export.elev$start()
      # ee_monitoring(export.elev)
    }
    #-------

    #-------
    # Now doing Land Surface Temperature
    #-------
    # Map$addLayer(tempHemi$first()$select('LST_Day_1km'), vizTempK, "Raw Surface Temperature")
    
    # JUST GET AN MASK THE RAW DATA FIRST
    tempCityAll <- TEMPERATURE$map(function(img){
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
    setNPts <- function(img){
      npts.now <- img$select("LST_Day_1km")$reduceRegion(reducer=ee$Reducer$count(), geometry=cityNow$geometry(), scale=1e3)
      p.now <- list(p_Pts=ee$Number(npts.now$get("LST_Day_1km"))$divide(npts.elev))
      p.now <- ee$Dictionary(p.now)
      
      # test <- img$set("n_Pts", npts.now$get("LST_Day_1km"))$set("p_Pts", p.now$get("p_Pts"))
      # test$get("p_Pts")$getInfo()
      return(img$set("n_Pts", npts.now$get("LST_Day_1km"))$set("p_Pts", p.now$get("p_Pts")))
    }
    
    
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
    yrString2 <- ee$List(paste(yrList$getInfo()))
    
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
    tempYrMean <- ee$ImageCollection$toBands(tempYrMean)$rename(yrString2)
    tempYrMean <- tempYrMean$setDefaultProjection(projLST)
    # ee_print(tempYrMean)
    # Map$addLayer(tempYrMean$select('2020'), vizTempK, 'Mean Surface Temperature (K)');
    
    export.TempMean <- ee_image_to_drive(image=tempYrMean, description=paste0(cityID, "_LST_Day_Tmean"), fileNamePrefix=paste0(cityID, "_LST_Day_Tmean"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e6, crs=projCRS, crsTransform=projTransform)
    export.TempMean$start()
    # ee_monitoring(export.TempMean)
    
    
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
    tempYrDev <- ee$ImageCollection$toBands(tempYrDev)$rename(yrString2)
    tempYrDev <- tempYrDev$setDefaultProjection(projLST)
    
    export.TempDev <- ee_image_to_drive(image=tempYrDev, description=paste0(cityID, "_LST_Day_Tdev"), fileNamePrefix=paste0(cityID, "_LST_Day_Tdev"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e6)
    export.TempDev$start()
    # ee_monitoring(export.TempDev)
    ## ----------------
  } # End Loop
  
}


##################### 
# 3 . Start extracting data for each city
# NOTE: This will need to become a loop, but lets get it working first
# https://r-spatial.github.io/rgee/articles/rgee03.html
##################### 
# 3.1 select the city
print(citiesUse$first()$propertyNames()$getInfo())


# If we're not trying to overwrite our files, remove files that were already done
if(!overwrite){
  ### Filter out sites that have been done!
  elev.done <- dir(file.path(path.google, GoogleFolderSave), "elevation.tif")
  tmean.done <- dir(file.path(path.google, GoogleFolderSave), "Tmean.tif")
  tdev.done <- dir(file.path(path.google, GoogleFolderSave), "Tdev.tif")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  citiesDone <- unlist(lapply(strsplit(elev.done, "_"), function(x){x[1]}))
  if(length(citiesDone)>0){
    for(i in 1:length(citiesDone)){
      cityCheck <- citiesDone[i] # Check by name because it's going to change number
      cityDONE <- any(grepl(cityCheck, tmean.done)) & any(grepl(cityCheck, tdev.done))
      if(cityDONE) next 
      citiesDone <- citiesDone[citiesDone!=cityCheck]
    }
  }# length(citiesDone)
  
  for(i in 1:length(citiesDone)){
    citiesUse <- citiesUse$filter(ee$Filter$neq('ISOURBID', citiesDone[i]))
  }
  ncitiesAll <- citiesUse$size()$getInfo()
}

citiesNorth <- citiesUse$filter(ee$Filter$gte('LATITUDE', 0))
citiesSouth <- citiesUse$filter(ee$Filter$lt('LATITUDE', 0))

# Figuring out how many cities we have (2682 in all)
ncitiesNorth <- citiesNorth$size()$getInfo()
ncitiesSouth <- citiesSouth$size()$getInfo()

# To co all of them
# citiesList <- citiesUse$toList(3)
citiesNorthList <- citiesNorth$toList(ncitiesNorth)
citiesSouthList <- citiesSouth$toList(ncitiesSouth)
# print(citiesSouthList$size()$getInfo())
# Map$addLayer(citiesSouth)

extractTempEE(CITIES=citiesSouthList, TEMPERATURE=lstSHFinal, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)

extractTempEE(CITIES=citiesNorthList, TEMPERATURE=lstNHFinal, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)

### FOR LOOP ENDS HERE
##################### 

