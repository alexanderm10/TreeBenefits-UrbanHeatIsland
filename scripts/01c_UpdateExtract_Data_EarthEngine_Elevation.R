# Create a script to redo the elevation for anything already done; then we can start the temperature extractions again

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
overwrite=T
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

# -----------
# 2.c  - Elevation (static = easy!)
## NEED TO MOVE TO A DIFFERENT ELEVATION DATASET to get cities far N! 
## var jaxa = ee.ImageCollection('JAXA/ALOS/AW3D30/V3_2');
## var elev3 = jaxa.select('DSM');

# -----------
# elev <- ee$Image('USGS/SRTMGL1_003')$select('elevation')
elev <- ee$ImageCollection('JAXA/ALOS/AW3D30/V3_2')$select("DSM")
proj.elev <- elev$first()$select(0)$projection()
elev <- elev$mosaic()$setDefaultProjection(proj.elev) # Mosaic everythign into 1 image that we can then reproject etc.
# ee_print(elev)
# Map$addLayer(elev$first(), list(min=-10, max=5e3))


# # I don't know why this is causing issues, but it is
# elevReproj <- elev$reduceResolution(reducer=ee$Reducer$mean())$reproject(projLST)
# ee_print(elevReprojA)

# Need to use this version of reproject :shrug:
elevReproj <- elev$reproject(projLST)
elevReproj <- elevReproj$updateMask(vegMask)
# ee_print(elevReproj)
# Map$addLayer(elevReproj$select("DSM"), list(min=-10, max=5e3))
##################### 


extractElevEE <- function(CITIES, GoogleFolderSave, overwrite=F, ...){
  cityseq <- seq_len(CITIES$length()$getInfo())
  pb <- txtProgressBar(min=0, max=max(cityseq), style=3)
  for(i in (cityseq - 1)){
    setTxtProgressBar(pb, i)
    # cityNow <- citiesUse$filter('NAME=="Chicago"')$first()
    cityNow <- ee$Feature(CITIES$get(i))
    # cityNow$first()$propertyNames()$getInfo()
    cityID <- cityNow$get("ISOURBID")$getInfo()
    # Map$centerObject(cityNow) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
    
    #-------
    # extracting elevation -- 
    #  NOTE: Doing outlier removal because there are some known issues with a couple points: https://developers.google.com/earth-engine/datasets/catalog/JAXA_ALOS_AW3D30_V3_2
    #-------
    # elevCity <- elevReproj$updateMask(cityMask)
    elevCity <- elevReproj$clip(cityNow)
    # Map$addLayer(elevCity, list(min=-10, max=500))
    
    elevOutlier <- function(img){
      # Calculate the means & sds for the region
      elevStats <- img$select("DSM")$reduceRegion(reducer=ee$Reducer$mean()$combine(
        reducer2=ee$Reducer$stdDev(), sharedInputs=T),
        geometry=cityNow$geometry(), scale=1e3)
      
      # Cacluate the key numbers for our sanity
      elevMean <- ee$Number(elevStats$get("DSM_mean"))
      elevSD <- ee$Number(elevStats$get("DSM_stdDev"))
      thresh <- elevSD$multiply(thresh.sigma)
      
      # Do the filtering
      dat.low <- img$gte(elevMean$subtract(thresh))
      dat.hi <- img$lte(elevMean$add(thresh))
      img <- img$updateMask(dat.low)
      img <- img$updateMask(dat.hi)
      
      # Map$addLayer(img$select('DSM'))
      return(img)
    }
    
    elevCity <- elevOutlier(elevCity)
    
    npts.elev <- elevCity$reduceRegion(reducer=ee$Reducer$count(), geometry=cityNow$geometry(), scale=1e3)
    npts.elev <- npts.elev$getInfo()$DSM
    
    ### *** ###  If we don't have many points in the elevation file, skip the city
    if(npts.elev<thresh.pts){ 
      print(warning(paste("Not enough Elevation Points; skipping : ", cityID)))
      next
    }
    
    # Save elevation only if it's worth our while -- Note: Still doing the extraction & computation first since we use it as our base
    if(overwrite | !any(grepl(cityID, elev.done))){
      export.elev <- ee_image_to_drive(image=elevCity, description=paste0(cityID, "_elevation"), fileNamePrefix=paste0(cityID, "_elevation"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e6, crs=projCRS, crsTransform=projTransform)
      export.elev$start()
      # ee_monitoring(export.elev)
    }
    #-------
  } # End i loop
} # End function


##################### 
# 3 . Start extracting data for each city -- only ones that were done before!
##################### 
print(citiesUse$first()$propertyNames()$getInfo())

elev.done <- dir(file.path(path.google, GoogleFolderSave), "elevation.tif")

citiesDone <- unlist(lapply(strsplit(elev.done, "_"), function(x){x[1]}))

citiesUse <- citiesUse$filter(ee$Filter$inList('ISOURBID', ee$List(citiesDone)))
ncitiesAll <- citiesUse$size()$getInfo()


citiesElevList <- citiesUse$toList(ncitiesAll) 
extractElevEE(CITIES=citiesElevList,  GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)

