# Create a script to redo the elevation for anything already done; then we can start the temperature extractions again

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- "/Volumes/GoogleDrive/My Drive"
GoogleFolderSave <- "UHI_Analysis_Output"


##################### 
# 0. Set up some choices for data quality thresholds
##################### 
# yr.analy <- 2001:2020
thresh.sigma <- 6 # Use 6-sigma outliers for the data filtering\
# thresh.pts <- 50
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
cityIdAll <- sdei.df$ISOURBID

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
# 2. Load in data layers  -- formatting in script 1!
####################
# -----------
# 2.c  - Elevation (static = easy!)
## Now using MERIT, which has combined several other products and removed bias, including from trees
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2017GL072874
# -----------
elevVis = list(
  min= 0,
  max= 5000,
  palette=c ('0000ff', '00ffff', 'ffff00', 'ff0000', 'ffffff')
);


# elev <- ee$Image('USGS/SRTMGL1_003')$select('elevation')
# elev <- ee$ImageCollection('JAXA/ALOS/AW3D30/V3_2')$select("DSM")
elev <- ee$Image('users/crollinson/MERIT-DEM-v1_1km_Reproj')#$select('elevation')
ee_print(elev)
# Map$addLayer(elevLoad, elevVis, "Elevation - Masked, reproj")

projElev = elev$projection()
projCRS = projElev$crs()
projTransform <- unlist(projElev$getInfo()$transform)

##################### 


extractElevEE <- function(CitySP, CityNames, ELEV, GoogleFolderSave, overwrite=F, ...){
  pb <- txtProgressBar(min=0, max=length(CityNames), style=3)
  for(i in 1:length(CityNames)){
    setTxtProgressBar(pb, i)
    cityID <- CityNames[i]
    # cityNow <- citiesUse$filter('NAME=="Chicago"')$first()
    cityNow <- CitySP$filter(ee$Filter$eq('ISOURBID', cityID))
    # Map$centerObject(cityNow) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
    # Map$addLayer(cityNow)
    
    #-------
    # extracting elevation -- 
    #  NOTE: Doing outlier removal because there are some known issues with a couple points: https://developers.google.com/earth-engine/datasets/catalog/JAXA_ALOS_AW3D30_V3_2
    #-------
    # elevCity <- elevReproj$updateMask(cityMask)
    elevCity <- ELEV$clip(cityNow)
    # Map$addLayer(elevCity, list(min=-10, max=500))
    
    # elevOutlier <- function(img){
    #   # Calculate the means & sds for the region
    #   elevStats <- img$select("dem")$reduceRegion(reducer=ee$Reducer$mean()$combine(
    #     reducer2=ee$Reducer$stdDev(), sharedInputs=T),
    #     geometry=cityNow$geometry(), scale=1e3)
    #   
    #   # Cacluate the key numbers for our sanity
    #   elevMean <- ee$Number(elevStats$get("dem_mean"))
    #   elevSD <- ee$Number(elevStats$get("dem_stdDev"))
    #   thresh <- elevSD$multiply(thresh.sigma)
    #   
    #   # Do the filtering
    #   dat.low <- img$gte(elevMean$subtract(thresh))
    #   dat.hi <- img$lte(elevMean$add(thresh))
    #   img <- img$updateMask(dat.low)
    #   img <- img$updateMask(dat.hi)
    #   
    #   # Map$addLayer(img$select('dem'))
    #   return(img)
    # }
    # 
    # elevCity <- elevOutlier(elevCity)
    
    # npts.elev <- elevCity$reduceRegion(reducer=ee$Reducer$count(), geometry=cityNow$geometry(), scale=1e3)
    # npts.elev <- npts.elev$getInfo()$DSM
    # 
    # ### *** ###  If we don't have many points in the elevation file, skip the city
    # if(npts.elev<thresh.pts){ 
    #   print(warning(paste("Not enough Elevation Points; skipping : ", cityID)))
    #   next
    # }
    
    # Save elevation only if it's worth our while -- Note: Still doing the extraction & computation first since we use it as our base
    export.elev <- ee_image_to_drive(image=elevCity, description=paste0(cityID, "_elevation"), fileNamePrefix=paste0(cityID, "_elevation"), folder=GoogleFolderSave, timePrefix=F, region=cityNow$geometry(), maxPixels=5e6, crs=projCRS, crsTransform=projTransform)
    export.elev$start()
      # ee_monitoring(export.elev)
    #-------
  } # End i loop
} # End function


##################### 
# 3 . Start extracting data for each city -- only ones that were done before!
##################### 
print(citiesUse$first()$propertyNames()$getInfo())


cityIdS <-sdei.df$ISOURBID[sdei.df$LATITUDE<0]
cityIdN <-sdei.df$ISOURBID[sdei.df$LATITUDE>=0]
# length(cityIdS); length(cityIdNW)

# If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  elev.done <- dir(file.path(path.google, GoogleFolderSave), "elevation.tif")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(elev.done, "_"), function(x){x[1]}))
  
  cityIdS <- cityIdS[!cityIdS %in% cityRemove]
  cityIdsN <- cityIdS[!cityIdN %in% cityRemove]
  
} # End remove cities loop

citiesSouth <- citiesUse$filter(ee$Filter$inList('ISOURBID', ee$List(cityIdS)))
citiesNorth <- citiesUse$filter(ee$Filter$inList('ISOURBID', ee$List(cityIdN)))
citiesSouth$size()$getInfo()
length(cityIdS)

# # All except 1 ran successfully
if(length(cityIdS)>0){
  extractElevEE(CitySP=citiesSouth, CityNames = cityIdS, ELEV = elev, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}
if(length(cityIdN)>0){
  extractElevEE(CitySP=citiesNorth, CityNames = cityIdN, ELEV = elev, GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)
}

# Create a vector with teh handful of cities that have been actually updated, but the extraction is just so dang slow today!
# If this gets too long, just sort files by newest, move the "good" elev to a new place and delete the old ones
# citiesUpdated <- c("AFG90032, AFG90009", "AFG37217", "AFG40389", "AGO60738")
# citiesDone <- citiesDone[!citiesDone %in% citiesUpdated,] 
extractElevEE(CITIES=citiesElevList,  GoogleFolderSave = GoogleFolderSave, overwrite=overwrite)

