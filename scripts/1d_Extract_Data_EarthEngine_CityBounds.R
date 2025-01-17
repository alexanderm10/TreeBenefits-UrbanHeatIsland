# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
user.google <- dir("~/Library/CloudStorage/")
path.google <- file.path("~/Library/CloudStorage", user.google, "My Drive")
GoogleFolderSave <- "UHI_Analysis_Output_Final_v2"

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
# 1. Load and select cities
#####################
sdei.df <- data.frame(vect("../data_raw/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp"))
sdei.df <- sdei.df[sdei.df$ES00POP>=100e3 & sdei.df$SQKM_FINAL>=1e2,]
cityIDsAll <- sdei.df$ISOURBID

sdei <- ee$FeatureCollection('users/crollinson/sdei-global-uhi-2013');
# print(sdei.first())

# Right now, just set all cities with >100k people in the metro area and at least 100 sq km in size
citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2)) 


# Making the buffer file a separate thing!
citiesBuff <- citiesUse$map(function(f){f$buffer(10e3)})
#####################


##################### 
# 2. Load in data layers  -- formatting in script 1!
####################
vegMask <- ee$Image("users/crollinson/MOD44b_1km_Reproj_VegMask")
# Map$addLayer(vegMask)

projMask = vegMask$projection()
projCRS = projMask$crs()
projTransform <- unlist(projMask$getInfo()$transform)

####################




#####################

# -----------
extractCityMask <- function(cityBuff, cityRaw, CityNames, BASE, GoogleFolderSave, overwrite=F, ...){
  pb <- txtProgressBar(min=0, max=length(CityNames), style=3)
  for(i in 1:length(CityNames)){
    setTxtProgressBar(pb, i)
    # cityID="USA26687" # Chicago
    cityID <- CityNames[i]
    # cityNow <- citiesUse$filter('NAME=="Chicago"')$first()
    cityNow <- cityRaw$filter(ee$Filter$eq('ISOURBID', cityID))
    cityNowBuff <- cityBuff$filter(ee$Filter$eq('ISOURBID', cityID)) # Note: this is only getting used for the geometry arguement.  We'll see how it works
    # Map$centerObject(cityNow) # NOTE: THIS IS REALLY IMPORTANT APPARENTLY!
    # Map$addLayer(cityNow)
    
    #-------
    # extracting elevation -- 
    #  NOTE: Doing outlier removal because there are some known issues with a couple points: https://developers.google.com/earth-engine/datasets/catalog/JAXA_ALOS_AW3D30_V3_2
    #-------
    # baseCity <- BASE$clip(cityNowBuff)
    # Map$addLayer(baseCity)
    baseCity <- BASE$clip(cityNow)
    # Map$addLayer(baseCity)

    # Save elevation only if it's worth our while -- Note: Still doing the extraction & computation first since we use it as our base
    export.mask <- ee_image_to_drive(image=baseCity, description=paste0(cityID, "_CityMask"), fileNamePrefix=paste0(cityID, "_CityMask"), folder=GoogleFolderSave, timePrefix=F, region=cityNowBuff$geometry(), maxPixels=5e6, crs=projCRS, crsTransform=projTransform)
    export.mask$start()
    # ee_monitoring(export.elev)
    #-------
  } # End i loop
} # End function


# -----------

##################### 
# 3 . Start extracting data for each city -- only ones that were done before!
##################### 

cityIdAll <-sdei.df$ISOURBID
length(cityIdAll)

# If we're not trying to overwrite our files, remove files that were already done
cityRemove <- vector()
if(!overwrite){
  ### Filter out sites that have been done!
  mask.done <- dir(file.path(path.google, GoogleFolderSave), "CityMask.tif")
  
  # Check to make sure a city has all three layers; if it doesn't do it again
  cityRemove <- unlist(lapply(strsplit(mask.done, "_"), function(x){x[1]}))
  
  cityIdAll <- cityIdAll[!cityIdAll %in% cityRemove]
} # End remove cities loop
length(cityIdAll)

if(length(cityIdAll)>0){
  extractCityMask(cityBuff=citiesBuff, cityRaw=citiesUse, CityNames=cityIdAll, BASE=vegMask, GoogleFolderSave, overwrite=F)
}


