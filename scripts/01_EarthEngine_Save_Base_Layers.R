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

mod44bReproj <- mod44bReproj$map(function(IMG){IMG$updateMask(vegMask)})

assetHome <- ee_get_assethome()

ee_print(mod44bReproj)

modTree <- ee$Image(mod44bReproj$select("Percent_Tree_Cover"))
# modTree <- ee$Image(mod44bReproj$select("Percent_Tree_Cover"))
ee_print(modTree)

saveTree <- ee_image_to_asset(mod44bReproj$select("Percent_Tree_Cover"), description="Save_Mod44bReproj_TreeCover", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_Tree_Cover"))
saveTree$start()

saveVeg <- ee_image_to_asset(mod44bReproj$select("Percent_NonTree_Vegetation"), description="Save_Mod44bReproj_OtherVegCover", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_NonTree_Vegetation"))
saveVeg$start()

# ----------

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
