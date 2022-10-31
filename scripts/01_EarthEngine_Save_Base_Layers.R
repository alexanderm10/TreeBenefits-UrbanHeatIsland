# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- "/Volumes/GoogleDrive/My Drive"
GoogleFolderSave <- "UHI_Analysis_Output"
assetHome <- ee_get_assethome()

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
# 2. Load in data layers 
####################
# -----------
# 2.a - Land Surface Temperature -- Not saving because of the complexities involved
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

saveVegMask <- ee_image_to_asset(vegMask, description="Save_VegetationMask", assetId=file.path(assetHome, "MOD44b_1km_Reproj_VegMask"), maxPixels = 10e9)
saveVegMask$start()


mod44bReproj <- mod44bReproj$map(function(IMG){IMG$updateMask(vegMask)})
ee_print(mod44bReproj)


yrMod <- ee$List(mod44bReproj$aggregate_array("year"))$distinct()
yrString <- ee$List(paste0("YR", yrMod$getInfo()))

modTree <- ee$ImageCollection$toBands(mod44bReproj$select("Percent_Tree_Cover"))$rename(yrString)
modVeg <- ee$ImageCollection$toBands(mod44bReproj$select("Percent_NonTree_Vegetation"))$rename(yrString)
modBare <- ee$ImageCollection$toBands(mod44bReproj$select("Percent_NonVegetated"))$rename(yrString)

# ee_print(modTree)

saveTree <- ee_image_to_asset(modTree, description="Save_Mod44bReproj_TreeCover", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_Tree_Cover"), maxPixels = 10e9)
saveTree$start()

saveVeg <- ee_image_to_asset(modVeg, description="Save_Mod44bReproj_OtherVegCover", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_NonTree_Vegetation"), maxPixels = 10e9)
saveVeg$start()

saveBare <- ee_image_to_asset(modBare, description="Save_Mod44bReproj_NonVeg", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_NonVegetated"), maxPixels = 10e9)
saveBare$start()
# ----------

# -----------
# Elevation
## Now using MERIT, which has combined several other products and removed bias, including from trees
# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2017GL072874
# -----------
elev <- ee$Image('MERIT/DEM/v1_0_3')#$select('elevation')
ee_print(elev)

elevReproj <- elev$reproject(projLST)
elevReproj <- elevReproj$updateMask(vegMask)
ee_print(elevReproj)

saveElev <- ee_image_to_asset(elevReproj, description="Save_MERIT_Elevation", assetId=file.path(assetHome, "MERIT-DEM-v1_1km_Reproj"), maxPixels = 10e9)
saveElev$start()
