# Migrating the Trees & Urban Heat Island workflow to using Google Earth Engine

library(rgee); library(raster); library(terra)
ee_check() # For some reason, it's important to run this before initializing right now
rgee::ee_Initialize(user = 'crollinson@mortonarb.org', drive=T)
path.google <- "/Volumes/GoogleDrive/My Drive"
GoogleFolderSave <- "UHI_Analysis_Output"
assetHome <- ee_get_assethome()


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
# bBoxS = ee$Geometry$BBox(-180, -90, 180, 0);
# bBoxNW = ee$Geometry$BBox(-180, 0, 0, 90);
# bBoxNE1 = ee$Geometry$BBox(0, 0, 75, 90);
# bBoxNE2 = ee$Geometry$BBox(75, 0, 80, 90);

# sdei <- ee$FeatureCollection('users/crollinson/sdei-global-uhi-2013');
# # print(sdei.first())
# 
# # Right now, just set all cities with >100k people in the metro area and at least 100 sq km in size
# citiesUse <- sdei$filter(ee$Filter$gte('ES00POP', 100e3))$filter(ee$Filter$gte('SQKM_FINAL', 1e2))
# # ee_print(citiesUse) # Thsi function gets the summary stats; this gives us 2,682 cities
# 
# # Use map to go ahead and create the buffer around everything
# citiesUse <- citiesUse$map(function(f){f$buffer(10e3)})
# # ee_print(citiesUse)
# 
# 
# citiesSouth <- citiesUse$filter(ee$Filter$lt('LATITUDE', 0))
# citiesNorthW <- citiesUse$filter(ee$Filter$gte('LATITUDE', 0))$filter(ee$Filter$lte('LONGITUDE', 0))
# citiesNorthE1 <- citiesUse$filter(ee$Filter$gte('LATITUDE', 0))$filter(ee$Filter$gt('LONGITUDE', 0))$filter(ee$Filter$lte('LONGITUDE', 75))
# citiesNorthE2 <- citiesUse$filter(ee$Filter$gte('LATITUDE', 0))$filter(ee$Filter$gt('LONGITUDE', 75))
# 
# # imageNE2 <- citiesNorthE2$reduceToImage()
# # ee_print(flatNE2)
# # ncitiesSouth <- citiesSouth$size()$getInfo() # 336 cities
# # ncitiesNorthW <- citiesNorthW$size()$getInfo() # 484 cities 
# # ncitiesNorthE1 <- citiesNorthE1$size()$getInfo() # 982 cities
# # ncitiesNorthE2 <- citiesNorthE2$size()$getInfo() # 880 cities

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
ee_print(vegMask)
Map$addLayer(vegMask)

maskGeom <- vegMask$geometry()$getInfo()
maskBBox <- ee$Geometry$BBox(-180, -90, 180, 90)

# proj4string: "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
saveVegMask <- ee_image_to_asset(vegMask, description="Save_VegetationMask", assetId=file.path(assetHome, "MOD44b_1km_Reproj_VegMask"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveVegMask$start()


mod44bReproj <- mod44bReproj$map(function(IMG){IMG$updateMask(vegMask)})
# ee_print(mod44bReproj)


yrMod <- ee$List(mod44bReproj$aggregate_array("year"))$distinct()
yrString <- ee$List(paste0("YR", yrMod$getInfo()))

modTree <- ee$ImageCollection$toBands(mod44bReproj$select("Percent_Tree_Cover"))$rename(yrString)
modVeg <- ee$ImageCollection$toBands(mod44bReproj$select("Percent_NonTree_Vegetation"))$rename(yrString)
modBare <- ee$ImageCollection$toBands(mod44bReproj$select("Percent_NonVegetated"))$rename(yrString)

vizTree <- list(
  # bands: ['Percent_Tree_Cover'],
  min=0.0,
  max=100.0,
  palette=c('bbe029', '0a9501', '074b03')
);
# ee_print(modTree)
Map$addLayer(modTree$select("YR2020"), vizTree, "TreeCover")

saveTree <- ee_image_to_asset(modTree, description="Save_Mod44bReproj_TreeCover", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_Tree_Cover"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveTree$start()


# 
# 
# saveTreeS <- ee_image_to_asset(image=treeS, description="Save_Mod44bReproj_TreeCover_South", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_Tree_Cover_South"), maxPixels=10e9, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
# saveTreeS$start()
# 
# saveTreeNW <- ee_image_to_asset(image=treeNW, description="Save_Mod44bReproj_TreeCover_NorthW", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_Tree_Cover_NorthW"), maxPixels=10e9, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
# saveTreeNW$start()
# 
# saveTreeNE1 <- ee_image_to_asset(image=treeNE1, description="Save_Mod44bReproj_TreeCover_NorthE1", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_Tree_Cover_NorthE1"), maxPixels=10e9, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
# saveTreeNE1$start()
# 
# saveTreeNE2 <- ee_image_to_asset(image=treeNE2, description="Save_Mod44bReproj_TreeCover_NorthE2", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_Tree_Cover_NorthE2"), maxPixels=10e9, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
# saveTreeNE2$start()
# 
# 
# vegS <- modVeg$clipToCollection(citiesSouth)
# vegNW <- modTree$clipToCollection(citiesNorthW)
# vegNE1 <- modTree$clipToCollection(citiesNorthE1)
# vegNE2 <- modTree$clipToCollection(citiesNorthE2)
# 
# saveVegS <- ee_image_to_asset(vegS, description="Save_Mod44bReproj_OtherVeg_South", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_NonTree_Vegetation_South"), maxPixels=10e9, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
# saveVegS$start()
# 
# bareS <- modBare$clipToCollection(citiesSouth)
# 
# saveBareS <- ee_image_to_asset(bareS, description="Save_Mod44bReproj_NonVeg_South", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_NonVegetated_South"), maxPixels=10e9, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
# saveBareS$start()


saveVeg <- ee_image_to_asset(modVeg, description="Save_Mod44bReproj_OtherVegCover", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_NonTree_Vegetation"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveVeg$start()

saveBare <- ee_image_to_asset(modBare, description="Save_Mod44bReproj_NonVeg", assetId=file.path(assetHome, "MOD44b_1km_Reproj_Percent_NonVegetated"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
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

elevVis = list(
  min= 0,
  max= 5000,
  palette=c ('0000ff', '00ffff', 'ffff00', 'ff0000', 'ffffff')
);
Map$addLayer(elevReproj, elevVis, "Elevation - Masked, reproj")


saveElev <- ee_image_to_asset(elevReproj, description="Save_MERIT_Elevation", assetId=file.path(assetHome, "MERIT-DEM-v1_1km_Reproj"), maxPixels = 10e9, scale=926.6, region = maskBBox, crs="SR-ORG:6974", crsTransform=c(926.625433056, 0, -20015109.354, 0, -926.625433055, 10007554.677), overwrite=T)
saveElev$start()

# elevS <- elevReproj$clipToCollection(citiesSouth)
# elevNW <- elevReproj$clipToCollection(citiesNorthW)
# elevNE1 <- elevReproj$clipToCollection(citiesNorthE1)
# elevNE2 <- elevReproj$clipToCollection(citiesNorthE2)
# # ee_print(treeS)
# 
# saveElevS <- ee_image_to_asset(image=elevS, description="Save_MERIT_Elevation_South", assetId=file.path(assetHome, "MERIT-DEM-v1_1km_Reproj_South"), maxPixels=10e9, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
# saveElevS$start()
# 
# saveElevNW <- ee_image_to_asset(image=elevNW, description="Save_MERIT_Elevation_NorthW", assetId=file.path(assetHome, "MERIT-DEM-v1_1km_Reproj_NorthW"), maxPixels=10e9, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
# saveElevNW$start()
# 
# saveElevNE1 <- ee_image_to_asset(image=elevNE1, description="Save_MERIT_Elevation_NorthE1", assetId=file.path(assetHome, "MERIT-DEM-v1_1km_Reproj_NorthE1"), maxPixels=10e9, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
# saveElevNE1$start()
# 
# saveElevNE2 <- ee_image_to_asset(image=elevNE2, description="Save_MERIT_Elevation_NorthE2", assetId=file.path(assetHome, "MERIT-DEM-v1_1km_Reproj_NorthE2"), maxPixels=10e9, crs="EPSG:4326", scale=1e3, crsTransform=c(1,0,0,0,1,0))
# saveElevNE2$start()
