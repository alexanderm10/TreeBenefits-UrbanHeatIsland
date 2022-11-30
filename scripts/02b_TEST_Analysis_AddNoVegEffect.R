# Testing adding a bareground effect of a handful of cities 
#Champaign: USA31965
library(raster); library(sp); library(terra); library(sf) 
library(ggplot2)
library(mgcv)

# Path to where Earth Engine is saving the spatial extractions
path.EEout <- "/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output_Final/"


cityNow <- "USA31965" # Champaign -- has postive tree effect
cityNow <- "DZA43317" # Has VERY positive tree effect
cityNow <- "SAU43980" # Has off the charts tree cooling


filesCity <- dir("/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output_Final/", cityNow)


maskCity <- raster(file.path(path.EEout, paste(cityNow, "CityMask.tif", sep="_")))
elevCity <- raster(file.path(path.EEout, paste(cityNow, "elevation.tif", sep="_")))
lstCity <- brick(file.path(path.EEout, paste(cityNow, "LST_Day_Tmean.tif", sep="_")))-273.15
treeCity <- brick(file.path(path.EEout, paste(cityNow, "Vegetation_PercentTree.tif", sep="_")))
vegCity <- brick(file.path(path.EEout, paste(cityNow, "Vegetation_PercentOtherVeg.tif", sep="_")))
bareCity <- brick(file.path(path.EEout, paste(cityNow, "Vegetation_PercentNoVeg.tif", sep="_")))

################################
# Load in data for the city ----
################################
# Elevation should be our most reliable data layer, so lets use that as our base
coordsCity <- data.frame(coordinates(elevCity)) 
coordsCity$location <- paste0("x", coordsCity$x, "y", coordsCity$y)
coordsCity$elevation <- getValues(elevCity)
coordsCity$cityBounds <- getValues(maskCity)
coordsCity$cityBounds <- !is.na(coordsCity$cityBounds) # NA = buffer = FALSE citybounds

# In case we're missing some years of LST (likely in the tropics); only pull certain layers
layers.use <- names(treeCity)[names(treeCity) %in% names(lstCity)]

coordsVeg <- data.frame(coordinates(treeCity))
coordsVeg$location <- paste0("x", coordsVeg$x, "y", coordsVeg$y)

valsCityVeg <- stack(data.frame(getValues(treeCity[[layers.use]])))
names(valsCityVeg) <- c("cover.tree", "year")
valsCityVeg$cover.veg <- stack(data.frame(getValues(vegCity[[layers.use]])))[,1]
valsCityVeg$cover.bare <- stack(data.frame(getValues(bareCity[[layers.use]])))[,1]
valsCityVeg$x <- coordsVeg$x
valsCityVeg$y <- coordsVeg$y
valsCityVeg$location <- coordsVeg$location

# nrow(coordsCity); nrow(coordsVeg)
if(all(coordsVeg$location == coordsCity$location)){
  valsCity <- valsCityVeg[,]
  valsCity$elevation <- coordsCity$elevation
  valsCity$cityBounds <- coordsCity$cityBounds
  # valsCity <- merge(coordsCity, valsCityVeg, all.x=T, all.y=T)
} else {
  stop("Veg and Elev Layer doesn't match. :-( gotta figure it out")
}

# Land Surface Temperature is mismatched with 
coordsLST <- data.frame(coordinates(lstCity))
coordsLST$location <- paste0("x", coordsLST$x, "y", coordsLST$y)

valsLST <- stack(data.frame(getValues(lstCity[[layers.use]])))
names(valsLST) <- c("LST_Day", "year")
valsLST$x <- coordsLST$x
valsLST$y <- coordsLST$y
valsLST$location <- coordsLST$location
summary(valsLST)

# locLSTAll <- unique(valsLST$location[!is.na(valsLST$LST_Day)])

# nrow(coordsCity); nrow(coordsLST)
if(all(coordsLST$location == coordsCity$location)){
  valsCity$LST_Day <- valsLST$LST_Day
  # valsCity <- merge(valsCity, valsLST, all.x=T, all.y=T)
}

valsCity$year <- as.numeric(substr(valsCity$year, 3, 6))
valsCity$CoverSum <- valsCity$cover.tree + valsCity$cover.veg + valsCity$cover.bare
valsCity$elev.dev <- valsCity$elevation - mean(valsCity$elevation, na.rm=T)
valsCity$tree.dev <- valsCity$cover.tree - mean(valsCity$cover.tree, na.rm=T)
valsCity$veg.dev <- valsCity$cover.veg - mean(valsCity$cover.veg, na.rm=T)
valsCity$bare.dev <- valsCity$cover.bare - mean(valsCity$cover.bare, na.rm=T)
valsCity$LST.dev <- valsCity$LST_Day - mean(valsCity$LST_Day, na.rm=T)


valsCity <- valsCity[!is.na(valsCity$elevation) & !is.na(valsCity$cover.tree),]
summary(valsCity)

modCity1 <- gam(LST_Day ~ cover.tree + cover.veg + elevation + s(x,y) + as.factor(year)-1, data=valsCity)
summary(modCity1)

# Adding bare ground alone definitely does weird things
# modCity2 <- gam(LST_Day ~ cover.tree + cover.veg + cover.bare + elevation + s(x,y) + as.factor(year)-1, data=valsCity)
# summary(modCity2)

modCity3 <- gam(LST_Day ~ tree.dev + veg.dev + elev.dev + s(x,y) + as.factor(year)-1, data=valsCity)
summary(modCity3)


modCity3b <- gamm(LST_Day ~ tree.dev + veg.dev + elev.dev + s(x,y) + as.factor(year)-1, random=list(year=~1), data=valsCity)
summary(modCity3b$gam)

modCity4 <- gam(LST.dev ~ tree.dev + veg.dev + elev.dev + s(x,y) + as.factor(year)-1, data=valsCity)
summary(modCity4)

modCity5 <- gam(LST.dev ~ tree.dev + veg.dev + bare.dev + elev.dev + s(x,y) + as.factor(year)-1, data=valsCity)
summary(modCity5)


################################
