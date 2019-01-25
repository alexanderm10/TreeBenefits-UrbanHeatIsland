# Exploring new dataset options thanks to NASA's Socioeconomic Data and Applicaitons Center (Hosted by CIESIN at Columbia)
# http://sedac.ciesin.columbia.edu/data/set/sdei-global-summer-lst-2013
# http://sedac.ciesin.columbia.edu/data/set/sdei-global-uhi-2013

# Summer Land Surface Temperature Grids for 2013:
# http://sedac.ciesin.columbia.edu/data/set/sdei-global-summer-lst-2013
# -- Global Grid at ~1km resolution produced from MODIS
# -- July-August mean for N hemisphere, Jan=Feb for Southern

# Water Bodies from here: http://openstreetmapdata.com/data/water-reduced-polygons

# Extract info on tree cover and mean summer temp for major urban areas around the world
# Datasets to use:
# 1. Urban Areas: LandScan https://landscan.ornl.gov
#     - Filter to areas >= 1,000,000 people (698 regions)
# 2. Tree Cover Data: UMD Global Forest Changehttp://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.5.html
#     - Spatial Resolution: 30 m
#     - Temporal Extent: 2010
# 3. Climate Data:
#     - Plan A: Landsat; https://landsat.usgs.gov/landsat-surface-temperature
#         ***** can't access during gov't shutdown
#         - Spatial Resolution: 30 m 
#         - Temporal Extent: ???
#     - Plan B: WorldClim v2.0 http://worldclim.org/version2
#         - Spatial Resolution: 30 arcseconds (approx 1 km)
#         - Temporal Extent: mean 1970-2000


library(sp); library(rgdal); library(raster)

# Using the SDEI urban database since it has additional info
sdei.urb <- readOGR("/Volumes/Morton_SDM/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
summary(sdei.urb)

cities.1mill <- sdei.urb[sdei.urb$ES00POP>1e6 &  !is.na(sdei.urb$NAME),]
summary(cities.1mill)
dim(cities.1mill)
plot(cities.1mill[cities.1mill$NAME=="Chicago",])
# plot(cities.1mill)

# Going to need to mask out water, so lets load a layer
# water <- readOGR("/Volumes/Morton_SDM/water-polygons-split-4326/water_polygons.shp")
# summary(water)
# water <- readOGR("~/Desktop/occurrence.tif.lyr")
oceans <- readOGR("~/Desktop/ocean-polygons-reduced-3857/ocean_reduced_z6.shp")
lakes  <- readOGR("~/Desktop/lakes-polygons-reduced-3857/lakes_reduced_z6.shp")
rivers <- readOGR("~/Desktop/river-polygons-reduced-3857/river_reduced_z6.shp")

oceans <- spTransform(oceans, projection(cities.1mill))
lakes <- spTransform(lakes, projection(cities.1mill))
rivers <- spTransform(rivers, projection(cities.1mill))

# Doing some indexing of tree cover data
path.trees <- "/Volumes/Morton_SDM/TreeCover/"
files.trees <- dir(path.trees, ".tif")
ftree.df <- data.frame(file=files.trees)
ftree.df$lat.char <- as.factor(unlist(lapply(stringr::str_split(ftree.df$file, "_"), function(x){x[4]})))
ftree.df$lon.char <- as.factor(unlist(lapply(stringr::str_split(ftree.df$file, "_"), function(x){substr(x[5], 1, 4)})))
ftree.df$lat <- ifelse(substr(ftree.df$lat.char,3,3)=="S", 
                       -as.numeric(substr(ftree.df$lat.char,1,2)),
                       as.numeric(substr(ftree.df$lat.char,1,2)))
ftree.df$lon <- ifelse(substr(ftree.df$lon.char,4,4)=="W", 
                       -as.numeric(substr(ftree.df$lon.char,1,3)),
                       as.numeric(substr(ftree.df$lon.char,1,3)))
# ftree.df$lat <- as.numeric(lapply())
summary(ftree.df)

# Testing the temperature data -- July 
tmax <- raster("/Volumes/Morton_SDM/sdei-global-summer-lst-2013-global/sdei-global-summer-lst-2013-day-max-global.tif")

# -----------------------------------------
# Looping through Cities
# -----------------------------------------
path.save <- "../data_processed/cities_full"
pb <- txtProgressBar(min=0, max=nrow(cities.1mill), style=3)
for(i in 1:nrow(cities.1mill)){
  setTxtProgressBar(pb, i)
  # Subset our shapefile
  # i=which(cities.1mill$NAME=="Chicago")
  city.sp <- cities.1mill[i,]
  city.name <- city.sp$NAME
  city.name <- sub(c("[?]"), "X", city.name) # Getting rid of the ? infront of some cities
  city.name <- sub(c("[?]"), "X", city.name) # Being lazy and doing this twice just in case to get rid of the rare ??
  bb.city <- bbox(city.sp)
  
  ocean.city <- crop(oceans, extent(city.sp))
  lakes.city <- crop(lakes, extent(city.sp))
  river.city <- crop(rivers, extent(city.sp))
  
  plot(city.sp);
  # if(length(ocean.city)>0) plot(ocean.city, add=T, col="blue3"); 
  # if(length(lakes.city)>0) plot(lakes.city, add=T, col="cadetblue2"); 
  # if(length(river.city)>0) plot(river.city, add=T, col="dodgerblue2")
  
  # Simplifying to try and remove some edge effects
  # city.simple <- rgeos::gSimplify(city, tol=0.01, topologyPreserve=T)
  # plot(city.simple)
  # projection(city.simple) <- projection(city.sp)
  
  # ---------------
  # Extract the climate data
  # ---------------
  # -- transform the projection of the city --> not actulaly necessary
  # chi.2 <- spTransform(city, projection(tmax.july))
  
  temp.city <- crop(tmax, city.sp)
  
  # Mask out water bodies
  if(length(ocean.city)>0) temp.city <- mask(temp.city, ocean.city, inverse=T)
  if(length(lakes.city)>0) temp.city <- mask(temp.city, lakes.city, inverse=T)
  if(length(river.city)>0) temp.city <- mask(temp.city, river.city, inverse=T)
  
  temp.city2 <- mask(temp.city, city.sp)
  # plot(temp.city); plot(city.sp, add=T)
  # plot(temp.city2); plot(city.sp, add=T)
  # ---------------
  
  # ---------------
  # Find our tree canopy file
  # ---------------
  f.city <- which(ftree.df$lat-10<=bb.city[2,1] & ftree.df$lat>=bb.city[2,2] &
                    ftree.df$lon<=bb.city[1,1] & ftree.df$lon+10>=bb.city[1,2])
  # ftree.df$file[f.city]
  if(length(f.city)==0){
    # Figure out which dimension fails
    test.lat <- which(ftree.df$lat-10<=bb.city[2,1] & ftree.df$lat>=bb.city[2,2])
    test.lon <- which(ftree.df$lon<=bb.city[1,1] & ftree.df$lon+10>=bb.city[1,2])
    
    if(length(test.lon)==0){
      test.lon <- which(ftree.df$lon<=bb.city[1,1]+10 & ftree.df$lon+10>=bb.city[1,2]-10)
      f.city <- test.lat[test.lat %in% test.lon]
    }
    if(length(test.lat)==0){
      test.lat <- which(ftree.df$lat-10<=bb.city[2,1]+10 & ftree.df$lat+10>=bb.city[2,2]-10)
      f.city <- test.lat[test.lat %in% test.lon]
    }
    
  }
  
  # if(length(f.city)==0) next
  if(length(f.city)==1){
    tree.city <- raster(file.path(path.trees, ftree.df$file[f.city]))
    tree.city
  } else if(length(f.city)==2) {
    tree1 <- raster(file.path(path.trees, ftree.df$file[f.city[1]]))
    tree2 <- raster(file.path(path.trees, ftree.df$file[f.city[2]]))
    
    ext1 <- extent(tree1)
    ext2 <- extent(tree2)
    ext.city <- extent(city.sp)
    tree1 <- crop(tree1, extent(c(max(ext1[1], ext.city[1]),
                                  min(ext1[2], ext.city[2]),
                                  max(ext1[3], ext.city[3]),
                                  min(ext1[4], ext.city[4]))))
    tree2 <- crop(tree2, extent(c(max(ext2[1], ext.city[1]),
                                  min(ext2[2], ext.city[2]),
                                  max(ext2[3], ext.city[3]),
                                  min(ext2[4], ext.city[4]))))
    
    tree.city <- mosaic(tree1, tree2, fun=mean)
  } else next
  
  # if(any(bb.tree[,1] > bb.city[,1] | bb.tree[,2] < bb.city[,2])) warning(paste0("Warning: city crosses tree tiles -- ", city.name))
  
  tree.city.raw <- crop(tree.city, city.sp)
  # plot(tree.city.raw)
  
  tree.city <- resample(tree.city.raw, temp.city)
  # plot(tree.city); plot(city.sp, add=T)
  
  # Mask out water bodies
  if(length(ocean.city)>0) tree.city <- mask(tree.city, ocean.city, inverse=T)
  if(length(lakes.city)>0) tree.city <- mask(tree.city, lakes.city, inverse=T)
  if(length(river.city)>0) tree.city <- mask(tree.city, river.city, inverse=T)
  
  
  
  tree.city2 <- mask(tree.city, city.sp)
  # plot(tree.city2); plot(city.sp, add=T)
  
  png(file.path(path.save, paste0(city.name, "_maps.png")), height=4, width=6, unit="in", res=180)
  par(mfrow=c(1,2))
  plot(temp.city2, main="Summer Day Max Temp\n2013, deg.C"); plot(city.sp, add=T)
  plot(tree.city2, main="Tree Cover\n 2000, perc. cover"); plot(city.sp, add=T)
  par(mfrow=c(1,1))
  dev.off()
  
  # test <- coordinates(tree.city2)
  
  df.city <- data.frame(Name=city.name, coordinates(temp.city2), cover.tree=getValues(tree.city2), temp.summer = getValues(temp.city2))
  df.city <- df.city[complete.cases(df.city),]
  write.csv(df.city, file.path(path.save, paste0(city.name, "_data_full.csv")), row.names=F)
  # summary(df.city)
  lm.city <- lm(temp.summer ~ cover.tree, data=df.city)
  sum.lm <- summary(lm.city)
  
  png(file.path(path.save, paste0(city.name, "_scatter.png")), height=4, width=4, unit="in", res=90)
  plot(temp.summer ~ cover.tree, data=df.city, cex=0.5,
       main=paste0("R2=",round(sum.lm$r.squared,2))); 
  abline(lm.city, col="red", lwd=2); 
  dev.off()
  
  
  cities.1mill[i,"tree.mean"] <- mean(df.city$cover.tree, na.rm=T)
  cities.1mill[i,"tree.sd"  ] <- sd(df.city$cover.tree, na.rm=T)
  cities.1mill[i,"tree.max" ] <- max(df.city$cover.tree, 0.90, na.rm=T)
  cities.1mill[i,"tree.min" ] <- min(df.city$cover.tree, 0.90, na.rm=T)
  cities.1mill[i,"july.mean"] <- mean(df.city$temp.july, na.rm=T)
  cities.1mill[i,"july.sd"  ] <- sd(df.city$temp.july, na.rm=T)
  cities.1mill[i,"july.max" ] <- max(df.city$temp.july, na.rm=T)
  cities.1mill[i,"july.min" ] <- min(df.city$temp.july, na.rm=T)
}
summary(cities.1mill)
write.csv(data.frame(cities.1mill), "../data_processed/cities_summary.csv", row.names=F)
# ---------------
