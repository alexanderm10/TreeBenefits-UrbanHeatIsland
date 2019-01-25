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

# Checking out the population data first:
cities <- readOGR("/Volumes/Morton_SDM/world_urban_areas/ne_10m_urban_areas_landscan.shp")
summary(cities)

cities.1mill <- cities[cities$max_pop_al>=1e6,]
summary(cities.1mill)
dim(cities.1mill)

plot(cities[cities$name_conve=="Chicago",])
summary(cities[cities$name_conve=="Chicago",])

# plot(cities.1mill)

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
tmax.july <- raster("/Volumes/Morton_SDM/wc2/wc2.0_30s_tmax_07.tif")
tmax.july

# -----------------------------------------
# Looping through Cities
# -----------------------------------------
path.save <- "../data_processed/cities_full"
for(i in 1:nrow(cities.1mill)){
  # Subset our shapefile
  city.sp <- cities[i,]
  city.name <- city.sp$name_conve
  # plot(city.sp)
  
  # Simplifying to try and remove some edge effects
  # city.simple <- rgeos::gSimplify(city, tol=0.01, topologyPreserve=T)
  # plot(city.simple)
  # projection(city.simple) <- projection(city.sp)
  
  # ---------------
  # Extract the climate data
  # ---------------
  # -- transform the projection of the city --> not actulaly necessary
  # chi.2 <- spTransform(city, projection(tmax.july))
  
  temp.city <- crop(tmax.july, city.sp)
  temp.city2 <- mask(temp.city, city.sp)
  # temp.city <- crop(tmax.july, chi.2)
  plot(temp.city); plot(city.sp, add=T)
  plot(temp.city2); plot(city.sp, add=T)
  # ---------------
  
  # ---------------
  # Find our tree canopy file
  # ---------------
  bb.city <- bbox(city.sp)
  
  f.city <- which(ftree.df$lat-11<=bb.city[2,1] & ftree.df$lat>=bb.city[2,2] &
                       ftree.df$lon<=bb.city[1,1] & ftree.df$lon+11>=bb.city[1,2])
  # ftree.df$file[f.city]
  tree.city <- raster(file.path(path.trees, ftree.df$file[f.city]))
  tree.city
  
  bb.tree <- bbox(tree.city)
  bb.city
  if(any(bb.tree[,1] > bb.city[,1] | bb.tree[,2] < bb.city[,2])) warning(paste0("Warning: city crosses tree tiles -- ", city.name))
  
  tree.city.raw <- crop(tree.city, city.sp)
  plot(tree.city.raw)
  
  tree.city <- resample(tree.city.raw, temp.city)
  plot(tree.city); plot(chi.2, add=T)
  
  tree.city2 <- mask(tree.city, city.sp)
  plot(tree.city2); plot(city.sp, add=T)
  
  png(file.path(path.save, paste0(city.name, "_Temp_Tree.png")), height=8, width=10, unit="in", res=180)
  par(mfrow=c(1,2))
  plot(temp.city2, main="WorldClim July Tmax\n1970-2000, deg.C"); plot(city.sp, add=T)
  plot(tree.city2, main="Tree Cover\n 2000, perc. cover"); plot(city.sp, add=T)
  par(mfrow=c(1,1))
  dev.off()
  
  # test <- coordinates(tree.city2)
  
  df.city <- data.frame(Name=city.name, coordinates(temp.city2), cover.tree=getValues(tree.city2), temp.july = getValues(temp.city2))
  write.csv(file.path(path.save, paste0(city.name, "_data_full.csv")), row.names=F)
  summary(df.city)
  
  cities[i,"tree.mean"] <- mean(df.city$cover.tree, na.rm=T)
  cities[i,"tree.sd"  ] <- sd(df.city$cover.tree, na.rm=T)
  cities[i,"tree.max" ] <- max(df.city$cover.tree, 0.90, na.rm=T)
  cities[i,"tree.min" ] <- min(df.city$cover.tree, 0.90, na.rm=T)
  cities[i,"july.mean"] <- mean(df.city$temp.july, na.rm=T)
  cities[i,"july.sd"  ] <- sd(df.city$temp.july, na.rm=T)
  cities[i,"july.max" ] <- max(df.city$temp.july, na.rm=T)
  cities[i,"july.min" ] <- min(df.city$temp.july, na.rm=T)
}

# ---------------
