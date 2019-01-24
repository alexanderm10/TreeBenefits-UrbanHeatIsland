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

plot(cities.1mill)

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
# Testing for Chicago
# -----------------------------------------
# Subset our shapefile
chicago <- cities[cities$name_conve=="Chicago",]
plot(chicago)

# Remove holes and try to genearlize a bit:
featureNumber=1 ; ringNumber=1
ring = SpatialPolygons(
  list(
    Polygons(
      list(
        chicago@polygons[[featureNumber]]@Polygons[[ringNumber]]),ID=1
      )
    )
  )
unholed = SpatialPolygonsDataFrame(ring,data=chicago@data,match.ID=FALSE)
plot(unholed)

# Simplifying to try and remove some edge effects
chi.simple <- rgeos::gSimplify(unholed, tol=0.01, topologyPreserve=T)
plot(chi.simple)
projection(chi.simple) <- projection(chicago)
bb.chicago <- bbox(chi.simple)

# ---------------
# Extract the climate data
# ---------------
# -- transform the projection of the city
chi.2 <- spTransform(chicago, projection(tmax.july))

temp.chicago <- crop(tmax.july, chi.2)
temp.chicago2 <- mask(temp.chicago, chi.2)
# temp.chicago <- crop(tmax.july, chi.2)
plot(temp.chicago); plot(chi.2, add=T)
plot(temp.chicago2); plot(chi.2, add=T)
# ---------------

# ---------------
# Find our tree canopy file
# ---------------
f.chicago <- which(ftree.df$lat-10<=bb.chicago[2,1] & ftree.df$lat>=bb.chicago[2,2] &
                     ftree.df$lon<=bb.chicago[1,1] & ftree.df$lon+10>=bb.chicago[1,2])
# ftree.df$file[f.chicago]
tree.chicago <- raster(file.path(path.trees, ftree.df$file[f.chicago]))
tree.chicago

tree.chicago.raw <- crop(tree.chicago, chi.2)
plot(tree.chicago.raw)

tree.chicago <- resample(tree.chicago.raw, temp.chicago)
plot(tree.chicago); plot(chi.2, add=T)

tree.chicago2 <- mask(tree.chicago, chi.3)
plot(tree.chicago2); plot(chi.2, add=T)

png("QuickChicago.png", height=8, width=10, unit="in", res=180)
par(mfrow=c(1,2))
plot(temp.chicago2, main="WorldClim\nMax Temp"); plot(chi.2, add=T)
plot(tree.chicago2, main="Tree Cover"); plot(chi.2, add=T)
par(mfrow=c(1,1))
dev.off()

values.tree <- getValues(tree.chicago2)
values.temp <- getValues(temp.chicago2)

tree.90 <- quantile(values.tree, 0.90, na.rm=T)
tree.10 <- quantile(values.tree, 0.10, na.rm=T)
temps.tree90 <- values.temp[which(values.tree>=tree.90)]
temps.tree10 <- values.temp[which(values.tree<=tree.10)]

summary(temps.tree90)
t.test(temps.tree90, temps.tree10)

plot(getValues(temp.chicago2) ~ getValues(tree.chicago2))
lm.tree <- lm(getValues(temp.chicago2) ~ getValues(tree.chicago2))
summary(lm.tree)
# ---------------
