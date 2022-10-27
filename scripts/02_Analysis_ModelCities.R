library(raster); library(sp); library(terra)
library(ggplot2)

# sdei <- read.csv("../sdei-global-uhi-2013.csv")
# sdei <- sdei[sdei$ES00POP>=100e3 & sdei$SQKM_FINAL>1e2,]
# dim(sdei)
# summary(sdei)

# summary(sdei[sdei$LATITUDE<0,])
# summary(as.factor(sdei$ISO3[sdei$LATITUDE<0]))

# summary(sdei[sdei$LATITUDE>=0 & sdei$LONGITUDE<=0,])
# summary(as.factor(sdei$ISO3[sdei$LATITUDE>=0 & sdei$LONGITUDE<=0]))

# summary(sdei[sdei$LATITUDE>=0 & sdei$LONGITUDE>0 & sdei$LONGITUDE<=75,])
# summary(as.factor(sdei$ISO3[sdei$LATITUDE>=0 & sdei$LONGITUDE>0 & sdei$LONGITUDE<=75]))

# summary(sdei[sdei$LATITUDE>=0 & sdei$LONGITUDE>75,])
# summary(as.factor(sdei$ISO3[sdei$LATITUDE>=0 & sdei$LONGITUDE>75]))


# summary(as.factor(sdei$ISO3[sdei$LATITUDE<0]))
# sdei[sdei$ISO3=="ZAF", "NAME"]
# sdei[grep("AUS", sdei$ISOURBID),]


path.google <- "/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output"
files.elev <- dir(path.google, "elevation")
files.lst <- dir(path.google, "LST_Day_Tmean")
files.tree <- dir(path.google, "PercentTree")
files.veg <- dir(path.google, "PercentOtherVeg")
length(files.elev); length(files.lst); length(files.tree); length(files.veg)

# Testing out some workflows: Sydney (AUS66430)
elev.test <- raster("/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output/AUS66430_elevation.tif")
lst.test <- brick("/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output/AUS66430_LST_Day_Tmean.tif")-273.15
tree.test <- brick("/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output/AUS66430_Vegetation_PercentTree.tif")
veg.test <- brick("/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output/AUS66430_Vegetation_PercentOtherVeg.tif")

lst.mean <- mean(lst.test)
tree.mean <- mean(tree.test)
veg.mean <- mean(veg.test)

lst.min <- min(lst.test)
tree.min <- min(tree.test)
veg.min <- min(veg.test)
tree.max <- max(tree.test)


# par(mfrow=c(2,2))
# plot(elev.test); plot(lst.mean); plot(tree.mean); plot(veg.mean)

coords.test <- data.frame(coordinates(lst.test))
coords.test$location <- paste0("x", coords.test$x, "y", coords.test$y)

names(lst.test)
names(tree.test)

# In case we're missing some years of LST (likely in the tropics); only pull certain layers
layers.use <- names(tree.test)[names(tree.test) %in% names(lst.test)]

# Put everythign into a single data frame 
vals.test <- stack(data.frame(getValues(lst.test[[layers.use]])))
names(vals.test) <- c("LST_Day", "year")
vals.test$cover.tree <- stack(data.frame(getValues(tree.test[[layers.use]])))[,1]
vals.test$cover.veg <- stack(data.frame(getValues(veg.test[[layers.use]])))[,1]
vals.test$elevation <- getValues(elev.test)
vals.test$year <- as.numeric(substr(vals.test$year, 2, 5))
vals.test[,c("x", "y", "location")] <- coords.test
vals.test <- vals.test[!is.na(vals.test$LST_Day),]
dim(vals.test); summary(vals.test)
length(unique(vals.test$location))


library(nlme); library(mgcv)
mod.test <- gam(LST_Day ~ cover.tree + cover.veg + elevation + s(x,y) + as.factor(year)-1, data=vals.test)
summary(mod.test)
par(mfrow=c(1,1)); plot(mod.test)

# For computational tractability, need to run each pixel independently.  Doing Hobart as a loop just takes a few seconds

summary.test <- aggregate(cbind(LST_Day, cover.tree, cover.veg, elevation) ~ x+y+location, data=vals.test, FUN=mean)
names(summary.test)[names(summary.test) %in% c("LST_Day", "cover.tree", "cover.veg")] <- c("LST.mean", "tree.mean", "veg.mean")
summary(summary.test)

summary.test[,c("LST.trend", "LST.p", "LST.R2")] <- NA
summary.test[,c("tree.trend", "tree.p", "tree.R2")] <- NA
summary.test[,c("veg.trend", "veg.p", "veg.R2")] <- NA

pb.lms <- txtProgressBar(min=0, max=nrow(summary.test), style=3)
for(i in 1:nrow(summary.test)){
	rows.test <- which(vals.test$location==summary.test$location[i])
	setTxtProgressBar(pb.lms, i)
	
	trend.LST <- lm(LST_Day ~ year, data=vals.test[rows.test,])
	sum.LST <- summary(trend.LST)
	summary.test[i,c("LST.trend", "LST.p")] <- sum.LST$coefficients["year",c(1,4)]
	summary.test[i,"LST.R2"] <- sum.LST $r.squared
	
	trend.tree <- lm(cover.tree ~ year, data=vals.test[rows.test,])
	sum.tree <- summary(trend.tree)
	summary.test[i,c("tree.trend", "tree.p")] <- sum.tree$coefficients["year",c(1,4)]
	summary.test[i,"tree.R2"] <- sum.tree$r.squared

	trend.veg <- lm(cover.veg ~ year, data=vals.test[rows.test,])
	sum.veg <- summary(trend.veg)
	summary.test[i,c("veg.trend", "veg.p")] <- sum.veg$coefficients["year",c(1,4)]
	summary.test[i,"veg.R2"] <- sum.veg$r.squared
	
}
summary(summary.test)

ggplot(data= summary.test, aes(x=x, y=y)) +
	# geom_tile(fill="white") +
	geom_tile(aes(fill=LST.trend), alpha=0.5) +
	geom_tile(data= summary.test[summary.test$LST.p<=0.05,], aes(x=x,y=y, fill=LST.trend), color="black", fill=NA, size=0.1) +
	scale_fill_gradient2(low="blue2", high="red2", mid="gray90", midpoint=0) + theme_bw()
	
ggplot(data= summary.test, aes(x=x, y=y)) +
	# geom_tile(fill="white") +
	geom_tile(aes(fill=tree.trend), alpha=0.5) +
	geom_tile(data= summary.test[summary.test$tree.p<=0.05,], aes(x=x,y=y, fill=tree.trend), color="black", fill=NA, size=0.1) +
	scale_fill_gradient2(low="blue2", high="red2", mid="gray90", midpoint=0) + theme_bw()
	
	
# Test for the relationship between warming trend and tree trend
ggplot(data=summary.test) +
	geom_point(aes(x=tree.trend, y=LST.trend))
	
tree.lst <- lm(LST.trend ~ tree.trend, data=summary.test)
summary(tree.lst)
	
