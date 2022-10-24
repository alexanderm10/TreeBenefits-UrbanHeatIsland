library(raster); library(sp); library(terra)
library(ggplot2)

sdei <- read.csv("../data_raw/sdei-global-uhi-2013.csv")
sdei <- sdei[sdei$ES00POP>=100e3 & sdei$SQKM_FINAL>1e2,]
dim(sdei)

sdei[grep("AUS", sdei$ISOURBID),]

lst.hobart <- brick("/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output/AUS226_LST_Day_Tmean.tif")-273.15
elev.hobart <- raster("/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output/AUS226_elevation.tif")


# # lst.cairns <- brick("/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output/AUS62365_LST_Day_Tmean.tif")-273.15
# elev.cairns <- raster("/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output/AUS62365_elevation.tif")

# lst.perth <- brick("/Volumes/GoogleDrive/My Drive/UHI_Analysis_Output/AUS65991_LST_Day_Tmean.tif")-273.15


# plot(lst.hobart)
lst.hobart; 
# lst.cairns
# lst.perth; 

# lstTrend.hobart <- lm(lst.hobart ~ . )
# tmean.hobart <- mean(lst.hobart)
# tmean.cairns <- mean(lst.cairns)
# tmean.perth <- mean(lst.perth)

# plot(elev.hobart); plot(tmean.hobart)
# plot(elev.cairns); plot(tmean.cairns)

coords.hobart <- data.frame(coordinates(lst.hobart))
coords.hobart$location <- paste0("x", coords.hobart$x, "y", coords.hobart$y)

vals.hobart <- data.frame(getValues(lst.hobart))
vals.hobart <- stack(data.frame(getValues(lst.hobart)))
names(vals.hobart) <- c("LST_Day", "year")
vals.hobart$year <- as.numeric(substr(vals.hobart$year, 2, 5))
vals.hobart[,c("x", "y", "location")] <- coords.hobart
vals.hobart$elevation <- getValues(elev.hobart)
vals.hobart <- vals.hobart[!is.na(vals.hobart$LST_Day),]
dim(vals.hobart); summary(vals.hobart)
length(unique(vals.hobart$location))

# For computational tractability, need to run each pixel independently.  Doing Hobart as a loop just takes a few seconds
summary.hobart <- coords.hobart[coords.hobart$location %in% unique(vals.hobart$location),]
summary.hobart[,c("trendLST", "pLST", "R2")] <- NA
for(i in 1:nrow(summary.hobart)){
	trendLST <- lm(LST_Day ~ year, data=vals.hobart[vals.hobart$location==coords.hobart$location[i],])
	sumLST <- summary(trendLST)
	summary.hobart[i,c("trendLST", "pLST")] <- sumLST$coefficients["year",c(1,4)]
	summary.hobart[i,"R2"] <- sumLST$r.squared
	
}
summary(summary.hobart)

ggplot(data= summary.hobart, aes(x=x, y=y)) +
	# geom_tile(fill="white") +
	geom_tile(aes(fill=trendLST)) +
	geom_tile(data= summary.hobart[summary.hobart$pLST<=0.05,], aes(x=x,y=y), color="black", fill=NA, size=1) +
	scale_fill_gradient2(low="blue2", high="red2", mid="gray90", midpoint=0) + theme_bw()
	
