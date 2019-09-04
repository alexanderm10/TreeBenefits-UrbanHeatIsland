library(ggplot2); library(RColorBrewer)
library(sp); library(rgdal); library(raster); library(rgeos); library(maps)

# path.figs <- "../figures/v6_vegonly"
path.dat <- "../data_processed/"
path.figs <- "/Volumes/GoogleDrive/My Drive/TreeBenefits_UrbanHeatIsland/figures/veg_only/city_maps"
dir.create(path.figs, recursive=T, showWarnings=F)

biome.pall = data.frame(biome=c("Desert", "Grassland/Savanna", "Mediterranean", "Tropical Forest", "Temperate Forest", "Boreal"),
                        color=c("#D55E00", "#E69F00", "#CC79A7", "#009E73", "#56B4E9", "#0072B2"),
                        color2=c("#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e"))

cat.3 <- data.frame(cover=c("tree", "other", "non-veg"),
                    color=c("#1b9e77", "#7570b3", "#d95f02"),
                    color2=c("#66c2a5", "#8da0cb", "#fc8d62"),
                    color3=c("#018571", "#4dac26", "#e66101"),
                    color4=c("#80cdc1", "#4dac26", "#fbd863"),
                    color5=c("#1b9e77", "#4dac26", "#d95f02"),
                    color6=c("#1b9e77", "#b2df8a", "#a6611a"))
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
grad.bare <- c("#5e3c99", "#b2abd2", "#f7f7f7", "#fbd863", "#e66101") # Ends with orange

# --------------------------------------------------------------
# Look at output
# --------------------------------------------------------------
# -----------
# Get the city shapefiles
# -----------
cities.sp <- readOGR("/Volumes/Morton_SDM/sdei-global-uhi-2013-shp/shp/sdei-global-uhi-2013.shp")
cities.sp$NAME <- as.character(cities.sp$NAME)
cities.sp[cities.sp$NAME=="Xi'an", "NAME"] <- "Xian" # because syntax won't work with recode
cities.sp$NAME[grepl("[?]", cities.sp$NAME)]

cities.sp$NAME <- car::recode(cities.sp$NAME, "'?stanbul'='Istanbul'; 'S?o Paulo'='Sao Paulo'; '?zmir'='Izmir'; 'S?o Jos? dos Campos'='Sao Jose dos Campos'; 'Ni?nij Novgorod'='Nizhny Novgorod'")
cities.sp$NAME <- gsub("[?]", "XX", cities.sp$NAME)
cities.sp$NAME <- gsub(" ", "", cities.sp$NAME)
cities.sp$NAME <- gsub("  ", "", cities.sp$NAME)
cities.sp$NAME <- as.factor(cities.sp$NAME)
cities.sp <- cities.sp[order(cities.sp$NAME), ]
# -----------
# cities.sp <- 


dat.uhi <- read.csv("../data_processed/analysis_cities_summary_sdei_v6.csv")
dat.uhi <- dat.uhi[order(dat.uhi$ISOURBID),]
dat.filter <- dat.uhi$prop.missing<0.33 & dat.uhi$prop.temp.n.lo<0.33
head(dat.uhi[,1:10])

cities.sp <- cities.sp[cities.sp$ISOURBID %in% dat.uhi$ISOURBID,]
cities.sp <- cities.sp[order(cities.sp$ISOURBID),]
# summary(cities.sp)0
head(cities.sp[,1:10])
dim(cities.sp); dim(dat.uhi)


summary(dat.uhi[dat.filter & dat.uhi$tree.pval<0.01 & dat.uhi$gam.r2>0.9, ])

dat.uhi[dat.filter & dat.uhi$tree.pval<0.01 & dat.uhi$gam.r2>0.5 & dat.uhi$Tdiff.trees2noveg.city>2, c("ISO3", "NAME", "WWF_BIOME", "ES00POP", "temp.summer.city", "cover.tree.city", "Tdiff.trees2noveg.city")]

grad.temp <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
grad.elev <- c("#993404", "#d95f0e", "#fe9929", "#fed98e", "#ffffd4")
grad.tree <- c("#a6611a", "#dfc27d", "#f5f5f5", "#80cdc1", "#018571") # ends with teal
grad.other <- c("#d01c8b", "#f1b6da", "#f7f7f7", "#b8e186", "#4dac26") # ends with green
grad.bare <- c("#5e3c99", "#b2abd2", "#f7f7f7", "#fbd863", "#e66101") # Ends with orange


pb <- txtProgressBar(min=0, max=nrow(dat.uhi), style=3)
# yr.process <- 2011:2015
for(i in 1:nrow(dat.uhi)){
  # i=which(dat.uhi$NAME=="Chicago")
  # i=which(dat.uhi$NAME=="MEDELLIN")
  # i=which(dat.uhi$NAME=="Tokyo")
  # i=which(dat.uhi$NAME=="Nashville-Davidson")
  # i=which(dat.uhi$NAME=="Atlanta")
  # i=which(dat.uhi$NAME=="Cuernavaca")
  city.name <- dat.uhi$NAME[i]
  
  setTxtProgressBar(pb, i)
  city.sp <- cities.sp[i,]
  
  path.city <- file.path(path.dat, "cities_full_sdei_v6", "analysis_all_years", paste0(dat.uhi$NAME[i], "_data_full_analyzed.csv"))
  
  if(!file.exists(path.city)) next
  
  # Read in the city data & round the coordinates so we get a regular grid; after exploratory stuff 5 digits should do it
  dat.city <- read.csv(path.city)
  dat.city$temp.summer <- dat.city$temp.summer-273.16
  dig=1
  dat.city$x2 <- round(dat.city$x*8*10^dig)/(8*10^dig)
  dat.city$y2 <- round(dat.city$y*8*10^dig)/(8*10^dig)
  summary(dat.city)
  
  vars.agg <- c("elevation", "temp.summer", "cover.tree", "cover.veg", "cover.noveg", "gam.pred", "pred.trees2noveg", "pred.trees2veg", "pred.veg2noveg")
  # filter n-sigma outliers
  # n.sig <- 4
  # for(VAR in vars.agg) {
  #   mn <- mean(dat.city[,VAR], na.rm=T)
  #   sdev <- sd(dat.city[,VAR], na.rm=T)
  # 
  #   # For graphing, remove 4-sigma outliers
  #   dat.city[dat.city[,VAR]< mn-n.sig*sdev,VAR] <- NA
  #   dat.city[dat.city[,VAR]> mn+n.sig*sdev,VAR] <- NA
  # }
  
  
  city.x <- aggregate(dat.city[,vars.agg],
                      by=dat.city[,c("Name", "x2", "y2", "location")],
                      FUN=mean)
  summary(city.x)
  dim(city.x)
  
  
  # city.x2 <- city.x
  # for(x in unique(city.x$x2)){
  # df.x <- data.frame(x2=x, y2=seq(min(city.x[city.x$x2==x, "y2"]),
  # max(city.x[city.x$x2==x, "y2"]),
  # by=1/(10^dig)))
  # city.x2 <- merge(city.x2, df.x, all=T)
  # }
  
  plot.temp <- ggplot(data=city.x[!is.na(city.x$temp.summer),]) +
    coord_equal() +
    # geom_tile(aes(x=x2, y=y2, fill=temp.summer)) +
    geom_raster(aes(x=x2, y=y2, fill=temp.summer)) +
    geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Summer\nTemp\n(deg. C)", colors=grad.temp) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
  plot.elev <- ggplot(data=city.x[!is.na(city.x$elevation),]) +
    coord_equal() +
    # geom_tile(aes(x=x2, y=y2, fill=temp.summer)) +
    geom_raster(aes(x=x2, y=y2, fill=elevation)) +
    geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Elevation\n(m)", colors=grad.elev) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
  plot.tree <- ggplot(data=city.x[!is.na(city.x$cover.tree),]) +
    coord_equal() +
    geom_raster(aes(x=x2, y=y2, fill=cover.tree)) +
    geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Tree\nCover\n(%)", colors=grad.tree) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
  plot.veg <- ggplot(data=city.x[!is.na(city.x$cover.tree),]) +
    coord_equal() +
    geom_raster(aes(x=x2, y=y2, fill=cover.tree)) +
    geom_path(data=city.sp, aes(x=long, y=lat, group=group)) +
    scale_fill_gradientn(name="Other Veg\nCover (%)", colors=grad.other) +
    theme(panel.background=element_rect(fill=NA, color="black"),
          panel.grid=element_blank(),
          axis.ticks.length = unit(-0.5, "lines"),
          axis.title=element_blank(),
          axis.text.x=element_text(margin=margin(t=1.5, unit="lines"), color="black"),
          axis.text.y=element_text(margin=margin(r=1.5, unit="lines"), color="black"))
  
    
  png(file.path(path.figs, paste0(city.name, ".png")), height=8, width=8, units="in", res=120)
  print(
    cowplot::plot_grid(plot.temp, plot.elev, plot.tree, plot.veg)
   )
  dev.off()
}
 