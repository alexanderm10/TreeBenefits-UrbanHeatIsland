# Analyzing effects of trees on the Urban Heat Island effect
library(sp); library(rgdal); library(raster); library(rgeos); library(maps)
library(ggplot2); library(RColorBrewer); library(nlme); library(mgcv); 
path.dat <- "../data_processed/"

# --------------------------------------------------------------
# Run models
# --------------------------------------------------------------
dat.uhi <- read.csv(file.path(path.dat, "cities_summary_sdei_v5.csv"))
# dat.uhi <- dat.uhi[!is.na(dat.uhi$elev.mean), ]
dat.uhi$temp.diff <- dat.uhi$temp.max - dat.uhi$temp.min
dat.uhi$tree.diff <- dat.uhi$tree.max - dat.uhi$tree.min
summary(dat.uhi)
dim(dat.uhi)
# dat.uhi[grepl("[?]", dat.uhi$NAME),"NAME"]

uhi.sp <- SpatialPointsDataFrame(coords=dat.uhi[,c("LONGITUDE", "LATITUDE")], data=dat.uhi, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
summary(uhi.sp)

# Getting some baseline ecoregion info: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
ecoregions <- readOGR("../data_raw/wwf_biomes_official/wwf_terr_ecos.shp")
# ecoregions <- readOGR("../data_raw/global200ecoregions/g200_terr.shp")
ecoregions$biome.name <- car::recode(ecoregions$BIOME, "'1'='tropical moist broadleaf forest'; 
                                     '2'='tropical dry broadleaf forest'; 
                                     '3'='tropical coniferous forest';
                                     '4'='temperate broadleaf/mixed forest'; 
                                     '5'='temperate coniferous forest'; 
                                     '6'='boreal forest/taiga'; 
                                     '7'='tropical grassland/savannas'; 
                                     '8'='temperate grassland/savanna'; 
                                     '9'='flodded grassland/savanna'; 
                                     '10'='montane grassland/savanna'; 
                                     '11'='tundra'; 
                                     '12'='mediterranean'; 
                                     '13'='desert/xeric shrublands'; 
                                     '14'='mangroves'")
unique(data.frame(ecoregions[ecoregions$BIOME==4, "G200_REGIO"]))
summary(ecoregions)
# plot(ecoregions)

# Filter outliers
filter.outliers <- function(DAT, n.sigma=6){
  tmp.x <- mean(DAT, na.rm=T); tmp.sd <- sd(DAT, na.rm=T)
  while(length(which(DAT < tmp.x-n.sigma*tmp.sd | DAT > tmp.x+n.sigma*tmp.sd))>0){
    DAT[DAT < tmp.x-n.sigma*tmp.sd]  <- NA
    DAT[DAT > tmp.x+n.sigma*tmp.sd]  <- NA
    
    tmp.x <- mean(DAT, na.rm=T); tmp.sd <- sd(DAT, na.rm=T)
  } # end while loop
  
  return(DAT)
} # End function

# New plan for quantifying effects of trees on UHI: 
# apply mean temperature of treeless area to whole area and calculate the difference
# Set some thresholds for analyses
tree.low <- 10

pb <- txtProgressBar(min=0, max=nrow(dat.uhi), style=3)
for(i in 1:nrow(dat.uhi)){
  # i=which(dat.uhi$NAME=="Chicago") # MEDELLIN
  # i=which(dat.uhi$NAME=="MEDELLIN")
  # i=which(dat.uhi$NAME=="Tokyo")
  # i=which(dat.uhi$NAME=="Nashville-Davidson")
  
  
  setTxtProgressBar(pb, i)
  biome <- extract(ecoregions, uhi.sp[i,], fun=modal)
  dat.uhi[i,"WWF_ECO"] <- biome$ECO_NAME
  dat.uhi[i,"WWF_BIOME"] <- biome$biome.name
  
  dat.city <- read.csv(file.path(path.dat, "cities_full_sdei_v5_old", paste0(dat.uhi$NAME[i], "_data_full.csv")))
  # summary(dat.city)
  
  # Remove impossible values and outliers
  dat.city[dat.city$cover.tree<0 | dat.city$cover.tree>100 , "cover.tree"] <- NA # Get rid of impossible values
  # dat.city$temp.summer <- filter.outliers(DAT=dat.city$temp.summer, n.sigma=4)
  # dat.city$temp.dev.summer <- filter.outliers(DAT=dat.city$temp.dev.summer, n.sigma=4)

  # Add a couple QAQC flags
  dat.uhi[i,"n.cells.tdev"] <- length(which(!is.na(dat.city$temp.dev.summer)))
  dat.uhi[i,"prop.missing"] <- length(which(is.na(dat.city$temp.dev.summer)))/nrow(dat.city)
  dat.uhi[i, "prop.tree.low"] <- length(which(dat.city$cover.tree<tree.low))/length(which(!is.na(dat.city$cover.tree)))
  dat.uhi[i,"tree.05"] <- quantile(dat.city$cover.tree, 0.1, na.rm=T)
  dat.uhi[i,"tree.10"] <- quantile(dat.city$cover.tree, 0.1, na.rm=T)
  dat.uhi[i,"tree.90"] <- quantile(dat.city$cover.tree, 0.9, na.rm=T)
  dat.uhi[i,"tree.95"] <- quantile(dat.city$cover.tree, 0.95, na.rm=T)
  
  dat.city <- dat.city[!is.na(dat.city$temp.dev.summer),]
  summary(dat.city)
  
  # dim(dat.city)
  
  # ggplot(data=dat.city) +
  #   coord_equal() +
  #   geom_raster(aes(x=x, y=y, fill=temp.dev.summer))
  # ggplot(data=dat.city) +
  #   coord_equal() +
  #   geom_raster(aes(x=x, y=y, fill=cover.tree))
  
  # Workign through spatial autocorrelation from here: http://rfunctions.blogspot.com/2017/06/how-to-identify-and-remove-spatial.html
  # mod.gls <- gls(temp.summer ~ cover.tree, data=dat.city)
  # semivario <- Variogram(mod.gls, form = ~x + y, resType = "normalized")
  # plot(semivario, smooth = TRUE)
  
  # Theoretically speaking and based on a quick EDA of a subset of data, the Gaussian spatial autocorrelation function seemed to be most appropriate and had the lowest AIC
  # gls.gaus <- gls( temp.summer ~ cover.tree , correlation = corGaus(form = ~x + y), data = dat.city )
  # sqrt(nrow(dat.city))
  # For smoothing, not sure if a soap smoother is appropriate or not
  dat.city[dat.city$cover.tree==0, "cover.tree"] <- 0.1
  mod.gam.lin <- gam(temp.dev.summer ~ cover.tree + elevation + s(x,y), data=dat.city)
  mod.gam.log <- gam(temp.dev.summer ~ log(cover.tree) + elevation + s(x,y), data=dat.city)

  AIC(mod.gam.lin, mod.gam.log)
  
  sum.lin <- summary(mod.gam.lin)
  sum.log <- summary(mod.gam.log)
  
  dat.uhi[i, "tree.slope.lin"] <- sum.lin$p.coeff["cover.tree"]
  dat.uhi[i, "tree.slope.log"] <- sum.log$p.coeff["log(cover.tree)"]
  if(sum.lin$r.sq >= sum.log$r.sq) {
    mod.gam <- mod.gam.lin
    
    dat.uhi[i, "model.type"] <- "linear"
    dat.uhi[i, "gam.r2"] <- sum.lin$r.sq
    dat.uhi[i, "gam.dev.exp"] <- sum.lin$dev.expl
    dat.uhi[i, "elevation.slope"] <- sum.lin$p.coeff["elevation"]
    dat.uhi[i, "tree.slope"] <- sum.lin$p.coeff["cover.tree"]
    dat.uhi[i, "tree.pval"] <- sum.lin$p.pv["cover.tree"]
    
  } else {
    mod.gam <- mod.gam.log
    
    dat.uhi[i, "model.type"] <- "log-effect"
    dat.uhi[i, "gam.r2"] <- sum.log$r.sq
    dat.uhi[i, "gam.dev.exp"] <- sum.log$dev.expl
    dat.uhi[i, "elevation.slope"] <- sum.log$p.coeff["elevation"]
    dat.uhi[i, "tree.slope"] <- sum.log$p.coeff["log(cover.tree)"]
    dat.uhi[i, "tree.pval"] <- sum.log$p.pv["log(cover.tree)"]
    
  }
  
  gam.summary <- summary(mod.gam)
  dat.city$gam.pred <- predict(mod.gam)
  dat.city$gam.resid <- resid(mod.gam)
  # plot(mod.gam)
  
  png(file.path("../data_processed/cities_full_sdei_v5", paste0(dat.uhi$NAME[i], "_GAM_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(2,2))
  plot(mod.gam)
  hist(dat.city$gam.resid)
  plot(gam.resid ~ gam.pred, data=dat.city); abline(h=0, col="red")
  plot(temp.dev.summer ~ gam.pred, data=dat.city); abline(a=0, b=1, col="red")
  par(mfrow=c(1,1))
  dev.off()
  
  # Creating a "world with no trees" scenario
  dat.new <- dat.city
  dat.new$cover.tree <- 0.1
  
  dat.city$gam.trees.no <- predict(mod.gam, newdata=dat.new)
  dat.city$diff.gam.no <- dat.city$temp.dev.summer - dat.city$gam.trees.no
  dat.uhi[i,"tree.cooling.no"] <- mean(dat.city$diff.gam.no, na.rm=T)

  # Try using the minimum tree cover in that city instead for heavily greened cities
  dat.new$cover.tree <- min(dat.city$cover.tree, na.rm=T)
  
  dat.city$gam.trees.lo <- predict(mod.gam, newdata=dat.new)
  dat.city$diff.gam.lo <- dat.city$temp.dev.summer - dat.city$gam.trees.lo
  dat.uhi[i,"tree.cooling.lo"] <- mean(dat.city$diff.gam.lo, na.rm=T)
  
  rm(dat.city)
}
dat.uhi$tree.puhi <- -dat.uhi$tree.cooling/(dat.uhi$D_T_DIFF-dat.uhi$tree.cooling)
dat.uhi$WWF_BIOME <- as.factor(dat.uhi$WWF_BIOME)
dat.uhi$model.type <- as.factor(dat.uhi$model.type)
summary(dat.uhi)

write.csv(dat.uhi, "../data_processed/analysis_cities_summary_sdei_v5.csv", row.names=F)
# --------------------------------------------------------------
# dat.uhi[is.na(dat.uhi$gam.r2),]

# --------------------------------------------------------------
# Look at output
# --------------------------------------------------------------
dat.uhi <- read.csv("../data_processed/analysis_cities_summary_sdei_v5.csv")
dat.filter <- dat.uhi$prop.missing<0.3 & dat.uhi$tree.90>10
dim(dat.uhi)
dim(dat.uhi[dat.filter,])
dim(dat.uhi[!dat.filter,])


hist(dat.uhi$D_T_DIFF)
mean(dat.uhi$D_T_DIFF); sd(dat.uhi$D_T_DIFF)
median(dat.uhi$D_T_DIFF)
quantile(dat.uhi$D_T_DIFF, c(0.025, 0.975))

hist(dat.uhi$tree.mean)
mean(dat.uhi$tree.mean); sd(dat.uhi$tree.mean)
median(dat.uhi$tree.mean)
quantile(dat.uhi$tree.mean, c(0.025, 0.975))

hist(dat.uhi$tree.max)
mean(dat.uhi$tree.max); sd(dat.uhi$tree.max)
median(dat.uhi$tree.max)
quantile(dat.uhi$tree.max, c(0.025, 0.975))

hist(dat.uhi$tree.10)
mean(dat.uhi$tree.10); sd(dat.uhi$tree.10)
median(dat.uhi$tree.10)
quantile(dat.uhi$tree.10, c(0.025, 0.975))

hist(dat.uhi$tree.90)
mean(dat.uhi$tree.90); sd(dat.uhi$tree.90)
median(dat.uhi$tree.90)
quantile(dat.uhi$tree.90, c(0.025, 0.975))

# Creating criteria by which to exclude results
summary(dat.uhi[dat.uhi$tree.cooling< -10,])


# Model Summary
hist(dat.uhi$gam.r2)
range(dat.uhi$gam.r2)
mean(dat.uhi$gam.r2); sd(dat.uhi$gam.r2)
summary(dat.uhi$tree.slope.lin)
summary(dat.uhi$elevation.slope) # adiabatic laspe = -9.8˚C/km = -9.8e-3

# Looking for significant 
dat.filter <- dat.uhi$prop.missing<0.3 & dat.uhi$tree.90>10
cool.filter <- dat.filter & dat.uhi$tree.pval<0.05 & dat.uhi$tree.cooling<0
warm.filter <- dat.filter & dat.uhi$tree.pval<0.05 & dat.uhi$tree.cooling>0

summary(dat.uhi[cool.filter,])
summary(dat.uhi[warm.filter,])
dat.uhi[dat.uhi$tree.cooling< -10,]

length(which(dat.uhi$tree.pval<0.05))/nrow(dat.uhi[,]) # Significant tree effect in 93% of ALL cities, even with bad data
length(which(cool.filter | warm.filter))/nrow(dat.uhi[dat.filter,]) # Significant tree effect in 97% of cities
length(which(cool.filter))/nrow(dat.uhi[dat.filter,]) # Significant tree cooling effect in 96% of cities
# Seeing which tree has warming
length(which(warm.filter))
dat.uhi[warm.filter,] 

t.test(dat.uhi[cool.filter,"tree.mean"], dat.uhi[warm.filter,"tree.mean"])
t.test(dat.uhi[cool.filter,"tree.sd"], dat.uhi[warm.filter,"tree.sd"])
t.test(dat.uhi[cool.filter,"gam.r2"], dat.uhi[warm.filter,"gam.r2"])

hist(dat.uhi$tree.cooling)
mean(dat.uhi$tree.cooling); sd(dat.uhi$tree.cooling)
median(dat.uhi$tree.cooling)
quantile(dat.uhi$tree.cooling, c(0.025, 0.975))

hist(dat.uhi$tree.puhi)
mean(dat.uhi$tree.puhi); sd(dat.uhi$tree.puhi)
median(dat.uhi$tree.puhi)
quantile(dat.uhi$tree.puhi, c(0.025, 0.975))


# dat.uhi <- dat.uhi[dat.uhi$tree.puhi>=-1,] # Can't have >100% cooling
# summary(dat.uhi)
# dat.uhi <- dat.uhi[dat.uhi$tree.cooling>=-4,]

summary(dat.uhi[dat.uhi$tree.slope>0,])

dat.uhi$tree.puhi.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$tree.cooling, NA)
dat.uhi$tree.cooling.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$tree.cooling, NA)
dat.uhi$tree.slope.lin.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$tree.slope.lin, NA)
dat.uhi$tree.slope.log.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$tree.slope.log, NA)
summary(dat.uhi)

range(dat.uhi$tree.cooling)
mean(dat.uhi$tree.cooling); sd(dat.uhi$tree.cooling)
quantile(dat.uhi$tree.cooling, c(0.025, 0.975))

range(dat.uhi$tree.slope)
mean(dat.uhi$tree.slope); sd(dat.uhi$tree.slope)
quantile(dat.uhi$tree.slope, c(0.025, 0.975))


# ---------------------------------
# Making some figures
# ---------------------------------
path.figs <- "../figures/v5"
dir.create(path.figs, recursive=T, showWarnings=F)

# Creating a categorical, binned warming response
dat.uhi$TreeEffect <- round(dat.uhi$tree.cooling*2)/2 # To the nearest 0.5 degree
dat.uhi$TreeEffect2 <- round(dat.uhi$tree.cooling) # To the nearest degree
summary(dat.uhi)
length(unique(dat.uhi$TreeEffect))


# bins <- 100
# cols <- c("darkblue","darkred")
# colGradient <- colorRampPalette(cols)
# cut.cols <- colGradient(bins)
# cuts <- cut(df$val,bins)
# names(cuts) <- sapply(cuts,function(t) cut.cols[which(as.character(t) == levels(cuts))])
# 
effect.lims <- seq(-1*max(abs(dat.uhi$TreeEffect)), 1*max(abs(dat.uhi$TreeEffect)), by=0.5)
effect.palette <- colorRampPalette(brewer.pal(11, "BrBG"))(length(effect.lims))

effect.lims2 <- seq(-1*max(abs(dat.uhi$TreeEffect2)), 1*max(abs(dat.uhi$TreeEffect2)), by=1)
effect.palette2 <- colorRampPalette(brewer.pal(11, "BrBG"))(length(effect.lims2))
# length(unique())

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_TreeCover_Map.png"), height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tree.mean), size=3) +
  scale_colour_distiller(name="Mean Tree Cover", palette=rev("BrBG"), trans="reverse") +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_TreeCover_Max_Map.png"), height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tree.max), size=3) +
  scale_colour_distiller(name="Maximum Tree Cover", palette=rev("BrBG"), trans="reverse") +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_TreeModel_Map.png"), height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(data=dat.uhi[!dat.filter,], aes(x=LONGITUDE, y=LATITUDE, color="data deficient"), size=1.5) +
  geom_point(data=dat.uhi[dat.filter & dat.uhi$tree.pval>=0.05,], aes(x=LONGITUDE, y=LATITUDE, color="no effect"), size=2) +
  geom_point(data=dat.uhi[dat.filter & dat.uhi$tree.pval<0.05,], aes(x=LONGITUDE, y=LATITUDE, color=model.type), size=3) +
  scale_color_manual(name="Model Type", values=c("gray70", "#a6cee3", "#1f78b4", "gray30")) +
  # scale_colour_distiller(name="Maximum Tree Cover", palette=rev("BrBG"), trans="reverse") +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()


effect.lims[which(effect.lims %in% unique(dat.uhi$TreeEffect))]
effect.use <- effect.palette[which(effect.lims %in% unique(dat.uhi$TreeEffect))]
effect.palette[26]
png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_TreeCooling_histogram_GAM.png"), height=4, width=8, units="in", res=220)
ggplot(data=dat.uhi[dat.filter,]) +
  coord_cartesian(expand=0) +
  geom_histogram(aes(x=factor(TreeEffect2), fill=factor(TreeEffect2)), stat="count", width=1) +
  geom_histogram(data=dat.uhi[dat.filter & dat.uhi$tree.pval>=0.05,],aes(x=factor(TreeEffect2)), fill="gray50", stat="count", width=1) +
  # geom_histogram(aes(x=tree.cooling, fill=cut(tree.cooling, 100)), binwidth=1, show.legend=F) +
  # geom_histogram(aes(x=tree.cooling, fill=), binwidth=1, show.legend=F) +
  scale_fill_manual(name="Tree Effect\non Temperature\n(deg. C)", values=rev(effect.palette2)[which(effect.lims2 %in% unique(dat.uhi$TreeEffect2))]) +
  # scale_fill_brewer(palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$tree.cooling))) +
  labs(x="Tree Effect on Temperature (deg. C)", y="City Count") +
  theme_bw()+
  theme(panel.background = element_rect(fill="gray80"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_TreeCooling_Map_GAM.png"), height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(data=dat.uhi[!dat.filter,], aes(x=LONGITUDE, y=LATITUDE), size=1, color="gray70") +
  geom_point(data=dat.uhi[dat.filter & dat.uhi$tree.pval>=0.05,], aes(x=LONGITUDE, y=LATITUDE), color="gray30", size=3) +
  geom_point(data=dat.uhi[dat.filter & dat.uhi$tree.pval<0.05,], aes(x=LONGITUDE, y=LATITUDE, color=tree.cooling), size=3) +
  scale_colour_distiller(name="Tree Effect\non Temperature\n(deg. C)", palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$tree.cooling))) +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_TreeCooling_Map_GAM_categorical.png"), height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(data=dat.uhi[!dat.filter,], aes(x=LONGITUDE, y=LATITUDE), size=1, color="gray70") +
  geom_point(data=dat.uhi[dat.filter & dat.uhi$tree.pval>=0.05,], aes(x=LONGITUDE, y=LATITUDE), color="gray30", size=3) +
  geom_point(data=dat.uhi[dat.filter & dat.uhi$tree.pval<0.05,], aes(x=LONGITUDE, y=LATITUDE, color=factor(TreeEffect2)), size=3) +
  scale_color_manual(name="Tree Effect\non Temperature\n(deg. C)", values=rev(effect.palette2)[which(effect.lims2 %in% unique(dat.uhi$TreeEffect2))]) +
  
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()


ggplot(data=dat.uhi[dat.filter,]) +
  geom_point(data=dat.uhi[!dat.filter,], aes(x=tree.mean, y=tree.cooling), color="gray50") +
  geom_point(aes(x=tree.mean, y=tree.cooling)) +
  theme_bw()

ggplot(data=dat.uhi[dat.filter,]) +
  geom_point(data=dat.uhi[!dat.filter,], aes(x=tree.max, y=tree.cooling), color="gray50") +
  geom_point(aes(x=tree.max, y=tree.cooling)) +
  theme_bw()

ggplot(data=dat.uhi[dat.filter,]) +
  geom_point(data=dat.uhi[!dat.filter,], aes(x=tree.90, y=tree.cooling), color="gray50") +
  geom_point(aes(x=tree.90, y=tree.cooling)) +
  theme_bw()

ggplot(data=dat.uhi[dat.filter,]) +
  geom_point(data=dat.uhi[!dat.filter,], aes(x=temp.mean, y=tree.cooling), color="gray50") +
  geom_point(aes(x=temp.mean, y=tree.cooling)) +
  theme_bw()

ggplot(data=dat.uhi[dat.filter,]) +
  geom_point(data=dat.uhi[!dat.filter,], aes(x=LATITUDE, y=tree.cooling), color="gray50") +
  geom_point(aes(x=LATITUDE, y=tree.cooling)) +
  theme_bw()


ggplot(data=dat.uhi[dat.filter,]) +
  geom_point(data=dat.uhi[!dat.filter,], aes(x=elev.sd, y=tree.cooling), color="gray50") +
  geom_point(aes(x=elev.sd, y=tree.cooling)) +
  theme_bw()

# --------------------------------------------------------------
