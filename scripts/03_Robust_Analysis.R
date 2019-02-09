# Doing some quick EDA for geography of Urban Heat island
library(ggplot2); library(RColorBrewer); library(nlme); library(mgcv); 
path.dat <- "../data_processed/"
dat.uhi <- read.csv(file.path(path.dat, "cities_summary_sdei_v2.csv"))
dat.uhi <- dat.uhi[dat.uhi$tree.min>=0 & dat.uhi$D_T_DIFF>=0,]
dat.uhi$temp.diff <- dat.uhi$temp.max - dat.uhi$temp.min
dat.uhi$tree.diff <- dat.uhi$tree.max - dat.uhi$tree.min
summary(dat.uhi)
dim(dat.uhi)

# New plan for quantifying effects of trees on UHI: 
# apply mean temperature of treeless area to whole area and calculate the difference
pb <- txtProgressBar(min=0, max=nrow(dat.uhi), style=3)
for(i in 1:nrow(dat.uhi)){
  # i=which(dat.uhi$NAME=="Chicago")
  setTxtProgressBar(pb, i)
  dat.city <- read.csv(file.path(path.dat, "cities_full_sdei_v2", paste0(dat.uhi$NAME[i], "_data_full.csv")))
  summary(dat.city)
  # # Trimming it down to test
  # dat.city <- dat.city[dat.city$x<=-87.5 & dat.city$y<=42.5,]
  
  dim(dat.city)
  
  # ggplot(data=dat.city) +
  #   coord_equal() +
  #   geom_raster(aes(x=x, y=y, fill=temp.summer))
  # ggplot(data=dat.city) +
  #   coord_equal() +
  #   geom_raster(aes(x=x, y=y, fill=cover.tree))
  
  # Workign through spatial autocorrelation from here: http://rfunctions.blogspot.com/2017/06/how-to-identify-and-remove-spatial.html
  mod.gls <- gls(temp.summer ~ cover.tree, data=dat.city)
  # semivario <- Variogram(mod.gls, form = ~x + y, resType = "normalized")
  # plot(semivario, smooth = TRUE)

  # Theoretically speaking and based on a quick EDA of a subset of data, the Gaussian spatial autocorrelation function seemed to be most appropriate and had the lowest AIC
  # gls.gaus <- gls( temp.summer ~ cover.tree , correlation = corGaus(form = ~x + y), data = dat.city )
  # sqrt(nrow(dat.city))
  mod.gam <- gam(temp.summer ~ cover.tree + s(x,y), data=dat.city)
  gam.summary <- summary(mod.gam)
  # plot(mod.gam)
  
  # plot(semivario, smooth = TRUE)
  
  dat.new <- dat.city
  dat.new$cover.tree <- 0

  dat.city$gam.pred <- predict(mod.gam)
  dat.city$gam.resid <- resid(mod.gam)
  dat.city$gls.notrees <- predict(mod.gls, newdata=dat.new)
  dat.city$gam.notrees <- predict(mod.gam, newdata=dat.new)
  dat.city$diff.gls <- dat.city$temp.summer - dat.city$gls.notrees
  dat.city$diff.gam <- dat.city$temp.summer - dat.city$gam.notrees
  summary(dat.city)
  
  # par(mfrow=c(1,1))
  # plot(temp.summer ~ gam.pred, data=dat.city)
  # plot(gam.resid ~ gam.pred, data=dat.city)
  # 
  # plot(temp.summer ~ gls.pred, data=dat.city)
  # plot(gls.resid ~ gls.pred, data=dat.city)
  
  
  # ggplot(data=dat.city) +
  #   coord_equal() +
  #   geom_raster(aes(x=x, y=y, fill=gam.resid))
  
  # Not necessarily the best, but using the mean treeless temperature as a baseline
  # temp.notree <- mean(dat.city[dat.city$cover.tree<0.5, "temp.summer"], na.rm=T)
  
  dat.uhi[i,"gam.r2"] <- gam.summary$r.sq
  dat.uhi[i,"tree.slope"] <- gam.summary$p.coeff["cover.tree"]
  dat.uhi[i,"tree.cooling"] <- mean(dat.city$diff.gam, na.rm=T)
  
  rm(dat.city)
}
summary(dat.uhi)

hist(dat.uhi$tree.cooling)
hist(dat.uhi$tree.slope)

# Pulling out some outliers
dat.uhi <- dat.uhi[dat.uhi$tree.cooling>=-4 & dat.uhi$tree.slope>-1,]

hist(dat.uhi$tree.cooling)
hist(dat.uhi$tree.slope)

range(dat.uhi$tree.cooling)
mean(dat.uhi$tree.cooling); sd(dat.uhi$tree.cooling)
mean(dat.uhi$tree.slope); sd(dat.uhi$tree.slope)

# Creating a categorical, binned warming response
dat.uhi$TreeEffect <- round(dat.uhi$tree.cooling*2)/2
summary(dat.uhi)
length(unique(dat.uhi$TreeEffect))
# bins <- 100
# cols <- c("darkblue","darkred")
# colGradient <- colorRampPalette(cols)
# cut.cols <- colGradient(bins)
# cuts <- cut(df$val,bins)
# names(cuts) <- sapply(cuts,function(t) cut.cols[which(as.character(t) == levels(cuts))])
# 
color.lims <- seq(-1*max(abs(dat.uhi$TreeEffect)), 1*max(abs(dat.uhi$TreeEffect)), by=0.5)
effect.palette <- colorRampPalette(brewer.pal(11, "BrBG"))(length(color.lims))
# length(unique())

png("../figures/TreeBenefits_UrbanHeatIsland_TreeCooling_histogram_GAM.png", height=4, width=8, units="in", res=220)
ggplot(data=dat.uhi) +
  coord_cartesian(expand=0) +
  geom_histogram(aes(x=factor(TreeEffect), fill=factor(TreeEffect)), stat="count", width=1) +
  # geom_histogram(aes(x=tree.cooling, fill=cut(tree.cooling, 100)), binwidth=1, show.legend=F) +
  # geom_histogram(aes(x=tree.cooling, fill=), binwidth=1, show.legend=F) +
  scale_fill_manual(name="Tree Effect\non Temperature\n(deg. C)", values=rev(effect.palette)) +
  # scale_fill_brewer(palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$tree.cooling))) +
  labs(x="Tree Effect on Temperature (deg. C)", y="City Count") +
  theme_bw() 
dev.off()

png("../figures/TreeBenefits_UrbanHeatIsland_TreeCooling_Map_GAM.png", height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tree.cooling), size=5) +
  scale_colour_distiller(name="Tree Effect\non Temperature\n(deg. C)", palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$tree.cooling))) +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()


png("../figures/TreeBenefits_UrbanHeatIsland_TreeSlope_Map_GAM.png", height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tree.slope), size=5) +
  scale_colour_distiller(name="Tree Effect\ndeg. C/% canopy)", palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$tree.slope))) +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()
