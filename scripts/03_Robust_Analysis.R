# Doing some quick EDA for geography of Urban Heat island
library(ggplot2); library(RColorBrewer); library(nlme); library(mgcv); 
path.dat <- "../data_processed/"
dat.uhi <- read.csv(file.path(path.dat, "cities_summary_sdei_v3.csv"))
# Remove cities where max tree cover <25%
dat.uhi <- dat.uhi[dat.uhi$tree.max>=25,]

dat.uhi$temp.diff <- dat.uhi$temp.max - dat.uhi$temp.min
dat.uhi$tree.diff <- dat.uhi$tree.max - dat.uhi$tree.min
summary(dat.uhi)
dim(dat.uhi)
dat.uhi[grepl("[?]", dat.uhi$NAME),"NAME"]

# New plan for quantifying effects of trees on UHI: 
# apply mean temperature of treeless area to whole area and calculate the difference
pb <- txtProgressBar(min=0, max=nrow(dat.uhi), style=3)
for(i in 1:nrow(dat.uhi)){
  # i=which(dat.uhi$NAME=="Chicago")
  setTxtProgressBar(pb, i)
  # if(grepl("[?]", dat.uhi$NAME[i])) next
  
  dat.city <- read.csv(file.path(path.dat, "cities_full_sdei_v3", paste0(dat.uhi$NAME[i], "_data_full.csv")))
  
  # Remove impossible values and outliers
  dat.city[dat.city$cover.tree<0 | dat.city$cover.tree>100 , "cover.tree"] <- NA # Get rid of impossible values
  
  tmax.mean <- mean(dat.city$temp.summer, na.rm=T)
  tmax.sd   <- sd(dat.city$temp.summer, na.rm=T)
  dat.city$temp.summer[dat.city$temp.summer>tmax.mean + 6*tmax.sd] <- NA
  dat.city$temp.summer[dat.city$temp.summer<tmax.mean - 6*tmax.sd] <- NA
  
  dat.city <- dat.city[complete.cases(dat.city),]
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
  # mod.gls <- gls(temp.summer ~ cover.tree, data=dat.city)
  # semivario <- Variogram(mod.gls, form = ~x + y, resType = "normalized")
  # plot(semivario, smooth = TRUE)

  # Theoretically speaking and based on a quick EDA of a subset of data, the Gaussian spatial autocorrelation function seemed to be most appropriate and had the lowest AIC
  # gls.gaus <- gls( temp.summer ~ cover.tree , correlation = corGaus(form = ~x + y), data = dat.city )
  # sqrt(nrow(dat.city))
  mod.gam <- gam(temp.summer ~ cover.tree + elevation + s(x,y), data=dat.city)
  gam.summary <- summary(mod.gam)
  dat.city$gam.pred <- predict(mod.gam)
  dat.city$gam.resid <- resid(mod.gam)
  # plot(mod.gam)
  
  png(file.path("../data_processed/cities_full_sdei_v3", paste0(dat.uhi$NAME[i], "_GAM_qaqc.png")), height=6, width=6, units="in", res=120)
  par(mfrow=c(2,2))
  plot(mod.gam)
  hist(dat.city$gam.resid)
  plot(gam.resid ~ gam.pred, data=dat.city); abline(h=0, col="red")
  plot(temp.summer ~ gam.pred, data=dat.city); abline(a=0, b=1, col="red")
  par(mrow=c(1,1))
  dev.off()
  
  # plot(semivario, smooth = TRUE)
  
  dat.new <- dat.city
  dat.new$cover.tree <- 0

  # dat.city$gls.notrees <- predict(mod.gls, newdata=dat.new)
  dat.city$gam.notrees <- predict(mod.gam, newdata=dat.new)
  # dat.city$diff.gls <- dat.city$temp.summer - dat.city$gls.notrees
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
  dat.uhi[i,"gam.dev.exp"] <- gam.summary$dev.expl
  dat.uhi[i,"elevation.slope"] <- gam.summary$p.coeff["elevation"]
  dat.uhi[i,"tree.slope"] <- gam.summary$p.coeff["cover.tree"]
  dat.uhi[i,"tree.pval"] <- gam.summary$p.pv["cover.tree"]
  dat.uhi[i,"tree.cooling"] <- mean(dat.city$diff.gam, na.rm=T)
  
  rm(dat.city)
}
dat.uhi$tree.puhi <- -dat.uhi$tree.cooling/(dat.uhi$D_T_DIFF-dat.uhi$tree.cooling)
summary(dat.uhi)

# dat.uhi[is.na(dat.uhi$gam.r2),]

dim(dat.uhi)
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


# Model Summary
hist(dat.uhi$gam.r2)
range(dat.uhi$gam.r2)
mean(dat.uhi$gam.r2); sd(dat.uhi$gam.r2)
summary(dat.uhi$tree.slope)
summary(dat.uhi$elevation.slope) # adiabatic laspe = -9.8˚C/km = -9.8e-3

# Looking for significant 
length(which(dat.uhi$tree.pval<0.05))/nrow(dat.uhi) # Significant tree cooling effect in 80% of cities
length(which(dat.uhi$tree.slope<0 & dat.uhi$tree.pval<0.05))/nrow(dat.uhi) # Significant tree cooling effect in 80% of cities
# Seeing which tree has warming
dat.uhi[dat.uhi$tree.pval<0.05 & dat.uhi$tree.slope>0,]

hist(dat.uhi$tree.slope)
mean(dat.uhi$tree.slope); sd(dat.uhi$tree.slope)
median(dat.uhi$tree.slope)
quantile(dat.uhi$tree.slope, c(0.025, 0.975))


hist(dat.uhi$tree.cooling)
mean(dat.uhi$tree.cooling); sd(dat.uhi$tree.cooling)
median(dat.uhi$tree.cooling)
quantile(dat.uhi$tree.cooling, c(0.025, 0.975))

hist(dat.uhi$tree.puhi)
mean(dat.uhi$tree.puhi); sd(dat.uhi$tree.puhi)
median(dat.uhi$tree.puhi)
quantile(dat.uhi$tree.puhi, c(0.025, 0.975))


hist(dat.uhi$D_T_DIFF[dat.uhi$D_T_DIFF<=2])
hist(dat.uhi$tree.puhi)
hist(dat.uhi$tree.cooling)
hist(dat.uhi$tree.slope)

dat.uhi <- dat.uhi[dat.uhi$tree.puhi>=-1,] # Can't have >100% cooling
summary(dat.uhi)
# dat.uhi <- dat.uhi[dat.uhi$tree.cooling>=-4,]


summary(dat.uhi[dat.uhi$tree.slope>0,])

dat.uhi$tree.cooling.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$tree.cooling, NA)
dat.uhi$tree.slope.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$tree.slope, NA)

range(dat.uhi$tree.cooling)
mean(dat.uhi$tree.cooling); sd(dat.uhi$tree.cooling)
quantile(dat.uhi$tree.cooling, c(0.025, 0.975))

range(dat.uhi$tree.slope)
mean(dat.uhi$tree.slope); sd(dat.uhi$tree.slope)
quantile(dat.uhi$tree.slope, c(0.025, 0.975))


# ---------------------------------
# Making some figures
# ---------------------------------
# Creating a categorical, binned warming response
dat.uhi$TreeEffect <- round(dat.uhi$tree.cooling*2)/2
summary(dat.uhi)
length(unique(dat.uhi$TreeEffect))

dat.uhi$TreeSlope <- round(dat.uhi$tree.slope*20)/20
summary(dat.uhi)
length(unique(dat.uhi$TreeSlope))

dim(dat.uhi[dat.uhi$tree.pval>=0.05,])

# bins <- 100
# cols <- c("darkblue","darkred")
# colGradient <- colorRampPalette(cols)
# cut.cols <- colGradient(bins)
# cuts <- cut(df$val,bins)
# names(cuts) <- sapply(cuts,function(t) cut.cols[which(as.character(t) == levels(cuts))])
# 
effect.lims <- seq(-1*max(abs(dat.uhi$TreeEffect)), 1*max(abs(dat.uhi$TreeEffect)), by=0.5)
slope.lims <- seq(-1*max(abs(dat.uhi$TreeSlope)), 1*max(abs(dat.uhi$TreeSlope)), by=0.05)
effect.palette <- colorRampPalette(brewer.pal(11, "BrBG"))(length(effect.lims))
slope.palette <- colorRampPalette(brewer.pal(11, "BrBG"))(length(slope.lims))
# length(unique())

png("../figures/TreeBenefits_UrbanHeatIsland_TreeCover_Map.png", height=4, width=8, units="in", res=220)
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

png("../figures/TreeBenefits_UrbanHeatIsland_TreeCover_Max_Map.png", height=4, width=8, units="in", res=220)
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


ggplot(data=dat.uhi) +
  geom_point(aes(x=tree.mean, y=tree.cooling, color=tree.slope)) +
  scale_colour_distiller(name="Tree Effect\n(deg. C / % canopy)", palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$tree.slope))) +
  theme_bw() +
  theme(panel.background=element_rect(fill="black"),
        panel.grid = element_blank())

png("../figures/TreeBenefits_UrbanHeatIsland_TreeMax_v_TreeSlope.png", height=4, width=8, units="in", res=220)
ggplot(data=dat.uhi) +
  geom_point(data=dat.uhi[dat.uhi$tree.pval<0.05,], aes(x=tree.max, y=tree.slope), size=2) +
  geom_point(data=dat.uhi[dat.uhi$tree.pval>=0.05,], aes(x=tree.max, y=tree.slope), color="gray70") +
  stat_smooth(data=dat.uhi[dat.uhi$tree.pval<0.05,], aes(x=tree.max, y=tree.slope)) +
  # scale_colour_distiller(name="Tree Effect\n(deg. C)", palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$tree.effect))) +
  theme_bw() 
dev.off()

ggplot(data=dat.uhi) +
  geom_point(data=dat.uhi[dat.uhi$tree.pval<0.05,], aes(x=tree.mean, y=tree.slope), size=2) +
  geom_point(data=dat.uhi[dat.uhi$tree.pval>=0.05,], aes(x=tree.mean, y=tree.slope), color="gray70") +
  # scale_colour_distiller(name="Tree Effect\n(deg. C)", palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$tree.effect))) +
  theme_bw() 

png("../figures/TreeBenefits_UrbanHeatIsland_TreeSlope_histogram_GAM.png", height=4, width=8, units="in", res=220)
ggplot(data=dat.uhi) +
  coord_cartesian(expand=0) +
  geom_histogram(aes(x=factor(TreeSlope), fill=factor(TreeSlope)), stat="count", width=1) +
  geom_histogram(data=dat.uhi[dat.uhi$tree.pval>=0.05,],aes(x=factor(TreeSlope)), fill="gray50", stat="count", width=1) +
  # geom_histogram(aes(x=tree.cooling, fill=cut(tree.cooling, 100)), binwidth=1, show.legend=F) +
  # geom_histogram(aes(x=tree.cooling, fill=), binwidth=1, show.legend=F) +
  scale_fill_manual(name="Tree Effect\n(deg. C / % canopy)", values=rev(slope.palette)) +
  # scale_fill_brewer(palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$tree.cooling))) +
  guides(fill=F) +
  labs(x="Tree Effect (deg. C / % canopy)", y="City Count") +
  theme_bw() 
dev.off()

png("../figures/TreeBenefits_UrbanHeatIsland_TreeSlope_Map_GAM.png", height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tree.slope), size=5) +
  scale_colour_distiller(name="Tree Effect\n(deg. C / % canopy)", palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$tree.slope))) +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()


png("../figures/TreeBenefits_UrbanHeatIsland_TreeCooling_histogram_GAM.png", height=4, width=8, units="in", res=220)
ggplot(data=dat.uhi[,]) +
  coord_cartesian(expand=0) +
  geom_histogram(aes(x=factor(TreeEffect), fill=factor(TreeEffect)), stat="count", width=1) +
  geom_histogram(data=dat.uhi[dat.uhi$tree.pval>=0.05,],aes(x=factor(TreeEffect)), fill="gray50", stat="count", width=1) +
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
  geom_point(data=dat.uhi[dat.uhi$tree.pval>=0.05,], aes(x=LONGITUDE, y=LATITUDE), color="gray50", size=3) +
  geom_point(data=dat.uhi[dat.uhi$tree.pval<0.05,], aes(x=LONGITUDE, y=LATITUDE, color=tree.cooling), size=5) +
  scale_colour_distiller(name="Tree Effect\non Temperature\n(deg. C)", palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$tree.cooling))) +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()

# ---------------------------------
