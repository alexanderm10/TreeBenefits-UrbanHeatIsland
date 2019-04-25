# --------------------------------------------------------------
# Look at output
# --------------------------------------------------------------
dat.uhi <- read.csv("../data_processed/analysis_cities_summary_sdei_v6.csv")
dat.filter <- dat.uhi$prop.missing<0.3 & dat.uhi$prop.temp.n.lo<0.5
summary(dat.uhi)
dim(dat.uhi)
dim(dat.uhi[dat.filter,])
dim(dat.uhi[!dat.filter,])

hist(dat.uhi$prop.missing)
hist(dat.uhi$prop.temp.n.lo)

# Calculated urban heat island effect
hist(dat.uhi$d.temp.summer.buff[!dat.filter])
mean(dat.uhi$d.temp.summer.buff[!dat.filter], na.rm=T); sd(dat.uhi$d.temp.summer.buff[!dat.filter], na.rm=T)
median(dat.uhi$d.temp.summer.buff[!dat.filter], na.rm=T)
quantile(dat.uhi$d.temp.summer.buff[!dat.filter], c(0.025, 0.975), na.rm=T)

# Urban tree cover
hist(dat.uhi$cover.tree.city)
mean(dat.uhi$cover.tree.city, na.rm=T); sd(dat.uhi$cover.tree.city, na.rm=T)
median(dat.uhi$cover.tree.city, na.rm=T)
quantile(dat.uhi$cover.tree.city, c(0.025, 0.975), na.rm=T)

# Difference between buffer & urban tree cover
t.test(dat.uhi$cover.tree.city, dat.uhi$cover.tree.buff, paired=T)
hist(dat.uhi$d.cover.tree.buff)
mean(dat.uhi$d.cover.tree.buff, na.rm=T); sd(dat.uhi$d.cover.tree.buff, na.rm=T)
median(dat.uhi$d.cover.tree.buff, na.rm=T)
quantile(dat.uhi$d.cover.tree.buff, c(0.025, 0.975), na.rm=T)


# No difference in tree-cover trends in the urban vs. buffer
t.test(dat.uhi$trend.cover.tree.city, dat.uhi$trend.cover.tree.buff, paired=T)
t.test(dat.uhi$trend.cover.tree.city) # Close, but no significant regional trend
hist(dat.uhi$trend.cover.tree.city)
mean(dat.uhi$trend.cover.tree.city, na.rm=T); sd(dat.uhi$trend.cover.tree.city, na.rm=T)
median(dat.uhi$trend.cover.tree.city, na.rm=T)
quantile(dat.uhi$trend.cover.tree.city, c(0.025, 0.975), na.rm=T)

# Model Summary
hist(dat.uhi$gam.r2)
range(dat.uhi$gam.r2, na.rm=T)
mean(dat.uhi$gam.r2, na.rm=T); sd(dat.uhi$gam.r2, na.rm=T)
summary(dat.uhi$tree.slope.lin)
summary(dat.uhi$elevation.slope) # adiabatic laspe = -9.8˚C/km = -9.8e-3

# Looking for significant 
summary(dat.uhi$WWF_BIOME)
dat.filter <- dat.uhi$prop.missing<0.25 & dat.uhi$prop.temp.n.lo<0.33 & !is.na(dat.uhi$gam.r2)
tree.filter <- dat.filter & dat.uhi$cover.tree.90>10
length(which(dat.filter)); length(which(tree.filter))

cool.filter <- dat.filter & dat.uhi$tree.pval<0.05 & dat.uhi$Tdiff.trees2noveg.city>0
warm.filter <- dat.filter & dat.uhi$tree.pval<0.05 & dat.uhi$Tdiff.trees2noveg.city<0
cool.tree <- tree.filter & dat.uhi$tree.pval<0.05 & dat.uhi$Tdiff.trees2noveg.city>0
warm.tree <- tree.filter & dat.uhi$tree.pval<0.05 & dat.uhi$Tdiff.trees2noveg.city<0
# length(which(cool.tree))/length(which(tree.filter))
summary(dat.uhi[dat.filter,])
summary(dat.uhi[tree.filter,])
summary(dat.uhi[cool.filter,])
summary(dat.uhi[warm.filter,]) # 6 cities (so far)
summary(dat.uhi[dat.filter & dat.uhi$Tdiff.trees2noveg.city > 10,])

# length(which(dat.uhi$tree.pval<0.05))/nrow(dat.uhi[,]) # Significant tree effect in 93% of ALL cities, even with bad data
length(which(cool.filter | warm.filter))/length(which(dat.filter)) # Significant tree effect in 87% of cities
length(which(cool.filter))/length(which(dat.filter)) # Significant tree cooling effect in 85% of cities
length(which(warm.filter))/length(which(dat.filter)) # Significant tree warming in effect in 2% of cities

# adding a tree filter
length(which((cool.tree | warm.tree)))/length(which(tree.filter)) # Significant tree effect in 87% of cities
length(which(cool.tree))/length(which(tree.filter)) # Significant tree cooling effect in 85% of cities
length(which(warm.tree))/length(which(tree.filter)) # Significant tree cooling effect in 85% of cities

# Seeing which tree has warming
length(which(warm.filter))
dat.uhi[warm.filter,] 

t.test(dat.uhi[cool.filter,"cover.tree.city"], dat.uhi[warm.filter,"cover.tree.city"])
t.test(dat.uhi[cool.filter,"cover.tree.buff"], dat.uhi[warm.filter,"cover.tree.buff"])
t.test(dat.uhi[cool.filter,"gam.r2"], dat.uhi[warm.filter,"gam.r2"])

hist(dat.uhi$Tdiff.trees2noveg.city)
mean(dat.uhi$Tdiff.trees2noveg.city, na.rm=T); sd(dat.uhi$Tdiff.trees2noveg.city, na.rm=T)
median(dat.uhi$Tdiff.trees2noveg.city, na.rm=T)
quantile(dat.uhi$Tdiff.trees2noveg.city, c(0.025, 0.975), na.rm=T)


# dat.uhi <- dat.uhi[dat.uhi$tree.puhi>=-1,] # Can't have >100% cooling
# summary(dat.uhi)
# dat.uhi <- dat.uhi[dat.uhi$Tdiff.trees2noveg.city>=-4,]

# summary(dat.uhi[dat.uhi$tree.slope.lin>0,])

# dat.uhi$tree.puhi.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$Tdiff.trees2noveg.city, NA)
dat.uhi$Tdiff.trees2noveg.city.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$Tdiff.trees2noveg.city, NA)
dat.uhi$tree.slope.lin.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$tree.slope.lin, NA)
dat.uhi$tree.slope.log.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$tree.slope.log, NA)
summary(dat.uhi)

range(dat.uhi$Tdiff.trees2noveg.city, na.rm=T)
mean(dat.uhi$Tdiff.trees2noveg.city, na.rm=T); sd(dat.uhi$Tdiff.trees2noveg.city, na.rm=T)
quantile(dat.uhi$Tdiff.trees2noveg.city, c(0.025, 0.975), na.rm=T)

range(dat.uhi$tree.slope)
mean(dat.uhi$tree.slope); sd(dat.uhi$tree.slope)
quantile(dat.uhi$tree.slope, c(0.025, 0.975))


# ---------------------------------
# Making some figures
# ---------------------------------
path.figs <- "../figures/v6"
dir.create(path.figs, recursive=T, showWarnings=F)

# Creating a categorical, binned warming response
dat.uhi$TreeEffect <- round(dat.uhi$Tdiff.trees2noveg.city*2)/2 # To the nearest 0.5 degree
dat.uhi$TreeEffect2 <- round(dat.uhi$Tdiff.trees2noveg.city) # To the nearest degree
summary(dat.uhi)
length(unique(dat.uhi$TreeEffect))


# bins <- 100
# cols <- c("darkblue","darkred")
# colGradient <- colorRampPalette(cols)
# cut.cols <- colGradient(bins)
# cuts <- cut(df$val,bins)
# names(cuts) <- sapply(cuts,function(t) cut.cols[which(as.character(t) == levels(cuts))])
# 
effect.lims <- seq(-1*max(abs(dat.uhi$TreeEffect), na.rm=T), 1*max(abs(dat.uhi$TreeEffect), na.rm=T), by=0.5)
effect.palette <- colorRampPalette(brewer.pal(11, "BrBG"))(length(effect.lims))

effect.lims2 <- seq(-1*max(abs(dat.uhi$TreeEffect2), na.rm=T), 1*max(abs(dat.uhi$TreeEffect2), na.rm=T), by=1)
effect.palette2 <- colorRampPalette(brewer.pal(11, "BrBG"))(length(effect.lims2))
# length(unique())

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_TreeCover_City_Map.png"), height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=cover.tree.city), size=3) +
  scale_colour_distiller(name="Mean Tree Cover", palette=rev("BrBG"), trans="reverse") +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_TreeCover_CityDiff_Map.png"), height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=-d.cover.tree.buff), size=3) +
  scale_colour_distiller(name="Difference Tree Cover", palette="BrBG", limits=c(-1,1)*max(abs(dat.uhi$d.cover.tree.buff), na.rm=T)) +
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
  # geom_histogram(aes(x=Tdiff.trees2noveg.city, fill=cut(Tdiff.trees2noveg.city, 100)), binwidth=1, show.legend=F) +
  # geom_histogram(aes(x=Tdiff.trees2noveg.city, fill=), binwidth=1, show.legend=F) +
  scale_fill_manual(name="Tree Cooling Effect\non Temperature\n(deg. C)", values=effect.palette2[which(effect.lims2 %in% unique(dat.uhi$TreeEffect2))]) +
  # scale_fill_brewer(palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$Tdiff.trees2noveg.city))) +
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
  geom_point(data=dat.uhi[dat.filter & dat.uhi$tree.pval<0.05,], aes(x=LONGITUDE, y=LATITUDE, color=-Tdiff.trees2noveg.city), size=3) +
  scale_colour_distiller(name="Tree Effect\non Temperature\n(deg. C)", palette="BrBG", limits=c(-1,1)*max(abs(dat.uhi$Tdiff.trees2noveg.city), na.rm=T)) +
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
  geom_point(data=dat.uhi[dat.filter & dat.uhi$tree.pval<0.05,], aes(x=LONGITUDE, y=LATITUDE, color=factor(-TreeEffect2)), size=3) +
  scale_color_manual(name="Tree Effect\non Temperature\n(deg. C)", values=rev(effect.palette2)[which(effect.lims2 %in% unique(-dat.uhi$TreeEffect2))]) +
  
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()

#--------------------------
# Looking at cover
#--------------------------

# x=dat.uhi[1,c("cover.tree.city", "cover.veg.city", "cover.noveg.city")]
dat.uhi$cover.dom.city <- apply(dat.uhi[,c("cover.tree.city", "cover.veg.city", "cover.noveg.city")], 1, FUN=function(x){ifelse(!is.na(x[1]), c("tree", "other veg", "no veg")[which(x==max(x, na.rm=T))], NA)})
dat.uhi$cover.dom.buff <- apply(dat.uhi[,c("cover.tree.buff", "cover.veg.buff", "cover.noveg.buff")], 1, FUN=function(x){ifelse(!is.na(x[1]), c("tree", "other veg", "no veg")[which(x==max(x, na.rm=T))], NA)})
# dat.uhi$cover.dom.city <- as.factor(dat.uhi$cover.dom.city)
# dat.uhi$cover.dom.buff <- as.factor(dat.uhi$cover.dom.buff)
summary(dat.uhi)

city.buffer <- stack(dat.uhi[,c("cover.dom.city", "cover.dom.buff")])
names(city.buffer) <- c("cover.dom", "location")
city.buffer$location <- ifelse(city.buffer$location=="cover.dom.city", "city", "buffer")
city.buffer$location <- factor(city.buffer$location, levels=c("city", "buffer"))
city.buffer$cover.tree <- stack(dat.uhi[,c("cover.tree.city", "cover.tree.buff")])[,1]
city.buffer$cover.veg <- stack(dat.uhi[,c("cover.veg.city", "cover.veg.buff")])[,1]
city.buffer$cover.noveg <- stack(dat.uhi[,c("cover.noveg.city", "cover.noveg.buff")])[,1]
city.buffer$trend.tree <- stack(dat.uhi[,c("trend.cover.tree.city", "trend.cover.tree.buff")])[,1]
city.buffer$trend.veg <- stack(dat.uhi[,c("trend.cover.veg.city", "trend.cover.veg.buff")])[,1]
city.buffer$trend.noveg <- stack(dat.uhi[,c("trend.cover.noveg.city", "trend.cover.noveg.buff")])[,1]
city.buffer[,c("cool.tree", "cool.veg")] <- NA
city.buffer[city.buffer$location=="city", "cool.tree"] <- dat.uhi$Tdiff.trees2noveg.city
city.buffer[city.buffer$location=="city", "cool.veg"] <- dat.uhi$Tdiff.veg2noveg.city 
summary(city.buffer)



cover.comp <- stack(city.buffer[,c("cover.tree", "cover.veg", "cover.noveg")])
names(cover.comp) <- c("cover", "type")
cover.comp[,c("location", "cover.dom", "cool.tree", "cool.veg")] <- city.buffer[,c("location", "cover.dom", "cool.tree", "cool.veg")]
cover.comp$trend <- stack(city.buffer[,c("trend.tree", "trend.veg", "trend.noveg")])[,1]
summary(cover.comp)

ggplot(data=city.buffer[!is.na(city.buffer$cover.dom),]) +
  geom_bar(aes(x=cover.dom, fill=location), stat="count", position="dodge")

ggplot(dat.uhi[!is.na(dat.uhi$d.temp.summer.buff),]) +
  geom_histogram(aes(x=d.temp.summer.buff, fill=cover.dom.buff)) +
  geom_vline(xintercept=0, linetype="dashed") +
  theme(legend.position="top")

ggplot(data=cover.comp) +
  facet_grid(type~location) +
  geom_histogram(aes(x=trend, fill=location)) +
  geom_vline(xintercept=0, linetype="dashed") +
  theme(legend.position="top") +
  coord_cartesian(xlim=c(-10,10))


# --------------------------------------------------------------
