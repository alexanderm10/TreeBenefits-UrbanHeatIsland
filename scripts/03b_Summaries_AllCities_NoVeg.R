library(ggplot2); library(RColorBrewer)
# path.figs <- "../figures/v6_Effect_NoVeg"
path.figs <- "/Volumes/GoogleDrive/My Drive/TreeBenefits_UrbanHeatIsland/figures/Effect_noVeg"
dir.create(path.figs, recursive=T, showWarnings=F)

# --------------------------------------------------------------
# Look at output
# --------------------------------------------------------------
dat.uhi <- read.csv("../data_processed/analysis_cities_summary_sdei_v6_noveg.csv")
dat.filter <- dat.uhi$prop.missing<0.33 & dat.uhi$prop.temp.n.lo<0.33

# Fix some biome mis-specifications
dat.uhi$WWF_ECO <- as.character(dat.uhi$WWF_ECO)
dat.uhi$WWF_BIOME <- as.character(dat.uhi$WWF_BIOME)
dat.uhi[is.na(dat.uhi$WWF_BIOME),]
dat.uhi[!is.na(dat.uhi$WWF_BIOME) & dat.uhi$WWF_BIOME=="98",]

# Ecoregions based on looking at them on a map on a websites
dat.uhi[dat.uhi$NAME=="NewYork", c("WWF_ECO", "WWF_BIOME")] <- c("Northeastern coastal forests", "temperate broadleaf/mixed forest")
dat.uhi[dat.uhi$NAME=="SanJose", c("WWF_ECO", "WWF_BIOME")] <- c("California interior chaparral and woodlands", "mediterranean")
dat.uhi[dat.uhi$NAME=="VirginiaBeach", c("WWF_ECO", "WWF_BIOME")] <- c("Middle Atlantic coastal forests", "temperate coniferous forest")
dat.uhi[dat.uhi$NAME=="as-Sib", c("WWF_ECO", "WWF_BIOME")] <- c("Gulf of Oman desert and semi-desert", "desert/xeric shrublands")
dat.uhi[dat.uhi$NAME=="Genova", c("WWF_ECO", "WWF_BIOME")] <- c("Italian sclerophyllous and semi-deciduous forests", "mediterranean")
dat.uhi[dat.uhi$NAME=="Itaquari", c("WWF_ECO", "WWF_BIOME")] <- c("Bahia coastal forests", "tropical moist broadleaf forest")
dat.uhi[dat.uhi$NAME=="Maracaibo", c("WWF_ECO", "WWF_BIOME")] <- c("Guajira-Barranquilla xeric scrub", "desert/xeric shrublands")
dat.uhi[dat.uhi$NAME=="Shenzhen", c("WWF_ECO", "WWF_BIOME")] <- c("South China-Vietnam subtropical evergreen forests", "tropical moist broadleaf forest")
dat.uhi[dat.uhi$NAME=="Toronto", c("WWF_ECO", "WWF_BIOME")] <- c("Southern Great Lakes forests", "temperate broadleaf/mixed forest")
dat.uhi[dat.uhi$NAME=="Rasht", c("WWF_ECO", "WWF_BIOME")] <- c("Caspian Hyrcanian mixed forests", "temperate coniferous forest")
dat.uhi$WWF_ECO <- as.factor(dat.uhi$WWF_ECO)
dat.uhi$WWF_BIOME <- as.factor(dat.uhi$WWF_BIOME)
dat.uhi[dat.uhi$WWF_BIOME=="mangroves",]
summary(dat.uhi$WWF_BIOME)

# Lumping the biomes a bit more to make easier to see figures
dat.uhi$Biome2 <- car::recode(dat.uhi$WWF_BIOME, 
                              "'mangroves'='Tropical Forest'; 
                              'boreal forest/taiga'='Boreal'; 
                              'desert/xeric shrublands'='Desert'; 
                              'flooded grassland/savanna'='Grassland/Savanna'; 
                              'mediterranean'='Mediterranean'; 
                              'montane grassland/savanna'='Grassland/Savanna'; 
                              'temperate broadleaf/mixed forest'='Temperate Forest';
                              'temperate coniferous forest'='Temperate Forest';
                              'temperate grassland/savanna'='Grassland/Savanna'; 
                              'tropical coniferous forest'='Tropical Forest'; 
                              'tropical dry broadleaf forest'='Tropical Forest'; 
                              'tropical grassland/savannas'='Grassland/Savanna';
                              'tropical moist broadleaf forest'='Tropical Forest'")
dat.uhi$Biome2 <- factor(dat.uhi$Biome2, levels=c("Desert", "Grassland/Savanna", "Mediterranean", "Tropical Forest", "Temperate Forest", "Boreal"))
summary(dat.uhi)
dim(dat.uhi)
dim(dat.uhi[dat.filter,])
dim(dat.uhi[!dat.filter,])

hist(dat.uhi$prop.missing)
hist(dat.uhi$prop.temp.n.lo)

# Calculated urban heat island effect
hist(dat.uhi$d.temp.summer.buff[dat.filter])
mean(dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T); sd(dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T)
median(dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T)
range(dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T)
quantile(dat.uhi$d.temp.summer.buff[dat.filter], c(0.025, 0.975), na.rm=T)

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffect_City_Map.png"), height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi[dat.filter,]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=d.temp.summer.buff), size=3) +
  scale_color_gradient2(name="Urban\nWarming\n(deg. C)", low = "dodgerblue2", high = "red3", mid = "white", midpoint =0) +
  theme_bw() +
  theme(legend.position="top",
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffect_Histogram_Biome.png"), height=4, width=8, units="in", res=220)
ggplot(data=dat.uhi[dat.filter,]) +
  geom_histogram(aes(x=d.temp.summer.buff, fill=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Urban Warming (deg. C)") +
  scale_y_continuous(name="# Cities", expand=c(0,0)) +
  # scale_fill_manual(name="Biome") + 
  theme_bw() +
  theme(legend.position="top")
dev.off()

ggplot(data=dat.uhi[dat.filter,]) +
  geom_point(aes(x=d.cover.tree.buff, y=d.temp.summer.buff, color=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_hline(yintercept=0, linetype="dashed")

ggplot(data=dat.uhi[dat.filter,]) +
  geom_point(aes(x=d.cover.tree.buff+d.cover.veg.buff, y=d.temp.summer.buff, color=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_hline(yintercept=0, linetype="dashed")


# Urban tree cover
hist(dat.uhi$cover.tree.city[dat.filter])
mean(dat.uhi$cover.tree.city[dat.filter], na.rm=T); sd(dat.uhi$cover.tree.city[dat.filter], na.rm=T)
median(dat.uhi$cover.tree.city[dat.filter], na.rm=T)
range(dat.uhi$cover.tree.city[dat.filter], na.rm=T)
quantile(dat.uhi$cover.tree.city[dat.filter], c(0.025, 0.975), na.rm=T)

mean(dat.uhi$cover.veg.city[dat.filter], na.rm=T); sd(dat.uhi$cover.veg.city[dat.filter], na.rm=T)
mean(dat.uhi$d.cover.veg.buff[dat.filter], na.rm=T); sd(dat.uhi$d.cover.veg.buff[dat.filter], na.rm=T)


# city with highest cover
dat.uhi[!is.na(dat.uhi$cover.tree.city) & dat.uhi$cover.tree.city==max(dat.uhi$cover.tree.city[dat.filter], na.rm=T),]
dat.uhi[!is.na(dat.uhi$cover.tree.city) & dat.uhi$cover.tree.buff==max(dat.uhi$cover.tree.buff[dat.filter], na.rm=T),]

# dat.uhi[dat.uhi$cover.]
dat.uhi[!is.na(dat.uhi$cover.tree.city) & dat.uhi$d.cover.tree.buff==max(dat.uhi$d.cover.tree.buff[dat.filter], na.rm=T),]

# Difference between buffer & urban tree cover
t.test(dat.uhi$cover.tree.city, dat.uhi$cover.tree.buff, paired=T)
hist(dat.uhi$d.cover.tree.buff)
mean(dat.uhi$d.cover.tree.buff, na.rm=T); sd(dat.uhi$d.cover.tree.buff, na.rm=T)
median(dat.uhi$d.cover.tree.buff, na.rm=T)
quantile(dat.uhi$d.cover.tree.buff, c(0.025, 0.975), na.rm=T)

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_TreeCover_City_Map.png"), height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi[dat.filter,]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=cover.tree.city), size=3) +
  scale_colour_distiller(name="Mean Tree Cover", palette=rev("BrBG"), trans="reverse") +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top",
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank())
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_TreeCover_CityDiff_Map.png"), height=4, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=dat.uhi[dat.filter,]) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=-d.cover.tree.buff), size=3) +
  scale_colour_distiller(name="Difference Tree Cover", palette="BrBG", limits=c(-1,1)*max(abs(dat.uhi$d.cover.tree.buff), na.rm=T)) +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top",
        panel.background = element_rect(fill="black"),
        panel.grid = element_blank())
dev.off()


# No difference in tree-cover trends in the urban vs. buffer
# number of cities with statistical trend in tree cover
length(which(dat.filter & dat.uhi$p.trend.cover.tree.city<0.05))/length(which(dat.filter)) # 70% of cities have a significant trend in tree cover, but it's mixed as to direction
length(which(dat.filter & dat.uhi$p.trend.cover.veg.city<0.05))/length(which(dat.filter)) # 
length(which(dat.filter & dat.uhi$p.trend.cover.noveg.city<0.05))/length(which(dat.filter)) # vast majority of cities have no trend in unvegetated cover

hist(dat.uhi[dat.filter & dat.uhi$p.trend.cover.tree.city<0.05, "trend.cover.tree.city"]) # 
hist(dat.uhi[dat.filter & dat.uhi$p.trend.cover.veg.city<0.05, "trend.cover.veg.city"]) # 
hist(dat.uhi[dat.filter & dat.uhi$p.trend.cover.noveg.city<0.05, "trend.cover.noveg.city"]) # 

t.test(dat.uhi$trend.cover.tree.city[dat.filter], dat.uhi$trend.cover.tree.buff[dat.filter], paired=T)
t.test(dat.uhi$trend.cover.tree.city) # global trend towards slighly increased cover (0.1%/yr), but no real ecological significance
hist(dat.uhi$trend.cover.tree.city)
mean(dat.uhi$trend.cover.tree.city, na.rm=T); sd(dat.uhi$trend.cover.tree.city, na.rm=T)
median(dat.uhi$trend.cover.tree.city, na.rm=T)
quantile(dat.uhi$trend.cover.tree.city, c(0.025, 0.975), na.rm=T)
# ------------------------------------------------



# ------------------------------------------------
# Digging into model & predicted tree effects
# ------------------------------------------------
dat.filter <- dat.uhi$prop.missing<0.33 & dat.uhi$prop.temp.n.lo<0.33 & !is.na(dat.uhi$gam.r2) & !is.na(dat.uhi$Biome2)

# Model Summary
hist(dat.uhi$gam.r2[dat.filter])
range(dat.uhi$gam.r2[dat.filter], na.rm=T)
mean(dat.uhi$gam.r2[dat.filter], na.rm=T); sd(dat.uhi$gam.r2[dat.filter], na.rm=T)
summary(dat.uhi$tree.slope.lin)
summary(dat.uhi$elevation.slope[dat.filter]) # adiabatic laspe = -9.8˚C/km = -9.8e-3

# Looking for significant 
summary(dat.uhi$WWF_BIOME)
tree.filter <- dat.uhi$cover.tree.90>10
length(which(dat.filter)); length(which(tree.filter))


length(which(cool.tree))/length(which(dat.filter))


cool.tree <- dat.filter & dat.uhi$tree.pval<0.05 & dat.uhi$Tdiff.trees2noveg.city>0
warm.tree <- dat.filter & dat.uhi$tree.pval<0.05 & dat.uhi$Tdiff.trees2noveg.city<0
cool.veg <- dat.filter & dat.uhi$veg.pval<0.05 & dat.uhi$Tdiff.veg2noveg.city>0
warm.veg <- dat.filter & dat.uhi$veg.pval<0.05 & dat.uhi$Tdiff.veg2noveg.city<0
# length(which(cool.tree))/length(which(tree.filter))
summary(dat.uhi[dat.filter,])
summary(dat.uhi[cool.tree,])
summary(dat.uhi[warm.tree,]) # 6 cities (so far)
summary(dat.uhi[cool.veg,])
summary(dat.uhi[warm.veg,]) 
summary(dat.uhi[dat.filter & dat.uhi$Tdiff.trees2noveg.city > 10,])

summary(dat.uhi[dat.filter,"gam.r2"])

# length(which(dat.uhi$tree.pval<0.05))/nrow(dat.uhi[,]) # Significant tree effect in 88% of ALL cities, even with bad data
length(which((cool.tree) | (warm.tree)))/length(which(dat.filter)) # Significant tree effect in 91% of cities
length(which(cool.tree))/length(which(dat.filter)) # Significant tree cooling effect in 89% of cities
length(which(warm.tree))/length(which(dat.filter)) # Significant tree warming in effect in 2% of cities
length(which(cool.veg))/length(which(dat.filter)) # Significant tree cooling effect in 90% of cities
length(which(warm.veg))/length(which(dat.filter)) # Significant tree warming in effect in 1% of cities

mean(dat.uhi[dat.filter,"Tdiff.trees2veg.city"]/dat.uhi[dat.filter,"Tdiff.veg2noveg.city"])
sd(dat.uhi[dat.filter,"Tdiff.trees2veg.city"]/dat.uhi[dat.filter,"Tdiff.veg2noveg.city"])

dat.uhi[warm.tree,] # 3 Southern Great Lakes forests; 2 Cuaca Valley dry forests, plus a mongolian steppe

# adding a tree filter
# length(which((cool.tree | warm.tree)))/length(which(tree.filter)) # Significant tree effect in 89% of cities
# length(which(cool.tree))/length(which(tree.filter)) # Significant tree cooling effect in 88% of cities
# length(which(warm.tree))/length(which(tree.filter)) # Significant tree cooling effect in 1% of cities

# Seeing which tree has warming
length(which(warm.tree))
dat.uhi[warm.tree,] 

t.test(dat.uhi[cool.tree,"cover.tree.city"], dat.uhi[warm.tree,"cover.tree.city"])
t.test(dat.uhi[cool.tree,"cover.tree.buff"], dat.uhi[warm.tree,"cover.tree.buff"])
t.test(dat.uhi[cool.tree,"gam.r2"], dat.uhi[warm.tree,"gam.r2"])

# Getting the Looking at estimated tree cooling
hist(dat.uhi$Tdiff.trees2noveg.city[dat.filter])
t.test(dat.uhi$Tdiff.trees2noveg.city[dat.filter])
mean(dat.uhi$Tdiff.trees2noveg.city[dat.filter], na.rm=T); sd(dat.uhi$Tdiff.trees2noveg.city[dat.filter], na.rm=T)
median(dat.uhi$Tdiff.trees2noveg.city[dat.filter], na.rm=T)
quantile(dat.uhi$Tdiff.trees2noveg.city[dat.filter], c(0.025, 0.975), na.rm=T)


# dat.uhi$tree.puhi.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$Tdiff.trees2noveg.city, NA)
dat.uhi$Tdiff.trees2noveg.city.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$Tdiff.trees2noveg.city, NA)
dat.uhi$tree.slope.lin.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$tree.slope.lin, NA)
dat.uhi$tree.slope.log.sig <- ifelse(dat.uhi$tree.pval<0.05, dat.uhi$tree.slope.log, NA)
summary(dat.uhi)

summary(dat.uhi$d.temp.summer.buff[dat.filter])
mean(dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T); sd(dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T)  # Estimated 1.4˚C Urban Warming

range(dat.uhi$Tdiff.trees2noveg.city[dat.filter], na.rm=T)
mean(dat.uhi$Tdiff.trees2noveg.city[dat.filter], na.rm=T); sd(dat.uhi$Tdiff.trees2noveg.city[dat.filter], na.rm=T)  # Estimated 2.8˚C cooling from trees (SD 3.0)
quantile(dat.uhi$Tdiff.trees2noveg.city[dat.filter], c(0.025, 0.975), na.rm=T)

range(dat.uhi$Tdiff.trees2noveg.city[dat.filter]/dat.uhi$cover.tree.city[dat.filter], na.rm=T)
mean(dat.uhi$Tdiff.trees2noveg.city[dat.filter]/dat.uhi$cover.tree.city[dat.filter], na.rm=T); sd(dat.uhi$Tdiff.trees2noveg.city[dat.filter]/dat.uhi$cover.tree.city[dat.filter], na.rm=T)
quantile(dat.uhi$Tdiff.trees2noveg.city[dat.filter]/dat.uhi$cover.tree.city[dat.filter], c(0.025, 0.975), na.rm=T)

mean(dat.uhi$Tdiff.trees2noveg.city[dat.filter]/dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T); sd(dat.uhi$Tdiff.trees2noveg.city[dat.filter]/dat.uhi$d.temp.summer.buff[dat.filter], na.rm=T)


summary(dat.uhi$cover.veg.city[dat.filter])
t.test(dat.uhi$Tdiff.veg2noveg.city[dat.filter])
range(dat.uhi$Tdiff.veg2noveg.city[dat.filter], na.rm=T)
mean(dat.uhi$Tdiff.veg2noveg.city[dat.filter], na.rm=T); sd(dat.uhi$Tdiff.veg2noveg.city[dat.filter], na.rm=T)
quantile(dat.uhi$Tdiff.veg2noveg.city[dat.filter], c(0.025, 0.975), na.rm=T)

range(dat.uhi$Tdiff.veg2noveg.city[dat.filter]/dat.uhi$cover.veg.city[dat.filter], na.rm=T)
mean(dat.uhi$Tdiff.veg2noveg.city[dat.filter]/dat.uhi$cover.veg.city[dat.filter], na.rm=T); sd(dat.uhi$Tdiff.veg2noveg.city[dat.filter]/dat.uhi$cover.veg.city[dat.filter], na.rm=T)
quantile(dat.uhi$Tdiff.veg2noveg.city[dat.filter]/dat.uhi$cover.veg.city[dat.filter], c(0.025, 0.975), na.rm=T)

t.test(dat.uhi$Tdiff.trees2noveg.city[dat.filter], dat.uhi$Tdiff.veg2noveg.city[dat.filter])



# ---------------------------------
# Making some figures
# ---------------------------------
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
dat.uhi[dat.filter & dat.uhi$cover.dom.city=="tree" & !is.na(dat.uhi$cover.tree.city) ,]
summary(as.factor(dat.uhi$cover.dom.city[dat.filter]))
summary(as.factor(dat.uhi$cover.dom.buff[dat.filter]))
summary(dat.uhi)

city.buffer <- stack(dat.uhi[dat.filter,c("cover.dom.city", "cover.dom.buff")])
names(city.buffer) <- c("cover.dom", "location")
city.buffer[,c("Name", "Biome2")] <- dat.uhi[dat.filter,c("NAME", "Biome2")]
city.buffer$location <- ifelse(city.buffer$location=="cover.dom.city", "city", "buffer")
city.buffer$location <- factor(city.buffer$location, levels=c("city", "buffer"))
city.buffer$cover.tree <- stack(dat.uhi[dat.filter,c("cover.tree.city", "cover.tree.buff")])[,1]
city.buffer$cover.veg <- stack(dat.uhi[dat.filter,c("cover.veg.city", "cover.veg.buff")])[,1]
city.buffer$cover.noveg <- stack(dat.uhi[dat.filter,c("cover.noveg.city", "cover.noveg.buff")])[,1]
city.buffer$trend.tree <- stack(dat.uhi[dat.filter,c("trend.cover.tree.city", "trend.cover.tree.buff")])[,1]
city.buffer$trend.veg <- stack(dat.uhi[dat.filter,c("trend.cover.veg.city", "trend.cover.veg.buff")])[,1]
city.buffer$trend.noveg <- stack(dat.uhi[dat.filter,c("trend.cover.noveg.city", "trend.cover.noveg.buff")])[,1]
city.buffer[,c("cool.tree", "cool.veg")] <- NA
city.buffer[city.buffer$location=="city", "cool.tree"] <- dat.uhi$Tdiff.trees2noveg.city[dat.filter]
city.buffer[city.buffer$location=="city", "cool.veg"] <- dat.uhi$Tdiff.veg2noveg.city[dat.filter] 
summary(city.buffer)



cover.comp <- stack(city.buffer[,c("cover.tree", "cover.veg", "cover.noveg")])
names(cover.comp) <- c("cover", "type")
cover.comp[,c("Name", "Biome2", "location", "cover.dom", "cool.tree", "cool.veg")] <- city.buffer[,c("Name", "Biome2", "location", "cover.dom", "cool.tree", "cool.veg")]
cover.comp$trend <- stack(city.buffer[,c("trend.tree", "trend.veg", "trend.noveg")])[,1]
summary(cover.comp)

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_DominantCover_CityBuff.png"), height=6, width=6, units="in", res=220)
ggplot(data=city.buffer[!is.na(city.buffer$cover.dom),]) +
  geom_bar(aes(x=cover.dom, fill=location), stat="count", position="dodge") +
  scale_x_discrete(name="Dominant Cover Type") +
  scale_y_continuous(name="# Cities", expand=c(0,0)) +
  scale_fill_manual(values=c("#af8dc3", "#7fbf7b")) +
  theme_bw() +
  theme(legend.position=c(0.8, 0.8))
dev.off()

ggplot(data=cover.comp) +
  facet_grid(type~location) +
  geom_histogram(aes(x=trend, fill=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  theme(legend.position="top") +
  coord_cartesian(xlim=c(-10,10))

# comparing difference in cover 
cover.comp2 <- stack(dat.uhi[dat.filter,c("d.cover.tree.buff", "d.cover.veg.buff", "d.cover.noveg.buff")])
cover.comp2$ind <- car::recode(cover.comp2$ind, "'d.cover.tree.buff'='tree'; 'd.cover.veg.buff'='other veg '; 'd.cover.noveg.buff'='no veg'")
cover.comp2[,c("Name", "Biome2")] <- dat.uhi[dat.filter, c("NAME", "Biome2")]
summary(cover.comp2)

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_VegetationCover_Histograms_Biome.png"), height=6, width=6, units="in", res=220)
ggplot(data=cover.comp2) +
  facet_grid(ind~.) +
  geom_histogram(aes(x=values, fill=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="% Cover", expand=c(0,0)) +
  scale_y_continuous(name="# Cities", expand=c(0,0)) +
  theme_bw() +
  theme(legend.position="top")
dev.off()

# Comparing cooling effects of the urban heat island effect
summary(dat.uhi)
dat.uhi$Temp_NoVeg <- dat.uhi$d.temp.summer.buff + dat.uhi$Tdiff.trees2noveg.city + dat.uhi$Tdiff.veg2noveg.city
veg.efffects <- stack(dat.uhi[dat.filter,c("d.temp.summer.buff", "Tdiff.trees2noveg.city", "Tdiff.veg2noveg.city", "Temp_NoVeg")])
veg.efffects[,c("Name", "Biome2", "LATITUDE", "LONGITUDE")] <- dat.uhi[dat.filter, c("NAME", "Biome2", "LATITUDE", "LONGITUDE")]
veg.efffects$ind <- car::recode(veg.efffects$ind, "'d.temp.summer.buff'='Urban Effect (Observed)'; 'Tdiff.trees2noveg.city'='Tree Effect'; 'Tdiff.veg2noveg.city'='Other Veg Effect'; 'Temp_NoVeg'='No Veg (Modeled)'")
veg.efffects$values <- ifelse(veg.efffects$ind %in% c("Urban Effect (Observed)", "No Veg (Modeled)"), veg.efffects$values, -veg.efffects$values)
veg.efffects$ind <- factor(veg.efffects$ind, levels=c("Urban Effect (Observed)", "Tree Effect", "Other Veg Effect", "No Veg (Modeled)"))
summary(veg.efffects)


png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffects_Veg_Map.png"), height=8, width=8, units="in", res=220)
world <- map_data("world")
ggplot(data=veg.efffects[veg.efffects$ind %in% c("Tree Effect", "Other Veg Effect"),]) +
  facet_grid(ind~.) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_polygon(data=world, aes(x=long, y=lat, group=group), fill="gray50") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=values), size=3) +
  scale_colour_distiller(name="Effect\non Temperature\n(deg. C)", palette="BrBG", limits=c(-1,1)*max(abs(veg.efffects$values[veg.efffects$ind %in% c("Tree Effect", "Other Veg Effect")]), na.rm=T)) +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top",
        panel.background=element_rect(fill="black"),
        panel.grid = element_blank())
dev.off()


# Play with Geoms
png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffects_Histograms.png"), height=6, width=6, units="in", res=220)
ggplot(data=veg.efffects) +
  facet_grid(ind~.) +
  geom_histogram(aes(x=values, fill=ind)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Temperature (deg. C)", expand=c(0,0)) +
  scale_y_continuous(name="# of Cities", expand=c(0,0)) +
  scale_fill_manual(values=c("black", "green4", "blue2", "red2")) +
  theme_bw() +
  theme(legend.position="top",
        legend.title = element_blank()) 
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffects_Histograms_Biome.png"), height=6, width=6, units="in", res=220)
ggplot(data=veg.efffects) +
  facet_grid(ind~.) +
  geom_histogram(aes(x=values, fill=Biome2)) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Temperature (deg. C)", expand=c(0,0)) +
  scale_y_continuous(name="# of Cities", expand=c(0,0)) +
  # scale_fill_manual(values=c("black", "green4", "blue2", "red2")) +
  theme_bw() +
  theme(legend.position="top",
        legend.title = element_blank()) 
dev.off()

png(file.path(path.figs, "TreeBenefits_UrbanHeatIsland_WarmingEffects_Density.png"), height=6, width=6, units="in", res=220)
ggplot(data=veg.efffects) +
  # facet_grid(ind~.) +
  geom_density(aes(x=values, fill=ind), alpha=0.5) +
  geom_vline(xintercept=0, linetype="dashed") +
  scale_x_continuous(name="Temperature (deg. C)", expand=c(0,0)) +
  scale_y_continuous(name="Proportion of Cities", expand=c(0,0)) +
  scale_fill_manual(values=c("black", "green3", "blue2", "red2")) +
  theme_bw() +
  theme(legend.position="top", 
        legend.title=element_blank(),
        panel.grid=element_blank()) 
dev.off()
# --------------------------------------------------------------
