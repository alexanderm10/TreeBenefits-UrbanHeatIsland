# Doing some quick EDA for geography of Urban Heat island
library(ggplot2)

dat.uhi <- read.csv("../data_processed/cities_summary_sdei_v2.csv")
dat.uhi <- dat.uhi[dat.uhi$tree.min>=0 & dat.uhi$D_T_DIFF>=0,]
dat.uhi$temp.diff <- dat.uhi$temp.max - dat.uhi$temp.min
dat.uhi$tree.diff <- dat.uhi$tree.max - dat.uhi$tree.min
summary(dat.uhi)
dim(dat.uhi)

# New plan for quantifying effects of trees on UHI: 
# apply mean temperature of treeless area to whole area and calculate the difference
pb <- txtProgressBar(min=0, max=nrow(dat.uhi), style=3)
for(i in 1:nrow(dat.uhi)){
  setTxtProgressBar(pb, i)
  dat.city <- read.csv(paste0("../data_processed/cities_full_sdei_v2/", dat.uhi$NAME[i], "_data_full.csv"))
  summary(dat.city)
  
  # Not necessarily the best, but using the mean treeless temperature as a baseline
  temp.notree <- mean(dat.city[dat.city$cover.tree<0.5, "temp.summer"], na.rm=T)
  
  dat.uhi[i,"temp.diff.notree"] <- mean(dat.city$temp.summer-temp.notree, na.rm=T)
  
  rm(dat.city)
}
summary(dat.uhi)
summary(dat.uhi[dat.uhi$temp.diff.notree>0,])
dim(dat.uhi[dat.uhi$temp.diff.notree>0,])
summary(dat.uhi[dat.uhi$temp.diff.notree< -10,])

hist(dat.uhi$temp.diff.notree)
world <- map_data("world")

dat.uhi <- dat.uhi[dat.uhi$temp.diff.notree>-10,]

range(dat.uhi$temp.diff.notree)
mean(dat.uhi$temp.diff.notree); sd(dat.uhi$temp.diff.notree)

# Creating a categorical, binned warming response
dat.uhi$TreeEffect <- round(dat.uhi$temp.diff.notree)
summary(dat.uhi)
length(unique(dat.uhi$TreeEffect))
# bins <- 100
# cols <- c("darkblue","darkred")
# colGradient <- colorRampPalette(cols)
# cut.cols <- colGradient(bins)
# cuts <- cut(df$val,bins)
# names(cuts) <- sapply(cuts,function(t) cut.cols[which(as.character(t) == levels(cuts))])
# 
color.lims <- round(-1*max(abs(dat.uhi$temp.diff.notree))):round(1*max(abs(dat.uhi$temp.diff.notree)))
effect.palette <- colorRampPalette(brewer.pal(11, "BrBG"))(length(color.lims))
# length(unique())

png("TreeBenefits_UrbanHeatIsland_TreeCooling_histogram.png", height=4, width=8, units="in", res=220)
ggplot(data=dat.uhi) +
  coord_cartesian(expand=0) +
  geom_histogram(aes(x=factor(TreeEffect), fill=factor(TreeEffect)), stat="count", width=1) +
  # geom_histogram(aes(x=temp.diff.notree, fill=cut(temp.diff.notree, 100)), binwidth=1, show.legend=F) +
  # geom_histogram(aes(x=temp.diff.notree, fill=), binwidth=1, show.legend=F) +
  scale_fill_manual(name="Tree Effect\non Temperature\n(deg. C)", values=rev(effect.palette)) +
  # scale_fill_brewer(palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$temp.diff.notree))) +
  labs(x="Tree Effect on Temperature (deg. C)", y="City Count") +
  theme_bw() 
dev.off()

png("TreeBenefits_UrbanHeatIsland_TreeCooling_Map.png", height=4, width=8, units="in", res=220)
ggplot(data=dat.uhi) +
  coord_equal(expand=0, ylim=c(-65,80)) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=temp.diff.notree), size=5) +
  scale_colour_distiller(name="Tree Effect\non Temperature\n(deg. C)", palette=rev("BrBG"), limits=c(-1,1)*max(abs(dat.uhi$temp.diff.notree))) +
  # scale_color_gradient2(low="turquoise4", mid="wheat", high="sienna3", midpoint=0) +
  # scale_color_gradient(low="003333", high="brown", midpoint=0) +
  theme_bw() +
  theme(legend.position="top")
dev.off()

ggplot(data=dat.uhi) +
  coord_equal(expand=0) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=tree.mean), size=5) +
  scale_color_gradient(low="gray80", high="green4") +
  theme_bw()

ggplot(data=dat.uhi) +
  coord_equal(expand=0) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=D_T_DIFF), size=5) +
  scale_color_gradient(low="gray80", high="red3") +
  theme_bw()

ggplot(data=dat.uhi[,]) +
  geom_point(aes(x=tree.mean, y=D_T_DIFF)) +
  stat_smooth(aes(x=tree.mean, y=D_T_DIFF), method="lm")

lm.tree.mean <- lm(URB_D_MEAN ~ tree.mean*BUF_D_MEAN, data=dat.uhi)
summary(lm.tree.mean)


ggplot(data=dat.uhi) +
  geom_histogram(aes(x=tree.mean))

ggplot(data=dat.uhi[,]) +
  geom_point(aes(x=tree.mean, y=D_T_DIFF)) +
  stat_smooth(aes(x=tree.mean, y=D_T_DIFF), method="lm")

lm.tree.mean2 <- lm(D_T_DIFF ~ tree.mean, data=dat.uhi)
summary(lm.tree.mean2)

ggplot(data=dat.uhi[,]) +
  geom_point(aes(x=tree.diff, y=temp.diff))

ggplot(data=dat.uhi[,]) +
  geom_point(aes(x=tree.mean, y=D_T_DIFF))

ggplot(data=dat.uhi[,]) +
  geom_point(aes(x=D_T_DIFF, y=slope)) +
  stat_smooth(aes(x=D_T_DIFF, y=slope), method="lm")

lm.tree.effect <- lm(slope ~ D_T_DIFF, data=dat.uhi)
summary(lm.tree.effect)

lm.tree.var <- lm(temp.sd ~ tree.sd, data=dat.uhi)
summary(lm.tree.var)


ggplot(data=dat.uhi) +
  coord_equal(expand=0) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=correlation), size=5) +
  scale_color_gradient(low="gray50", high="green3") +
  theme_bw()
  
ggplot(data=dat.uhi[dat.uhi$correlation>0.3,]) +
  coord_equal(expand=0) +
  geom_path(data=world, aes(x=long, y=lat, group=group), color="gray80") +
  geom_point(data=dat.uhi, aes(x=LONGITUDE, y=LATITUDE), size=3, color="gray15") +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=slope), size=5) +
  scale_color_gradient(low="green4", high="gray50") +
  theme_bw()


