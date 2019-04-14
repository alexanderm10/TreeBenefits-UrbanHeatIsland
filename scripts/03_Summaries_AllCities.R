# Doing some quick EDA for geography of Urban Heat island
# Ecoregion info extracted from here: https://www.worldwildlife.org/publications/global-200
library(ggplot2); library(RColorBrewer); library(nlme); library(mgcv); 
path.dat <- "../data_processed/"
dat.uhi <- read.csv(file.path(path.dat, "cities_summary_sdei_v3.csv"))
# Remove cities where max tree cover <25%
dat.uhi <- dat.uhi[dat.uhi$tree.max>=25,]

# "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

dat.uhi$temp.diff <- dat.uhi$temp.max - dat.uhi$temp.min
dat.uhi$tree.diff <- dat.uhi$tree.max - dat.uhi$tree.min
dat.uhi <- dat.uhi[order(dat.uhi$tree.mean),]
summary(dat.uhi)
dim(dat.uhi)
dat.uhi[grepl("[?]", dat.uhi$NAME),"NAME"]

# New plan for quantifying effects of trees on UHI: 
# apply mean temperature of treeless area to whole area and calculate the difference
pb <- txtProgressBar(min=0, max=nrow(dat.uhi), style=3)
dat.all <- data.frame()
for(i in 1:nrow(dat.uhi)){
  # i=which(dat.uhi$NAME=="Chicago")
  setTxtProgressBar(pb, i)
  # if(grepl("[?]", dat.uhi$NAME[i])) next
  
  dat.city <- read.csv(file.path(path.dat, "cities_full_sdei_v3", paste0(dat.uhi$NAME[i], "_data_full.csv")))
  
  dat.all <- rbind(dat.all, dat.city[complete.cases(dat.city),])
}
dat.all$Name <- factor(dat.all$Name, levels=dat.uhi$NAME) # Ordering based on how dat.uhi is sorted
summary(dat.all)



ggplot(data=dat.all) +
  coord_flip() +
  geom_boxplot(aes(x=Name, y=cover.tree), outlier.size=0.1, outlier.stroke = 0.1) +
  scale_y_continuous(limits=c(0,100), expand=c(0,0)) +
  theme_bw() +
  theme(axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y = element_blank())
