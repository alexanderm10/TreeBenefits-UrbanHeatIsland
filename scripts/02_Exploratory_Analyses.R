# Doing some quick EDA for geography of Urban Heat island
library(ggplot2)

dat.uhi <- read.csv("../data_processed/cities_summary_sdei.csv")
dat.uhi <- dat.uhi[dat.uhi$tree.min>=0 & dat.uhi$D_T_DIFF>=0,]
dat.uhi$temp.diff <- dat.uhi$temp.max - dat.uhi$temp.min
dat.uhi$tree.diff <- dat.uhi$tree.max - dat.uhi$tree.min
summary(dat.uhi)
dim(dat.uhi)

world <- map_data("world")

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
  # scale_color_gradient2(low="gray50", high="green3", midpoint=0.25) +
  theme_bw()
  
ggplot(data=dat.uhi) +
  coord_equal(expand=0) +
  geom_path(data=world, aes(x=long, y=lat, group=group)) +
  geom_point(aes(x=LONGITUDE, y=LATITUDE, color=slope), size=5) +
  scale_color_gradient(low="green4", high="gray50") +
  theme_bw()


