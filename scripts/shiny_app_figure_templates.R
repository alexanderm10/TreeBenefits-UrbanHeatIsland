# preliminary plotting scripts for figures to be included in shiny
library(ggplot2)
library(shiny)
library(bsTooltip)
path.save <- file.path("C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/")

# loading in data sets----
effectsUHI <- readRDS(file.path(path.save, "UHI_effects_fig_data.Rds"))
StatsCombined <- readRDS(file.path(path.save, "Distribution_fig_data.Rds"))
TrendsTreeAll <- readRDS(file.path(path.save, "Tree_trends_data.RDS"))
# plotting figures
# want this to be filtered by State, FEMA region, Biome, and even select a city by the user

# Figure templates----
# Temperature effects
uhi.figs <- ggplot(data=effectsUHI, aes(x=biomeCode, y=values, fill=ind)) + 
  geom_bar(stat="summary", fun="median") +
  geom_hline(yintercept=0, size=0.5, color="black") +
  # scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[4], "Remaining UHI"="#fb6a4a")) +
  scale_fill_manual(name="Temp. Source", values=c("Tree"="#005a32", "Non-Tree Veg"=rev(grad.other)[3], "Remaining UHI"=rev(grad.lst)[3])) +
  
  geom_text(x="Des", y=-7, hjust=1, label="Cooling Effect") +
  geom_text(x="Des", y=2.5, hjust=1, label="Warming Effect") +
  coord_cartesian(ylim=c(-7.5, 2.5)) +
  labs(x="Biome", y="Temperature Effect (˚C)") +
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_text(face="bold"),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        axis.text.x=element_text(color="black", angle=-20, hjust=0),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))


# Tree Cover Targets
## Want to be able to select the Y variable from a list of variables
## want the standard filters to be used
## want points to show city name on hover.
TreeCoverTarget <- ggplot(data=StatsCombined, aes(x=biomeCode, y=TreeCoverTargetUHI, fill="Biome Target", color="Biome Target")) +  #facet_grid(~ fema.region, scales = "free_x") +
  geom_bar(stat="summary", fun="median") +
  # geom_segment(yend=0, aes(xend=biomeCode), stat="summary", fun="median", size=2) +
  geom_jitter(aes(x=biomeCode, y=value.tree.core, fill="Current", color="Current"), width=0.2, alpha = 0.85) +
  geom_violin(aes(x=biomeCode, y=value.tree.core, fill="Current", color="Current"), scale="width", alpha=0.35) +
  # geom_point(stat="summary", fun="median", size=5) +
  scale_fill_manual(name="Tree Cover", values=c("Current"="#005a32", "Biome Target"=grad.tree[4])) +
  scale_color_manual(name="Tree Cover", values=c("Current"="#005a32", "Biome Target"=grad.tree[4])) +
  labs(x="Biome", y="Tree Cover (%)") +
  scale_y_continuous(limits=c(0,70), expand=c(0,0)) +
  theme_bw()+
  theme(legend.position="top",
        legend.title=element_text(face='bold'),
        panel.background = element_rect(fill=NA),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text.y=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        axis.text.x=element_text(color="black", angle=-20, hjust=0),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))



# Linear time series plots
## want the standard filters to be used
## would like to be able hover over lines to get a specific city with starting and ending points
# want final display to show single biome in each panel for the filtered FEMA region

figTrends <- ggplot(data=TrendsTreeAll[TrendsTreeAll$N.Analyzed>=10,]) +
  facet_wrap(~biomeCode) +
  geom_segment(data=StatsCombined[StatsCombined$biomeName %in% TrendsTreeAll$biomeName[TrendsTreeAll$N.Analyzed>=10],], aes(x=2001, xend=2020, y=EstTree2001, yend=EstTree2020), size=0.1, alpha=0.7, color="gray80") +
  geom_segment(aes(x=2001, xend=2020, y=EstTree2001, yend=EstTree2020, color="Observed Trend"), size=2) +
  geom_segment(aes(x=2001, xend=2020, y=EstTree2001, yend=YendUHI, color="Mitigate Development Warming"), size=2, linetype="dashed") +
  geom_segment(aes(x=2001, xend=2020, y=EstTree2001, yend=YendWarm, color="Mitigate All Warming"), size=2, linetype="dashed") +
  geom_text(aes(x=2020, y=EstTree2020-1, label=paste0("+",round(20*TrendObsTreeMed, 1), "%"), color="Observed Trend"), hjust=0, show.legend=F, size=3 ) +
  geom_text(aes(x=2020, y=YendUHI+1, label=paste0("+", round(20*TargetUHITreeMed, 1), "%"), color="Mitigate Development Warming"), hjust=0, show.legend=F, size=3) +
  geom_text(aes(x=2020, y=YendWarm+3, label=paste0("+", round(20*TargetWarmTreeMed, 1), "%"), color="Mitigate All Warming"), hjust=0, show.legend=F, size=3) +
  scale_color_manual(name="Tree Cover Trends", values=c("Observed Trend" = "#005a32", "Mitigate Development Warming"="#3FA242", "Mitigate All Warming"="#A3D16B"))+
  scale_x_continuous(name="Year", breaks=c(2001, 2020), limits=c(1999, 2025)) +
  scale_y_continuous(name="Tree Cover (%)") +
  guides(label=NA) +
  theme(legend.position="top",
        legend.title=element_text(face='bold'),
        legend.key = element_rect(fill=NA),
        panel.background = element_rect(fill=NA, color="black"),
        panel.grid=element_blank(),
        axis.ticks.length = unit(-0.25, "lines"),
        axis.text=element_text(color="black"),
        axis.title=element_text(color="black", face="bold"),
        strip.text = element_text(color="black", face="bold"),
        # axis.text.x=element_blank(),
        # axis.title.x=element_blank(),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines"))


################################
# Shiny dev code----

suppressPackageStartupMessages(
  suppressWarnings({
    library(RSQLite)
    library(shiny)
    library(DT)
    library(sp)
    library(leaflet)
    library(htmltools)
    library(ggplot2)
    library(gameofthrones)
    library(plotly)
  })
)

# GLOBALS
options(shiny.host = '0.0.0.0')
options(shiny.port = 5050)

regions <- unique(StatsCombined$fema.region)
biome <- unique(StatsCombined$biomeCode)



