# preliminary plotting scripts for figures to be included in shiny
library(ggplot2)
library(shiny)
library(shinyBS)
# Loading in colors----
grad.lst <- c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#fbbdc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
grad.lstHot <- c("#ffffcc", "#ffeda0", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#bd0026", "#800026") # ends with red
grad.treeDiff <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30") # ends with teal
grad.tree <- c("#ffffe5", "#f7fcb9", "#d9f0a3", "#addd8e", "#78c679", "#41ab5d", "#238443", "#006837", "#004529") # ends with green
grad.otherDiff <- c("#8e0152", "#c51b7d", "#de77ae", "#f1b6da", "#fde0ef", "#e7f5d0", "#b8e186", "#7fbc41", "#4d9221", "#276419") # Green
grad.other <- c("#f7fcf0", "#e0f3db", "#ccebc5", "#a8ddb5", "#7bccc4", "#4eb3d3", "#2b8cbe", "#0868ac", "#084081")
grad.modfit <- c("#fff7f3", "#fde0dd", "#fcc5c0", "#fa9fb5", "#f768a1", "#dd3497", "#ae017e", "#7a0177", "#49006a")
biome.pall.all = c("Taiga"= "#2c5c74", 
                   "Tundra"="#6d8e9d",
                   "Temperate Broadleaf Forest" = "#7f310f",
                   "Temperate Conifer Forest" = "#4d1e10",
                   "Temperate Grassland/Savanna" = "#b09c41",
                   "Montane Grassland/Savanna" = "#a0b8c7",
                   "Mediterranean" = "#bf772e",
                   "Desert" = "#c89948",
                   "Flooded Grassland/Savanna" = "#e0dfa1",
                   "Tropical Grassland/Savanna" = "#a6b39e",
                   "Tropical Dry Broadleaf Forest" = "#7a9c64",
                   "Tropical Conifer Forest" = "#488458",
                   "Tropical Moist Broadleaf Forest"= "#266240",
                   "Mangroves" = "#9c8c94")

biomeCode.pall.all = c("Tai"= "#2c5c74", 
                       "Tun"="#6d8e9d",
                       "TeBF" = "#7f310f",
                       "TeCF" = "#4d1e10",
                       "TeGS" = "#b09c41",
                       "MGS" = "#a0b8c7",
                       "Med" = "#bf772e",
                       "Des" = "#c89948",
                       "FGS" = "#e0dfa1",
                       "TrGS" = "#a6b39e",
                       "TrDBF" = "#7a9c64",
                       "TrCF" = "#488458",
                       "TrMBF"= "#266240",
                       "Man" = "#9c8c94")

fema.cols <- c("region1" = "#725cfd",
               "region2" = "#301fab",
               "region3" = "#2537d5",
               "region4" = "#0a6cdf",
               "region5" = "#85c0f0",
               "region6" = "#d7d148", 
               "region7" = "#c8bf0c",
               "region8" = "#f6d144",
               "region9" = "#e1b329",
               "region10" = "#976026")



path.save <- file.path("C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/")

# loading in data sets----
effectsUHI <- readRDS(file.path(path.save, "UHI_effects_fig_data.Rds"))
StatsCombined <- readRDS(file.path(path.save, "Distribution_fig_data.Rds"))
# TrendsTreeAll <- readRDS(file.path(path.save, "Tree_trends_data.RDS"))
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

TreeCoverTarget

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

# set filters----
regions <- as.character(unique(StatsCombined$fema.region))
biome <- as.character(unique(StatsCombined$biomeCode))


# dynamic Temperature effects figure----

ui <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))),
  
  # selectInput("Collection", "Choose a Collection:", choices=c(list(Collection=as.list(paste(unique(dat.pheno$collection))))), 
  selectInput("Region", "Choose a FEMA Region:", list(Region=as.list(regions))), 
  selectInput("Biome", "Choose a Biome:", list(Biome=as.list(biome))), 
  mainPanel(uiOutput("plot.ui", click="plot_click"), height="100%"),
  verbatimTextOutput("info")
  
  
)

  
server <- function(input ,output){
 
  nInd <- reactive({
    length(unique(effectsUHI$ISOURBID[effectsUHI$biome==input$Biome]))
  })
  plotHeight <- reactive(15*nInd())
  
  output$plot1<-renderPlot({
    dat.subs <- effectsUHI$femaRegion==input$Region & effectsUHI$biomeCode==input$Biome
    
    ggplot(data=effectsUHI[dat.subs,], aes(x=biomeCode, y=values, fill=ind)) + 
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
    })
  output$plot.ui <- renderUI({
    plotOutput("plot1", click="plot_click")
  })
}
shinyApp(ui, server)

  
# Dynamic tree targets----

ui2 <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))),
  
  # selectInput("Collection", "Choose a Collection:", choices=c(list(Collection=as.list(paste(unique(dat.pheno$collection))))), 
  selectInput("Region", "Choose a FEMA Region:", list(Region=as.list(regions))), 
  mainPanel(uiOutput("plot.ui", click="plot_click"), height="100%"),
  verbatimTextOutput("info")
  
  
)


server2 <- function(input ,output){
  
  # nInd <- reactive({
  #   length(unique(StatsCombined$ISOURBID[StatsCombined$biomeCode==input$Biome]))
  # })
  # plotHeight <- reactive(
  #   15*nInd()
  #   )
  
  output$plot1<-renderPlot({
    dat.subs <- StatsCombined$fema.region==input$Region
    
    ggplot(data=StatsCombined[dat.subs,], aes(x=biomeCode, y=TreeCoverTargetUHI, fill="Biome Target", color="Biome Target")) +  #facet_grid(~ fema.region, scales = "free_x") +
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
  })
  output$plot.ui <- renderUI({
    plotOutput("plot1", click="plot_click")
  })
}
shinyApp(ui2, server2)

############################################################
# Dynamic linear plots----
# Need to think about the underlying data that make up these plots. May have to bring in the non-aggregated data to do on the fly calculations.

ui3 <- fluidPage(
  # Some custom CSS for a smaller font for preformatted text
  tags$head(
    tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))),
  
  # selectInput("Collection", "Choose a Collection:", choices=c(list(Collection=as.list(paste(unique(dat.pheno$collection))))),
  selectInput("Region", "Choose a FEMA Region:", list(Region=as.list(regions))), 
  selectInput("Biome", "Choose a Biome:", list(Biome=as.list(biome))), 
  mainPanel(uiOutput("plot.ui", click="plot_click"), height="100%"),
  verbatimTextOutput("info")
  
  
)


server3 <- function(input ,output){
  
  nInd <- reactive({
    length(unique(StatsCombined$ISOURBID[StatsCombined$biomeCode==input$Biome]))
  })
  plotHeight <- reactive(
    15*nInd()
    )
  
  output$plot1<-renderPlot({
    dat.subs <- StatsCombined$fema.region==input$Region & StatsCombined$biomeCode==input$Biome
    
    StatsCombined.use <- StatsCombined[dat.subs,]
    
    intensifyUHI <- StatsCombined.use$value.LST.diff>0 & StatsCombined.use$value.tree.diff.p < 0.01 & StatsCombined.use$trend.LST.core>0 & StatsCombined.use$trend.LST.diff>0 & StatsCombined.use$trend.LST.diff.p<0.01
    citiesWarm <- StatsCombined.use$trend.LST.core>0 & StatsCombined.use$trend.LST.p.core < 0.01
    
    trend.temp <- aggregate(trend.LST.diff ~ fema.region + biomeCode, data=StatsCombined.use[intensifyUHI,], FUN=median, na.rm=T)
    
    # Observed Stats
    TrendTreesObs <- aggregate(ISOURBID ~ fema.region + biomeName + biomeCode, data=StatsCombined.use, FUN=length)
    names(TrendTreesObs)[4] <- "N.Analyzed"
    TrendTreesObs$EstTree2001 <- aggregate(EstTree2001 ~ biomeName, data=StatsCombined.use, FUN=median, na.rm=T)[,2]
    TrendTreesObs$EstTree2020 <- aggregate(EstTree2020 ~ biomeName, data=StatsCombined.use, FUN=median, na.rm=T)[,2]
    TrendTreesObs$TrendObsTreeMed <- aggregate(trend.tree.core ~ biomeName, data=StatsCombined.use, FUN=median, na.rm=T)[,2]
    TrendTreesObs
    
    
    # UHI Stats
    TrendTreesUHI <- aggregate(ISOURBID ~ fema.region + biomeName, data=StatsCombined.use[intensifyUHI,], FUN=length)
    names(TrendTreesUHI)[3] <- "N.UHI"
    TrendTreesUHI$TrendUHIMed <- aggregate(trend.LST.diff ~ fema.region + biomeName, data=StatsCombined.use[intensifyUHI,], FUN=median, na.rm=T)[,3]
    TrendTreesUHI$TrendUHITreeMed <- aggregate(trend.tree.core ~ fema.region + biomeName, data=StatsCombined.use[intensifyUHI,], FUN=median, na.rm=T)[,3]
    TrendTreesUHI$TargetUHITreeMed <- aggregate(TargetConstantUHI ~ fema.region + biomeName, data=StatsCombined.use[intensifyUHI,], FUN=median, na.rm=T)[,3]
    TrendTreesUHI <- merge(TrendTreesObs, TrendTreesUHI, all=T)
    TrendTreesUHI
    
    
    # Warming stats
    TrendTreesWarm <- aggregate(ISOURBID ~ fema.region + biomeName, data=StatsCombined[citiesWarm,], FUN=length)
    names(TrendTreesWarm)[3] <- "N.Warm"
    TrendTreesWarm$TrendWarmMed <- aggregate(trend.LST.core ~ fema.region + biomeName, data=StatsCombined.use[citiesWarm,], FUN=median, na.rm=T)[,3]
    TrendTreesWarm$TrendWarmTreeMed <- aggregate(trend.tree.core ~ fema.region + biomeName, data=StatsCombined.use[citiesWarm,], FUN=median, na.rm=T)[,3]
    TrendTreesWarm$TargetWarmTreeMed <- aggregate(TargetOffsetWarming ~ fema.region + biomeName, data=StatsCombined.use[citiesWarm,], FUN=median, na.rm=T)[,3]
    TrendTreesWarm <- merge(TrendTreesObs, TrendTreesWarm, all=T)
    TrendTreesWarm
    
    # merging together
    TrendTreesWarm <- merge(TrendTreesObs, TrendTreesWarm)
    
    TrendsTreeAll <- merge(TrendTreesUHI, TrendTreesWarm, all=T)
    TrendsTreeAll$YendUHI <- TrendsTreeAll$EstTree2001+20*TrendsTreeAll$TargetUHITreeMed
    TrendsTreeAll$YendWarm <- TrendsTreeAll$EstTree2001+20*TrendsTreeAll$TargetWarmTreeMed
    
    # Creating a vector to help annotate
    TrendsTreeAll$YlabUHI <-TrendsTreeAll$YendUHI
    TrendsTreeAll$YlabWarm <-TrendsTreeAll$YendWarm
    TrendsTreeAll
    
    
    ggplot(data=TrendsTreeAll) +
      # facet_wrap(re~biomeCode) +
      geom_hline(yintercept=0, linetype="dashed") +
      geom_segment(data=StatsCombined.use, aes(x=2001, xend=2020, y=EstTree2001, yend=EstTree2020), size=0.1, alpha=0.7, color="gray80") +
      geom_segment(aes(x=2001, xend=2020, y=EstTree2001, yend=EstTree2020), size=0.1, alpha=0.7, color="gray80") +
      geom_segment(aes(x=2001, xend=2020, y=EstTree2001, yend=EstTree2020, color="Observed Trend"), size=2) +
      geom_segment(aes(x=2001, xend=2020, y=EstTree2001, yend=YendUHI, color="Mitigate Development Warming"), size=2, linetype="dashed") +
      geom_segment(aes(x=2001, xend=2020, y=EstTree2001, yend=YendWarm, color="Mitigate All Warming"), size=2, linetype="dashed") +
      geom_text(aes(x=2020, y=EstTree2020-1, label=paste0("+",round(20*TrendObsTreeMed, 1), "%"), color="Observed Trend"), hjust=0, show.legend=F, size=3 ) +
      geom_text(aes(x=2020, y=YendUHI+1, label=paste0("+", round(20*TargetUHITreeMed, 1), "%"), color="Mitigate Development Warming"), hjust=0, show.legend=F, size=3) +
      geom_text(aes(x=2020, y=YendWarm+3, label=paste0("+", round(20*TargetWarmTreeMed, 1), "%"), color="Mitigate All Warming"), hjust=0, show.legend=F, size=3) +
      scale_color_manual(name="Tree Cover Trends", values=c("Observed Trend" = "#005a32", "Mitigate Development Warming"="#3FA242", "Mitigate All Warming"="#A3D16B"))+
      scale_x_continuous(name="Year", breaks=c(2001, 2020), limits=c(1999, 2025)) +
      scale_y_continuous(name="Tree Cover (%)", limits=c(0, 65)) +
      labs(title = paste(TrendsTreeAll$fema.region, TrendsTreeAll$biomeName, sep=" "), caption = paste0("Number of Cities Analyzed: ", TrendsTreeAll$N.Analyzed)) +
      guides(label=NA) +
      theme(legend.position="bottom",
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
  
  })
   
  output$plot.ui <- renderUI({
    plotOutput("plot1", click="plot_click")
  })
}
shinyApp(ui3, server3)



