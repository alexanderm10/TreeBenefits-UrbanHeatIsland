# making a widget that would calculate potential cooling mitigation for region and biome
library(ggplot2)
library(shiny)
library(shinyBS)
library(shinyChakraSlider)

path.save <- file.path("C:/Users/malexander/Documents/r_files/northstar2023/1km_modis/")

# loading in data sets----

StatsCombined <- readRDS(file.path(path.save, "Distribution_fig_data.Rds"))
head(StatsCombined)

regions <- as.character(unique(StatsCombined$fema.region))
biome <- as.character(unique(StatsCombined$biomeCode))

ui4 <- fluidPage(
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
  sliderInput("treeCover", "Percent Tree Cover:",
         value=10,
         min= 0,
         max=100,
         step=0.5),
  numericInput("potentialCooling", "Median Potential Cooling from Trees (\u00B0C):", value = ""),
  numericInput("potentialCoolingF", "Median Potential Cooling from Trees (\u00B0F):", value = ""),
  
  mainPanel(uiOutput("plot.ui", click="plot_click"), height="100%"),
  verbatimTextOutput("info")
  
    
)

server4 <- function(input ,output, session){
  
  # want an option for use all regions if region is blank and all biomes if biome is blank and the combination
  
  
  observeEvent(c(input$treeCover, input$Region, input$Biome),{
    dat.subs <- StatsCombined$fema.region==input$Region & StatsCombined$biomeCode==input$Biome
    data.use <- StatsCombined[dat.subs,] 
    median.tree.slope <- median(data.use$model.tree.slope, na.rm=T)
    updateNumericInput(session, "potentialCooling", value = round(input$treeCover*median.tree.slope,2))
    updateNumericInput(session, "potentialCoolingF", value = round((input$treeCover*median.tree.slope)*(9/5),2))
       }
    )
  }
shinyApp(ui4, server4)
