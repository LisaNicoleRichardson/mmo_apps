#Shiny app (basic)
#Load required libraries
library(shiny)
library(shinydashboard)
library(graphics)
library(mapplots)
library(data.table)
library(geo)
library(shapefiles)
library(lattice)
library(latticeExtra)
library(ggplot2)
library(reshape)
library(reshape2)
library(plyr)
library(memisc)
library(xlsx)

ui<- fluidPage(
  tabsetPanel(
    tabPanel("Metadata", 
             fluidRow(
               column(8, tags$h1("Methodology and caveats of Landings data by ICES rectangle for all UK registered vessels")
               ),
               column(4,
                      tags$img( height = 220, width = 277, src = "MMO_logo2.jpg")
               )
             ),
             fluidRow(
               {column(10,
                       box(width = 10, title = "MMO Interactive Mapping App"
                           
                       )
               )}
             ),
             fluidRow(
               column(10,
                      box( width = 10, title = "Abstract and Lineage", 
                           {
                             div(style = "overflow-y:scroll; max-height: 600px", 
                                 textOutput("text1",),   
                                 tags$a(href="http://www.fao.org/fishery/topic/1617/en", "http://www.fao.org/fishery/topic/1617/en"),
                                 tags$p(),
                                 textOutput("text2")
                                 
                             )
                           }
                      )
               )
             )
    ),
    tabPanel("Maps",
             fluidRow(
               column(8, tags$h1("Landings data by ICES rectangle Map by selection"),
                      
                      uiOutput("selectMapSpecies"),                      
                      
                      actionButton(inputId = "mapGo", label = "Update")
               ),
               column(4,
                      tags$img( height = 220, width = 277, src = "MMO_logo2.jpg")
               )
             ),
             fluidRow(
               box( width = 12, title = "Estimated Quantity Landed by ICES Rectangle"     
                    , plotOutput("Map_q", width = 1600, height= 400 ))
             ),
             fluidRow(
               box( width = 12, title = "Estimated Value Landed by ICES Rectangle"     
                    , plotOutput("Map_v", width = 1600, height= 400 ))
             ),
             fluidRow( 
               column(12,
                      downloadButton("downloadData", "Click to download .txt map data"),
                      tags$br(),
                      tags$hr()
                      
               )
             )
    )
    
  )
)

server <- function(input, output){
  
  #FOR METADATA
  #Text Outputs 
  {  
    output$text1 <- renderText("This data set provides summaries of fishing activity for UK commercial fishing vessels that
                               are deemed to have been fishing within a specified calendar year. These summaries have been 
                               aggregated by month of landing, the ICES division and rectangle fishing activity took place 
                               in, the length group of the vessel and the gear group used. For each aggregation the quantity 
                               (tonnes) of live weight fish landed, the actual landed weight (tonnes) and value (sterling) of 
                               live weight fish landed are given for specific species, with the remaining species combined 
                               into a composite group based on the species group they are classified to.  The gear categories
                               are comprised as follows; beam trawl; pelagic seine; demersal trawl & seine (includes all trawl
                               gears (except beam trawl) and all seine gears (except purse seine); dredges; drift & fixed nets
                               ; gears using hooks; other mobile gears; other passive gears; and pots & traps. More information 
                               on the exact nature of fishing gears can be found at:"
                               )
    
    
    output$text2 <- renderText("Data on fishing activity is taken from the IFISH data system. This is a UK repository of key elements 
                               of the administrative data commercial fishermen are required to report under EU legislation to enable 
                               competent authorities to monitor and manage their activity. There are three key sources of data - the 
                               logbook of activity while at sea, the landing declaration recording the accurate weight of fish when 
                               landed, and sales notes created when the fish are first sold after landing. The first two of these are 
                               required from all fishermen operating vessels over 10 metres overall length, the sales notes information 
                               is required for sales by all licensed vessels irrespective of length. The data includes the species, weight
                               and value of fish landed along with details of the vessel involved, where the fish was caught, and with what 
                               gear. These data are collected by the four fisheries administrations within the UK, checked and validated 
                               and collated into the IFISH repository holding full UK data on licensed commercial fishing activity. The 
                               finest available level of spatial resolution for activity data within IFISH is an ICES rectangle. Each 
                               rectangle represents an area of the sea that measures 0.5 degrees of latitude by 1 degree of longitude.")
    
    
  }
  #FOR MAPS
  mapData <- read.csv("/srv/shiny-server/app1/mapData_2012_2016.csv", header=TRUE, sep=",")
  #Shapefiles and ranges
  coastline <- read.shapefile("/srv/shiny-server/app1/ne_10m_coastline")
  coastline2 <- read.shapefile("/srv/shiny-server/app1/eez_lr")
  
  #Create map data
  colnames(mapData)<- c("Year","Rect","Species","QtyT","ValGBP")
  
  # Map
  mapData <- as.data.table(mapData)
  mapData <- mapData[!(mapData$Rect =="UNK"),]
  mapData <- mapData[!(mapData$Rect =="00Z5"),]
  mapData <- mapData[!(mapData$Rect =="00Z6"),]
  mapData[, Latitude := ir2d(mapData[,Rect])[1]]
  mapData[, Longitude := ir2d(mapData[,Rect])[2]]
  mapData<- mapData[order(Year),]
  
  # Map data for download
  data <- reactive({mapData[mapData$Species == input$MapSpecies]})
  
  #Reactive content
  sp<-unique(mapData$Species)
  output$selectMapSpecies<- renderUI({
    selectInput(inputId = "MapSpecies", label = "Select Species:", 
                sp, selected = NULL, multiple= FALSE)})
  mapQty<- reactive({mapData$QtyT[mapData$Species == input$MapSpecies]})
  a<- reactive({as.matrix(mapQty())})
  nmapQty<- reactive({as.numeric(a())})
  
  mapVal<- reactive({mapData$ValGBP[mapData$Species == input$MapSpecies]})
  b<- reactive({as.matrix(mapVal())})
  nmapVal<- reactive({as.numeric(b())})
  
  mapLo<- reactive({mapData$Longitude[mapData$Species == input$MapSpecies]})
  Lo<- reactive({as.matrix(mapLo())})
  mapLong<- reactive({as.numeric(Lo())})
  
  mapLa<- reactive({mapData$Latitude[mapData$Species == input$MapSpecies]})
  La<- reactive({as.matrix(mapLa())})
  mapLat<- reactive({as.numeric(La())})
  
  mapYear<- reactive({mapData$Year[mapData$Species == input$MapSpecies]})
  Ye<- reactive({as.matrix(mapYear())})
  mapY<- reactive({as.numeric(Ye())})
  
  
  xlim1 <- reactive({range(mapLong())})
  ylim1 <- reactive({range(mapLat())})
  byx = 1    
  byy = 0.5 
  
  Landings_grid_q <- eventReactive(input$MapSpecies,  
                                   {make.multigrid(Lo(), La(), 
                                                   nmapQty(), mapY(), byx, byy, xlim1(), ylim1())})
  
  max_annual_Landings_q <- reactive({ceiling(max(breaks.grid(Landings_grid_q(), quantile = 1, ncol = 7, zero = FALSE)))}) 
  colour_scale <- c("peachpuff","rosybrown1","rosybrown", "hotpink", "deeppink", "deeppink3", "deeppink4")
  # breakpoints<- (length(colour_scale)+1)
  # annual_breaks_Landings_q <- reactive({seq(min(a()), max(a()), length.out=breakpoints)})
  annual_breaks_Landings_q <-reactive({quantile(a(), prob = c(0.125, 0.25, 0.375,0.5,0.625,0.75,0.875,1))})
  
  Landings_grid_v <- eventReactive(input$MapSpecies,  
                                   {make.multigrid(Lo(), La(), 
                                                   nmapVal(), mapY(), byx, byy, xlim1(), ylim1())})
  
  max_annual_Landings_v <- reactive({ceiling(max(breaks.grid(Landings_grid_v(), quantile = 1, ncol = 7, zero = FALSE)))}) 
  #breakpoints<- (length(colour_scale)+1)
  #annual_breaks_Landings_v <- reactive({seq(min(b()), max(b()), length.out=breakpoints)})
  annual_breaks_Landings_v <-reactive({quantile(b(), prob = c(0.125, 0.25, 0.375,0.5,0.625,0.75,0.875,1))})
  
  #Plots
  map_q<- 
    eventReactive(input$mapGo,{
      par(mfrow=c(1,5))
      for(s in names(Landings_grid_q())) {
        basemap(xlim1(), ylim1(), axes = FALSE, xlab = "", ylab = "", col= "grey", bg ="white")
        draw.rect()
        draw.grid(Landings_grid_q()[[s]], annual_breaks_Landings_q(), col = colour_scale)
        title(main = s)
        draw.shape(coastline, col = "cornsilk")
        draw.shape(coastline2, fillOddEven=TRUE, col = "transparent", lwd = 3)
        
        
      }
      
      legend.grid("bottomleft", breaks = annual_breaks_Landings_q(), type = 2, inset = c(0.02, 0.02),
                  title = paste("Landings (tonnes)"), 
                  y.intersp = 0.8, bg = "cornsilk", col = colour_scale, cex = 1.2)
    })
  
  map_v<- 
    eventReactive(input$mapGo,{
      par(mfrow=c(1,5))
      for(s in names(Landings_grid_v())) {
        basemap(xlim1(), ylim1(), axes = FALSE, xlab = "", ylab = "", col= "grey", bg ="white")
        draw.rect()
        draw.grid(Landings_grid_v()[[s]], annual_breaks_Landings_v(), col = colour_scale)
        title(main = s)
        draw.shape(coastline, col = "cornsilk")
        draw.shape(coastline2, fillOddEven=TRUE, col = "transparent", lwd = 3)
        
        
      }
      legend.grid("bottomleft", breaks = (annual_breaks_Landings_v()/1000), type = 2, inset = c(0.02, 0.02),
                  title = paste("Value (£ thousands)"), 
                  y.intersp = 0.8, bg = "cornsilk", col = colour_scale, cex = 1.2)
    })
  
  #Update button for maps
  mapButton <- eventReactive(input$mapGo, {Map_q()
    Map_v()})
  
  #output for maps
  output$Map_q <-  renderPlot({map_q()})
  output$Map_v <-  renderPlot({map_v()})
  
  #Download button for data table
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("My_landings_data_", Sys.Date(), ".txt")
    },
    content = function(con) {
      write.table(data(),sep=",", con)
    })
  
}

shinyApp(ui= ui, server=server)