### Workflow
#start here
### 1) library Load libraries

library(devtools)
library(stringr)
library(reshape2)
library(plyr)
library(dplyr)
library(readr)
library(leaflet)
library(htmlwidgets)
library(webshot)
#library(leaflet.esri) #Package no longer exists
library(leaflet.extras)
library(doBy)
library(leafem)
library(leaflet.minicharts)
library(mapview) 
library(sf)
library(sp)
library(doBy)
library(readxl)

rm(list=ls())
### 2) import 
df <- read_excel("Gag_Fisherman Feedback_2026_Clean for analysis.xls")
### 2.1 duplicates check for duplicates here!!!

df <- df[!duplicated(df), ]
colnames(df) <- make.names(colnames(df))
### create an id variable
df$ID <- 1:nrow(df)
df$'General.Location.of.Observation' <- as.character(df$'General.Location.of.Observation')

# # Create a vector of the original type variable
# type <- c(NA, NA, NA, "neutral", NA, "negative", NA, NA, "negative", "positive", NA, "positive", NA)
# 

#df$type <- recode(df$type, positive = "1", nuetral = "0", negative="-1")

### 3) obsPerArea Determine number of observations by area. 

### 3.1)  get number of observations per grid

########################################################################################
### Create number of observations per area to merge with shrimp
### Check about strings as factors = false

## df
loc <- data.frame(x=df$'General.Location.of.Observation', stringsAsFactors = FALSE)

loc$y <- trimws(loc$x)

### object to hold results
out <- c()

for(i in 1:length(loc$y)){
  vars <- str_split(loc$y[i], ";") ## use for semicolon separator
  vars2 <- unlist(vars[1])
  vars3 <- trimws(vars2)
  vars4 <- data.frame(vars3)
  vars4$id <- i
  colnames(vars4) <- c("Location", "id")
  out <- rbind(out, vars4)
}

out$val <- 1
out2 <- dcast(out, id ~ Location, value.var ="val")
out2[is.na(out2)] <- 0
save(out2, file="out2.RData") ## for use with the  sentiment maps

out3 <- data.frame(colSums(out2))
out3$Location <- rownames(out3)
rownames(out3) <- NULL

##  Subset and sort the data
out3 <- subset(out3, Location!="id")
out3 <- subset(out3, Location!="Other")
out4 <- arrange(out3, Location)


nums <- data.frame(parse_number(out4$Location))
out4$nums <- parse_number(out4$Location)
out5 <- subset(out4, out4$nums !="na")
out5 <- arrange(out5, out5[,3])
out6 <- data.frame(out5[,3], out5[,1])
colnames(out6) <- c("ID", "Responses")
out7 <- arrange(out6, ID)

################### warning #################################################
### summarize responses by ID, this added for gag, seems right but double check
library(doBy)
outx <- summaryBy( Responses ~ ID,  data=out7, FUN=sum, keep.names=TRUE)
out7 <- outx

### 3.2: georeference 
############################################ Import the shrimp grids ########################
### This section will produce a map of the number of responses by zone:

### 3.2: grid Import shrimp grid for georeferencing

# gridShrimp <- readOGR(dsn="shp/ShrimpStatZones_1thru21_GCSWGS84.shp", 
#                       layer="ShrimpStatZones_1thru21_GCSWGS84")

#### use sf to import then conver to sp.  need to check in R 4.3+
gridShrimp_sf <- st_read("shp/ShrimpStatZones_1thru21_GCSWGS84.shp")
gridShrimp <- as(gridShrimp_sf, "Spatial")

## Extract datatable and add zone numbers
gridShrimp@data$ROW <- 0:29 

## Convert data slot to a data frame
gridShrimpdf <- gridShrimp@data
## remove some unnnecessary columns, only the first 3 are relevant
gridShrimpdf <- gridShrimpdf[,c(1:3, 9)] 


### this section in updated from previous version to accomodate situations where some grids don't have
### any responses associated with them.  In this case, the merge will result in a NA for that observation
### which should be replace by 0.  
gridShrimpdf2 <- merge(gridShrimpdf, out7, by="ID", all=TRUE)
gridShrimpdf2 <- gridShrimpdf2[1:30,] #added 8 29 2024

### replace NA with 0.  This is correct in this use case.
gridShrimpdf2$Responses[is.na(gridShrimpdf2$Responses)] <- 0
gridShrimpdf3 <- arrange(gridShrimpdf2, ROW)
gridShrimp@data <- gridShrimpdf3



### 4) obsMap create map

### 4.1 centroidLabel

### gridShrimp.RData from CobiaSF (April 2020) https://github.com/jfroeschke/CobiaSF
##load("RData/gridShrimp.RData")
pieGrid <- gridShrimp
centroids <- data.frame(coordinates(pieGrid))
centroids$ID <- pieGrid@data$ID
centroids$ROW <- pieGrid@data$ROW
colnames(centroids) <- c("x", "y", "ID", "ROW")
centroidsPie <- centroids
centroidsPieMerge <- merge(centroidsPie, gridShrimpdf3, all=TRUE)
centroidsPieMerge2 <- summaryBy(x +y ~  ID, data=centroidsPieMerge, 
                                FUN=mean, keep.names=TRUE, id="ROW")
centroidsPieMerge3 <- arrange(centroidsPieMerge2 , ROW)
centroidsPie2 <- summaryBy(x +y ~  ID, data=centroidsPie, FUN=mean, keep.names=TRUE)
centroidsPie3 <- centroidsPie2
centroidsPie3$id <- as.numeric(as.character(centroidsPie2$ID))
centroidsPie4 <- arrange(centroidsPie3, (id))
centroidsPie3 <- centroidsPie4
centroidsPie3[5,2] <- -83.64990
centroidsPie3[5,3] <- 27.54724
centroidsPie3[6,2] <- - 83.89160
centroidsPie3[6,3] <- 28.55558
centroidsPie3[9,2] <- - 86.48438
centroidsPie3[9,3] <- 29.85970
centroidsPie3[10,2] <- -87.40723
centroidsPie3[10,3] <- 29.89781
centroidsPie3[12,2] <- -89.38477
centroidsPie3[12,3] <- 29.91685
centroidsPie3[14,2] <- -90.54932
centroidsPie3[14,3] <- 28.57487

centroidsPie4 <- merge(centroidsPie3, gridShrimpdf3, all=TRUE)
centroidsPie3 <- centroidsPie4


#### nLabels

nLabels <- centroidsPieMerge3

#nLabels$n <- rowSums(nLabels[,4:6])
nLabels$n <- nLabels$Responses
nLabels[1,2] <- -81.25560
nLabels[1,3] <- 23.80347
nLabels[2,2] <- -82.5
nLabels[2,3] <- 23.80347
nLabels[3,2] <- -83.25
nLabels[3,3] <- 25.5
nLabels[4,2] <- -83.50
nLabels[5,2] <- -84.4
nLabels[5,3] <- 27.75
nLabels[6,2] <- -84.4
nLabels[6,3] <- 28.5
nLabels[7,2] <- -84.4
nLabels[7,3] <- 29.25
nLabels[8,2] <- -85.3
nLabels[8,3] <- 28.7
nLabels[9,2] <- -86.4
nLabels[9,3] <- 29.25
nLabels[10,2] <- -87.85
nLabels[10,3] <- 29.25
nLabels[11,2] <- -88.85
nLabels[11,3] <- 29.25
nLabels[12,3] <- 30
nLabels[13,2] <- -89.8
nLabels[13,3] <- 28.3
nLabels[14,2] <- -90.8
nLabels[14,3] <- 27.9
nLabels[15,2] <- -91.8
nLabels[15,3] <- 27.9
nLabels[16,2] <- -92.8
nLabels[16,3] <- 27.9
nLabels[17,2] <- -93.8
nLabels[17,3] <- 27.9
nLabels[18,2] <- -94.75
nLabels[18,3] <- 27.9
nLabels[20,2] <- -96.3
nLabels[20,3] <- 27.35
nLabels[21,2] <- -96.3

### Map
# library(htmlwidgets)
# library(webshot)
#webshot::install_phantomjs()


# Create a color palette for the map:
mypalette <- colorNumeric( palette="viridis", domain=gridShrimp@data$Responses, na.color="transparent", reverse=TRUE)

m <- leaflet(gridShrimp, padding=25) %>% ## added padding for whitespace
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "ShadedRelief") %>% 
 # addEsriBasemapLayer(esriBasemapLayers$ShadedRelief, autoLabels = TRUE, group="ShadedRelief") %>% 
  #addTiles()  %>%
  setView( lat=27.5, lng=-89.5 , zoom=6) %>%
  addPolygons( fillColor = ~mypalette(Responses),  fillOpacity = 0.5,
               stroke=TRUE, weight=1, color="#6E6E6E") %>% 
  addLegend("bottomright", pal = mypalette, values = ~Responses,
            title = "Number of responses",
            #labFormat = labelFormat(prefix = "$"),
            opacity = 0.5, position=("bottomleft")
  ) %>% 
  addScaleBar(position="bottomleft", options = scaleBarOptions(imperial=TRUE, metric=FALSE))  %>% 
  removeMapJunk( "zoomControl") ### remove zoom control for export

m

## save html to png
saveWidget(m, "Map Plots/ResponsePlot.html", selfcontained = FALSE)
# webshot("ResponsePlot.html", file = "ResponsePlot.png",
#         cliprect = "viewport") Doesn't work correctly with padding, use below
webshot("Map Plots/ResponsePlot.html", file = "Map Plots/ResponsePlot.png")
############## 5: Sentiment 
#### This section will produce two maps: 1) a map of the sentiment analysis using the Bing lexicon library
####  and 2) a manual analysis 

### Thesed data were combined with the scamp (df) dataframe as prepoplulated earlier in this script.
### Section 5.1: Load required libraries

### Sectio 5.2: read in data

tmp <- df

### Sectio 5.3: map the manual sentiement analysis
## Extract two columns of interest, rename, and remove blanks and code with 'n'
### need to format with an ID column 1:nrows in future, should create a script e.g., download data to get data, add appropriate
### columns, i.e., ID, sentiment, manual Sentiment, Abundance and provide to staff as necessary.  

tmp2Out2 <- cbind(tmp,out2)
NAMES <- colnames(tmp2Out2)
NAMES2 <- make.names(NAMES)
names(tmp2Out2) <- NAMES2
tmp2Out2$Other <- NULL
colnames(tmp2Out2)[9] <- "Abundance"
colnames(tmp2Out2)[8] <- "Sentiment"

### Calculate sentiment for each grid
Xout_list <- list()

for (i in 1:21) {
  var_name <- paste0("X", i)
  if (var_name %in% names(tmp2Out2)) {
    tmpWide <- data.frame(Area = -9999, Negative = -9999,Neutral = -9999,Positive = -9999)
    X <- subset(tmp2Out2, tmp2Out2[[var_name]] == 1)
    X <- select(X, Abundance, Sentiment, ID, all_of(var_name))
    Xwide <- dcast(X, var_name ~ Sentiment, fun.aggregate = length, value.var = var_name)
    names(Xwide)[names(Xwide) == "-1"] <- "Negative"
    names(Xwide)[names(Xwide) == "0"]  <- "Neutral"
    names(Xwide)[names(Xwide) == "1"]  <- "Positive"
    colnames(Xwide)[1] <- "Area"
    Xwide <- merge(tmpWide, Xwide, all = TRUE)
    Xwide <- subset(Xwide, Area != -9999)
    Xwide$Area <- i
    Xout_list[[i]] <- Xwide
  } else {
    print(paste(var_name, "does not exist."))
  }
}
#### automate the rbind
Xout <- do.call(rbind, Xout_list)


### Replace na's with 0.  In this case Na's are actual zeros
Xout[is.na(Xout)] <- 0

colnames(Xout) <- c("ID", "Negative", "Neutral", "Positive")
write.csv(Xout, "SampleArea.csv", row.names=FALSE)
### Now merge with Centroids


### start with shrimp example
library(sp)
#load("gridShrimp.RData")
pieGrid <- gridShrimp

centroids <- data.frame(coordinates(pieGrid))
centroids$ID <- pieGrid@data$ID
colnames(centroids) <- c("x", "y", "ID")
centroidsPie <- merge(centroids, Xout, by="ID")

#library(doBy)
centroidsPie2 <- summaryBy(x + y ~ ID, data=centroidsPie, FUN=mean, keep.names=TRUE, id=c("Negative", "Neutral", "Positive"))

centroidsPie3 <- centroidsPie2
centroidsPie3$id <- as.numeric(as.character(centroidsPie2$ID))
centroidsPie3 <- arrange(centroidsPie3, (id))

centroidsPie3[5,2] <- -83.64990
centroidsPie3[5,3] <- 27.54724
centroidsPie3[6,2] <- - 83.89160
centroidsPie3[6,3] <- 28.55558
centroidsPie3[9,2] <- - 86.48438
centroidsPie3[9,3] <- 29.85970
centroidsPie3[10,2] <- -87.40723
centroidsPie3[10,3] <- 29.89781
#centroidsPie3[12,2] <- -90.7
# nLabels[12,3] <- 28.7
# nLabels[12,2] <- -90.7
centroidsPie3[12,3] <- 29.87
# centroidsPie3[14,2] <- -96.9
# centroidsPie3[14,3] <- 26.5
centroidsPie2 <- centroidsPie3
### check for duplicates, scale size of pie to sample size, select colors
#write.csv(centroidsPie2, "centroidsPie2.csv", row.names=FALSE)
#### nLabels

nLabels <- centroidsPie2

nLabels$n <- rowSums(nLabels[,4:6])

write.csv(nLabels, "nlabels.csv", row.names=FALSE)

colors <- c("#fc8d59", "#ffffbf", "#99d594") # from https://colorbrewer2.org/#type=diverging&scheme=Spectral&n=3
m <- leaflet(pieGrid, padding=25) %>% 
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "ShadedRelief") %>% 
  #addEsriBasemapLayer(esriBasemapLayers$ShadedRelief, autoLabels = FALSE, group="ShadedRelief") %>%
  
  setView( lat=27, lng=-89 , zoom=6) %>%
  addPolygons(data=pieGrid,stroke=TRUE, weight=1, color="#6E6E6E") %>% 
  
  ### add pie chart
  addMinicharts(
    centroidsPie2$x, centroidsPie2$y,
    type = "pie",
    chartdata = centroidsPie2[, c("Negative", "Neutral", "Positive")], 
    colorPalette = colors,
    width = 30, transitionTime = 0
  ) %>% 
  addScaleBar(position="bottomleft", options = scaleBarOptions(imperial=TRUE, metric=FALSE))%>% 
  addMouseCoordinates() %>% 
  
  addLabelOnlyMarkers(
    lng = nLabels[,2] , lat = nLabels[,3] ,
    label = ~paste( nLabels[,8]),
    #label = ~paste( nLabels[,7]),
    #label = ~paste("n = ", nLabels[,8]),
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>% 
  addLabelOnlyMarkers(
    lng = -97.25 , lat = 20.5 ,
    label = ~paste("Number in each grid indicates sample size"),
    #label = ~paste("n = ", nLabels[,8]),
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>% 
  removeMapJunk( "zoomControl") ### remove zoom control for export
m

## save html to png
saveWidget(m, "Map Plots/Sentiment.html", selfcontained = FALSE)
webshot("Map Plots/Sentiment.html", file = "Map Plots/Sentiment.png")


######################################## Manual Sentiment related to abundance ###########

### subset to abundance only
### this should have fewer observations than previous dataset
tmp2Out2 <- subset(tmp2Out2, Abundance=="y")

## fix inconsistent name in this spreadsheet
colnames(tmp2Out2)[colnames(tmp2Out2) == "Final..Stock.Condition"] <- "Final.Stock.Condition.Sentiment"


### create a template dataframe
Xout_list <- list()

for (i in 1:21) {
  var_name <- paste0("X", i)
  if (var_name %in% names(tmp2Out2)& sum(tmp2Out2[[var_name]] > 0)) {
    tmpWide <- data.frame(Area = -9999, Negative = -9999,Neutral = -9999,Positive = -9999)
    X <- subset(tmp2Out2, tmp2Out2[[var_name]] == 1)
    X <- select(X, Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, all_of(var_name))
    X$Final.Stock.Condition.Sentiment <- factor(X$Final.Stock.Condition.Sentiment,levels = c(-1, 0, 1))
    Xwide <- dcast(X, var_name ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = var_name)
    names(Xwide)[names(Xwide) == "-1"] <- "Negative"
    names(Xwide)[names(Xwide) == "0"]  <- "Neutral"
    names(Xwide)[names(Xwide) == "1"]  <- "Positive"
    colnames(Xwide)[1] <- "Area"
    Xwide <- merge(tmpWide, Xwide, all = TRUE)
    Xwide <- subset(Xwide, Area != -9999)
    Xwide$Area <- i
    Xout_list[[i]] <- Xwide
  } else {
    print(paste(var_name, "does not exist."))
  }
}
#### automate the rbind
Xout <- do.call(rbind, Xout_list)
Xout <- Xout[,c("Area", "Negative", "Neutral", "Positive")]
### Replace na's with 0.  In this case Na's are actual zeros
Xout[is.na(Xout)] <- 0

colnames(Xout) <- c("ID", "Negative", "Neutral", "Positive")
### Now merge with Centroids

write.csv(Xout, "SampleAreaAbundance.csv", row.names=FALSE)


#load("gridShrimp.RData")
pieGrid <- gridShrimp

centroids <- data.frame(coordinates(pieGrid))
centroids$ID <- pieGrid@data$ID
colnames(centroids) <- c("x", "y", "ID")
centroidsPie <- merge(centroids, Xout, by="ID")

#library(doBy)
centroidsPie2 <- summaryBy(x + y ~ ID, data=centroidsPie, FUN=mean, keep.names=TRUE, id=c("Negative", "Neutral", "Positive"))
centroidsPie3 <- centroidsPie2
centroidsPie3$id <- as.numeric(as.character(centroidsPie2$ID))
centroidsPie3 <- arrange(centroidsPie3, (id))

centroidsPie3[5,2] <- -83.64990
centroidsPie3[5,3] <- 27.54724
centroidsPie3[6,2] <- - 83.89160
centroidsPie3[6,3] <- 28.55558
centroidsPie3[9,2] <- - 86.48438
centroidsPie3[9,3] <- 29.85970
centroidsPie3[10,2] <- -87.40723
centroidsPie3[10,3] <- 29.89781
centroidsPie3[12,3] <- 29.87
centroidsPie2 <- centroidsPie3

nLabels <- centroidsPie2

nLabels$n <- rowSums(nLabels[,4:6])

write.csv(nLabels, "nlabels.csv", row.names=FALSE)


colors <- c("#fc8d59", "#ffffbf", "#99d594") # from https://colorbrewer2.org/#type=diverging&scheme=Spectral&n=3
m <- leaflet(pieGrid, padding=25) %>% 
  addProviderTiles(providers$Esri.WorldShadedRelief, group = "ShadedRelief") %>% 
  #addEsriBasemapLayer(esriBasemapLayers$ShadedRelief, autoLabels = FALSE, group="ShadedRelief") %>%
  
  setView( lat=27, lng=-89 , zoom=6) %>%
  addPolygons(data=pieGrid,stroke=TRUE, weight=1, color="#6E6E6E") %>% 
  
  ### add pie chart
  addMinicharts(
    centroidsPie2$x, centroidsPie2$y,
    type = "pie",
    chartdata = centroidsPie2[, c("Negative", "Neutral", "Positive")], 
    colorPalette = colors,
    width = 30, transitionTime = 0
  ) %>% 
  addScaleBar(position="bottomleft", options = scaleBarOptions(imperial=TRUE, metric=FALSE))%>% 
  addMouseCoordinates() %>% 
  
  addLabelOnlyMarkers(
    lng = nLabels[,2] , lat = nLabels[,3] ,
    label = ~paste( nLabels[,8]),
    #label = ~paste( nLabels[,7]),
    #label = ~paste("n = ", nLabels[,8]),
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>% 
  addLabelOnlyMarkers(
    lng = -97.25 , lat = 20.5 ,
    label = ~paste("Number in each grid indicates sample size"),
    #label = ~paste("n = ", nLabels[,8]),
    labelOptions = labelOptions(noHide = T, textOnly = TRUE)) %>% 
  removeMapJunk( "zoomControl") %>% ### remove zoom control for export
  addLogo("North.png", position = "bottomright", alpha = 0.3)
m

## save html to png
saveWidget(m, "Map Plots/SentimentAbundance.html", selfcontained = FALSE)
webshot("Map Plots/SentimentAbundance.html", file = "Map Plots/SentimentAbundance.png")


###########################################################################################
###  Keep for documentation
session.Info <- sessionInfo()
save(session.Info, file="session.Info.RData")
save.image("redsbaoo=.RData")
