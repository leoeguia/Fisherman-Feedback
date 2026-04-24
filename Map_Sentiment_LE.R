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
library(leaflet.esri) #Package no longer exists
library(leaflet.extras)
library(doBy)
library(leafem)
library(leaflet.minicharts)
library(mapview) 
library(sf)
library(sp)
library(doBy)
library(readxl)


### 2) import 
#df <- read.csv("rsCommentsJF.csv",header=T, stringsAsFactors = FALSE)
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
out4$nums <- data.frame(parse_number(out4$Location))
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
saveWidget(m, "ResponsePlot.html", selfcontained = FALSE)
# webshot("ResponsePlot.html", file = "ResponsePlot.png",
#         cliprect = "viewport") Doesn't work correctly with padding, use below
webshot("ResponsePlot.html", file = "ResponsePlot.png")
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
if ("X1" %in% names(tmp2Out2)) {
  #if ("X1" %in% names(tmp2Out2) & sum(tmp2Out2$X1 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999)
  X1 <- subset(tmp2Out2,X1==1)
  X1 <- select(X1,  Abundance, Sentiment, ID, X1)
  X1wide <- dcast(X1, X1 ~ Sentiment, fun.aggregate = length, value.var = "X1")
  names(X1wide)[names(X1wide) == "-1"] <- "Negative"
  names(X1wide)[names(X1wide) == "0"] <- "Neutral"
  names(X1wide)[names(X1wide) == "1"] <- "Positive"
  colnames(X1wide)[1] <- "Area"
  X1wide <- join(tmpWide,X1wide, type="full")
  X1wide <- subset(X1wide, Area!=-9999)
  X1wide$Area <- 1
} else {
  print("Variable does not exist.")
}

if ("X2" %in% names(tmp2Out2)) {
  #if ("X2" %in% names(tmp2Out2) & sum(tmp2Out2$X2 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X2 <- subset(tmp2Out2,X2==1)
  X2 <- select(X2,  Abundance, Sentiment, ID, X2)
  X2wide <- dcast(X2, X2 ~ Sentiment, fun.aggregate = length, value.var = "X2")
  names(X2wide)[names(X2wide) == "-1"] <- "Negative"
  names(X2wide)[names(X2wide) == "0"] <- "Neutral"
  names(X2wide)[names(X2wide) == "1"] <- "Positive"
  colnames(X2wide)[1] <- "Area"
  X2wide <- join(tmpWide,X2wide, type="full")
  X2wide <- subset(X2wide, Area!=-9999)
  X2wide$Area <- 2
} else {
  print("Variable does not exist.")
}

if ("X3" %in% names(tmp2Out2)) {
  #if ("X3" %in% names(tmp2Out2) & sum(tmp2Out2$X3 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X3 <- subset(tmp2Out2,X3==1)
  X3 <- select(X3, Abundance, Sentiment, ID, X3)
  X3wide <- dcast(X3, X3 ~ Sentiment, fun.aggregate = length, value.var = "X3")
  names(X3wide)[names(X3wide) == "-1"] <- "Negative"
  names(X3wide)[names(X3wide) == "0"] <- "Neutral"
  names(X3wide)[names(X3wide) == "1"] <- "Positive"
  colnames(X3wide)[1] <- "Area"
  X3wide <- join(tmpWide,X3wide, type="full")
  X3wide <- subset(X3wide, Area!=-9999)
  X3wide$Area <- 3
} else {
  print("Variable does not exist.")
}

if ("X4" %in% names(tmp2Out2)) {
  #if ("X4" %in% names(tmp2Out2) & sum(tmp2Out2$X4 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X4 <- subset(tmp2Out2,X4==1)
  X4 <- select(X4, Abundance,  Sentiment, ID, X4)
  X4wide <- dcast(X4, X4 ~ Sentiment, fun.aggregate = length, value.var = "X4")
  names(X4wide)[names(X4wide) == "-1"] <- "Negative"
  names(X4wide)[names(X4wide) == "0"] <- "Neutral"
  names(X4wide)[names(X4wide) == "1"] <- "Positive"
  colnames(X4wide)[1] <- "Area"
  X4wide <- join(tmpWide,X4wide, type="full")
  X4wide <- subset(X4wide, Area!=-9999)
  X4wide$Area <- 4
} else {
  print("Variable does not exist.")
}

if ("X5" %in% names(tmp2Out2)) {
  #if ("X5" %in% names(tmp2Out2) & sum(tmp2Out2$X5 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X5 <- subset(tmp2Out2,X5==1)
  X5 <- select(X5,  Abundance,Sentiment, Sentiment, ID, X5)
  X5wide <- dcast(X5, X5 ~ Sentiment, fun.aggregate = length, value.var = "X5")
  names(X5wide)[names(X5wide) == "-1"] <- "Negative"
  names(X5wide)[names(X5wide) == "0"] <- "Neutral"
  names(X5wide)[names(X5wide) == "1"] <- "Positive"
  colnames(X5wide)[1] <- "Area"
  X5wide <- join(tmpWide,X5wide, type="full")
  X5wide <- subset(X5wide, Area!=-9999)
  X5wide$Area <- 5
} else {
  print("Variable does not exist.")
}

if ("X6" %in% names(tmp2Out2)) {
  #if ("X6" %in% names(tmp2Out2) & sum(tmp2Out2$X6 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X6 <- subset(tmp2Out2,X6==1)
  X6 <- select(X6,  Abundance,Sentiment, Sentiment, ID, X6)
  X6wide <- dcast(X6, X6 ~ Sentiment, fun.aggregate = length, value.var = "X6")
  names(X6wide)[names(X6wide) == "-1"] <- "Negative"
  names(X6wide)[names(X6wide) == "0"] <- "Neutral"
  names(X6wide)[names(X6wide) == "1"] <- "Positive"
  colnames(X6wide)[1] <- "Area"
  X6wide <- join(tmpWide,X6wide, type="full")
  X6wide <- subset(X6wide, Area!=-9999)
  X6wide$Area <- 6
} else {
  print("Variable does not exist.")
}


if ("X7" %in% names(tmp2Out2)) {
  #if ("X7" %in% names(tmp2Out2) & sum(tmp2Out2$X7 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X7 <- subset(tmp2Out2,X7==1)
  X7 <- select(X7,  Abundance,Sentiment, Sentiment, ID, X7)
  X7wide <- dcast(X7, X7 ~ Sentiment, fun.aggregate = length, value.var = "X7")
  names(X7wide)[names(X7wide) == "-1"] <- "Negative"
  names(X7wide)[names(X7wide) == "0"] <- "Neutral"
  names(X7wide)[names(X7wide) == "1"] <- "Positive"
  colnames(X7wide)[1] <- "Area"
  X7wide <- join(tmpWide,X7wide, type="full")
  X7wide <- subset(X7wide, Area!=-9999)
  X7wide$Area <- 7
} else {
  print("Variable does not exist.")
}


if ("X8" %in% names(tmp2Out2)) {
  #if ("X8" %in% names(tmp2Out2) & sum(tmp2Out2$X8 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X8 <- subset(tmp2Out2,X8==1)
  X8 <- select(X8,  Abundance,Sentiment, Sentiment, ID, X8)
  X8wide <- dcast(X8, X8 ~ Sentiment, fun.aggregate = length, value.var = "X8")
  names(X8wide)[names(X8wide) == "-1"] <- "Negative"
  names(X8wide)[names(X8wide) == "0"] <- "Neutral"
  names(X8wide)[names(X8wide) == "1"] <- "Positive"
  colnames(X8wide)[1] <- "Area"
  X8wide <- join(tmpWide,X8wide, type="full")
  X8wide <- subset(X8wide, Area!=-9999)
  X8wide$Area <- 8
} else {
  print("Variable does not exist.")
}

if ("X9" %in% names(tmp2Out2)) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X9 <- subset(tmp2Out2,X9==1)
  X9 <- select(X9,  Abundance,Sentiment, Sentiment, ID, X9)
  X9wide <- dcast(X9, X9 ~ Sentiment, fun.aggregate = length, value.var = "X9")
  names(X9wide)[names(X9wide) == "-1"] <- "Negative"
  names(X9wide)[names(X9wide) == "0"] <- "Neutral"
  names(X9wide)[names(X9wide) == "1"] <- "Positive"
  colnames(X9wide)[1] <- "Area"
  X9wide <- join(tmpWide,X9wide, type="full")
  X9wide <- subset(X9wide, Area!=-9999)
  X9wide$Area <- 9
} else {
  print("Variable does not exist.")
}


if("X10" %in% names(tmp2Out2)) {
  # if ("X10" %in% names(tmp2Out2) & sum(tmp2Out2$X10 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X10 <- subset(tmp2Out2,X10==1)
  
  
  X10 <- select(X10,  Abundance,Sentiment, Sentiment, ID, X10)
  X10wide <- dcast(X10, X10 ~ Sentiment, fun.aggregate = length, value.var = "X10")
  names(X10wide)[names(X10wide) == "-1"] <- "Negative"
  names(X10wide)[names(X10wide) == "0"] <- "Neutral"
  names(X10wide)[names(X10wide) == "1"] <- "Positive"
  colnames(X10wide)[1] <- "Area"
  X10wide <- join(tmpWide,X10wide, type="full")
  X10wide <- subset(X10wide, Area!=-9999)
  X10wide$Area <- 10
} else {
  print("Variable does not exist.")
}


if ("X11" %in% names(tmp2Out2)) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X11 <- subset(tmp2Out2,X11==1)
  X11 <- select(X11,  Abundance,Sentiment, Sentiment, ID, X11)
  X11wide <- dcast(X11, X11 ~ Sentiment, fun.aggregate = length, value.var = "X11")
  names(X11wide)[names(X11wide) == "-1"] <- "Negative"
  names(X11wide)[names(X11wide) == "0"] <- "Neutral"
  names(X11wide)[names(X11wide) == "1"] <- "Positive"
  colnames(X11wide)[1] <- "Area"
  X11wide <- join(tmpWide,X11wide, type="full")
  X11wide <- subset(X11wide, Area!=-9999)
  X11wide$Area <- 11
} else {
  print("Variable does not exist.")
}


if ("X12" %in% names(tmp2Out2)) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X12 <- subset(tmp2Out2,X12==1)
  X12 <- select(X12,  Abundance,Sentiment, Sentiment, ID, X12)
  X12wide <- dcast(X12, X12 ~ Sentiment, fun.aggregate = length, value.var = "X12")
  names(X12wide)[names(X12wide) == "-1"] <- "Negative"
  names(X12wide)[names(X12wide) == "0"] <- "Neutral"
  names(X12wide)[names(X12wide) == "1"] <- "Positive"
  colnames(X12wide)[1] <- "Area"
  X12wide <- join(tmpWide,X12wide, type="full")
  X12wide <- subset(X12wide, Area!=-9999)
  X12wide$Area <- 12
} else {
  print("Variable does not exist.")
}


if ("X13" %in% names(tmp2Out2)) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X13 <- subset(tmp2Out2,X13==1)
  X13 <- select(X13,  Abundance,Sentiment, Sentiment, ID, X13)
  X13wide <- dcast(X13, X13 ~ Sentiment, fun.aggregate = length, value.var = "X13")
  names(X13wide)[names(X13wide) == "-1"] <- "Negative"
  names(X13wide)[names(X13wide) == "0"] <- "Neutral"
  names(X13wide)[names(X13wide) == "1"] <- "Positive"
  colnames(X13wide)[1] <- "Area"
  X13wide <- join(tmpWide,X13wide, type="full")
  X13wide <- subset(X13wide, Area!=-9999)
  X13wide$Area <- 13
} else {
  print("Variable does not exist.")
}


if ("X14" %in% names(tmp2Out2)) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X14 <- subset(tmp2Out2,X14==1)
  X14 <- select(X14,  Abundance,Sentiment, Sentiment, ID, X14)
  X14wide <- dcast(X14, X14 ~ Sentiment, fun.aggregate = length, value.var = "X14")
  names(X14wide)[names(X14wide) == "-1"] <- "Negative"
  names(X14wide)[names(X14wide) == "0"] <- "Neutral"
  names(X14wide)[names(X14wide) == "1"] <- "Positive"
  colnames(X14wide)[1] <- "Area"
  X14wide <- join(tmpWide,X14wide, type="full")
  X14wide <- subset(X14wide, Area!=-9999)
  X14wide$Area <- 14
} else {
  print("Variable does not exist.")
}


if ("X15" %in% names(tmp2Out2)) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X15 <- subset(tmp2Out2,X15==1)
  X15 <- select(X15,  Abundance,Sentiment, Sentiment, ID, X15)
  X15wide <- dcast(X15, X15 ~ Sentiment, fun.aggregate = length, value.var = "X15")
  names(X15wide)[names(X15wide) == "-1"] <- "Negative"
  names(X15wide)[names(X15wide) == "0"] <- "Neutral"
  names(X15wide)[names(X15wide) == "1"] <- "Positive"
  colnames(X15wide)[1] <- "Area"
  X15wide <- join(tmpWide,X15wide, type="full")
  X15wide <- subset(X15wide, Area!=-9999)
  X15wide$Area <- 15
} else {
  print("Variable does not exist.")
}


if ("X16" %in% names(tmp2Out2)) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X16 <- subset(tmp2Out2,X16==1)
  X16 <- select(X16,  Abundance,Sentiment, Sentiment, ID, X16)
  X16wide <- dcast(X16, X16 ~ Sentiment, fun.aggregate = length, value.var = "X16")
  names(X16wide)[names(X16wide) == "-1"] <- "Negative"
  names(X16wide)[names(X16wide) == "0"] <- "Neutral"
  names(X16wide)[names(X16wide) == "1"] <- "Positive"
  colnames(X16wide)[1] <- "Area"
  X16wide <- join(tmpWide,X16wide, type="full")
  X16wide <- subset(X16wide, Area!=-9999)
  X16wide$Area <- 16
} else {
  print("Variable does not exist.")
}


if ("X17" %in% names(tmp2Out2)) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X17 <- subset(tmp2Out2,X17==1)
  X17 <- select(X17,  Abundance,Sentiment, Sentiment, ID, X17)
  X17wide <- dcast(X17, X17 ~ Sentiment, fun.aggregate = length, value.var = "X17")
  names(X17wide)[names(X17wide) == "-1"] <- "Negative"
  names(X17wide)[names(X17wide) == "0"] <- "Neutral"
  names(X17wide)[names(X17wide) == "1"] <- "Positive"
  colnames(X17wide)[1] <- "Area"
  X17wide <- join(tmpWide,X17wide, type="full")
  X17wide <- subset(X17wide, Area!=-9999)
  X17wide$Area <- 17
} else {
  print("Variable does not exist.")
}

#if ("X18" %in% names(tmp2Out2) ) {
if ("X18" %in% names(tmp2Out2) & sum(tmp2Out2$X18 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X18 <- subset(tmp2Out2,X18==1)
  X18 <- select(X18,  Abundance,Sentiment, Sentiment, ID, X18)
  X18wide <- dcast(X18, X18 ~ Sentiment, fun.aggregate = length, value.var = "X18")
  names(X18wide)[names(X18wide) == "-1"] <- "Negative"
  names(X18wide)[names(X18wide) == "0"] <- "Neutral"
  names(X18wide)[names(X18wide) == "1"] <- "Positive"
  colnames(X18wide)[1] <- "Area"
  X18wide <- join(tmpWide,X18wide, type="full")
  X18wide <- subset(X18wide, Area!=-9999)
  X18wide$Area <- 18
} else {
  print("Variable does not exist.")
}

if ("X19" %in% names(tmp2Out2)) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X19 <- subset(tmp2Out2,X19==1)
  X19 <- select(X19,  Abundance,Abundance, Sentiment, ID, X19)
  X19wide <- dcast(X19, X19 ~ Sentiment, fun.aggregate = length, value.var = "X19")
  names(X19wide)[names(X19wide) == "-1"] <- "Negative"
  names(X19wide)[names(X19wide) == "0"] <- "Neutral"
  names(X19wide)[names(X19wide) == "1"] <- "Positive"
  colnames(X19wide)[1] <- "Area"
  X19wide <- join(tmpWide,X19wide, type="full")
  X19wide <- subset(X19wide, Area!=-9999)
  X19wide$Area <- 19
} else {
  print("Variable does not exist.")
}

if ("X20" %in% names(tmp2Out2)) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X20 <- subset(tmp2Out2,X20==1)
  X20 <- select(X20,  Abundance, Sentiment, ID, X20)
  X20wide <- dcast(X20, X20 ~ Sentiment, fun.aggregate = length, value.var = "X20")
  names(X20wide)[names(X20wide) == "-1"] <- "Negative"
  names(X20wide)[names(X20wide) == "0"] <- "Neutral"
  names(X20wide)[names(X20wide) == "1"] <- "Positive"
  colnames(X20wide)[1] <- "Area"
  X20wide <- join(tmpWide,X20wide, type="full")
  X20wide <- subset(X20wide, Area!=-9999)
  X20wide$Area <- 20
} else {
  print("Variable does not exist.")
}


if ("X21" %in% names(tmp2Out2)) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X21 <- subset(tmp2Out2,X21==1)
  X21 <- select(X21,  Abundance, Sentiment, ID, X21)
  X21wide <- dcast(X21, X21 ~ Sentiment, fun.aggregate = length, value.var = "X21")
  names(X21wide)[names(X21wide) == "-1"] <- "Negative"
  names(X21wide)[names(X21wide) == "0"] <- "Neutral"
  names(X21wide)[names(X21wide) == "1"] <- "Positive"
  colnames(X21wide)[1] <- "Area"
  X21wide <- join(tmpWide,X21wide, type="full")
  X21wide <- subset(X21wide, Area!=-9999)
  X21wide$Area <- 21
} else {
  print("Variable does not exist.")
}


#### automate the rbind
Xout <- c()
if (exists("X1wide")) {
  Xout <- rbind(Xout, X1wide )
} else {
  print("Object does not exist.")
}

if (exists("X2wide")) {
  Xout <- rbind(Xout, X2wide )
} else {
  print("Object does not exist.")
}

if (exists("X3wide")) {
  Xout <- rbind(Xout, X3wide )
} else {
  print("Object does not exist.")
}

if (exists("X4wide")) {
  Xout <- rbind(Xout, X4wide )
} else {
  print("Object does not exist.")
}
if (exists("X5wide")) {
  Xout <- rbind(Xout, X5wide )
} else {
  print("Object does not exist.")
}

if (exists("X6wide")) {
  Xout <- rbind(Xout, X6wide )
} else {
  print("Object does not exist.")
}

if (exists("X7wide")) {
  Xout <- rbind(Xout, X7wide )
} else {
  print("Object does not exist.")
}
if (exists("X8wide")) {
  Xout <- rbind(Xout, X8wide )
} else {
  print("Object does not exist.")
}

if (exists("X9wide")) {
  Xout <- rbind(Xout, X9wide )
} else {
  print("Object does not exist.")
}

if (exists("X10wide")) {
  Xout <- rbind(Xout, X10wide )
} else {
  print("Object does not exist.")
}
if (exists("X11wide")) {
  Xout <- rbind(Xout, X11wide )
} else {
  print("Object does not exist.")
}

if (exists("X12wide")) {
  Xout <- rbind(Xout, X12wide )
} else {
  print("Object does not exist.")
}

if (exists("X13wide")) {
  Xout <- rbind(Xout, X13wide )
} else {
  print("Object does not exist.")
}
if (exists("X14wide")) {
  Xout <- rbind(Xout, X14wide )
} else {
  print("Object does not exist.")
}


if (exists("X15wide")) {
  Xout <- rbind(Xout, X15wide )
} else {
  print("Object does not exist.")
}

if (exists("X16wide")) {
  Xout <- rbind(Xout, X16wide )
} else {
  print("Object does not exist.")
}
if (exists("X17wide")) {
  Xout <- rbind(Xout, X17wide )
} else {
  print("Object does not exist.")
}

if (exists("X18wide")) {
  Xout <- rbind(Xout, X18wide )
} else {
  print("Object does not exist.")
}

if (exists("X19wide")) {
  Xout <- rbind(Xout, X19wide )
} else {
  print("Object does not exist.")
}

if (exists("X20wide")) {
  Xout <- rbind(Xout, X20wide )
} else {
  print("Object does not exist.")
}

if (exists("X21wide")) {
  Xout <- rbind(Xout, X21wide )
} else {
  print("Object does not exist.")
}


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
saveWidget(m, "Sentiment.html", selfcontained = FALSE)
webshot("Sentiment.html", file = "Sentiment.png")


######################################## Manual Sentiment related to abundance ###########

### subset to abundance only
### this should have fewer observations than previous dataset
tmp2Out2 <- subset(tmp2Out2, Abundance=="y")

## fix inconsistent name in this spreadsheet
colnames(tmp2Out2)[colnames(tmp2Out2) == "Final..Stock.Condition"] <- "Final.Stock.Condition.Sentiment"


### create a template dataframe
#if ("X1" %in% names(tmp2Out2)) {
if ("X1" %in% names(tmp2Out2) & sum(tmp2Out2$X1 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999)
  X1 <- subset(tmp2Out2,X1==1)
  X1 <- select(X1,  Abundance, Final.Stock.Condition.Sentiment, ID, X1)
  
  X1wide <- dcast(X1, X1 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X1")
  names(X1wide)[names(X1wide) == "-1"] <- "Negative"
  names(X1wide)[names(X1wide) == "0"] <- "Neutral"
  names(X1wide)[names(X1wide) == "1"] <- "Positive"
  colnames(X1wide)[1] <- "Area"
  X1wide <- join(tmpWide,X1wide, type="full")
  X1wide <- subset(X1wide, Area!=-9999)
  X1wide$Area <- 1
} else {
  print("Variable does not exist.")
}



#if ("X2" %in% names(tmp2Out2)) {
if ("X2" %in% names(tmp2Out2) & sum(tmp2Out2$X2 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X2 <- subset(tmp2Out2,X2==1)
  X2 <- select(X2,  Abundance, Final.Stock.Condition.Sentiment, ID, X2)
  X2wide <- dcast(X2, X2 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X2")
  names(X2wide)[names(X2wide) == "-1"] <- "Negative"
  names(X2wide)[names(X2wide) == "0"] <- "Neutral"
  names(X2wide)[names(X2wide) == "1"] <- "Positive"
  colnames(X2wide)[1] <- "Area"
  X2wide <- join(tmpWide,X2wide, type="full")
  X2wide <- subset(X2wide, Area!=-9999)
  X2wide$Area <- 2
} else {
  print("Variable does not exist.")
}

#if ("X3" %in% names(tmp2Out2)) {
if ("X3" %in% names(tmp2Out2) & sum(tmp2Out2$X3 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X3 <- subset(tmp2Out2,X3==1)
  X3 <- select(X3, Abundance, Final.Stock.Condition.Sentiment, ID, X3)
  X3wide <- dcast(X3, X3 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X3")
  names(X3wide)[names(X3wide) == "-1"] <- "Negative"
  names(X3wide)[names(X3wide) == "0"] <- "Neutral"
  names(X3wide)[names(X3wide) == "1"] <- "Positive"
  colnames(X3wide)[1] <- "Area"
  X3wide <- join(tmpWide,X3wide, type="full")
  X3wide <- subset(X3wide, Area!=-9999)
  X3wide$Area <- 3
} else {
  print("Variable does not exist.")
}

#if ("X4" %in% names(tmp2Out2)) {
if ("X4" %in% names(tmp2Out2) & sum(tmp2Out2$X4 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X4 <- subset(tmp2Out2,X4==1)
  X4 <- select(X4, Abundance,  Final.Stock.Condition.Sentiment, ID, X4)
  X4wide <- dcast(X4, X4 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X4")
  names(X4wide)[names(X4wide) == "-1"] <- "Negative"
  names(X4wide)[names(X4wide) == "0"] <- "Neutral"
  names(X4wide)[names(X4wide) == "1"] <- "Positive"
  colnames(X4wide)[1] <- "Area"
  X4wide <- join(tmpWide,X4wide, type="full")
  X4wide <- subset(X4wide, Area!=-9999)
  X4wide$Area <- 4
} else {
  print("Variable does not exist.")
}

#if ("X5" %in% names(tmp2Out2)) {
if ("X5" %in% names(tmp2Out2) & sum(tmp2Out2$X5 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X5 <- subset(tmp2Out2,X5==1)
  X5 <- select(X5,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X5)
  X5wide <- dcast(X5, X5 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X5")
  names(X5wide)[names(X5wide) == "-1"] <- "Negative"
  names(X5wide)[names(X5wide) == "0"] <- "Neutral"
  names(X5wide)[names(X5wide) == "1"] <- "Positive"
  colnames(X5wide)[1] <- "Area"
  X5wide <- join(tmpWide,X5wide, type="full")
  X5wide <- subset(X5wide, Area!=-9999)
  X5wide$Area <- 5
} else {
  print("Variable does not exist.")
}

#if ("X6" %in% names(tmp2Out2)) {
if ("X6" %in% names(tmp2Out2) & sum(tmp2Out2$X6 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X6 <- subset(tmp2Out2,X6==1)
  X6 <- select(X6,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X6)
  X6wide <- dcast(X6, X6 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X6")
  names(X6wide)[names(X6wide) == "-1"] <- "Negative"
  names(X6wide)[names(X6wide) == "0"] <- "Neutral"
  names(X6wide)[names(X6wide) == "1"] <- "Positive"
  colnames(X6wide)[1] <- "Area"
  X6wide <- join(tmpWide,X6wide, type="full")
  X6wide <- subset(X6wide, Area!=-9999)
  X6wide$Area <- 6
} else {
  print("Variable does not exist.")
}


#if ("X7" %in% names(tmp2Out2)) {
if ("X7" %in% names(tmp2Out2) & sum(tmp2Out2$X7 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X7 <- subset(tmp2Out2,X7==1)
  X7 <- select(X7,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X7)
  X7wide <- dcast(X7, X7 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X7")
  names(X7wide)[names(X7wide) == "-1"] <- "Negative"
  names(X7wide)[names(X7wide) == "0"] <- "Neutral"
  names(X7wide)[names(X7wide) == "1"] <- "Positive"
  colnames(X7wide)[1] <- "Area"
  X7wide <- join(tmpWide,X7wide, type="full")
  X7wide <- subset(X7wide, Area!=-9999)
  X7wide$Area <- 7
} else {
  print("Variable does not exist.")
}


#if ("X8" %in% names(tmp2Out2)) {
if ("X8" %in% names(tmp2Out2) & sum(tmp2Out2$X8 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X8 <- subset(tmp2Out2,X8==1)
  X8 <- select(X8,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X8)
  X8wide <- dcast(X8, X8 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X8")
  names(X8wide)[names(X8wide) == "-1"] <- "Negative"
  names(X8wide)[names(X8wide) == "0"] <- "Neutral"
  names(X8wide)[names(X8wide) == "1"] <- "Positive"
  colnames(X8wide)[1] <- "Area"
  X8wide <- join(tmpWide,X8wide, type="full")
  X8wide <- subset(X8wide, Area!=-9999)
  X8wide$Area <- 8
} else {
  print("Variable does not exist.")
}

#if ("X9" %in% names(tmp2Out2)) {
if ("X9" %in% names(tmp2Out2) & sum(tmp2Out2$X9 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X9 <- subset(tmp2Out2,X9==1)
  X9 <- select(X9,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X9)
  X9wide <- dcast(X9, X9 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X9")
  names(X9wide)[names(X9wide) == "-1"] <- "Negative"
  names(X9wide)[names(X9wide) == "0"] <- "Neutral"
  names(X9wide)[names(X9wide) == "1"] <- "Positive"
  colnames(X9wide)[1] <- "Area"
  X9wide <- join(tmpWide,X9wide, type="full")
  X9wide <- subset(X9wide, Area!=-9999)
  X9wide$Area <- 9
} else {
  print("Variable does not exist.")
}


#if ("X10" %in% names(tmp2Out2)) {
if ("X10" %in% names(tmp2Out2) & sum(tmp2Out2$X10 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X10 <- subset(tmp2Out2,X10==1)
  X10 <- select(X10,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X10)
  X10wide <- dcast(X10, X10 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X10")
  names(X10wide)[names(X10wide) == "-1"] <- "Negative"
  names(X10wide)[names(X10wide) == "0"] <- "Neutral"
  names(X10wide)[names(X10wide) == "1"] <- "Positive"
  colnames(X10wide)[1] <- "Area"
  X10wide <- join(tmpWide,X10wide, type="full")
  X10wide <- subset(X10wide, Area!=-9999)
  X10wide$Area <- 10
} else {
  print("Variable does not exist.")
}


#if ("X11" %in% names(tmp2Out2)) {
if ("X11" %in% names(tmp2Out2) & sum(tmp2Out2$X11 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X11 <- subset(tmp2Out2,X11==1)
  X11 <- select(X11,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X11)
  X11wide <- dcast(X11, X11 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X11")
  names(X11wide)[names(X11wide) == "-1"] <- "Negative"
  names(X11wide)[names(X11wide) == "0"] <- "Neutral"
  names(X11wide)[names(X11wide) == "1"] <- "Positive"
  colnames(X11wide)[1] <- "Area"
  X11wide <- join(tmpWide,X11wide, type="full")
  X11wide <- subset(X11wide, Area!=-9999)
  X11wide$Area <- 11
} else {
  print("Variable does not exist.")
}


#if ("X12" %in% names(tmp2Out2)) {
if ("X12" %in% names(tmp2Out2) & sum(tmp2Out2$X12 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X12 <- subset(tmp2Out2,X12==1)
  X12 <- select(X12,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X12)
  X12wide <- dcast(X12, X12 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X12")
  names(X12wide)[names(X12wide) == "-1"] <- "Negative"
  names(X12wide)[names(X12wide) == "0"] <- "Neutral"
  names(X12wide)[names(X12wide) == "1"] <- "Positive"
  colnames(X12wide)[1] <- "Area"
  X12wide <- join(tmpWide,X12wide, type="full")
  X12wide <- subset(X12wide, Area!=-9999)
  X12wide$Area <- 12
} else {
  print("Variable does not exist.")
}


#if ("X13" %in% names(tmp2Out2)) {
if ("X13" %in% names(tmp2Out2) & sum(tmp2Out2$X13 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X13 <- subset(tmp2Out2,X13==1)
  X13 <- select(X13,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X13)
  X13wide <- dcast(X13, X13 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X13")
  names(X13wide)[names(X13wide) == "-1"] <- "Negative"
  names(X13wide)[names(X13wide) == "0"] <- "Neutral"
  names(X13wide)[names(X13wide) == "1"] <- "Positive"
  colnames(X13wide)[1] <- "Area"
  X13wide <- join(tmpWide,X13wide, type="full")
  X13wide <- subset(X13wide, Area!=-9999)
  X13wide$Area <- 13
} else {
  print("Variable does not exist.")
}


#if ("X14" %in% names(tmp2Out2)) {
if ("X14" %in% names(tmp2Out2) & sum(tmp2Out2$X14 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X14 <- subset(tmp2Out2,X14==1)
  X14 <- select(X14,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X14)
  X14wide <- dcast(X14, X14 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X14")
  names(X14wide)[names(X14wide) == "-1"] <- "Negative"
  names(X14wide)[names(X14wide) == "0"] <- "Neutral"
  names(X14wide)[names(X14wide) == "1"] <- "Positive"
  colnames(X14wide)[1] <- "Area"
  X14wide <- join(tmpWide,X14wide, type="full")
  X14wide <- subset(X14wide, Area!=-9999)
  X14wide$Area <- 14
} else {
  print("Variable does not exist.")
}


#if ("X15" %in% names(tmp2Out2)) {
if ("X15" %in% names(tmp2Out2) & sum(tmp2Out2$X15 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X15 <- subset(tmp2Out2,X15==1)
  X15 <- select(X15,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X15)
  X15wide <- dcast(X15, X15 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X15")
  names(X15wide)[names(X15wide) == "-1"] <- "Negative"
  names(X15wide)[names(X15wide) == "0"] <- "Neutral"
  names(X15wide)[names(X15wide) == "1"] <- "Positive"
  colnames(X15wide)[1] <- "Area"
  X15wide <- join(tmpWide,X15wide, type="full")
  X15wide <- subset(X15wide, Area!=-9999)
  X15wide$Area <- 15
} else {
  print("Variable does not exist.")
}


#if ("X16" %in% names(tmp2Out2)) {
if ("X16" %in% names(tmp2Out2) & sum(tmp2Out2$X16 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X16 <- subset(tmp2Out2,X16==1)
  X16 <- select(X16,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X16)
  X16wide <- dcast(X16, X16 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X16")
  names(X16wide)[names(X16wide) == "-1"] <- "Negative"
  names(X16wide)[names(X16wide) == "0"] <- "Neutral"
  names(X16wide)[names(X16wide) == "1"] <- "Positive"
  colnames(X16wide)[1] <- "Area"
  X16wide <- join(tmpWide,X16wide, type="full")
  X16wide <- subset(X16wide, Area!=-9999)
  X16wide$Area <- 16
} else {
  print("Variable does not exist.")
}


#if ("X17" %in% names(tmp2Out2)) {
if ("X17" %in% names(tmp2Out2) & sum(tmp2Out2$X17 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X17 <- subset(tmp2Out2,X17==1)
  X17 <- select(X17,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X17)
  X17wide <- dcast(X17, X17 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X17")
  names(X17wide)[names(X17wide) == "-1"] <- "Negative"
  names(X17wide)[names(X17wide) == "0"] <- "Neutral"
  names(X17wide)[names(X17wide) == "1"] <- "Positive"
  colnames(X17wide)[1] <- "Area"
  X17wide <- join(tmpWide,X17wide, type="full")
  X17wide <- subset(X17wide, Area!=-9999)
  X17wide$Area <- 17
} else {
  print("Variable does not exist.")
}

#if ("X18" %in% names(tmp2Out2)) {
if ("X18" %in% names(tmp2Out2) & sum(tmp2Out2$X18 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X18 <- subset(tmp2Out2,X18==1)
  X18 <- select(X18,  Abundance,Final.Stock.Condition.Sentiment, Sentiment, ID, X18)
  X18wide <- dcast(X18, X18 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X18")
  names(X18wide)[names(X18wide) == "-1"] <- "Negative"
  names(X18wide)[names(X18wide) == "0"] <- "Neutral"
  names(X18wide)[names(X18wide) == "1"] <- "Positive"
  colnames(X18wide)[1] <- "Area"
  X18wide <- join(tmpWide,X18wide, type="full")
  X18wide <- subset(X18wide, Area!=-9999)
  X18wide$Area <- 18
} else {
  print("Variable does not exist.")
}

#if ("X19" %in% names(tmp2Out2)) {
if ("X19" %in% names(tmp2Out2) & sum(tmp2Out2$X19 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X19 <- subset(tmp2Out2,X19==1)
  X19 <- select(X19,  Abundance,Abundance, Final.Stock.Condition.Sentiment, ID, X19)
  X19wide <- dcast(X19, X19 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X19")
  names(X19wide)[names(X19wide) == "-1"] <- "Negative"
  names(X19wide)[names(X19wide) == "0"] <- "Neutral"
  names(X19wide)[names(X19wide) == "1"] <- "Positive"
  colnames(X19wide)[1] <- "Area"
  X19wide <- join(tmpWide,X19wide, type="full")
  X19wide <- subset(X19wide, Area!=-9999)
  X19wide$Area <- 19
} else {
  print("Variable does not exist.")
}

#if ("X20" %in% names(tmp2Out2)) {
if ("X20" %in% names(tmp2Out2) & sum(tmp2Out2$X20 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X20 <- subset(tmp2Out2,X20==1)
  X20 <- select(X20,  Abundance, Final.Stock.Condition.Sentiment, ID, X20)
  X20wide <- dcast(X20, X20 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X20")
  names(X20wide)[names(X20wide) == "-1"] <- "Negative"
  names(X20wide)[names(X20wide) == "0"] <- "Neutral"
  names(X20wide)[names(X20wide) == "1"] <- "Positive"
  colnames(X20wide)[1] <- "Area"
  X20wide <- join(tmpWide,X20wide, type="full")
  X20wide <- subset(X20wide, Area!=-9999)
  X20wide$Area <- 20
} else {
  print("Variable does not exist.")
}


#if ("X21" %in% names(tmp2Out2)) {
if ("X21" %in% names(tmp2Out2) & sum(tmp2Out2$X21 > 0) ) {
  tmpWide <- data.frame(Area=-9999, Negative = -9999, Neutral=-9999, Positive=-9999 )
  X21 <- subset(tmp2Out2,X21==1)
  X21 <- select(X21,  Abundance, Final.Stock.Condition.Sentiment, ID, X21)
  X21wide <- dcast(X21, X21 ~ Final.Stock.Condition.Sentiment, fun.aggregate = length, value.var = "X21")
  names(X21wide)[names(X21wide) == "-1"] <- "Negative"
  names(X21wide)[names(X21wide) == "0"] <- "Neutral"
  names(X21wide)[names(X21wide) == "1"] <- "Positive"
  colnames(X21wide)[1] <- "Area"
  X21wide <- join(tmpWide,X21wide, type="full")
  X21wide <- subset(X21wide, Area!=-9999)
  X21wide$Area <- 21
} else {
  print("Variable does not exist.")
}


#### automate the rbind
Xout <- c()
if (exists("X1wide")) {
  Xout <- rbind(Xout, X1wide )
} else {
  print("Object does not exist.")
}

if (exists("X2wide")) {
  Xout <- rbind(Xout, X2wide )
} else {
  print("Object does not exist.")
}

if (exists("X3wide")) {
  Xout <- rbind(Xout, X3wide )
} else {
  print("Object does not exist.")
}

if (exists("X4wide")) {
  Xout <- rbind(Xout, X4wide )
} else {
  print("Object does not exist.")
}
if (exists("X5wide")) {
  Xout <- rbind(Xout, X5wide )
} else {
  print("Object does not exist.")
}

if (exists("X6wide")) {
  Xout <- rbind(Xout, X6wide )
} else {
  print("Object does not exist.")
}

if (exists("X7wide")) {
  Xout <- rbind(Xout, X7wide )
} else {
  print("Object does not exist.")
}
if (exists("X8wide")) {
  Xout <- rbind(Xout, X8wide )
} else {
  print("Object does not exist.")
}

if (exists("X9wide")) {
  Xout <- rbind(Xout, X9wide )
} else {
  print("Object does not exist.")
}

if (exists("X10wide")) {
  Xout <- rbind(Xout, X10wide )
} else {
  print("Object does not exist.")
}
if (exists("X11wide")) {
  Xout <- rbind(Xout, X11wide )
} else {
  print("Object does not exist.")
}

if (exists("X12wide")) {
  Xout <- rbind(Xout, X12wide )
} else {
  print("Object does not exist.")
}

if (exists("X13wide")) {
  Xout <- rbind(Xout, X13wide )
} else {
  print("Object does not exist.")
}
if (exists("X14wide")) {
  Xout <- rbind(Xout, X14wide )
} else {
  print("Object does not exist.")
}


if (exists("X15wide")) {
  Xout <- rbind(Xout, X15wide )
} else {
  print("Object does not exist.")
}

if (exists("X16wide")) {
  Xout <- rbind(Xout, X16wide )
} else {
  print("Object does not exist.")
}
if (exists("X17wide")) {
  Xout <- rbind(Xout, X17wide )
} else {
  print("Object does not exist.")
}

if (exists("X18wide")) {
  Xout <- rbind(Xout, X18wide )
} else {
  print("Object does not exist.")
}

if (exists("X19wide")) {
  Xout <- rbind(Xout, X19wide )
} else {
  print("Object does not exist.")
}

if (exists("X20wide")) {
  Xout <- rbind(Xout, X20wide )
} else {
  print("Object does not exist.")
}

if (exists("X21wide")) {
  Xout <- rbind(Xout, X21wide )
} else {
  print("Object does not exist.")
}

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
  addLogo("North.png", src = "local",position = "bottomright", alpha = 0.3)
m

## save html to png
saveWidget(m, "SentimentAbundance.html", selfcontained = FALSE)
webshot("SentimentAbundance.html", file = "SentimentAbundance.png")


###########################################################################################
###  Keep for documentation
session.Info <- sessionInfo()
save(session.Info, file="session.Info.RData")
save.image("redsbaoo=.RData")
