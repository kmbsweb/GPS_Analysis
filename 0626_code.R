install.packages("trackeR")
library(maptools)
library(ggplot2)
library(rgdal)
library(trackeR)

field <- dir(pattern = "\\.tcx$", ".")

GPSData <- NULL
for(i in field){
  print(paste0("...", i, "を処理しています。"))
  walkDF <- readContainer(i,timezone="GMT")
  WalkDFF <- fortify(walkDF)
  WalkDFF <- subset(WalkDFF,select=c("Index","latitude","longitude","altitude","speed"))
  nn <- nrow(WalkDFF)
  DAT <- data.frame(matrix(i, nrow = nn, ncol = 1))
  WalkDFF <- cbind(WalkDFF,DAT)
  GPSData <- rbind(GPSData,WalkDFF)
}

save(GPSData, file = "GPSData.rda")
load("GPSData.rda")

##Reference
##https://www.omnicalculator.com/other/azimuth
##https://github.com/cran/maptools/blob/5a21425197adf1e2e5b4cf1c98a6fb57368ef56e/R/azimuth.R


##calculate direction
direction <- as.matrix(cbind(GPSData$longitude,GPSData$latitude))
direction <- trackAzimuth(direction)
direction <- as.data.frame(direction)
GPSData <- GPSData[-1,]
GPSData <- cbind(GPSData,direction)
View(GPSData)

##remove "na"
library(tidyr)
dropna_GPS <- GPSData %>% drop_na()

library(fancycut)
dropna_GPS$compas <- fancycut(x = dropna_GPS$direction,
                             intervals = c("[-180,-135)", "[-135,-45)", "[-45,45)","[45,135)","[135, 180]"),
                             buckets = c("South", "West", "North","East","South"),
                             unmatched.bucket = "範囲外")
summary(dropna_GPS$compas)

write.csv(dropna_GPS,"dropna_GPS.csv")

##Wakayama map


library(sp)
library(rgeos)
library(maptools)
library(spatstat)
library(rgdal)

setwd("C:/Users/mkeig/Desktop/shape")
##spatial line data Frame
railway <- rgdal::readOGR(".","railways")
points <- rgdal::readOGR(".","points")
class(points)
AA <- as.linnet(railway)
class(AA)

##spatial point data frame
#xc = c(1.2,1.5,2.5)
#yc = c(1.5,2.2,1.6)
#Spoints = SpatialPoints(cbind(xc, yc))
#plot(Spoints)
##get the point on line
pp <- snapPointsToLines(points, railway)
plot(pp,col="red")
BB <- as.ppp(pp)


##create "lpp" "ppx" class
##point on line
##BB[1] "linnet" "list" 
##AA[2] "ppp"
X <- lpp(BB, AA)
class(X)
##[1] "lpp" "ppx"
plot(X)

##smoothing kernel on network
dna <- density(unmark(X),0.008)
plot(AA,col="#c8c8cb",main="Kernel estimate for the points")
plot(dna,style="width", adjust=0.9, add=TRUE)
library(GISTools)  
map.scale(xc = 135.45, yc=34.63, len=0.05, units = "km",subdiv = 2, 
          sfcol = "black", ndivs = 2)