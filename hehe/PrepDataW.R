# 20 Aug 2010, merge raw WikiLeaks data to our 'violent' dataset
#			convert lat/lon coordinates to UTM, zone 41N
#
##########################################################################

library(rgdal)		# for spTransform

# INSTRUCTIONS:
#	- download full WikiLeaks data, afg.csv
#	- set the data directory prior to analysis:
dataDir = "C:/???/"

vlntEvents<-read.csv(paste(dataDir, "WikiLeaks/afg_violent.csv", sep=""), as.is = TRUE)
names(vlntEvents)
origData<-read.csv(paste(dataDir, "WikiLeaks/afg.csv", sep=""), header=FALSE, as.is = TRUE)
colnames(origData)<-c("reportkey", "date", "type", "category", "tracking_n", "title", "summary", "region", "attackon", "complex", "reportingu", "unitname", "typeofunit", "FriendlyWo", "FriendlyKi", "HostNatWou", "HostNatKil", "CivWounded", "CivKill", "EnemyWound", "EnemyKille", "EnemyDetai", "MGRS", "lat", "long", "Originator", "UpdatedByG", "CCIR", "sigact", "affiliatio", "dcolor", "classification")
names(origData)
origData$count<-1

allEvents<-merge(vlntEvents, origData, by="reportkey")
names(allEvents)
length(allEvents$reportkey)
sum(allEvents$count)

# convert coords to UTM
SP <- SpatialPoints(cbind(allEvents$long, allEvents$lat), proj4string=CRS("+proj=longlat"))
UTMcoords <- spTransform(SP, CRS("+proj=utm +zone=41 +datum=WGS84"))
coordsDF <- data.frame(UTMcoords)
colnames(coordsDF) <- c("utm_x", "utm_y")
#plot(coordsDF)
allEvents<- cbind(allEvents, coordsDF)
names(allEvents)

# delete a few fields to make file size smaller
write.csv(allEvents[, c(1:9, 19:40)], file=paste(dataDir, "WikiLeaks/afg5_utm41N.csv", sep=""), row.names=F)
