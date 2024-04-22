# 31 Jul 2010, generate quarterly distributions of AfgWiki violent events, urb/rur
#	so 2 bar graphs AfgUrb, AfgRur
#		each with 2 categories: ISAF/friend & insurgents/enemy
#
#	for comparison, area of Afg = 644,000 km2
#		urban area within Afg = 3,000 km2 (0.5%)
#	total ISAF rural:  8011 (87.2), urban: 1174
#	total ins. rural: 35039 (93.7), urban: 2344
#
##########################################################################

library(spdep)		# also loads the 'sp' package with the 'overlay' function

# INSTRUCTIONS: set these directories prior to analysis
dataDir = "C:/???/"
outDir = "C:/???/"

# as.is prevents text from being converted to categorical/factor format
allEvents<-read.csv(paste(dataDir, "WikiLeaks/afg5_utm41N.csv", sep=""), as.is = TRUE)
allEvents$FREQ <- 1		# used for aggregating events

# subset events
afgEvents<-allEvents[allEvents$violent == 1, ]
length(afgEvents$reportkey)
names(afgEvents)

# convert text date to a POSIXlt object
	# %Y = year with century, %y = 2 digit year
dates<-strptime(afgEvents$Date_, "%m/%d/%Y")
startYr<-as.numeric(min(format(dates, "%Y")))
#summary(dates)

# construct unique quarter id & label
afgEvents$qtrLbl <- paste(startYr-1+ceiling(afgEvents$quarter/4), " Q", afgEvents$quarter-((ceiling(afgEvents$quarter/4)-1)*4), sep="")
#numMonths<-max(afgEvents$monthID)
numQuarters<-max(afgEvents$quarter)
qtrLabels<-data.frame(table(afgEvents$qtrLbl))

# load urban shapefile
urbanShp <- readShapePoly(paste(dataDir, "DMSP_nightlights/DMSP_UrbAfgPrj.shp", sep=""), 
				proj4string=CRS("+proj=utm +zone=41 +datum=WGS84"))
#names(urbanShp)

# overlay events on shapefile and record 1 or 0 if it is within the polygon of interest
allCoords <- subset(afgEvents, select=c(utm_x, utm_y))
coordinates(allCoords) <- ~utm_x+utm_y
urbPoly <- overlay(allCoords, urbanShp)
urbPoly[is.na(urbPoly)] <- 0		# replace NAs with 0s
eventsUrb<-cbind(afgEvents, urbPoly)
names(eventsUrb)
summary(eventsUrb$urbPoly)

#*********************************************************************************
# function to aggregate events by quarter
#	requires events$qtrID to be set properly
#	requires events$FREQ to be set properly
# 	returns a data frame with the quarter ID & counts
quarterAgg <- function(events, numQtrs) {
	# aggregate events, making sure not to skip any 'empty' months!
	qCounts <- numeric(numQtrs)
	tmp <- data.frame(aggregate(events$FREQ, list(quarter = events$quarter), sum))
	qCounts[tmp$quarter] <- tmp$x
	qCounts <- data.frame(qCounts)
	colnames(qCounts) <- "counts"
	return(qCounts)
}


# AfgUrb
commonYlim <- 6000
clrs<-c("#267300FF", "#E64C00FF")
# Urban graph/data
qAfgUrb <- cbind(qtrLabels$Var1,
				quarterAgg(eventsUrb[eventsUrb$affiliatio=="FRIEND" & eventsUrb$urbPoly==1, ], numQuarters), 
				quarterAgg(eventsUrb[eventsUrb$affiliatio=="ENEMY" & eventsUrb$urbPoly==1, ], numQuarters))
colnames(qAfgUrb) <- c("Label", "AfgUrbISAF", "AfgUrbInsurg")
write.csv(qAfgUrb,  paste(outDir, "qtrWikiUrb.csv", sep=""), row.names=FALSE)
pdf(paste(outDir, "qtrWikiUrb.pdf", sep=""), height=6, width=10)
barplot(t(as.matrix(qAfgUrb[,2:3])), main="Urban violence by actor", cex.main=1.5, cex.lab=1.3, cex=1.2, cex.axis=1.2, ylab="Total number of events", ylim=c(0, commonYlim), beside=F, col=clrs, axes=F)
lines(x=c(0,29), y=c(0,0))	# add line along bottom
# Place the legend at the top-left corner with no frame using rainbow colors
# reverse legend order to match stack order
legend(x=0, y=commonYlim-10, c("Insurgents", "ISAF-ANF"), cex=1.3, bty="n", fill=c(clrs[2], clrs[1]))
axis(2)
axis(1, tick=F, lab=F)
# axp = vector of the form c(x1, x2, n) giving the coordinates of the extreme tick marks and the number of intervals between tick-marks 
text(axTicks(1, axp=c(1,28.5,numQuarters-1)), par("usr")[3] - commonYlim/40, srt=45, adj=1, labels=qAfgUrb$Label, xpd=T, cex=0.9)
dev.off()

# AfgRur
qAfgRur <- cbind(qtrLabels$Var1,
				quarterAgg(eventsUrb[eventsUrb$affiliatio=="FRIEND" & eventsUrb$urbPoly==0, ], numQuarters), 
				quarterAgg(eventsUrb[eventsUrb$affiliatio=="ENEMY" & eventsUrb$urbPoly==0, ], numQuarters))
colnames(qAfgRur) <- c("Label", "AfgRurISAF", "AfgRurInsurg")
write.csv(qAfgRur,  paste(outDir, "qtrWikiRur.csv", sep=""), row.names=FALSE)
pdf(paste(outDir, "qtrWikiRur.pdf", sep=""), height=6, width=10)
barplot(t(as.matrix(qAfgRur[,2:3])), main="Rural violence by actor", cex.main=1.5, cex.lab=1.3, cex=1.2, cex.axis=1.2, ylab="Total number of events", ylim=c(0, commonYlim), beside=F, col=clrs, axes=F)
lines(x=c(0,29), y=c(0,0))	# add line along bottom
# Place the legend at the top-left corner with no frame using rainbow colors
# reverse legend order to match stack order
legend(x=0, y=commonYlim-10, c("Insurgents", "ISAF-ANF"), cex=1.3, bty="n", fill=c(clrs[2], clrs[1]))
axis(2)
axis(1, tick=F, lab=F)
# axp = vector of the form c(x1, x2, n) giving the coordinates of the extreme tick marks and the number of intervals between tick-marks 
text(axTicks(1, axp=c(1,28.5,numQuarters-1)), par("usr")[3] - commonYlim/40, srt=45, adj=1, labels=qAfgRur$Label, xpd=T, cex=0.9)
dev.off()

