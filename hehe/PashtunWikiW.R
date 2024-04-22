# 2 Aug 2010, generate quarterly distributions of AfgWiki violent events according to Pashtun borders
#		each with 2 categories: ISAF/friend & insurgents/enemy
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

# load Pashtun shapefile
borderShp <- readShapePoly(paste(dataDir, "../Analysis/Pashtun/Pashtun.shp", sep=""), 
				proj4string=CRS("+proj=utm +zone=41 +datum=WGS84"))
#names(borderShp)


# overlay events on shapefile and record 1 or 0 if it is within the polygon of interest
allCoords <- subset(afgEvents, select=c(utm_x, utm_y))
coordinates(allCoords) <- ~utm_x+utm_y
brderPoly <- overlay(allCoords, borderShp)
brderPoly[is.na(brderPoly)] <- 0		# replace NAs with 0s
eventsBrder<-cbind(afgEvents, brderPoly)
names(eventsBrder)
summary(eventsBrder$brderPoly)

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

commonYlim <- 6000
clrs<-c("#267300FF", "#E64C00FF")
# AfgPashtun
# simple graphs with R: http://www.harding.edu/fmccown/R/
qAfgPashtun <- cbind(qtrLabels$Var1,
				quarterAgg(eventsBrder[eventsBrder$affiliatio=="FRIEND" & eventsBrder$brderPoly==1, ], numQuarters), 
				quarterAgg(eventsBrder[eventsBrder$affiliatio=="ENEMY" & eventsBrder$brderPoly==1, ], numQuarters))
colnames(qAfgPashtun) <- c("Label", "AfgBrderISAF", "AfgBrderInsurg")
write.csv(qAfgPashtun,  paste(outDir, "qtrWikiPashtun.csv", sep=""), row.names=FALSE)
pdf(paste(outDir, "qtrWikiPashtun.pdf", sep=""), height=6, width=10)
barplot(t(as.matrix(qAfgPashtun[,2:3])), main="Violence within Pashtun dominant areas", cex.main=1.5, cex.lab=1.3, cex=1.2, cex.axis=1.2, ylab="Total number of events", ylim=c(0, commonYlim), beside=F, col=clrs, axes=F)
lines(x=c(0,29), y=c(0,0))	# add line along bottom
# Place the legend at the top-left corner with no frame using rainbow colors
# reverse legend order to match stack order
legend(x=0, y=commonYlim-10, c("Insurgents", "ISAF-ANF"), cex=1.3, bty="n", fill=c(clrs[2], clrs[1]))
axis(2)
axis(1, tick=F, lab=F)
# axp = vector of the form c(x1, x2, n) giving the coordinates of the extreme tick marks and the number of intervals between tick-marks 
text(axTicks(1, axp=c(1,28.5,numQuarters-1)), par("usr")[3] - commonYlim/40, srt=45, adj=1, labels=qAfgPashtun$Label, xpd=T, cex=0.9)
dev.off()

# AfgNotPashtun
qAfgNotPashtun <- cbind(qtrLabels$Var1,
				quarterAgg(eventsBrder[eventsBrder$affiliatio=="FRIEND" & eventsBrder$brderPoly==0, ], numQuarters), 
				quarterAgg(eventsBrder[eventsBrder$affiliatio=="ENEMY" & eventsBrder$brderPoly==0, ], numQuarters))
colnames(qAfgNotPashtun) <- c("Label", "AfgNotPashtunISAF", "AfgNotPashtunInsurg")
write.csv(qAfgNotPashtun,  paste(outDir, "qtrWikiNotPashtun.csv", sep=""), row.names=FALSE)
pdf(paste(outDir, "qtrWikiNotPashtun.pdf", sep=""), height=6, width=10)
barplot(t(as.matrix(qAfgNotPashtun[,2:3])), main="Violence outside Pashtun dominant areas", cex.main=1.5, cex.lab=1.3, cex=1.2, cex.axis=1.2, ylab="Total number of events", ylim=c(0, commonYlim), beside=F, col=clrs, axes=F)
lines(x=c(0,29), y=c(0,0))	# add line along bottom
# Place the legend at the top-left corner with no frame using rainbow colors
# reverse legend order to match stack order
legend(x=0, y=commonYlim-10, c("Insurgents", "ISAF-ANF"), cex=1.3, bty="n", fill=c(clrs[2], clrs[1]))
axis(2)
axis(1, tick=F, lab=F)
# axp = vector of the form c(x1, x2, n) giving the coordinates of the extreme tick marks and the number of intervals between tick-marks 
text(axTicks(1, axp=c(1,28.5,numQuarters-1)), par("usr")[3] - commonYlim/40, srt=45, adj=1, labels=qAfgNotPashtun$Label, xpd=T, cex=0.9)
dev.off()

