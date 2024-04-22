# 4 Aug 2010, explore quarterly slope trends of Afg Wiki violent events
#		use break of 4 degrees
#		AfgFlat, AfgSteep
#			2 actors for each category
#	total ISAF flat: 7018 (76.4), hilly/steep: 2167
#	total ins. flat: 28092 (75.1), hilly/steep: 9291
#	2009 ISAF flat: 3133 (85.0), hilly/steep: 551
#	2009 ins. flat: 13230 (77.3), hilly/steep: 3890
#
##########################################################################

library(foreign)	# read dbf files

# INSTRUCTIONS: set these directories prior to analysis
dataDir = "C:/???/"
outDir = "C:/???/"

############ load & process FRIEND events
friendEvts<-read.dbf(paste(dataDir, "afg5_friendSlpJoin.dbf", sep=""), as.is = TRUE)
friendEvts$FREQ <- 1
names(friendEvts)
length(friendEvts$reportkey)

# convert text date to a POSIXlt object
	# %Y = year with century, %y = 2 digit year
dates<-strptime(friendEvts$Date_, "%Y-%m-%d")	# ArcMap switched the date format
summary(dates)

# deleted quarter by mistake, so rebuild here
startYr<-as.integer(min(format(dates, "%Y")))
startMth<-1
# construct unique monthly id & label
friendEvts$monthID <- as.integer((as.numeric(format(dates, "%Y"))-startYr)*12 + as.numeric(format(dates, "%m")))
friendEvts$monthLbl <- paste(format(dates, "%b"),format(dates, "%Y"))
#numMonths<-max(friendEvts$monthID)

# construct unique quarter id & label
friendEvts$qtrID <- as.integer(ceiling(friendEvts$monthID / 3))
friendEvts$qtrLbl <- paste(startYr-1+ceiling(friendEvts$qtrID/4), " Q", friendEvts$qtrID-((ceiling(friendEvts$qtrID/4)-1)*4), sep="")
summary(friendEvts$qtrID)

############ load & process ENEMY events
enemyEvts<-read.dbf(paste(dataDir, "afg5_enemySlpJoin.dbf", sep=""), as.is = TRUE)
enemyEvts$FREQ <- 1
names(enemyEvts)
length(enemyEvts$reportkey)

# convert text date to a POSIXlt object
	# %Y = year with century, %y = 2 digit year
dates<-strptime(enemyEvts$Date_, "%Y-%m-%d")	# ArcMap switched the date format
summary(dates)

# deleted quarter by mistake, so rebuild here
startYr<-as.integer(min(format(dates, "%Y")))
startMth<-1
# construct unique monthly id & label
enemyEvts$monthID <- as.integer((as.numeric(format(dates, "%Y"))-startYr)*12 + as.numeric(format(dates, "%m")))
enemyEvts$monthLbl <- paste(format(dates, "%b"),format(dates, "%Y"))
#numMonths<-max(enemyEvts$monthID)

# construct unique quarter id & label
enemyEvts$qtrID <- as.integer(ceiling(enemyEvts$monthID / 3))
enemyEvts$qtrLbl <- paste(startYr-1+ceiling(enemyEvts$qtrID/4), " Q", enemyEvts$qtrID-((ceiling(enemyEvts$qtrID/4)-1)*4), sep="")
summary(enemyEvts$qtrID)

# get qtrLabels from enemy data since there are no friend events for 2004 Q1
qtrLabels<-data.frame(table(enemyEvts$qtrLbl))
numQuarters<-max(enemyEvts$qtrID)

#*********************************************************************************
# function to aggregate events by quarter
#	requires events$qtrID to be set properly
#	requires events$FREQ to be set properly
# 	returns a data frame with the quarter ID & counts
quarterAgg <- function(events, numQtrs) {
	# aggregate events, making sure not to skip any 'empty' months!
	qCounts <- numeric(numQtrs)
	tmp <- data.frame(aggregate(events$FREQ, list(quarter = events$qtrID), sum))
	qCounts[tmp$quarter] <- tmp$x
	qCounts <- data.frame(qCounts)
	colnames(qCounts) <- "counts"
	return(qCounts)
}

############### generate monthly aggregations for each slope category
brk1 <- 4
flatFriend <- friendEvts[friendEvts$ZSTATS_AVG<brk1,]
steepFriend <- friendEvts[friendEvts$ZSTATS_AVG>=brk1,]
length(friendEvts$FREQ)
length(flatFriend$FREQ)+length(steepFriend$FREQ)

flatEnemy <- enemyEvts[enemyEvts$ZSTATS_AVG<brk1,]
steepEnemy <- enemyEvts[enemyEvts$ZSTATS_AVG>=brk1,]
length(enemyEvts$FREQ)
length(flatEnemy$FREQ)+length(steepEnemy$FREQ)


commonYlim <- 6000
clrs<-c("#267300FF", "#E64C00FF")

# Flat
qFlat <- cbind(qtrLabels$Var1, quarterAgg(flatFriend, numQuarters), 
								quarterAgg(flatEnemy, numQuarters))
colnames(qFlat) <- c("Label", "FlatISAF", "FlatInsurg")
write.csv(qFlat,  paste(outDir, "qtrWikiFlat.csv", sep=""), row.names=FALSE)
pdf(paste(outDir, "qtrWikiFlat.pdf", sep=""), height=6, width=10)
barplot(t(as.matrix(qFlat[,2:3])), main="Violence in flat terrain", cex.main=1.5, cex.lab=1.3, cex=1.2, cex.axis=1.2, ylab="Total number of events", ylim=c(0, commonYlim), beside=F, col=clrs, axes=F)
lines(x=c(0,29), y=c(0,0))	# add line along bottom
# Place the legend at the top-left corner with no frame using rainbow colors
# reverse legend order to match stack order
legend(x=0, y=commonYlim-10, c("Insurgents", "ISAF/ANSF"), cex=1.3, bty="n", fill=c(clrs[2], clrs[1]))
axis(2)
axis(1, tick=F, lab=F)
# axp = vector of the form c(x1, x2, n) giving the coordinates of the extreme tick marks and the number of intervals between tick-marks 
text(axTicks(1, axp=c(1,28.5,numQuarters-1)), par("usr")[3] - commonYlim/40, srt=45, adj=1, labels=qFlat$Label, xpd=T, cex=0.9)
dev.off()

# Steep
qSteep <- cbind(qtrLabels$Var1, quarterAgg(steepFriend, numQuarters), 
								quarterAgg(steepEnemy, numQuarters))
colnames(qSteep) <- c("Label", "SteepISAF", "SteepInsurg")
write.csv(qSteep,  paste(outDir, "qtrWikiSteep.csv", sep=""), row.names=FALSE)
pdf(paste(outDir, "qtrWikiSteep.pdf", sep=""), height=6, width=10)
barplot(t(as.matrix(qSteep[,2:3])), main="Violence in hilly/steep terrain", cex.main=1.5, cex.lab=1.3, cex=1.2, cex.axis=1.2, ylab="Total number of events", ylim=c(0, commonYlim), beside=F, col=clrs, axes=F)
lines(x=c(0,29), y=c(0,0))	# add line along bottom
# Place the legend at the top-left corner with no frame using rainbow colors
# reverse legend order to match stack order
legend(x=0, y=commonYlim-10, c("Insurgents", "ISAF/ANSF"), cex=1.3, bty="n", fill=c(clrs[2], clrs[1]))
axis(2)
axis(1, tick=F, lab=F)
# axp = vector of the form c(x1, x2, n) giving the coordinates of the extreme tick marks and the number of intervals between tick-marks 
text(axTicks(1, axp=c(1,28.5,numQuarters-1)), par("usr")[3] - commonYlim/40, srt=45, adj=1, labels=qSteep$Label, xpd=T, cex=0.9)
dev.off()
