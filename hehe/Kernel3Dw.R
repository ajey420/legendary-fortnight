# 2 Aug 2010, generate annual 3D violence intensity surfaces from WiliLeaks data
#
##########################################################################

library(spatstat)
library(maptools)
library(fields)		# for drape.plot

# INSTRUCTIONS: set these directories prior to analysis
dataDir = "C:/???/"
outDir = "C:/???/"

# as.is prevents text from being converted to categorical/factor format
allEvents<-read.csv(paste(dataDir, "WikiLeaks/afg5_utm41N.csv", sep=""), as.is = TRUE)

afgEvents<-allEvents[allEvents$violent == 1, ]
afgEvents$qtrID <- afgEvents$quarter
length(afgEvents$reportkey)
names(afgEvents)

# convert text date to a POSIXlt object
	# %Y = year with century, %y = 2 digit year
dates<-strptime(afgEvents$Date_, "%m/%d/%Y")
startYr<-as.numeric(min(format(dates, "%Y")))

# get bounding polygon
	# points were originally clipped using GADM points, so use those borders here
border <- readShapePoly(paste(dataDir, "AdminBorders/GADM_borders/Afg_adm0prj", sep=""), proj4string=CRS("+proj=utm +zone=41 +datum=WGS84"))

# convert shapefile to "observation window" using code from spatstatWorkshop_pn0y.pdf
borderSP <- as(border, "SpatialPolygons")
#summary(borderSP)
borderW <- as(borderSP, "owin")
#summary(borderW)
#plot(borderW)

# setup kernel density options
# sigma is std. dev. of isotropic Guassian smoothing kernel
cellSize<-20000
#cellSize<-10000	# too many grids to display for publication
minX<-269000
minY<-3254000
dimX<-130/2		# divide by 2 to go from 10km to 20km grids
dimY<-104/2
stdDev<-20000	# 20km std dev

startTime <- proc.time()
numYears<-max(afgEvents$year)
#zlim = c(-1e-23, 1e-06)		# for 10x10km grid & sigma=20km
zlim = c(-2e-23, 8e-07)	# for 20x20km grid & sigma=20km
zlimcol = c(-3e-07, 8e-07)	# for 20x20km grid & sigma=20km
for(i in 1:numYears){
	yrEvents <- afgEvents[afgEvents$year == i, ]
	pobj <- ppp(yrEvents$utm_x, yrEvents$utm_y, window=borderW)

	densObj<-density.ppp(x=pobj, sigma=stdDev,
		eps=cellSize, dimyx=c(dimY, dimX),
		xy=list(x=minX+(1:dimX)*cellSize, y=minY+(1:dimY)*cellSize))
	# uncomment to determine the maximum zlim range
#	zlim<-range( c(densObj$v), na.rm=TRUE)
#	print(zlim, digits=3)
	pdf(paste(outDir, "allEvents",i+startYr-1,".pdf", sep=""), height=6, width=6)
	#  must transpose the z-values for drape.plot
	trspDens<-t(densObj$v)
	# theta = compass angle, phi = tilt angle
	drape.plot(densObj$xcol, densObj$yrow, trspDens, theta=180, phi=30, zlim=zlim, zlim2=zlimcol, main=paste("Violence surface ",i+startYr-1, sep=""))
	dev.off()
#	sgdf <- as(densObj, "SpatialGridDataFrame")
#	writeAsciiGrid(sgdf, paste(outDir, "grid", i+2003, ".txt", sep=""))
}
endTime <- proc.time()
print(paste("elapsed time: ", round(endTime[3]-startTime[3]), " seconds", sep=""))
