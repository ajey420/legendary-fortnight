# GP documention: C:\Program Files\ArcGIS\Documentation\Geoprocessor.pdf

# Calculate the mean center & std ellipse for each year & save it as a new shapefile
#   this script took ~1 min to execute, 3 aug 10

# import the modules needed
import arcgisscripting
import sys, os, string, random, traceback
from time import strftime

# create & configure the geoprocessor object
gp = arcgisscripting.create()
gp.overwriteoutput = 1

#*********************************************************************************
# INSTRUCTIONS: execute this script 2 times, once for each actor
#   also, be sure to set the working directory AND output directory below
#actor = "FRIEND"
actor = "ENEMY"
eventCount = 0

# qtr is the string representing the year & quarter
def analyzeLayer(qtr, lyr):
    global eventCount
    global actor        # lazy programming here

    count = gp.GetCount_management(lyr)
    print "    Number selected = ", count
    eventCount = eventCount + count

    if count > 0:
        outputLyr = actor + "MeanCenter" + qtr
        gp.MeanCenter_stats(lyr, outputLyr)
        print "    Created mean center shapefile for " + outputLyr
        outputLyr = actor + "StdEllipse" + qtr
        gp.DirectionalDistribution_stats(lyr, outputLyr, "1 Standard Deviation")
        print "    Created standard deviational ellipse for " + outputLyr
    else:
        print "  ERROR: no records selected"


#*********************************************************************************


#This is the main body.
try:
    startStr = "Analysis starting, time = " + strftime("%Y-%m-%d %H:%M:%S") + "\n"
    print startStr

# INSTRUCTIONS: set the working directory
    gp.workspace = "C:\Users\...\WikiLeaks"
    if actor == "FRIEND":
        eventsShp = "afg5_friend.shp"
    elif actor == "ENEMY":
        eventsShp = "afg5_enemy.shp"
    else:
        print "invalid actor: " + actor
    print "number pre-select = " + str(gp.GetCount_management(eventsShp))

    # the temp layer for analysis
    actorLyr = "actorLyr"
    analysisLyr = "analysisLyr"

    gp.makeFeatureLayer(eventsShp, actorLyr)
    countTODO = gp.GetCount_management(actorLyr)
    print "Total number of Events for analysis = ", countTODO

# INSTRUCTIONS: set the output directory
    gp.workspace = "C:\Users\...\ActorEllipses\EllipseShps"

    for year in range(2004,2010):
        print "Year = " + str(year)

        # EVENT_DATE no longer a date field, so use qtrID instead
        query = ' "year" = '+str(year-2003)+''
        print query
        gp.makeFeatureLayer(actorLyr, analysisLyr, query)
        analyzeLayer(str(year), analysisLyr)

    if eventCount != countTODO:
        print "Total number of events analyzed ("+str(eventCount)+") not equal to input count ("+str(countTODO)+")!!!!"
        
    endStr = "Analysis done, time = " + strftime("%Y-%m-%d %H:%M:%S") + "\n"
    print endStr    
   
    if gp: del gp

except:
    print "ERROR"
    print gp.getmessages(0)

    # get the traceback object
    tb = sys.exc_info()[2]
    # tbinfo contains the line number that the code failed on and the code from that line
    tbinfo = traceback.format_tb(tb)[0]
    # concatenate information together concerning the error into a message string
    pymsg = "PYTHON ERRORS:\nTraceback Info:\n" + tbinfo + "\nError Info:\n    " + \
            str(sys.exc_type)+ ": " + str(sys.exc_value) + "\n"
    # generate a message string for any geoprocessing tool errors
    msgs = "GP ERRORS:\n" + gp.GetMessages(2) + "\n"

    # return gp messages for use with a script tool
    gp.AddError(msgs)
    gp.AddError(pymsg)

    # print messages for use in Python/PythonWin
    print msgs
    print pymsg
    
    if gp: del gp
    
print "Execution complete"
