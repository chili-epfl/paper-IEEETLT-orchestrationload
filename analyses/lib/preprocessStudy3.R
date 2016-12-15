# Download dataset (JDC2015, see https://zenodo.org/record/198709), incl. errata 
# (https://zenodo.org/record/204063 and https://zenodo.org/record/204819)
# aggregate metrics in 10s windows, and merge with manually videocoded data
library(XML)
source("./lib/rollingWindows.R")
source("./lib/aggregateEpisodeData.R")
source("./lib/preprocessVideoCoding.R")

preprocessStudy3 <- function(){
  
  cleandatafile <- "study3ProcessedData.Rda"
  origDir <- getwd()
  datadir <- paste(origDir,"data","study3",sep=.Platform$file.sep)
  if(!dir.exists(datadir)) dir.create(datadir, recursive=T)
  setwd(datadir)
  
  print("Downloading Study 3 data. This may take some time...")
  download.file("https://zenodo.org/record/198709/files/JDC2015-CodingData.zip", destfile="JDC2015-CodingData.zip", method="curl")
  unzip("JDC2015-CodingData.zip")
  download.file("https://zenodo.org/record/204063/files/JDC2015-EyetrackingData-erratum.zip", destfile="JDC2015-EyetrackingData-erratum.zip", method="curl")
  unzip("JDC2015-EyetrackingData-erratum.zip")
  download.file("https://zenodo.org/record/198709/files/JDC2015-InitEnd-Alignment.csv", destfile="JDC2015-InitEnd-Alignment.csv", method="curl")
  
  download.file("https://zenodo.org/record/204819/files/JDC2015-CodingData-erratum.zip", destfile="JDC2015-CodingData-erratum.zip", method="curl")
  unzip("JDC2015-CodingData-erratum.zip")
  
  print("Download Study 3 complete! Aggregating episode data...")
  
  sessions <-  c("JDC2015-Session1","JDC2015-Session2","JDC2015-Session3","JDC2015-Session4")

  totaldata <- data.frame()
  
  annotationsData <- preprocessVideoCoding(datadir,sessions)
  initendtimes <- annotationsData[annotationsData$tier=="Recording" 
                                  & annotationsData$annotation == "Recording",
                                  c("start","end","session")]

  initendtimesEye <- initendtimes
  initendtimesEye$session <- paste(initendtimesEye$session, "-eyetracking", sep="") # Fix inconsistency in session/file labelling
    
  eyedata <- aggregateEpisodeData(initendtimesEye$session, datadir=datadir, initendtimes=initendtimesEye, SEPARATOR=";") # For this study the raw data is semicolon-separated, at least the fixation/saccades!
  eyedata <- eyedata[,c(1:5,12)] # We select only the load-related metrics
  
  # We calculate the video coding values for the same windows
  videocodedata <- aggregateVideoCodingData(sessions, datadir=datadir, initendtimes=initendtimes)
  videocodedata$session <- paste(videocodedata$session, "-eyetracking", sep="")
  
  totaldata <- merge(eyedata,videocodedata,by=c("session","time"),all=T)
  

  save(totaldata, file=cleandatafile)
  print(paste("Saving to",cleandatafile,". Study 3 preprocessing complete!"))
  setwd(origDir)
}