# Download dataset (JDC2014, see https://zenodo.org/record/16515)
# aggregate metrics in 10s windows, and merge with manually videocoded data
library(XML)
source("./lib/rollingWindows.R")
source("./lib/aggregateEpisodeData.R")

preprocessStudy4 <- function(){
  
  cleandatafile <- "study4ProcessedData.Rda"
  origDir <- getwd()
  datadir <- paste(origDir,"data","study4",sep=.Platform$file.sep)
  if(!dir.exists(datadir)) dir.create(datadir, recursive=T)
  setwd(datadir)
  
  print("Downloading Study 4 data. This may take some time...")
  download.file("https://zenodo.org/record/16515/files/JDC2014-EyetrackingData.zip", destfile="JDC2014-EyetrackingData.zip", method="curl")
  unzip("JDC2014-EyetrackingData.zip")
  download.file("https://zenodo.org/record/16515/files/JDC2014-VideoCodingData.zip", destfile="JDC2014-VideoCodingData.zip", method="curl")
  unzip("JDC2014-VideoCodingData.zip")
  
  print("Download Study 4 complete! Aggregating episode data...")
  
  sessions <- c("JDC2014-Session1-eyetracking","JDC2014-Session2-eyetracking","JDC2014-Session3-eyetracking")
  totaldata <- data.frame()
  
  data <- aggregateEpisodeData(sessions, datadir=datadir, initendtimes=NULL, SEPARATOR=",") # For this study the raw data is comma-separated!
  data <- data[,c(1:5,12)] # We select only the load-related metrics
  # We load and add the video coding data with the social, activity and main gaze focus dimensions
  videocodes <- data.frame()
  sessionsvid <- c("JDC2014-Session1","JDC2014-Session2","JDC2014-Session3") # For some reason, the filenames for the videocoding are not consistent with the previous session labels
  for(session in sessionsvid){
    sessioncodes <- read.csv(paste(session,"-videocoding.csv",sep = ""), sep=",")
    if(nrow(videocodes)==0) videocodes <- sessioncodes
    else videocodes <- rbind(videocodes,sessioncodes)
  }
  videocodes$session <- videocodes$Session
  totaldata <- merge(data,videocodes,by=c("session","time"),all=T)
  save(totaldata, file=cleandatafile)

  print(paste("Saving to",cleandatafile,". Study 4 preprocessing complete!"))
  setwd(origDir)
}