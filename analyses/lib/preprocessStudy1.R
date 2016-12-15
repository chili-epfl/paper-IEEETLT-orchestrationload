# Download dataset (DELANA, see https://zenodo.org/record/16514),
# aggregate metrics in 10s windows, and merge with manually videocoded data
source("./lib/rollingWindows.R")
source("./lib/aggregateEpisodeData.R")

preprocessStudy1 <- function(){
  
  origDir <- getwd()
  datadir <- paste(origDir,"data","study1",sep=.Platform$file.sep)
  if(!dir.exists(datadir)) dir.create(datadir, recursive=T)
  setwd(datadir)
  
  print("Downloading Study 1 data. This may take some time...")
  download.file("https://zenodo.org/record/16514/files/DELANA-VideoCodingData.zip", destfile="DELANA-VideoCodingData.zip", method="curl")
  unzip("DELANA-VideoCodingData.zip")
  download.file("https://zenodo.org/record/16514/files/DELANA-EyetrackingData.zip", destfile="DELANA-EyetrackingData.zip", method="curl")
  unzip("DELANA-EyetrackingData.zip")
  print("Download Study 1 complete! Aggregating episode data...")
  
  sessions <-  c("DELANA-Session1-Expert-eyetracking","DELANA-Session2-Expert-eyetracking","DELANA-Session3-Novice-eyetracking")
  totaldata <- data.frame()
  cleandatafile <- "study1ProcessedData.Rda"
  # Aggregate eyetracking metrics into 10-second episodes (sliding window 5s overlap)
  data <- aggregateEpisodeData(sessions, datadir=datadir, initendtimes=NULL, SEPARATOR=";") # For this study the raw data is semicolon-separated, for the fix/sac at least!
  data <- data[,c(1:5,12)] # We select only the cognitive load-related metrics
  # We load and add the video coding data with the social, activity and main gaze focus dimensions
  videocodes <- read.csv("DELANA-videocoding.csv", sep=",")
  videocodes$session <- videocodes$Session
  totaldata <- merge(data,videocodes,by=c("session","time"),all=T)
  save(totaldata, file=cleandatafile)
  print(paste("Saving to",cleandatafile,". Study 1 preprocessing complete!"))
  setwd(origDir)
}