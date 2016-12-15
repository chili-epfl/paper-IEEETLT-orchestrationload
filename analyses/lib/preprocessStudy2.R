# Download datasets ISL2014BASELINE (https://zenodo.org/record/16551/) or 
# ISL2015NOVEL (https://zenodo.org/record/198681), erratum (https://zenodo.org/record/203958),
# aggregate metrics in 10s windows, and merge with manually videocoded data
source("./lib/rollingWindows.R")
source("./lib/aggregateEpisodeData.R")

preprocessStudy2 <- function(){
  
  cleandatafile <- "study2ProcessedData.Rda"
  origDir <- getwd()
  datadir <- paste(origDir,"data","study2",sep=.Platform$file.sep)
  if(!dir.exists(datadir)) dir.create(datadir, recursive=T)
  setwd(datadir)
  
  print("Downloading Study 2 data. This may take some time...")
  download.file("https://zenodo.org/record/16551/files/ISL2014BASELINE-QuestionnaireData.zip", destfile="ISL2014BASELINE-QuestionnaireData.zip", method="curl")
  unzip("ISL2014BASELINE-QuestionnaireData.zip")
  download.file("https://zenodo.org/record/16551/files/ISL2015BASELINE-CodingData.zip", destfile="ISL2015BASELINE-CodingData.zip", method="curl")
  unzip("ISL2015BASELINE-CodingData.zip")
  download.file("https://zenodo.org/record/16551/files/ISL2014BASELINE-EyetrackingData.zip", destfile="ISL2014BASELINE-EyetrackingData.zip", method="curl")
  unzip("ISL2014BASELINE-EyetrackingData.zip")
  
  download.file("https://zenodo.org/record/198681/files/ISL2015NOVEL-CodingData.zip", destfile="ISL2015NOVEL-CodingData.zip", method="curl")
  unzip("ISL2015NOVEL-CodingData.zip")
  download.file("https://zenodo.org/record/198681/files/ISL2015NOVEL-EyetrackingData.zip", destfile="ISL2015NOVEL-EyetrackingData.zip", method="curl")
  unzip("ISL2015NOVEL-EyetrackingData.zip")
  download.file("https://zenodo.org/record/198681/files/ISL2015NOVEL-QuestionnaireData.zip", destfile="ISL2015NOVEL-QuestionnaireData.zip", method="curl")
  unzip("ISL2015NOVEL-QuestionnaireData.zip")
  download.file("https://zenodo.org/record/203958/files/ISL2015NOVEL-videocoding-erratum.csv", destfile="ISL2015NOVEL-videocoding-erratum.csv", method="curl")

  print("Download Study 2 complete! Aggregating episode data...")
  sessions <-  c("ISL2014BASELINE-Session1-eyetracking","ISL2014BASELINE-Session2-eyetracking","ISL2015NOVEL-Session1-eyetracking","ISL2015NOVEL-Session2-eyetracking")
  
  totaldata <- data.frame()
  
  data <- aggregateEpisodeData(sessions, datadir=datadir, initendtimes=NULL, SEPARATOR=";") # For this study the raw data is semicolon-separated, at least the fixation/saccades!
  data <- data[,c(1:5,12)] # We select only the load-related metrics
  # We load and add the video coding data with the social, activity and main gaze focus dimensions
  # clean and put all videocoding data into a single file
  videocodes1 <- read.csv("ISL2014BASELINE-videocoding.csv", sep=",")
  videocodes1$session <- videocodes1$Session

  videocodes2 <- read.csv("ISL2015NOVEL-videocoding-erratum.csv", sep=",")
  videocodes2$session <- videocodes2$Session

  videocodes <- rbind(videocodes1,videocodes2)
  totaldata <- merge(data,videocodes,by=c("session","time"),all=T)

  save(totaldata, file=cleandatafile)
  print(paste("Saving to",cleandatafile,". Study 2 preprocessing complete!"))
  setwd(origDir)
}