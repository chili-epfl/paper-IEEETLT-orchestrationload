# This function loads the time series pupil data from the eyetracker, the fixation and saccade details (in separate
# txt/csv files), and calculates the averages/counts of multiple eyetracking metrics
# and game variables for rolling windows of 10s (5s of slide between windows)
# it also receives a data frame with the starting and ending point for the calculations in each session (if not present, the whole file is used)
# Files from each session should be called [session]-events.txt, [session]-fixationDetails, [session]-saccadeDetails.txt
aggregateEpisodeData <- function(sessions, datadir=".", initendtimes=NULL, window=10, slide=5, SEPARATOR=";"){
    
    # Some basic parameters for the sliding windows (in seconds), now at the function parameters
    
    totaldata <- data.frame()
    
    for (session in sessions){
        
        # We check whether the clean data is already in place - if so, we skip this pre-processing
        if(!file.exists(paste(datadir,"/",session,".EyetrackerEvents.rda",sep="")) ||
               !file.exists(paste(datadir,"/",session,".EyetrackerFixations.rda",sep="")) ||
               !file.exists(paste(datadir,"/",session,".EyetrackerSaccades.rda",sep=""))){
            
            # We load the raw events export
            filename = paste(datadir,"/",session,"-eventexport.txt", sep="")
            filedata <- read.csv(filename,as.is=T,comment.char="#", sep=",")
            
            # From all the data, we only need timestamp, pupil diameter (L,R, in mm)
            filedata <- filedata[c(1,6,9)]
            filedata$session <- session
            pupildata <- data.frame(filedata)
            
            # We calculate the time baseline of the session
            time0 <- min(pupildata$Time)
            pupildata$Time.ms <- (pupildata$Time - time0) / 1000
            
            # We load the fixation details file
            filename = paste(datadir,"/",session,"-fixationDetails.txt", sep="")
            filedata <- read.csv(filename,comment.char="#", sep=SEPARATOR)
            
            # we select the meaningful columns (for now, only fixation start, duration, end in ms)
            # it is different in the exports we have for different teachers!
            filedata <- filedata[,c(8,9,10,16,17)]

            filedata$session <- session
            fixdata <- data.frame(filedata)
            
            #We set the time of the fixation in the middle of the fixation
            fixdata$Time.ms <- (fixdata$Fixation.Start..ms. + (fixdata$Fixation.Duration..ms./2)) 
            # We create a Time field so that we have the time in both timestamp and ms formats
            fixdata$Time <- time0 + (fixdata$Time.ms)*1000
            fixdata$Fixation.Dispersion <- sqrt((fixdata$Dispersion.X*fixdata$Dispersion.X)+(fixdata$Dispersion.Y*fixdata$Dispersion.Y))
            
            # We load the saccade details file
            filename = paste(datadir,"/",session,"-saccadeDetails.txt", sep="")
            filedata <- read.csv(filename,comment.char="#", sep=SEPARATOR)
            
            # we select the meaningful columns (for now, only saccade start, duration, end in ms and amplitude in degrees)
            # it is different in the exports we have for different teachers!
            filedata <- filedata[,c(8,9,10:15)]
            filedata$session <- session
            sacdata <- data.frame(filedata)
            # We add the saccade speed and linear velocity for each saccade
            sacdata$Saccade.Speed <- sacdata[,8] / sacdata$Saccade.Duration..ms.
            sacdata$Saccade.Length<-sqrt(((sacdata$StartPosition.X-sacdata$EndPosition.X)*(sacdata$StartPosition.X-sacdata$EndPosition.X))
                                         +((sacdata$StartPosition.Y-sacdata$EndPosition.Y)*(sacdata$StartPosition.Y-sacdata$EndPosition.Y)))
            sacdata$Saccade.Velocity<-sacdata$Saccade.Length/sacdata$Saccade.Duration..ms.
            
            
            #We set the time of saccade in the middle of the fixation
            sacdata$Time.ms <- (sacdata$Saccade.Start..ms. + (sacdata$Saccade.Duration..ms./2)) 
            # We create a Time field so that we have the time in both timestamp and ms formats
            sacdata$Time <- time0 + (sacdata$Time.ms)*1000
            
            # We save the clean(er) data to smaller, more efficient rda files
            save(pupildata,file=paste(datadir,"/",session,".EyetrackerEvents.rda",sep=""),compress=TRUE)
            save(fixdata,file=paste(datadir,"/",session,".EyetrackerFixations.rda",sep=""),compress=TRUE)
            save(sacdata,file=paste(datadir,"/",session,".EyetrackerSaccades.rda",sep=""),compress=TRUE)
            
        }
        
        # We load the clean data, just in case we did not the previous steps
        pupildata <- get(load(paste(datadir,"/",session,".EyetrackerEvents.rda",sep="")))
        fixdata <- get(load(paste(datadir,"/",session,".EyetrackerFixations.rda",sep="")))
        sacdata <- get(load(paste(datadir,"/",session,".EyetrackerSaccades.rda",sep="")))
        
        # We get the start and end times for this session
        inittime <- ifelse(is.null(initendtimes), 0, initendtimes[initendtimes$session == session,"start"])
        endtime <- ifelse(is.null(initendtimes), max(pupildata$Time.ms), initendtimes[initendtimes$session == session,"end"])
        
        # We get the rolling window for the mean pupil diameter, and its median value for a median cut
        meandata <- rollingMean(pupildata$Time.ms,pupildata$L.Pupil.Diameter..mm.,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        # We get the rolling window for the SD of pupil diameter, and its median value for a median cut
        sddata <- rollingSd(pupildata$Time.ms,pupildata$L.Pupil.Diameter..mm.,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        # We get the number of long fixations in the window, and its median
        longdata <- rollingLong(fixdata$Time.ms,fixdata$Fixation.Duration..ms.,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        # We get the saccade speed in the window
        sacspdata <- rollingMean(sacdata$Time.ms,sacdata$Saccade.Speed,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        # Fixation duration
        fdurdata <- rollingMean(fixdata$Time.ms,fixdata$Fixation.Duration..ms.,window*1000,slide*1000, inittime=inittime, endtime=endtime)

        # Fixation dispersion
        fdisdata <- rollingMean(fixdata$Time.ms,fixdata$Fixation.Dispersion,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        # Saccade Duration
        sdurdata <- rollingMean(sacdata$Time.ms,sacdata$Saccade.Duration..ms.,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        # Saccade Amplitude
        sampdata <- rollingMean(sacdata$Time.ms,sacdata[,8],window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        # Saccade Length
        slendata <- rollingMean(sacdata$Time.ms,sacdata$Saccade.Length,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        # Saccade Velocity
        sveldata <- rollingMean(sacdata$Time.ms,sacdata$Saccade.Velocity,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        
        data <- merge(meandata,sddata,by="time",suffixes = c(".Mean",".SD"),all=T)
        data <- merge(data,longdata,by="time",all=T)
        names(data)[[4]] <- paste(names(data)[[4]],"Fix",sep=".")
        data <- merge(data,sacspdata,by="time",all=T)
        names(data)[[5]] <- paste(names(data)[[5]],"Sac",sep=".")
        data <- merge(data,fdurdata,by="time",all=T)
        names(data)[[6]] <- paste(names(data)[[6]],"Fix.Dur",sep=".")
        data <- merge(data,fdisdata,by="time",all=T)
        names(data)[[7]] <- paste(names(data)[[7]],"Fix.Disp",sep=".")
        data <- merge(data,sdurdata,by="time",all=T)
        names(data)[[8]] <- paste(names(data)[[8]],"Sac.Dur",sep=".")
        data <- merge(data,sampdata,by="time",all=T)
        names(data)[[9]] <- paste(names(data)[[9]],"Sac.Amp",sep=".")
        data <- merge(data,slendata,by="time",all=T)
        names(data)[[10]] <- paste(names(data)[[10]],"Sac.Len",sep=".")
        data <- merge(data,sveldata,by="time",all=T)
        names(data)[[11]] <- paste(names(data)[[11]],"Sac.Vel",sep=".")
        
        
        
        
        data$session <- session

        # We join the game data to our global dataset
        if(length(totaldata)==0) totaldata <- data
        else totaldata <- rbind(totaldata,data)


    }

    #print("Aggregation into 10-second episodes finished. Writing clean datafile: JDC15-AggregatedEyetrackData.Rda")
    #save(totaldata,file=paste(datadir,"/JDC15-AggregatedEyetrackData.Rda",sep=""),compress=TRUE)
    
    #unlink("ISL2014BASELINE-AggregatedEyetrackData.Rda")
    
    totaldata
    
}


# This function loads the video coding intervals from the human video coding of the sessions (in separate
# csv files), and for each of the tiers of interest calculates the dominant code in that episode
# for rolling windows of 10s (5s of slide between windows)
# it also receives a data frame with the starting and ending point for the calculations in each session (if not present, the whole file is used)
# Coding should be in a file called cleanAnnotationData.Rda
aggregateVideoCodingData <- function(sessions, datadir=".", initendtimes=NULL, window=10, slide=5){
    
    # Some basic parameters for the sliding windows (in seconds)
    totaldata <- data.frame()
    
    data <- get(load(paste(datadir,"/cleanAnnotationData.Rda",sep="")))
    
    videocodingdata <- data.frame()
    
    for (session in sessions){
        
        sessiondata <- data[data$session==session,]
        
        # We get the start and end times for this session
        inittime <- ifelse(is.null(initendtimes), 0, initendtimes[initendtimes$session == session,"start"])
        endtime <- ifelse(is.null(initendtimes), max(sessiondata$end), initendtimes[initendtimes$session == session,"end"])
        
        tiers <- c("Experimental","Social","Activity")
        
        sessionvideocodes <- data.frame()
        
        for(tier in tiers){
            
            tierdata <- sessiondata[sessiondata$tier==tier,]
            
            codedata <- rollingCode(tierdata,window*1000,slide*1000, inittime=inittime, endtime=endtime)
            
            if(nrow(sessionvideocodes)==0){
                sessionvideocodes <- codedata
                names(sessionvideocodes)[[2]] <- paste("value",tier,sep=".")
            } else {
                sessionvideocodes <- cbind(sessionvideocodes,codedata[,"value"])
                names(sessionvideocodes)[[length(names(sessionvideocodes))]] <- paste("value",tier,sep=".")
            }
            
        }
        
        sessionvideocodes$session <- session
        # add the data to the other sessions data
        if(nrow(videocodingdata)==0) videocodingdata <- sessionvideocodes
        else videocodingdata <- rbind(videocodingdata,sessionvideocodes)
    }
    
    #print("Aggregation into 10-second episodes finished. Writing clean datafile: JDC15-AggregatedVideoCodeData.Rda")
    #save(videocodingdata,file=paste(datadir,"/JDC15-AggregatedVideoCodeData.Rda",sep=""),compress=TRUE)
    #unlink("ISL2014BASELINE-AggregatedEyetrackData.Rda")
    
    videocodingdata
    
}


# This function receives a dataframe with the time series EEG data and calculates the averages 
# for rolling windows of 10s (5s of slide between windows) of Theta and Attention variables
# it also receives a data frame with the starting and ending point for the calculations in each session (if not present, the whole file is used)
aggregateEEGData <- function(df, initendtimes=NULL, window=10, slide=5){
    
    # Some basic parameters for the sliding windows (in seconds)
    totaldata <- data.frame()
    sessions <- unique(df$session)
    
    for (session in sessions){
        
        data <- df[df$session == session,]
        
        # We get the start and end times for this session
        inittime <- ifelse(is.null(initendtimes), 0, initendtimes[initendtimes$session == session,"start"])
        endtime <- ifelse(is.null(initendtimes), max(data$Time..s.), initendtimes[initendtimes$session == session,"end"])
        
        # We get the rolling window for the Attention and the Theta
        eledata <- rollingMean(data$Time..s.,data$Electrode,window,slide, inittime=inittime, endtime=endtime)
        attdata <- rollingMean(data$Time..s.,data$Attention,window,slide, inittime=inittime, endtime=endtime)
        meddata <- rollingMean(data$Time..s.,data$Meditation,window,slide, inittime=inittime, endtime=endtime)
        deldata <- rollingMean(data$Time..s.,data$Delta,window,slide, inittime=inittime, endtime=endtime)
        thedata <- rollingMean(data$Time..s.,data$Theta,window,slide, inittime=inittime, endtime=endtime)
        laldata <- rollingMean(data$Time..s.,data$Low.Alpha,window,slide, inittime=inittime, endtime=endtime)
        haldata <- rollingMean(data$Time..s.,data$High.Alpha,window,slide, inittime=inittime, endtime=endtime)
        lbedata <- rollingMean(data$Time..s.,data$Low.Beta,window,slide, inittime=inittime, endtime=endtime)
        hbedata <- rollingMean(data$Time..s.,data$High.Beta,window,slide, inittime=inittime, endtime=endtime)
        lgadata <- rollingMean(data$Time..s.,data$Low.Gamma,window,slide, inittime=inittime, endtime=endtime)
        mgadata <- rollingMean(data$Time..s.,data$Mid.Gamma,window,slide, inittime=inittime, endtime=endtime)
        bstdata <- rollingMean(data$Time..s.,data$Blink.Strength,window,slide, inittime=inittime, endtime=endtime)

        newdata <- merge(eledata,attdata,by="time",suffixes = c(".Electrode",".Attention"),all=T)
        
        newdata <- merge(newdata,meddata,by="time",all=T)
        names(newdata)[[4]] <- paste(names(newdata)[[4]],"Meditation",sep=".")
        newdata <- merge(newdata,deldata,by="time",all=T)
        names(newdata)[[5]] <- paste(names(newdata)[[5]],"Delta",sep=".")
        newdata <- merge(newdata,thedata,by="time",all=T)
        names(newdata)[[6]] <- paste(names(newdata)[[6]],"Theta",sep=".")
        newdata <- merge(newdata,laldata,by="time",all=T)
        names(newdata)[[7]] <- paste(names(newdata)[[7]],"Low.Alpha",sep=".")
        newdata <- merge(newdata,haldata,by="time",all=T)
        names(newdata)[[8]] <- paste(names(newdata)[[8]],"High.Alpha",sep=".")
        newdata <- merge(newdata,lbedata,by="time",all=T)
        names(newdata)[[9]] <- paste(names(newdata)[[9]],"Low.Beta",sep=".")
        newdata <- merge(newdata,hbedata,by="time",all=T)
        names(newdata)[[10]] <- paste(names(newdata)[[10]],"High.Beta",sep=".")
        newdata <- merge(newdata,lgadata,by="time",all=T)
        names(newdata)[[11]] <- paste(names(newdata)[[11]],"Low.Gamma",sep=".")
        newdata <- merge(newdata,mgadata,by="time",all=T)
        names(newdata)[[12]] <- paste(names(newdata)[[12]],"Mid.Gamma",sep=".")
        newdata <- merge(newdata,bstdata,by="time",all=T)
        names(newdata)[[13]] <- paste(names(newdata)[[13]],"Blink.Strength",sep=".")
        newdata$session <- session
        
        
        # We join the game data to our global dataset
        if(length(totaldata)==0) totaldata <- newdata
        else totaldata <- rbind(totaldata,newdata)
        
    }
    
    #print("Aggregation into 10-second episodes finished. Writing clean datafile: JDC15-AggregatedEEGData.Rda")
    #save(totaldata,file=paste(datadir,"/JDC15-AggregatedEEGData.Rda",sep=""),compress=TRUE)
    
    #unlink("ISL2014BASELINE-AggregatedEyetrackData.Rda")
    
    totaldata
    
}

# This function receives a dataframe with the time series Accelerometer data and calculates the averages and SDs
# for rolling windows of 10s (5s of slide between windows) of X, Y, Z axes
# it also receives a data frame with the starting and ending point for the calculations in each session 
# starting from 0, that is, the start of the file (if not present, the whole file is used)
aggregateAccelerometerData <- function(df, initendtimes=NULL, window=10, slide=5){
    
    # Some basic parameters for the sliding windows (in seconds)
    totaldata <- data.frame()
    sessions <- unique(df$session)
    
    for (session in sessions){
        
        data <- df[df$session == session,]
        
        # We get the start and end times for this session
        inittime <- ifelse(is.null(initendtimes), min(data$timestamp), min(data$timestamp)+initendtimes[initendtimes$session == session,"start"])
        endtime <- ifelse(is.null(initendtimes), max(data$timestamp), min(data$timestamp)+initendtimes[initendtimes$session == session,"end"])
        
        # We get the rolling window for the Attention and the Theta
        xmeandata <- rollingMean(data$timestamp,data$accelerationX,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        xsddata <- rollingSd(data$timestamp,data$accelerationX,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        ymeandata <- rollingMean(data$timestamp,data$accelerationY,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        ysddata <- rollingSd(data$timestamp,data$accelerationY,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        zmeandata <- rollingMean(data$timestamp,data$accelerationZ,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        zsddata <- rollingSd(data$timestamp,data$accelerationZ,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        
        newdata <- merge(xmeandata,xsddata,by="time",suffixes = c(".X.Mean",".X.SD"),all=T)
        newdata <- merge(newdata,ymeandata,by="time",all=T)
        names(newdata)[[4]] <- paste(names(newdata)[[4]],"Y.Mean",sep=".")
        newdata <- merge(newdata,ysddata,by="time",all=T)
        names(newdata)[[5]] <- paste(names(newdata)[[5]],"Y.SD",sep=".")
        newdata <- merge(newdata,zmeandata,by="time",all=T)
        names(newdata)[[6]] <- paste(names(newdata)[[6]],"Z.Mean",sep=".")
        newdata <- merge(newdata,zsddata,by="time",all=T)
        names(newdata)[[7]] <- paste(names(newdata)[[7]],"Z.SD",sep=".")
        newdata$session <- session
        
        # We join the game data to our global dataset
        if(length(totaldata)==0) totaldata <- newdata
        else totaldata <- rbind(totaldata,newdata)
        
    }
    
    #print("Aggregation into 10-second episodes finished. Writing clean datafile: JDC15-AggregatedEEGData.Rda")
    #save(totaldata,file=paste(datadir,"/JDC15-AggregatedEEGData.Rda",sep=""),compress=TRUE)
    #unlink("ISL2014BASELINE-AggregatedEyetrackData.Rda")
    
    totaldata
    
}

# This function receives a dataframe with the time series Accelerometer data and calculates the total jerk (dif. acceleration/dif. time)
# and the averages and SDs for rolling windows of Xs (X/2s of slide between windows) of jerk, X, Y, Z axes
# it also receives a data frame with the starting and ending point for the calculations in each session 
# starting from 0, that is, the start of the file (if not present, the whole file is used)
aggregateAccelerometerDataWithJerk <- function(df, initendtimes=NULL, window=10, slide=5){
    
    # Some basic parameters for the sliding windows (in seconds)
    totaldata <- data.frame()
    sessions <- unique(df$session)
    
    for (session in sessions){
        
        data <- df[df$session == session,]

        # We calculate the jerk of each time point
        data$jerk <- calculateJerk(t=data$timestamp, x=data$accelerationX, y=data$accelerationY, z=data$accelerationZ)
        
        # We get the start and end times for this session
        inittime <- ifelse(is.null(initendtimes), min(data$timestamp), min(data$timestamp)+initendtimes[initendtimes$session == session,"start"])
        endtime <- ifelse(is.null(initendtimes), max(data$timestamp), min(data$timestamp)+initendtimes[initendtimes$session == session,"end"])
        
        # We get the rolling window for the Attention and the Theta
        xmeandata <- rollingMean(data$timestamp,data$accelerationX,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        xsddata <- rollingSd(data$timestamp,data$accelerationX,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        ymeandata <- rollingMean(data$timestamp,data$accelerationY,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        ysddata <- rollingSd(data$timestamp,data$accelerationY,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        zmeandata <- rollingMean(data$timestamp,data$accelerationZ,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        zsddata <- rollingSd(data$timestamp,data$accelerationZ,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        jerkmeandata <- rollingMean(data$timestamp,data$jerk,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        jerksddata <- rollingSd(data$timestamp,data$jerk,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        newdata <- merge(xmeandata,xsddata,by="time",suffixes = c(".X.Mean",".X.SD"),all=T)
        newdata <- merge(newdata,ymeandata,by="time",all=T)
        names(newdata)[[4]] <- paste(names(newdata)[[4]],"Y.Mean",sep=".")
        newdata <- merge(newdata,ysddata,by="time",all=T)
        names(newdata)[[5]] <- paste(names(newdata)[[5]],"Y.SD",sep=".")
        newdata <- merge(newdata,zmeandata,by="time",all=T)
        names(newdata)[[6]] <- paste(names(newdata)[[6]],"Z.Mean",sep=".")
        newdata <- merge(newdata,zsddata,by="time",all=T)
        names(newdata)[[7]] <- paste(names(newdata)[[7]],"Z.SD",sep=".")
        newdata <- merge(newdata,jerkmeandata,by="time",all=T)
        names(newdata)[[8]] <- paste(names(newdata)[[8]],"Jerk.Mean",sep=".")
        newdata <- merge(newdata,jerksddata,by="time",all=T)
        names(newdata)[[9]] <- paste(names(newdata)[[9]],"Jerk.SD",sep=".")
        newdata$session <- session
        
        # We join the game data to our global dataset
        if(length(totaldata)==0) totaldata <- newdata
        else totaldata <- rbind(totaldata,newdata)
        
    }
    
    #print("Aggregation into 10-second episodes finished. Writing clean datafile: JDC15-AggregatedEEGData.Rda")
    #save(totaldata,file=paste(datadir,"/JDC15-AggregatedEEGData.Rda",sep=""),compress=TRUE)
    #unlink("ISL2014BASELINE-AggregatedEyetrackData.Rda")
    
    totaldata
    
}

calculateJerk <- function(t, x, y, z){
    
    jerk <- numeric(length(t))
    
    jerk[1] <- NA
    
    for(i in 2:length(t)){
        jerk[i] <- sqrt((x[i]-x[i-1])^2+(y[i]-y[i-1])^2+(z[i]-z[i-1])^2)/(t[i]-t[i-1])
    }
    
    jerk
}


# This function receives a dataframe with the time series Accelerometer data and calculates the total jerk (dif. acceleration/dif. time)
# and the averages and SDs for rolling windows of Xs (X/2s of slide between windows) of jerk, X, Y, Z axes, and
# The first fftcomp components of the FFT of the jerk
# it also receives a data frame with the starting and ending point for the calculations in each session 
# starting from 0, that is, the start of the file (if not present, the whole file is used)
aggregateAccelerometerDataWithJerkFFT <- function(df, initendtimes=NULL, window=10, slide=5, fftcomp=30){
    
    # Some basic parameters for the sliding windows (in seconds)
    totaldata <- data.frame()
    sessions <- unique(df$session)
    
    for (session in sessions){
        
        data <- df[df$session == session,]
        
        # We calculate the jerk of each time point
        data$jerk <- calculateJerk(t=data$timestamp, x=data$accelerationX, y=data$accelerationY, z=data$accelerationZ)
        
        # We get the start and end times for this session
        inittime <- ifelse(is.null(initendtimes), min(data$timestamp), min(data$timestamp)+initendtimes[initendtimes$session == session,"start"])
        endtime <- ifelse(is.null(initendtimes), max(data$timestamp), min(data$timestamp)+initendtimes[initendtimes$session == session,"end"])
        
        # We get the rolling window for the Attention and the Theta
        xmeandata <- rollingMean(data$timestamp,data$accelerationX,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        xsddata <- rollingSd(data$timestamp,data$accelerationX,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        ymeandata <- rollingMean(data$timestamp,data$accelerationY,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        ysddata <- rollingSd(data$timestamp,data$accelerationY,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        zmeandata <- rollingMean(data$timestamp,data$accelerationZ,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        zsddata <- rollingSd(data$timestamp,data$accelerationZ,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        jerkmeandata <- rollingMean(data$timestamp,data$jerk,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        jerksddata <- rollingSd(data$timestamp,data$jerk,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        jerkfftdata <- rollingFFT(data$timestamp,data$jerk,window*1000,slide*1000, inittime=inittime, endtime=endtime, fftcomp=fftcomp)
        
        newdata <- merge(xmeandata,xsddata,by="time",suffixes = c(".X.Mean",".X.SD"),all=T)
        newdata <- merge(newdata,ymeandata,by="time",all=T)
        names(newdata)[[4]] <- paste(names(newdata)[[4]],"Y.Mean",sep=".")
        newdata <- merge(newdata,ysddata,by="time",all=T)
        names(newdata)[[5]] <- paste(names(newdata)[[5]],"Y.SD",sep=".")
        newdata <- merge(newdata,zmeandata,by="time",all=T)
        names(newdata)[[6]] <- paste(names(newdata)[[6]],"Z.Mean",sep=".")
        newdata <- merge(newdata,zsddata,by="time",all=T)
        names(newdata)[[7]] <- paste(names(newdata)[[7]],"Z.SD",sep=".")
        newdata <- merge(newdata,jerkmeandata,by="time",all=T)
        names(newdata)[[8]] <- paste(names(newdata)[[8]],"Jerk.Mean",sep=".")
        newdata <- merge(newdata,jerksddata,by="time",all=T)
        names(newdata)[[9]] <- paste(names(newdata)[[9]],"Jerk.SD",sep=".")
        
        newdata <- merge(newdata,jerkfftdata,by="time",all=T)
        for(i in 1:fftcomp){
            names(newdata)[[9+i]] <- paste("value.Jerk.FFT",i,sep=".")
        }        
        
        newdata$session <- session
        
        
        # We join the game data to our global dataset
        if(length(totaldata)==0) totaldata <- newdata
        else totaldata <- rbind(totaldata,newdata)
        
    }
    
    #print("Aggregation into 10-second episodes finished. Writing clean datafile: JDC15-AggregatedEEGData.Rda")
    #save(totaldata,file=paste(datadir,"/JDC15-AggregatedEEGData.Rda",sep=""),compress=TRUE)
    #unlink("ISL2014BASELINE-AggregatedEyetrackData.Rda")
    
    totaldata
    
}



# This function loads the time series for faces and blur of the video, and calculates 
# receives a dataframe with the whole raw dataset indicating time, numfaces, blur index, and session
aggregateVideoData <- function(data, initendtimes=NULL, window=10, slide=5){
    
    # Some basic parameters for the sliding windows (in seconds), now at the function parameters
    
    totaldata <- data.frame()
    
    for (session in sessions){
        
        sessionraw <- data[data$session==session,]
        
        # We get the start and end times for this session
        inittime <- ifelse(is.null(initendtimes), 0, initendtimes[initendtimes$session == session,"start"])
        endtime <- ifelse(is.null(initendtimes), max(pupildata$Time.ms), initendtimes[initendtimes$session == session,"end"])
        
        # Blur features:
        # Mean
        meanblur <- rollingMean(sessionraw$time,sessionraw$blur,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # SD blur
        sdblur <- rollingSd(sessionraw$time,sessionraw$blur,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Median blur
        medianblur <- rollingMedian(sessionraw$time,sessionraw$blur,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Max blur
        maxblur <- rollingMax(sessionraw$time,sessionraw$blur,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Blurry frames (threshold = 0.4 ... the mean/median is around 0.36)
        numblur <- rollingCountThrsh(sessionraw$time,sessionraw$blur,0.4,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Blurry episodes (threshold = 0.4 ... the mean/median is around 0.36)
        runblur <- rollingCountRunsThrsh(sessionraw$time,sessionraw$blur,0.4,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Median length of blurry episodes
        medrunblur <- rollingMedianRunsThrsh(sessionraw$time,sessionraw$blur,0.4,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # SD length of blurry episodes
        sdrunblur <- rollingSdRunsThrsh(sessionraw$time,sessionraw$blur,0.4,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Max length of blurry episodes
        maxrunblur <- rollingMaxRunsThrsh(sessionraw$time,sessionraw$blur,0.4,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Median length of clear episodes
        medrunclear <- rollingMedianRunsThrshBelow(sessionraw$time,sessionraw$blur,0.4,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # SD length of clear episodes
        sdrunclear <- rollingSdRunsThrshBelow(sessionraw$time,sessionraw$blur,0.4,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Max length of clear episodes
        maxrunclear <- rollingMaxRunsThrshBelow(sessionraw$time,sessionraw$blur,0.4,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        # Faces features:
        # Mean faces per window
        meanfaces <- rollingMean(sessionraw$time,sessionraw$numfaces,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # SD faces per window
        sdfaces <- rollingSd(sessionraw$time,sessionraw$numfaces,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Median faces per window
        medianfaces <- rollingMedian(sessionraw$time,sessionraw$numfaces,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Max faces per window
        maxfaces <- rollingMax(sessionraw$time,sessionraw$numfaces,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Number of frames with faces
        countfaces <- rollingCountThrsh(sessionraw$time,sessionraw$numfaces,0,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Number of facey episodes
        runfaces <- rollingCountRunsThrsh(sessionraw$time,sessionraw$numfaces,0,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Median length of facey episode
        medrunfaces <- rollingMedianRunsThrsh(sessionraw$time,sessionraw$numfaces,0,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # SD length of facey episodes
        sdrunfaces <- rollingSdRunsThrsh(sessionraw$time,sessionraw$numfaces,0,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Max length of facey episodes
        maxrunfaces <- rollingMaxRunsThrsh(sessionraw$time,sessionraw$numfaces,0,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Median length of non-facey episodes
        medrunnofaces <- rollingMedianRunsThrshBelow(sessionraw$time,sessionraw$numfaces,0,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # SD length of non-facey episodes
        sdrunnofaces <- rollingSdRunsThrshBelow(sessionraw$time,sessionraw$numfaces,0,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Max length of non-facey episodes
        maxrunnofaces <- rollingMaxRunsThrshBelow(sessionraw$time,sessionraw$numfaces,0,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # Avg number of faces per CLEAR frame
        meanfacesclear <- rollingMeanOtherThrshBelow(sessionraw$time,sessionraw$numfaces,sessionraw$blur,0.4,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        # SD number of faces per CLEAR frame
        sdfacesclear <- rollingSdOtherThrshBelow(sessionraw$time,sessionraw$numfaces,sessionraw$blur,0.4,window*1000,slide*1000, inittime=inittime, endtime=endtime)
        
        sessdata <- Reduce(function(x, y) merge(x, y, by="time", all=TRUE), 
                           list(meanblur, sdblur, medianblur, maxblur, numblur,
                                runblur, medrunblur, sdrunblur, maxrunblur, 
                                medrunclear, sdrunclear, maxrunclear,
                                meanfaces, sdfaces, medianfaces, maxfaces, countfaces,
                                runfaces, medrunfaces, sdrunfaces, maxrunfaces,
                                medrunnofaces, sdrunnofaces, maxrunnofaces,
                                meanfacesclear, sdfacesclear))
        
        names(sessdata) <- c("time", "Mean.Blur", "SD.Blur", "Median.Blur", "Max.Blur", "Num.Blur",
                             "Num.Ep.Blur", "Med.Ep.Blur", "SD.Ep.Blur", "Max.Ep.Blur",
                             "Med.Ep.NoBlur", "SD.Ep.NoBlur", "Max.Ep.NoBlur",
                             "Mean.Faces", "SD.Faces", "Median.Faces", "Max.Faces", "Count.Faces",
                             "Num.Ep.Faces", "Med.Ep.Faces", "SD.Ep.Faces", "Max.Ep.Faces",
                             "Med.Ep.NoFaces", "SD.Ep.NoFaces", "Max.Ep.NoFaces",
                             "Mean.Faces.NoBlur", "SD.Faces.NoBlur")
        
        sessdata$session <- session
        
        # We join the game data to our global dataset
        if(length(totaldata)==0) totaldata <- sessdata
        else totaldata <- rbind(totaldata,sessdata)
        
        
    }
    
    #print("Aggregation into 10-second episodes finished. Writing clean datafile: JDC15-AggregatedEyetrackData.Rda")
    #save(totaldata,file=paste(datadir,"/JDC15-AggregatedEyetrackData.Rda",sep=""),compress=TRUE)
    
    #unlink("ISL2014BASELINE-AggregatedEyetrackData.Rda")
    
    totaldata
    
}


