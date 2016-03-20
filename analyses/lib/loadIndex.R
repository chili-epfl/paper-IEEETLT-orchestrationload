# Given a dataset with a number of load-related metrics, marked by columns, 
# it calculates the coarse and fine load indices
calculateCoarseFineLoadIndex <- function(data, loadcolumns, normalize=F, inversecols=NULL, stablenorm=3){
    
    sessions <- unique(data$session)
    
    totaldata <- data.frame()
    
    for(session in sessions){
        
        #We select the session data
        sessiondata <- data[data$session == session,]
        
        #For each load-metric column
        colnamecuts <- character()
        colnameperc <- character()
        for(col in loadcolumns){
            colname <- names(sessiondata)[[col]]
            
            #If normalization is needed, we normalize by the first window value
            #Alternative: Use the average of the first X episodes instead (i.e., =1 for the old method), to make it more stable?
            colnamenorm <- ""
            if(normalize){
                colnamenorm <- paste(colname,".norm",sep="")
                normal <- mean(sessiondata[1:stablenorm,col], na.rm = T)
                if(normal!=0) sessiondata[,colnamenorm] <- sessiondata[,col]/normal
                else sessiondata[,colnamenorm] <- sessiondata[,col] # If it is the rare case of the median being a zero count
            }
            
            #Calculate the median of the session, and the median cut
            colnamecut <- paste(colname,".above",sep="")
            medsession <- 0
            if(normalize){
                medsession <- median(sessiondata[,colnamenorm], na.rm=T)   
                sessiondata[,colnamecut] <- as.numeric(sessiondata[,colnamenorm] > medsession)
            }
            else{
                medsession <- median(sessiondata[,col], na.rm=T)   
                sessiondata[,colnamecut] <- as.numeric(sessiondata[,col] > medsession)
            }
            colnamecuts <- append(colnamecuts, colnamecut)
            
            #Calculate the percentile within the session
            colnamep <- paste(colname,".percent",sep="")
            percentile <- 0
            if(normalize){
                percentile <- ecdf(sessiondata[,colnamenorm])
                sessiondata[,colnamep] <- percentile(sessiondata[,colnamenorm])*100
            }
            else{
                percentile <- ecdf(sessiondata[,col])
                sessiondata[,colnamep] <- percentile(sessiondata[,col])*100
            }
            colnameperc <- append(colnameperc, colnamep)
            
        }

        if(!is.null(inversecols)){
            
            for(col in inversecols){
                colname <- names(sessiondata)[[col]]
                
                #If normalization is needed, we normalize by the first window value
                colnamenorm <- ""
                if(normalize){
                    colnamenorm <- paste(colname,".norm",sep="")
                    if(sessiondata[1,col]!=0) sessiondata[,colnamenorm] <- sessiondata[,col]/sessiondata[1,col]
                    else sessiondata[,colnamenorm] <- sessiondata[,col] # If it is the rare case of the median being a zero count
                }
                
                #Calculate the median of the session, and the median cut
                colnamecut <- paste(colname,".below",sep="")
                medsession <- 0
                if(normalize){
                    medsession <- median(sessiondata[,colnamenorm], na.rm=T)   
                    sessiondata[,colnamecut] <- as.numeric(sessiondata[,colnamenorm] < medsession)
                }
                else{
                    medsession <- median(sessiondata[,col], na.rm=T)   
                    sessiondata[,colnamecut] <- as.numeric(sessiondata[,col] < medsession)
                }
                colnamecuts <- append(colnamecuts, colnamecut)
                
                #Calculate the percentile within the session
                colnamep <- paste(colname,".ipercent",sep="")
                percentile <- 0
                if(normalize){
                    percentile <- ecdf(sessiondata[,colnamenorm])
                    sessiondata[,colnamep] <- 100-(percentile(sessiondata[,colnamenorm])*100)
                }
                else{
                    percentile <- ecdf(sessiondata[,col])
                    sessiondata[,colnamep] <- 100-(percentile(sessiondata[,col])*100)
                }
                colnameperc <- append(colnameperc, colnamep)
                
            }

        }
                
        # We add up the median cuts, to get the coarse index
        sessiondata$CoarseLoad <- apply(sessiondata[,colnamecuts],1,sum)
                
        # We average the percentiles, to get the fine index
        sessiondata$FineLoad <- apply(sessiondata[,colnameperc],1,mean, na.rm = T)
        
        # We paste the session data together
        if(nrow(totaldata)==0) totaldata <- sessiondata
        else totaldata <- rbind(totaldata,sessiondata)
    }
    
    totaldata
}

# This function gets a data frame with eye metrics for 10s windows
# in a session, and calculates the Load Index for each window in that session,
# returning the same dataset with added columns
# Since in some cases the field labels may vary, they can also be specified explicitly
calculateLoadIndexSession <- function(data, meanlabel="value.pupilMean", sdlabel="value.pupilSD", fixlabel="value.longFix", saclabel="value.sacSpd"){
    
    # We find out the median value for each parameter, and do the median cut
    meansessionmed <- median(data[,meanlabel], na.rm=T)
    data$Above.Mean <- as.numeric(data[,meanlabel] > meansessionmed)
    sdsessionmed <- median(data[,sdlabel], na.rm=T)
    data$Above.SD <- as.numeric(data[,sdlabel] > sdsessionmed)
    longsessionmed <- median(data[,fixlabel], na.rm=T)
    data$Above.Fix <- as.numeric(data[,fixlabel] > longsessionmed)
    sacsessionmed <- median(data[,saclabel], na.rm=T)
    data$Above.Sac <- as.numeric(data[,saclabel] > sacsessionmed)
    # We calculate the Load Index simply summing the different median cuts
    data$Load <- data$Above.Mean + data$Above.SD + data$Above.Fix + data$Above.Sac
    
    data
}


# This function gets a data frame with eye metrics for 10s windows
# in a session, normalizes each session data with that of the first episode
# in the session, and calculates the Load Index for each window in that session,
# using the median, not of that session, but of a "session of reference".
# Returns the same dataset with added columns
# Since in some cases the field labels may vary, they can also be specified explicitly
calculateLoadIndexOtherSessionNormalized <- function(data, refsession, meanlabel="value.pupilMean", sdlabel="value.pupilSD", fixlabel="value.longFix", saclabel="value.sacSpd"){

    # We normalize the data for each session with that of the first episode
    normdata <- data.frame()
    for(session in sessions){

        sessdata <- data[data$Session == session,]
        
        normsessdata <- sessdata
        normsessdata[,meanlabel] <- sessdata[,meanlabel]/sessdata[1,meanlabel]
        normsessdata[,sdlabel] <- sessdata[,sdlabel]/sessdata[1,sdlabel]
        normsessdata[,fixlabel] <- sessdata[,fixlabel]/sessdata[1,fixlabel]
        normsessdata[,saclabel] <- sessdata[,saclabel]/sessdata[1,saclabel]
        
        # We join the new data into a dataset with all sessions normalized data
        if(length(normdata)==0) normdata <- normsessdata
        else normdata <- rbind(normdata,normsessdata)
    }  
    
    # We calculate the session of reference's (normalized) medians
    refdata <- normdata[normdata$Session == refsession,]
    meansessionmed <- median(refdata[,meanlabel], na.rm=T)
    sdsessionmed <- median(refdata[,sdlabel], na.rm=T)
    longsessionmed <- median(refdata[,fixlabel], na.rm=T)
    sacsessionmed <- median(refdata[,saclabel], na.rm=T)
    
    
    # We calculate the load indices using the reference medians
    normdata$Above.Mean <- as.numeric(normdata[,meanlabel] > meansessionmed)
    normdata$Above.SD <- as.numeric(normdata[,sdlabel] > sdsessionmed)
    normdata$Above.Fix <- as.numeric(normdata[,fixlabel] > longsessionmed)
    normdata$Above.Sac <- as.numeric(normdata[,saclabel] > sacsessionmed)
    # We calculate the Load Index simply summing the different median cuts
    normdata$Load <- normdata$Above.Mean + normdata$Above.SD + normdata$Above.Fix + normdata$Above.Sac
    
    normdata
}

# This function gets a data frame with eye metrics for 10s windows
# in a session, normalizes each session data with that of the first episode
# in the session, and calculates the Load Index for each window in that session,
# using the median, not of that session, but of a "session of reference".
# Returns the same dataset with added columns
# Since in some cases the field labels may vary, they can also be specified explicitly
calculateLoadIndexOtherSessionNormalized8vars <- function(data, refsession, meanlabel="value.Mean", sdlabel="value.SD", fixlabel="value.Fix", saclabel="value.Sac", attlabel="value.Attention", thetalabel="value.Theta", jerkmeanlabel="value.Jerk.Mean", jerksdlabel="value.Jerk.SD"){
    
    # We normalize the data for each session with that of the first episode
    normdata <- data.frame()
    sessions <- levels(data$session)
    for(session in sessions){
        
        sessdata <- data[data$session == session,]
        
        normsessdata <- sessdata
        normsessdata[,meanlabel] <- sessdata[,meanlabel]/sessdata[1,meanlabel]
        normsessdata[,sdlabel] <- sessdata[,sdlabel]/sessdata[1,sdlabel]
        
        if(sessdata[1,fixlabel]!=0) normsessdata[,fixlabel] <- sessdata[,fixlabel]/sessdata[1,fixlabel]
        else normsessdata[,fixlabel] <- sessdata[,fixlabel] # If it is the rare case of the median being a zero count

        normsessdata[,saclabel] <- sessdata[,saclabel]/sessdata[1,saclabel]

        normsessdata[,attlabel] <- sessdata[,attlabel]/sessdata[1,attlabel]
        normsessdata[,thetalabel] <- sessdata[,thetalabel]/sessdata[1,thetalabel]
        normsessdata[,jerkmeanlabel] <- sessdata[,jerkmeanlabel]/sessdata[1,jerkmeanlabel]
        normsessdata[,jerksdlabel] <- sessdata[,jerksdlabel]/sessdata[1,jerksdlabel]
        
        # We join the new data into a dataset with all sessions normalized data
        if(length(normdata)==0) normdata <- normsessdata
        else normdata <- rbind(normdata,normsessdata)
    }  
    
    # We calculate the session of reference's (normalized) medians
    refdata <- normdata[normdata$session == refsession,]
    meansessionmed <- median(refdata[,meanlabel], na.rm=T)
    sdsessionmed <- median(refdata[,sdlabel], na.rm=T)
    longsessionmed <- median(refdata[,fixlabel], na.rm=T)
    sacsessionmed <- median(refdata[,saclabel], na.rm=T)

    attsessionmed <- median(refdata[,attlabel], na.rm=T)
    thetasessionmed <- median(refdata[,thetalabel], na.rm=T)
    jerkmeansessionmed <- median(refdata[,jerkmeanlabel], na.rm=T)
    jerksdsessionmed <- median(refdata[,jerksdlabel], na.rm=T)
    
    
    # We calculate the load indices using the reference medians
    normdata$Above.Mean <- as.numeric(normdata[,meanlabel] > meansessionmed)
    normdata$Above.SD <- as.numeric(normdata[,sdlabel] > sdsessionmed)
    normdata$Above.Fix <- as.numeric(normdata[,fixlabel] > longsessionmed)
    normdata$Above.Sac <- as.numeric(normdata[,saclabel] > sacsessionmed)

    normdata$Above.Att <- as.numeric(normdata[,attlabel] > attsessionmed)
    normdata$Below.Theta <- as.numeric(normdata[,thetalabel] < thetasessionmed)
    normdata$Above.Jerkmean <- as.numeric(normdata[,jerkmeanlabel] > jerkmeansessionmed)
    normdata$Above.Jerksd <- as.numeric(normdata[,jerksdlabel] > jerksdsessionmed)
    # We calculate the Load Index simply summing the different median cuts
    normdata$Load <- normdata$Above.Mean + normdata$Above.SD + normdata$Above.Fix + normdata$Above.Sac + 
        normdata$Above.Att + normdata$Below.Theta + normdata$Above.Jerkmean + normdata$Above.Jerksd
    
    normdata
}

