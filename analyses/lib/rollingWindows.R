

# We get the number of values over a value in a rolling window with the parameters set when calling the function (everything in ms, or whatever unit the timestamping is on)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingLong <- function(times,values,window,slide,inittime=min(times), endtime=max(times),threshold=500){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rolllong <- data.frame(time=numeric(), value=numeric())
    
    # We calculate the window at least once
    repeat{
        
        tim <- init + (window/2)
        
        val <- sum(times >= init & times <= end & values > threshold)
        
        if(nrow(rolllong)==0) rolllong <- data.frame(time=tim,value=val)
        else rolllong <- rbind(rolllong,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rolllong
    
}


# We get the mean over a rolling window with the parameters set when calling the function (everything in ms, or whatever unit the timestamping is on)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingMean <- function(times,values,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollmean <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        val <- mean(values[times >= init & times <= end])
        
        if(nrow(rollmean)==0) rollmean <- data.frame(time=tim,value=val)
        else rollmean <- rbind(rollmean,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollmean
    
}


# We get the max value over a rolling window with the parameters set when calling the function (everything in ms, or whatever unit the timestamping is on)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingMax <- function(times,values,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollmean <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        val <- max(values[times >= init & times <= end])
        
        if(nrow(rollmean)==0) rollmean <- data.frame(time=tim,value=val)
        else rollmean <- rbind(rollmean,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollmean
    
}




# We get the median over a rolling window with the parameters set when calling the function (everything in ms, or whatever unit the timestamping is on)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingMedian <- function(times,values,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollmean <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        val <- median(values[times >= init & times <= end])
        
        if(nrow(rollmean)==0) rollmean <- data.frame(time=tim,value=val)
        else rollmean <- rbind(rollmean,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollmean
    
}


# We get the PD mean over a rolling window with the parameters set when calling the function (everything in ms, or whatever unit the timestamping is on)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingSd <- function(times,values,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollsd <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        val <- sd(values[times >= init & times <= end])
        
        if(nrow(rollsd)==0) rollsd <- data.frame(time=tim,value=val)
        else rollsd <- rbind(rollsd,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollsd
    
}


# We get the count of values over a certain threshold
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingCountThrsh <- function(times,values,threshold,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollcount <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        win <- values[times >= init & times <= end]
        val <- sum(win > threshold)
        
        if(nrow(rollcount)==0) rollcount <- data.frame(time=tim,value=val)
        else rollcount <- rbind(rollcount,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollcount
    
}


# We get the count of runs of a value being over a certain threshold (e.g., number of high episodes)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingCountRunsThrsh <- function(times,values,threshold,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollcount <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        win <- values[times >= init & times <= end]
        
        run <- rle(as.numeric(win>threshold))
        
        val <- length(run$values[run$values==1])
        
        if(nrow(rollcount)==0) rollcount <- data.frame(time=tim,value=val)
        else rollcount <- rbind(rollcount,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollcount
    
}

# We get the median length of runs of a value being over a certain threshold (e.g., high episodes length in frames)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingMedianRunsThrsh <- function(times,values,threshold,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollcount <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        win <- values[times >= init & times <= end]
        
        run <- rle(as.numeric(win>threshold))
        
        if(length(run$lengths[run$values==1])==0) val <- 0
        else val <- median(run$lengths[run$values==1])
        
        if(nrow(rollcount)==0) rollcount <- data.frame(time=tim,value=val)
        else rollcount <- rbind(rollcount,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollcount
    
}

# We get the SD length of runs of a value being over a certain threshold (e.g., high episodes length in frames)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingSdRunsThrsh <- function(times,values,threshold,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollcount <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        win <- values[times >= init & times <= end]
        
        run <- rle(as.numeric(win>threshold))
        
        if(length(run$lengths[run$values==1])<=1) val <- 0
        else val <- sd(run$lengths[run$values==1])
        
        if(nrow(rollcount)==0) rollcount <- data.frame(time=tim,value=val)
        else rollcount <- rbind(rollcount,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollcount
    
}


# We get the max length of runs of a value being over a certain threshold (e.g., high episodes length in frames)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingMaxRunsThrsh <- function(times,values,threshold,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollcount <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        win <- values[times >= init & times <= end]
        
        run <- rle(as.numeric(win>threshold))
        
        if(length(run$lengths[run$values==1])==0) val <- 0
        else val <- max(run$lengths[run$values==1])
        
        if(nrow(rollcount)==0) rollcount <- data.frame(time=tim,value=val)
        else rollcount <- rbind(rollcount,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollcount
    
}



# We get the median length of runs of a value being BELOW a certain threshold (e.g., low episodes length in frames)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingMedianRunsThrshBelow <- function(times,values,threshold,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollcount <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        win <- values[times >= init & times <= end]
        
        run <- rle(as.numeric(win<=threshold))
        
        if(length(run$lengths[run$values==1])==0) val <- 0
        else val <- median(run$lengths[run$values==1])
        
        if(nrow(rollcount)==0) rollcount <- data.frame(time=tim,value=val)
        else rollcount <- rbind(rollcount,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollcount
    
}

# We get the SD length of runs of a value being BELOW a certain threshold (e.g., low episodes length in frames)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingSdRunsThrshBelow <- function(times,values,threshold,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollcount <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        win <- values[times >= init & times <= end]
        
        run <- rle(as.numeric(win<=threshold))
        
        if(length(run$lengths[run$values==1])<=1) val <- 0
        else val <- sd(run$lengths[run$values==1])
        
        if(nrow(rollcount)==0) rollcount <- data.frame(time=tim,value=val)
        else rollcount <- rbind(rollcount,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollcount
    
}


# We get the max length of runs of a value being BELOW a certain threshold (e.g., high episodes length in frames)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingMaxRunsThrshBelow <- function(times,values,threshold,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollcount <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        win <- values[times >= init & times <= end]
        
        run <- rle(as.numeric(win<=threshold))
        
        if(length(run$lengths[run$values==1])==0) val <- 0
        else val <- max(run$lengths[run$values==1])
        
        if(nrow(rollcount)==0) rollcount <- data.frame(time=tim,value=val)
        else rollcount <- rbind(rollcount,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollcount
    
}



# We get the mean over a rolling window, of values of a column, coditioned to OTHER column being BELOW a certain threshold (e.g., faces values in non-blurry episodes)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingMeanOtherThrshBelow <- function(times,values,valuesthr,threshold,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollmean <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        winvalues <- values[times >= init & times <= end]
        winvaluesthr <- valuesthr[times >= init & times <= end]
        
        if(length(winvalues[winvaluesthr<=threshold])==0) val <- 0
        else val <- mean(winvalues[winvaluesthr<=threshold])
        
        if(nrow(rollmean)==0) rollmean <- data.frame(time=tim,value=val)
        else rollmean <- rbind(rollmean,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollmean
    
}


# We get the SD over a rolling window, of values of a column, coditioned to OTHER column being BELOW a certain threshold (e.g., faces values in non-blurry episodes)
# Returns a data frame with the time of each window (halfway point of each window) and the mean of the points within that window
# times and values should have the same length
rollingSdOtherThrshBelow <- function(times,values,valuesthr,threshold,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollmean <- data.frame(time=numeric(), value=numeric())
    
    repeat{
        
        tim <- init + (window/2)
        
        winvalues <- values[times >= init & times <= end]
        winvaluesthr <- valuesthr[times >= init & times <= end]
        
        if(length(winvalues[winvaluesthr<=threshold])<=1) val <- 0
        else val <- sd(winvalues[winvaluesthr<=threshold])
        
        if(nrow(rollmean)==0) rollmean <- data.frame(time=tim,value=val)
        else rollmean <- rbind(rollmean,c(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollmean
    
}





# We get the longest video code over a rolling window with the parameters set when calling the function (everything in ms, or whatever unit the timestamping is on)
# Returns a data frame with the time of each window (halfway point of each window) and the major video code
rollingCode <- function(annotations,window,slide,inittime=min(times), endtime=max(times)){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollcode <- data.frame(time=numeric(), value=factor(levels=levels(annotations$annotation)))
    
    repeat{
        
        tim <- init + (window/2)
        
        # We get the relevant codes for this window
        relevant <- annotations[annotations$start<end & annotations$end>init,]
        
        # We add up the times for each code
        codetimes = data.frame(value=factor(levels=levels(annotations$annotation)),time=numeric())
        for(i in 1:nrow(relevant)){
            rel <- relevant[i,]
            codestart = ifelse(rel$start < init,init,rel$start) # We chop off the part of the code outside the window
            codeend = ifelse(rel$end > end,end,rel$end) # We chop off the part of the code outside the window
            
            if(nrow(subset(codetimes,value==rel$annotation))==0){# New code in the list
                code = data.frame(value=rel$annotation, time=codeend-codestart)
                codetimes <- rbind(codetimes,code)
            }else{# The code was already in the list
                codetimes[codetimes$value==rel$annotation,"time"] <- codetimes[codetimes$value==rel$annotation,"time"]+(codeend-codestart)
            }
        }
        
        # We order in terms of time, and add the longest running code
        codetimes <- codetimes[order(-codetimes$time),]
        if(nrow(subset(codetimes,time==max(time)))>1) val <- NA # If there is a tie, we add an NA
        else val <- codetimes[1,"value"]
        
        if(nrow(rollcode)==0) rollcode <- data.frame(time=tim,value=val)
        else rollcode <- rbind(rollcode,data.frame(time=tim,value=val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }
    
    rollcode
    
}

# We get the FFT of the samples over a rolling window with the parameters set when calling the function 
# (everything in ms, or whatever unit the timestamping is on), and get the fftcomp first components of such FFT
# Returns a data frame with the time of each window (halfway point of each window) and the fftcomp components
# times and values should have the same length
rollingFFT <- function(times,values,window,slide,inittime=min(times), endtime=max(times), fftcomp=30){
    
    #Limits for the first window
    init <- inittime
    end <- inittime+window
    
    rollfft <- data.frame()
    
    repeat{
        
        tim <- init + (window/2)
        
        subdata <- values[times >= init & times <= end]
        
        # We get the (normalized) fftcomp first components
        N <- length(na.omit(subdata))
        val <- (abs(fft(na.omit(subdata)))/(N/2))[1:fftcomp]
        
        if(nrow(rollfft)==0) rollfft <- data.frame(matrix(c(tim,val),nrow=1))
        else rollfft <- rbind(rollfft,c(tim,val))
        
        init <- init+slide
        end <- end+slide
        
        if(end >= endtime){
            break  
        } 
        
    }

    names(rollfft)[1] <- "time"
    names(rollfft)[2:(fftcomp+1)] <- paste("value",1:fftcomp,sep="")
    
    rollfft
    
}

