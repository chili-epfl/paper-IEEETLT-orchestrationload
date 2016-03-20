# This function takes a csv file with the time windows to extract from a video,
# and generates a bash script that extracts snippets of those time windows from a 
# whole-session video file (using mencoder), named [session]-videoexport.avi
generateVideoSnippetScript <- function(timesFile="./TimesToExtractVideo-ISL.csv",videoDir = ".",window=10){
    
    snippetData <- read.csv(timesFile,sep=",")
    
    sessions <- unique(snippetData$Session)
    
    originalDir <- getwd()
    setwd(videoDir)
    
    fileConn<-file("extractSnippets.sh")
    
    lines <- "#!/bin/bash"
    
    for (i in 1:nrow(snippetData)){
        Window.Center <- snippetData[i,"Window.Center"]
        
        startTime <- Window.Center - (window/2)*1000
        endTime <- Window.Center + (window/2)*1000
        
        originalFile <- paste(snippetData$Session[[i]],"-videoexport.avi",sep="")
        #print(paste("Processing video... ",originalFile))
        #if(!file.exists(originalFile)){
        #    print("Missing video!")
        #}
        
        snippetLabel <- paste(snippetData$Session[[i]],"-snippet",i,sep="")
        
        command <- paste("mencoder -ss ",msToHMS(startTime)," -endpos 00:00:10 -oac copy -ovc copy ",originalFile," -o ",snippetLabel,".avi\n", sep="")
        lines[i+1] <- command
        #command2 <- paste("ffmpeg -ss ",msToHMS(startTime)," -t 00:00:10 -i ",originalFile," -vcodec copy -acodec copy ",snippetLabel,".avi", sep="")
        #system(command)
    }
    
    writeLines(lines,fileConn)
    
    close(fileConn)
    
    setwd(originalDir)
    #print(paste("Check for your extract command in",videoDir))
}


msToHMS <- function(ms){
    H <- floor(ms/(60*60*1000))
    M <- floor((ms-(H*60*60*1000))/(60*1000))
    S <- floor((ms-(H*60*60*1000)-(M*60*1000))/1000)
    
    
    H <- formatC(H, width = 2, format = "d", flag = "0")
    M <- formatC(M, width = 2, format = "d", flag = "0")
    S <- formatC(S, width = 2, format = "d", flag = "0")
    
    paste(H,M,S,sep=":")
}
