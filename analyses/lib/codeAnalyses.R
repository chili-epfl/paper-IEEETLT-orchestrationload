require(stringr)

flattenCodeData <- function(data, codecol, additionalcol=NULL){
    
    codes <- unique(str_trim(unlist(strsplit(as.character(data[,codecol]),split = ",",fixed = T))))
    
    # We go over the data, and create a new dataframe
    codecount <- data.frame()
    for(i in 1:nrow(data)){
        episode <- data[i,]
        episodecodes <- unique(str_trim(unlist(strsplit(as.character(episode[,codecol]),split = ",",fixed = T))))
        if(length(episodecodes)>0){
            # For each code in the episode, we create a new row in the plain data            
            for(code in episodecodes){
                if(nrow(codecount)==0){
                    codecount <- data.frame(Episode.ID = i, Code = code) 
                    if(!is.null(additionalcol)){
                        for(col in additionalcol){
                            codecount[,names(episode)[col]] <- episode[,col]
                        }
                    }
                } 
                else{
                    newrow <- data.frame(Episode.ID = i,
                                         Code = code)
                    if(!is.null(additionalcol)){
                        for(col in additionalcol){
                            newrow[,names(episode)[col]] <- episode[,col]
                        }
                    }
                    codecount <- rbind(codecount,newrow)
                }
            }
        }
    }    
    
    codecount
}

