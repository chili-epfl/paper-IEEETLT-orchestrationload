# Gets a ELAN xml file and extracts the annotations, putting them in a clean dataframe
preprocessVideoCoding <- function(datadir, sessions){
    
    
    annotationsData <- data.frame()
    cleandatafile <- "cleanAnnotationData.Rda"
    
    if(!file.exists(paste(datadir,cleandatafile,sep=.Platform$file.sep))){
        
        for (session in sessions){
            
            # We load the video coding data
            # Parse the XML
            rawXML <- xmlParse(paste(datadir,.Platform$file.sep,session,"-videoCoding.eaf",sep=""))
            parsedXML <- xmlToList(rawXML)
            # Extract the time markers for the annotations, to be used later
            annotationTimes <- data.frame(matrix(unlist(parsedXML[["TIME_ORDER"]]), nrow=length(parsedXML[["TIME_ORDER"]]), byrow=T))
            names(annotationTimes) <- c("TIME_SLOT_ID", "TIME_VALUE")
            
            # We get the data for each Annotation Tier
            comments <- data.frame(start=numeric(), end=numeric(), annotation=character())
            social <- data.frame(start=numeric(), end=numeric(), annotation=character())
            experimental <- data.frame(start=numeric(), end=numeric(), annotation=character())
            activity <- data.frame(start=numeric(), end=numeric(), annotation=character())
            focus <- data.frame(start=numeric(), end=numeric(), annotation=character())
            recording <- data.frame(start=numeric(), end=numeric(), annotation=character())
            for (tier in parsedXML[names(parsedXML) %in% "TIER"]){
                if(class(tier) != "list") next; #If it is an empty tier, it will not be a list, we just pass
                
                tierName <- tier$.attrs["TIER_ID"]
                
                if(tierName=="Comments"){# The Comments tier
                    for(annot2 in tier[names(tier) %in% "ANNOTATION"]){
                        annot <- annot2$ALIGNABLE_ANNOTATION
                        annotation <- data.frame(start=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF1"],"TIME_VALUE"])), 
                                                 end=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF2"],"TIME_VALUE"])),
                                                 annotation=annot$ANNOTATION_VALUE)
                        comments <- rbind(comments,annotation)
                    }
                }
                
                if(tierName=="Recording markers"){# Recording markers tier
                    for(annot2 in tier[names(tier) %in% "ANNOTATION"]){
                        annot <- annot2$ALIGNABLE_ANNOTATION
                        annotation <- data.frame(start=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF1"],"TIME_VALUE"])), 
                                                 end=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF2"],"TIME_VALUE"])),
                                                 annotation=annot$ANNOTATION_VALUE)
                        recording <- rbind(recording,annotation)
                    }
                }
                
                if(tierName=="Activity"){# Activities tier
                    for(annot2 in tier[names(tier) %in% "ANNOTATION"]){
                        annot <- annot2$ALIGNABLE_ANNOTATION
                        annotation <- data.frame(start=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF1"],"TIME_VALUE"])), 
                                                 end=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF2"],"TIME_VALUE"])),
                                                 annotation=annot$ANNOTATION_VALUE)
                        activity <- rbind(activity,annotation)
                    }
                }
                
                if(tierName=="Social plane"){# social plane tier
                    for(annot2 in tier[names(tier) %in% "ANNOTATION"]){
                        annot <- annot2$ALIGNABLE_ANNOTATION
                        annotation <- data.frame(start=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF1"],"TIME_VALUE"])), 
                                                 end=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF2"],"TIME_VALUE"])),
                                                 annotation=annot$ANNOTATION_VALUE)
                        social <- rbind(social,annotation)
                    }
                }
                
                if(tierName=="Experimental part"){# Experimental part tier
                    for(annot2 in tier[names(tier) %in% "ANNOTATION"]){
                        annot <- annot2$ALIGNABLE_ANNOTATION
                        annotation <- data.frame(start=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF1"],"TIME_VALUE"])), 
                                                 end=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF2"],"TIME_VALUE"])),
                                                 annotation=annot$ANNOTATION_VALUE)
                        experimental <- rbind(experimental,annotation)
                    }
                }

                if(tierName=="Main Focus"){# Main focus tier
                    for(annot2 in tier[names(tier) %in% "ANNOTATION"]){
                        annot <- annot2$ALIGNABLE_ANNOTATION
                        if(!is.null(annot$ANNOTATION_VALUE)){
                            annotation <- data.frame(start=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF1"],"TIME_VALUE"])), 
                                                     end=as.numeric(as.character(annotationTimes[annotationTimes$TIME_SLOT_ID==annot$.attrs["TIME_SLOT_REF2"],"TIME_VALUE"])),
                                                     annotation=annot$ANNOTATION_VALUE)
                            focus <- rbind(focus,annotation)
                        }
                    }
                }

            }
            
            #Add tier field to all data frames, and put them together, and add the session field too
            if(nrow(comments)>0) comments$tier = "Comments"
            if(nrow(recording)>0) recording$tier = "Recording"
            if(nrow(activity)>0) activity$tier = "Activity"
            if(nrow(social)>0) social$tier = "Social"
            if(nrow(experimental)>0) experimental$tier = "Experimental"
            if(nrow(focus)>0) focus$tier = "Focus"
            
            sessionAnnotations = rbind(comments,recording,activity,social,experimental,focus)
            sessionAnnotations$session = session
            # Join all sessions in a single dataframe
            if(nrow(annotationsData)==0) annotationsData = sessionAnnotations
            else annotationsData = rbind(annotationsData,sessionAnnotations)
            
        }
        
        save(annotationsData, file = paste(datadir,cleandatafile,sep=.Platform$file.sep))
        
        
    }else{
        annotationsData <- get(load(paste(datadir,cleandatafile,sep=.Platform$file.sep)))
    }
    # annotationsData now has the clean video coding data
    
    annotationsData
    
}