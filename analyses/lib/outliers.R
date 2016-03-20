# Counts the number of outliers, according to the criteria specified
# returns a data vector of the same length, but with the outliers (defined as x times the sd, or x times the inter-quartile range)
countOutliers <- function(data, coef=3, method="sd"){
    newdata <- data
    count <- 0
    # Method based on the inter quartile range IQR (as in boxplot)
    if(method=="iqr"){
        ind <- boxplot.stats(newdata, coef=coef)
        count <- sum(newdata < ind$stats[1] | newdata > ind$stats[5]) 
    }else if(method=="sd"){
        # Alternative method: coef is the number of SDs around the mean for defining an outlier
        count <- sum(newdata < mean(newdata, na.rm=T)-coef*sd(newdata, na.rm=T) | newdata > mean(newdata, na.rm=T)+coef*sd(newdata, na.rm=T))
    }else{
        stop('Invalid method!')
    }
    count
}

# Remove outliers, substituting them with NAs or a certain value (e.g., the max/min of the outlier range)
# returns a data vector of the same length, but with the outliers 
# (defined as x times the sd, or x times the inter-quartile range)
replaceOutliers <- function(data, valueNA=T, coef=3, method="sd"){
    newdata <- data
    # Method based on the inter quartile range IQR (as in boxplot)
    if(method=="iqr"){
        ind <- boxplot.stats(newdata, coef=coef)
        if(valueNA) newdata[newdata < ind$stats[1] | newdata > ind$stats[5]] <- NA
        else{
            newdata[newdata < ind$stats[1]] <- ind$stats[1]
            newdata[newdata > ind$stats[5]] <- ind$stats[1]
        }
    }else if(method=="sd"){
        # Alternative method: coef is the number of SDs around the mean for defining an outlier
        if(valueNA) newdata[newdata < mean(newdata, na.rm=T)-coef*sd(newdata, na.rm=T) | newdata > mean(newdata, na.rm=T)+coef*sd(newdata, na.rm=T)] <- NA
        else{
            newdata[newdata < mean(newdata, na.rm=T)-coef*sd(newdata, na.rm=T)] <- (mean(newdata, na.rm=T)-coef*sd(newdata, na.rm=T))            
            newdata[newdata > mean(newdata, na.rm=T)+coef*sd(newdata, na.rm=T)] <- (mean(newdata, na.rm=T)+coef*sd(newdata, na.rm=T))
        }
    }else{
        stop('Invalid method!')
    }
    newdata
}
