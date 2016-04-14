library(dummies)


predmodel <- function(model,data,cols){
    preds <- numeric()
    for(row in 1:nrow(data)){
        sum <- model$coefficients["(Intercept)"]
        for(i in cols){
            coef <- model$coefficients[names(data[row,])[i]]
            if(!is.na(coef)){
                sum <- sum+(coef*data[row,i])   
            }
        }
        preds[row] <- sum        
    }
    
    preds
}

getModelError <- function(data,factorcolnames,model,targetvar){

    origncol <- ncol(data)    
    data2 <- data
    
    for(name in factorcolnames){
        data2 <- cbind(data2,data.frame(dummy(name,data2)))
    }
    finalncol <- ncol(data2)
    
    resid <- data2[,targetvar] - predmodel(model,data2,origncol+1:finalncol)
    
    resid
}
