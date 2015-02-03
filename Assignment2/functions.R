getCVerror <- function(n , m = m_a) {
    flds <- createFolds(1:length(mydata[[1]]), k = 5, list = TRUE, returnTrain = FALSE)
    error_sum = 0
    for (i in 1:5){
        test = mydata[flds[[i]] , ]
        train = mydata[-flds[[i]] ,  ]
        
        #bagging is done here
        forest <- randomForest(V1 ~. , data = train , xtest = test[,c(2:17)] , ytest = test[,1] , ntree = n , mtry = m, sampsize = length(train[[1]]), replace = TRUE) #Bagging Done and 4 attributes used
        error_sum = error_sum + 1 - (sum(diag(forest$test$confusion))/length(test[[1]]))
    }
    return(error_sum/5)
}

getCVerrorNoBagging <- function(n , Data_size) {
    
    flds <- createFolds(1:length(mydata[[1]]), k = 5, list = TRUE, returnTrain = FALSE)
    error_sum = 0
    for (i in 1:5){
        test = mydata[flds[[i]] , ]
        train = mydata[-flds[[i]] ,  ]
        rows = length(train[[1]])
        
        forest <- randomForest(V1 ~. , data = train , xtest = test[,c(2:17)] , ytest = test[,1],  ntree = n, sampsize = 160*Data_size, replace = FALSE,  mtry = 16) #Bagging Done and 4 attributes used
        error_sum = error_sum + 1 - (sum(diag(forest$test$confusion))/length(test[[1]]))
    }
    return(error_sum/5) #5 fold cross validation
}

getCutoffTreeNumber <- function(Data_size) {
    
    start = 2
    end = 450
    
    min_error = getCVerrorNoBagging(end , Data_size )
    
    repeat {
        mid = round((start + end)/2)
        mid_error = getCVerrorNoBagging(mid , Data_size)
        
        if(mid == start || mid == end)
            break
        if((0.045*min_error <= (mid_error - min_error)) && ((mid_error - min_error) <= 0.05*min_error))
            break 
        #Mid is the point of interest
        #Iterate till you get to optimal range
        if((mid_error - min_error) > 0.05*min_error)
            start = mid
        if((mid_error - min_error) < 0.045*min_error)
            end = mid
    }
    
    return(c(mid , mid_error))
    
}