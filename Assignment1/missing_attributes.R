   
if (Missing_Attribute_Test == "UseNA"){
        test[,2:8][test[,2:8] == 0] <- NA # Replaces 0 (missing data) with NA ; Doing replacement in columsn 2 to 8 only
    } else if (Missing_Attribute_Test == "UseMean"){ 
        training_avg_Values1 = colMeans(train[train$Label == 1,] , na.rm = TRUE)
        training_avg_Values0 = colMeans(train[train$Label == 0,] , na.rm = TRUE)
        
        for (i in 2:8){
        #Replace test missing data with train data means 
        test[test$Label == 1,][i][test[test$Label == 1,][i] == 0] <- training_avg_Values1[i]
        test[test$Label == 0,][i][test[test$Label == 0,][i] == 0] <- training_avg_Values0[i]
        }
    }
  
        
    
    if (Missing_Attribute_Train == "UseNA"){
        train[,2:8][train[,2:8] == 0] <- NA # Replaces 0 (missing data) with NA ; Doing replacement in columsn 2 to 8 only
    }else if (Missing_Attribute_Train == "UseMean"){ 
        #Replacing missing values by class means
        train[train$Label == 1,][,2:8] <- apply(train[train$Label == 1,][,2:8] , 2 , function(x) {x[x==0] <- mean(x,na.rm = TRUE); x})
        train[train$Label == 0,][,2:8] <- apply(train[train$Label == 0,][,2:8] , 2 , function(x) {x[x==0] <- mean(x,na.rm = TRUE); x})
    }
   
    