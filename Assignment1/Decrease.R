library("party")
library("rpart")
library("caret")

setwd('/Users/ayusek/Desktop/Academics/MLT/MLT-Machine-Learning-Techniques-CS771-IITK/Assignment1') #Need to be changed accordingly 

source("config.R")

rows = length(mydata[[1]])
flds <- createFolds(1:rows, k = 5, list = TRUE, returnTrain = FALSE)

#print(paste("{\bf","Data" ,"}", "&" ,"{\bf",  "accuracy","}" ,"&" ,"{\bf",  "precision" ,"}", "&","{\bf", "recall" ,"}","&" ,"{\bf", "TP Rate" ,"}","&", "{\bf","FP Rate" ,"}","&", "{\bf","F1 Score","}"))

cp_list = seq(0.001 , 0.2 , 0.0005) #with a veriation of 0.0005
accuracy_list = numeric(length(cp_list))

for (i in 1:5){
    test = mydata[flds[[i]] , ]
    train = mydata[setdiff(1:rows , flds[[i]]) ,  ]
    
    source("missing_attributes.R")   
    
    for (j in 1:length(cp_list)){
        #Making a decision tree with different values of minsplit parameter
        decision_tree <- rpart(Label ~ . , data=train , method="class" ,parms = list(split = Impurity_Function) , control =rpart.control(cp = cp_list[j] , minsplit = 1 ))
        prediction = predict(decision_tree , test , type = "class")
        actual = test[[9]]
        TP = length(which(prediction == 1 & actual == 1))
        FP = length(which(prediction == 1 & actual == 0))
        FN = length(which(prediction == 0 & actual == 1))
        TN = length(which(prediction == 0 & actual == 0))
        P = TP + FN 
        N = FP + TN
        
        accuracy_list[j] <- (TP + TN )/(P+N)
    }
    
    #plot(cp_list , accuracy_list)
    best_cp = cp_list[which.max(accuracy_list)]
    decision_tree <- rpart(Label ~ . , data=train , method="class" ,parms = list(split = Impurity_Function) , control =rpart.control(minsplit = 1 , cp = best_cp ))
    source("compute.R")
}

#Overall Results
print(paste("{\bf I =", Impurity_Function , ", MATr =", Missing_Attribute_Train , ", MATe =", Missing_Attribute_Test , " } & {\bf",format(round(accuracy/5,3),3),"}" ,"&" ,"{\bf",  format(round(precision/5,3),3) ,"}", "&", "{\bf",format(round(recall/5,3),3),"}" ,"&" , "{\bf",format(round(TP_Rate/5,3),3),"}" ,"&","{\bf", format(round(FP_Rate/5,3),3),"}" ,"&","{\bf", format(round(F1/5,3),3),"}"))


