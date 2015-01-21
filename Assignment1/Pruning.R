library("party")
library("rpart")
source("config.R")
source("missing_attributes.R")

rows = length(mydata[[1]])
flds <- createFolds(1:rows, k = 5, list = TRUE, returnTrain = FALSE)

#print(paste("{\bf","Data" ,"}", "&" ,"{\bf",  "accuracy","}" ,"&" ,"{\bf",  "precision" ,"}", "&","{\bf", "recall" ,"}","&" ,"{\bf", "TP Rate" ,"}","&", "{\bf","FP Rate" ,"}","&", "{\bf","F1 Score","}"))

cp_list = seq(0 , 0.025 , 0.001)
average_accuracy = numeric(length(cv_list))

#for (j in 1:length(cv_list)){
 #   accuracy = 0
for (i in 1:5){
    test = mydata[flds[[i]] , ]
    train = mydata[setdiff(1:rows , flds[[i]]) ,  ]
    
    source("missing_attributes.R")   
    
    fit <- rpart(Label ~ . , data=train , method="class" ,parms = list(split = Impurity_Function) , control =rpart.control(cp = 0 , minsplit = 1))
    #printcp(decision_tree)
    #plotcp(fit)  #To isualise the error with respect to cp 
    #decision_tree <- prune(fit , cp = cp_list[j])
    decision_tree<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
    #post(decision_tree , file="tree.ps" , use.n = FALSE , pretty = TRUE ,  title="Classification Tree for Predicting Diabetes", horizontal = FALSE)
    
    source("compute.R") #Added computations to another file to avaoid confusion 
}
    average_accuracy[j] <- accuracy/5
#}


#Overall Results
print(paste("{\bf I =", Impurity_Function , ", MATr =", Missing_Attribute_Train , ", MATe =", Missing_Attribute_Test , " } & {\bf",format(round(accuracy/5,3),3),"}" ,"&" ,"{\bf",  format(round(precision/5,3),3) ,"}", "&", "{\bf",format(round(recall/5,3),3),"}" ,"&" , "{\bf",format(round(TP_Rate/5,3),3),"}" ,"&","{\bf", format(round(FP_Rate/5,3),3),"}" ,"&","{\bf", format(round(F1/5,3),3),"}"))


