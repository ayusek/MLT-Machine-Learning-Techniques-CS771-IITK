setwd('/Users/ayusek/Desktop/Academics/MLT/Assignment1') #Need to be changed accordingly 

set.seed(2) #Setting a seed for deterministic evaluation

mydata = read.csv("pima-indians-diabetes.data.txt",header = FALSE)
#Giving attribute names to imported data 
colnames(mydata) = c("Num_Preg","PGC","BP","Tricept_Thickness","Insulin","BMI","DPF","Age","Label")

#Decision Tree Parameters
#Impurity_Function = "gini"
Impurity_Function = "information"

#Missing_Attribute_Train = "UseNA"
Missing_Attribute_Train = "UseMean"

Missing_Attribute_Test = "UseNA"
#Missing_Attribute_Test = "UseMean"

#Initial Values of Parameters
i = 1
accuracy = 0 
precision = 0 
recall = 0 
TP_Rate = 0
FP_Rate = 0 
F1 = 0 
