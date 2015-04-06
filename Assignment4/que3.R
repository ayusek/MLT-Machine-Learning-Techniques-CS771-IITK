#Name : Ayush Sekhari
#Roll No : 12185

library('e1071')
library('caret')


#- best performance: 0.0561504 

#I am scaling the data here
setwd('/Users/ayusek/Desktop/Academics/MLT/MLT-Machine-Learning-Techniques-CS771-IITK/Assignment4') #Need to be changed accordingly
mydata = read.csv('spambase.data',header = FALSE)
model = tune(svm, V58~., data=mydata, scale = TRUE , kernel = "radial", tunecontrol = tune.control(sampling = c("cross") , cross = 5) , ranges = list(gamma = (1:10)/100, cost = 2^(0:4) , epsilon = (1:10)/10) )

##- best parameters:
##gamma cost epsilon
#0.02    4     0.1

# A more specialised search 
#  best performance: 0.05765552 

model1 = tune(svm, V58~., data=mydata, scale = TRUE , kernel = "radial", tunecontrol = tune.control(sampling = c("cross") , cross = 5) , ranges = list(gamma = (15:25)/1000, cost = (3:8) , epsilon = 5*(1:4)/100) )

#- best parameters:
#    gamma cost epsilon
#0.024    4    0.05

#- best performance: 0.05786254 

model3 = tune(svm, V58~., data=mydata, scale = TRUE , kernel = "radial", tunecontrol = tune.control(sampling = c("cross") , cross = 5) , ranges = list(gamma = (23:30)/1000, cost = 4 , epsilon = (1:10)/100 ))
#- best parameters:
#    gamma cost epsilon
#0.028    4    0.06

#- best performance: 0.0561504 

