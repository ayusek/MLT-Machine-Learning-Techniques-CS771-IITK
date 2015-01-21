prediction = predict(decision_tree , test , type = "class")
actual = test[[9]]
TP = length(which(prediction == 1 & actual == 1))
FP = length(which(prediction == 1 & actual == 0))
FN = length(which(prediction == 0 & actual == 1))
TN = length(which(prediction == 0 & actual == 0))
P = TP + FN 
N = FP + TN

#print(paste("CV Set",i,"&", format(round((TP + TN )/(P+N),3),3) ,"&" ,  format(round(TP/(TP + FP),3),3) , "&", format(round(TP/(TP + FN),3),3) ,"&" , format(round(TP/P,3),3) ,"&", format(round(FP/N,3),3) ,"&", format(round(2*TP/(2*TP + FP + FN),3),3)))

accuracy = accuracy +   (TP + TN )/(P+N)
precision = precision + TP/(TP + FP)
recall = recall + TP/(TP + FN)
TP_Rate = TP_Rate + TP/P
FP_Rate = FP_Rate + FP/N
F1 = F1 + 2*TP/(2*TP + FP + FN)
