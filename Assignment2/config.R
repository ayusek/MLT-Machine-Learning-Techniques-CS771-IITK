setwd('/Users/ayusek/Desktop/Academics/MLT/MLT-Machine-Learning-Techniques-CS771-IITK/Assignment2') #Need to be changed accordingly 

set.seed(2) #Setting a seed for deterministic evaluation

mydata = read.csv('letter-recognition.data.txt',header = FALSE)
#I am not giving the column names initially

problem = 1
m_a = 4 #I have taken m = 4 for part a 