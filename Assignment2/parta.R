library("randomForest")
library("caret")
setwd('/Users/ayusek/Desktop/Academics/MLT/MLT-Machine-Learning-Techniques-CS771-IITK/Assignment2')#Need to be changed accordingly

source('config.R')
source('functions.R')


#I would be doing binary search for 2 to 450 tress
if(problem == 1){
    
    no_of_trees = numeric(0)
    error_n = numeric(0)
    
    start = 2
    end = 450
    
    #Initializing vectors
    min_error = getCVerror(end)
    no_of_trees = c(no_of_trees , end)
    error_n = c(error_n , min_error)
    no_of_trees = c(no_of_trees , start)
    error_n = c(error_n , getCVerror(start))
    
repeat {
    mid = round((start + end)/2)
    mid_error = getCVerror(mid)
    
    #making vectors for plotting
    no_of_trees = c(no_of_trees , mid)
    error_n = c(error_n , mid_error)
    
    if(mid == start || mid == end)
        break
    
    if((0.045*min_error <= (mid_error - min_error)) && ((mid_error - min_error) <= 0.05*min_error))
        break #Mid is the point of interest
    #Iterate till you get to optimal range
    if((mid_error - min_error) > 0.05*min_error)
        start = mid
    if((mid_error - min_error) < 0.045*min_error)
        end = mid
    
    print(mid)
}

    y = no_of_trees
    x = error_n

    z = order(y)
    y = y[z]
    x = x[z]*100

    plot(y , x , xlab="No of Trees" , ylab = "Error" , col=ifelse(y==198, "red", "black"))
}

#Plotting the error vs number of trees
if(problem == 1.5)
{
    trees = seq(2,452,25)
    error_tree = numeric(0)
    for (i in trees){
        error_tree = c(error_tree, getCVerror(i))  
        print(i) 
    }
    
    for (i in seq(3,25,5)){
        error_tree = c(error_tree, getCVerror(i))  
        trees = c(trees , i)
        print(i) 
    }
    
    plot(trees[order(trees)] , error_tree[order(trees)] , xlab="No of Trees" , ylab = "Error" , type = 'b')
    grid()
}

if(problem == 2){
    
    #Finding OOB errors
    number_of_trees = 198
    forest <- randomForest(V1 ~. , data = mydata , ntree = number_of_trees ,  replace = TRUE) #Bagging Done and sqrt(16) = 4 attributes used
    
    print(forest$err.rate[198,1])

}

if(problem == 3){
    number_of_trees = 1.25*198
    for (mval in seq(1,8,1)){
        print(paste("{\bf m =", mval ,"} & " , getCVerror(number_of_trees , mval)))    
    }
    
}

if(problem == 4){
    print("Start")
    range = seq(10 , 80 , 5)
    error_d = numeric(0)
    for (size in range){
        error = getCutoffTreeNumber(size)[2]
        error_d = c(error_d, error)
        print(size)
        print(error)
    }
    plot(range, error_d*100 , type = "b" , xlab="percentage of L used in sample size" , ylab="Error Rate")
    text_list = paste0("(" , range , "," , round(error_d,4) , ")")
    grid()
    text(range , error_d*100 , text_list , pos = 3 , cex = 0.4)
    
}
