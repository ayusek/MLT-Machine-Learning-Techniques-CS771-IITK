#Ayush Sekhari
#Roll No : 12185
#MLT - Assignment 3 
#I am submititng this code file, it also has error statisitics in it for various cases. 
vm

#=========== OVERALL ANALYSIS =================
#Our basic clustering gives an error of 0.1566
#Using the mixture gaussian model and searching around a little, I got the minimal possible error of 0.14 on the test set  in the techqioe when all components are varied simultenously along a grid. 
#lda performs the best here with an error fo 0.136 
#qda gives the error of 0.153
#This shows that a mixture model made using simple and fast clustering is also very good as compared to powerful algorithms like lda and qda

#=========== LIBRARIES USED ==================
library(MASS)
library("mvtnorm")

#=========INITIAL FUNCTIONS ===============
h1 <- function(i){
    return(max(6 - abs(i-11) , 0))
}

h2 <- function(i){
    return(h1(i-4))
}

h3 <- function(i){
    return(h1(i+4))
}

set.seed(1)

# ======== MAKING THE DATA =================
make_data1 <- function(){
random_vector <- runif(600)
class_matrix = matrix(data = 0 , nrow = 600 , ncol = 22)
class_matrix[,22] = 1
for (i in 1:21) {
    randerror <- rnorm(600)
    class_matrix[,i] = random_vector*h1(i) + (1-random_vector)*h2(i) + randerror 
}
return(class_matrix)
}


make_data2 <- function(){
    random_vector <- runif(600)
    class_matrix = matrix(data = 0 , nrow = 600 , ncol = 22)
    class_matrix[,22] = 2
    for (i in 1:21) {
        randerror <- rnorm(600)
        class_matrix[,i] = random_vector*h1(i) + (1-random_vector)*h3(i) + randerror 
    }
    return(class_matrix)
}

make_data3 <- function(){
    random_vector <- runif(600)
    class_matrix = matrix(data = 0 , nrow = 600 , ncol = 22)
    class_matrix[,22] = 3
    for (i in 1:21) {
        randerror <- rnorm(600)
        class_matrix[,i] = random_vector*h2(i) + (1-random_vector)*h3(i) + randerror 
    }
    return(class_matrix)
}


class1_matrix = make_data1()
class2_matrix = make_data2()
class3_matrix = make_data3()

train <- matrix( 0 , nrow = 1200 , ncol = 22)
validation <- matrix( 0 , nrow = 300 , ncol = 22)
test <- matrix( 0 , nrow = 300 , ncol = 22)


#generating the data matrices

train[1:400 , ] <- class1_matrix[1:400 , ]
train[401:800 , ] <- class2_matrix[1:400 , ]
train[801:1200 , ] <- class3_matrix[1:400 , ]

validation[1:100 , ] <- class1_matrix[401:500 , ]
validation[101:200 , ] <- class2_matrix[401:500 , ]
validation[201:300 , ] <- class3_matrix[401:500 , ]

test[1:100 , ] <- class1_matrix[501:600 , ]
test[101:200 , ] <- class2_matrix[501:600 , ]
test[201:300 , ] <- class3_matrix[501:600 , ]



size = length(train[,1])
SIGMA = matrix( 0 , nrow = 21 , ncol = 21)
mus = matrix(0, nrow = 9 , ncol = 21)

#========= INTIAL MODEL ====================
#Pass the data as a vector without class labels
#Independent of the number of rows in the training data
getKmeans <- function(data){
    data_matrix <- matrix(data, ncol = 21)
    model <- kmeans(data_matrix , 3)
    n1 = length(model$cluster[model$cluster==1]) - 1 
    n2 = length(model$cluster[model$cluster==2]) - 1
    n3 = length(model$cluster[model$cluster==3]) - 1
    SIGMA <<- SIGMA + n1*cov(data_matrix[model$cluster == 1,]) + n2*cov(data_matrix[model$cluster == 2,]) + n3*cov(data_matrix[model$cluster == 3,]) 
    return(model$centers)
}

mus[1:3,] = getKmeans(train[train[,22] == 1 , 1:21])
mus[4:6,] = getKmeans(train[train[,22] == 2 , 1:21])
mus[7:9,] = getKmeans(train[train[,22] == 3 , 1:21])

SIGMA = SIGMA / (size - 9)

SIGMAi = ginv(SIGMA) #Inverse of SIGMA

#========= AUXILLIARY FUNCITONS===================

#calculated error for a model defined by mu_vectors and sigma's for the data
#Give the data vector with the class lables , I am assuming SIGMAi to be constant 

#Returns the pi set for the particular class and three mu's
#Here 21 dimentions of the matrix should be passed 
get_pi_set <- function(class_mu , data_vector)
{
    data_matrix = matrix(data_vector , ncol = 21)
    size = length(data_matrix[,1])
    
    cluster_class = numeric(size)
    
    for (i in 1:size) {
        vector = data_matrix[i , 1:21]
        cluster_class[i] = which.min(c(norm(vector - class_mu[1,],type="2") , norm(vector - class_mu[2,],type="2") , norm(vector - class_mu[3,],type="2")))
    }
    
    return(c(sum(cluster_class == 1)/size,sum(cluster_class == 2)/size,sum(cluster_class == 3)/size))
}

#here also, assume that the passed matrix has class labels as well
get_pis <- function(mu_vector ,  data_vector)
{
    data_matrix = matrix(data_vector , ncol = 22)
    size = length(data_matrix[,1])
    pis = numeric(9)
    
    pis[1:3] <- get_pi_set(mu_vector[1:3 , ] , data_matrix[data_matrix[,22] == 1 , 1:21])
    pis[4:6] <- get_pi_set(mu_vector[4:6 , ] , data_matrix[data_matrix[,22] == 2 , 1:21])
    pis[7:9] <- get_pi_set(mu_vector[7:9 , ] , data_matrix[data_matrix[,22] == 3 , 1:21])
    
    return(pis)
}

#Data _vector is a single vector 
model_p <- function(class ,  data_vector , mu_matrix , pi_vector){
probability = 0 
for (i in 1:3){
    probability = probability + pi_vector[class*3 - 3 + i]*dmvnorm(data_vector , mean = matrix(mu_matrix[ class*3 -3 + i , 1:21]) , sigma = SIGMA)
}
return(probability)
}

#calculates the error Here, data_vector has classes assosciated with them. SIGMA is the same old SIGMA
geterror <- function(mu_vectors , data_vector){
    data_matrix = matrix(data_vector , ncol = 22)
    size = length(data_matrix[,1])
    
    pis = get_pis(mu_vectors , train) #pis are on the training data 
    predictions = numeric(size)
    for (i in 1:size){
        vector = data_matrix[i,1:21]
        predictions[i] = which.max(c(model_p(1 , vector , mu_vectors , pis) , model_p(2 , vector , mu_vectors , pis) , model_p(3 , vector , mu_vectors , pis))) 
    }
    return(1 - sum(predictions == data_matrix[,22])/size)
}

#=========********* BASIC MODEL ACCURACIES =============
geterror(mus , train ) #0.12333333
geterror(mus, validation) #0.16666667
geterror(mus , test) #0.15666667

#========= VARYING THE MODEL COMPLETELY STOCHASTICLLY==================
mu_best = matrix(0 , nrow = 9 , ncol = 21)
error_best = geterror(mus , validation) #Here answer = 0.1633333

for (i in 1 : 5000){
    if(i %% 100 == 0){
    print(i)}
    u = matrix(runif(9*21 , min = -1 , max = 1) , nrow = 9 , ncol = 21)
    mu_new = mus + u %*% SIGMA
    error = geterror(mu_new , validation)    
    if (error < error_best){
        error_best = error 
        mu_best = mu_new 
        print("Found")
        print(error_best)
    }
}

#==================== VARYING THE MODEL PARTIALLY STOCHASTICALLY ==========
mu_best = matrix(0 , nrow = 9 , ncol = 21)
error_best = geterror(mus , validation) #Here answer = 0.1633333

for (i in 1 : 5000){
    if(i %% 100 == 0){
        print(i)}
    u = matrix( 0 , nrow = 9 , ncol = 21)
    u[sample(1:9 , 1) , sample(1:21 , 1)] = runif(1 , min = -1, max= +1)
    mu_new = mus + u %*% SIGMA
    error = geterror(mu_new , validation)    
    if (error < error_best){
        error_best = error 
        mu_best = mu_new 
        print("Found")
        print(error_best)
    }
}

#=================== VARYING ONLY ONE OF THEM RANDOMLY ===========
mu_best = matrix(0 , nrow = 9 , ncol = 21)
error_best = geterror(mus , validation) #Here answer = 0.1633333

for (i in 1 : 5000){
    if(i %% 100 == 0){
        print(i)}
    X = sample(1:9 , 1 )
    Y = sample(1:21 , 1 )
    mu_new = mus
    mu_new[X , Y] = mu_new[X,Y] +  runif(1 , min = -1  , max = 1 )* (SIGMA[Y , Y ])
    error = geterror(mu_new , validation)    
    if (error < error_best){
        error_best = error 
        mu_best = mu_new 
        print("Found")
        print(error_best)
    }
}

#Again vary the model stochiastically 

#=================== Let us assume that all the components are varied simulatenaouly ===================
mu_best = mus 
error_best = geterror(mus , validation) #Here, I got best error to be 0.14 

unit = matrix( 1 , 9 , 21)
range =  c(-0.5,0,0.5)
constant_matrix = expand.grid(range, range , range , range, range , range, range, range , range)
i = 0 
getbest <- function(vector){
    if(i%%200 == 0)
    {print(i)}
    i <<- i + 1
    print(vector*unit)
    mu_new = mu_new + (vector*unit) %*% SIGMA
    error = geterror(mu_new, validation)
    if(error < error_best)
    {
        error_best <<- error
        mu_best <<- mu_new
        print(error_best)
    }
}
apply(constant_matrix , 1 , getbest)

#==============Getting accuracy on Test Data with mu_best ===============
geterror(mu_best , test) #0.17333
geterror(mus , test) #0.1566666
geterror(mu_best , validation ) #0.14

#============== Another round but now closer to the actual solution =============
mu_best = mus #Here, I got best error to be 0.15666
error_best = geterror(mus , validation)

unit = matrix( 1 , 9 , 21)
range =  c(-0.1,0,0.1) #Closer Bounds
constant_matrix = expand.grid(range, range , range , range, range , range, range, range , range)
i = 0 
getbest <- function(vector){
    if(i%%200 == 0)
    {print(i)}
    i <<- i + 1
    mu_new = mu_new + (vector*unit) %*% SIGMA
    error = geterror(mu_new, validation)
    if(error < error_best)
    {
        error_best <<- error
        mu_best <<- mu_new
        print(error_best)
    }
}
apply(constant_matrix , 1 , getbest)

geterror(mu_best , test) #0.16333
geterror(mus , test) #0.1566666
geterror(mu_best , validation ) #0.153333

#========== LDA and QDA ====================
lda <- lda(X22 ~ ., data.frame(train))
pred <- predict(lda,data.frame(test[,1:21]),type="response")$class
error = 1 - sum(pred == test[,-1])/300 #0.1366667

qda <- qda(X22 ~ ., data.frame(train))
pred <- predict(qda,data.frame(test[,1:21]),type="response")$class
error = 1 - sum(pred == test[,-1])/300 #0.1533333

