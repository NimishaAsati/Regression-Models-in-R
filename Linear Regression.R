#------------------ Batch and Stochastic gradient descent for Auto dataset

library(ISLR)
auto_df = ISLR::Auto

Auto_data<- within(Auto, rm("name","horsepower","cylinders","acceleration","displacement","origin"))

FeatureScaling <- function(x){
  mu <- mean(x)
  si <- sd(x)
  x_norm <- (x - mu)/si
  return(x_norm)
}

str(Auto)
y<-Auto_data$mpg
x1<-FeatureScaling(Auto_data$weight)
x2<-FeatureScaling(Auto_data$year)

X<-cbind(1,x1,x2)
theta<-c(0,0,0)

len_d <- length(y)
Cost <- sum(((X%*%theta)- y)^2)/(2*len_d)
print(Cost)

y<-Auto_data$mpg
x1<-FeatureScaling(Auto_data$weight)
x2<-FeatureScaling(Auto_data$year)

###### LM MODEL #####
LM_Model = lm(y ~ x1 + x2, data=Auto_data)
summary(LM_Model)

#### Batch Gradient Descent ####
alpha <- 0.0001         # Set learning parameter
iterations <- 1500      # Number of iterations
theta<-c(0,0,0)
for(i in 1:iterations)
{
  #print(i)
  theta[1] <- theta[1] - alpha * (1/len_d) * sum(((X%*%theta)- y))
  theta[2] <- theta[2] - alpha * (1/len_d) * sum(((X%*%theta)- y)*X[,2])
  theta[3] <- theta[3] - alpha * (1/len_d) * sum(((X%*%theta)- y)*X[,3])
  LM_BG_cost <- sum(((X%*%theta)- y)^2)/(2*len_d)
}

print(theta)
print (LM_BG_cost)

#### Stochastic Gradient Descent ####

alpha <- 0.6            # Set learning parameter
iterations <- 300       # Number of iterations

theta<-c(0,0,0)
X=cbind(y,X)
set.seed(587)
nr<-dim(X)[1]
randomSample<-X[sample.int(nr),]
y<- randomSample[,1]
X<- randomSample[,2:4]
t=1
for(i in 1:iterations){
  for(t in 1:len_d)
  {
    theta[1] <- theta[1] - alpha *((X[t,]%*%theta)- y[t])
    theta[2] <- theta[2] - alpha * ((X[t,]%*%theta)- y[t])*X[t,2]
    theta[3] <- theta[3] - alpha * ((X[t,]%*%theta)- y[t])*X[t,3]
  }
}
print(theta)
LM_SG_cost <- sum(((X%*%theta)- y)^2)/(2*len_d)
print(LM_SG_cost)

