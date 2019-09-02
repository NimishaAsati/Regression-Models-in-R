# ----------------------------- Logistic Regression and Evalutaions

# Installing the required packages:
library (ISLR)
library (caret)
library (ROCR)
library (pROC)
library(PRROC)

# Loading the Weekly data and understanding its statistics
main_data=Weekly
summary(main_data)
str(main_data)

# Splitting the data into training and test data
training_data=main_data[main_data$Year<2009,]    #985 observations got shortlisted here

test_data=main_data[main_data$Year>2008,]        #104 observations got shortlisted here

train <- Year<=2008
test <-  !train
training_dir=Direction[train]
test_dir=Direction[test]

# ----------------------------- Part (c) -----------------------------

logit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=training_data, family=binomial)
summary(logit)
test_model<- predict(logit,test_data,type = "response")
training_model_dir <- rep("Down",985)
training_model_dir[test_model > 0.5] <-"Up" 
test_model_dir <- rep("Down",104)
test_model_dir[test_model > 0.5] <-"Up" 
training_conf_mat <- table(training_model_dir,training_dir)
test_conf_mat <- table(test_model_dir,test_dir)
confusionMatrix(training_conf_mat)
confusionMatrix(test_conf_mat)
training_precision<-training_conf_mat[2,2]/sum(training_conf_mat[2,1:2])
test_precision<-test_conf_mat[2,2]/sum(test_conf_mat[2,1:2])
training_recall<-training_conf_mat[2,2]/sum(training_conf_mat[1:2,2])
test_recall<-test_conf_mat[2,2]/sum(test_conf_mat[1:2,2])
training_Fscore<-(2*training_precision*training_recall)/(training_precision+training_recall)
test_Fscore<-(2*test_precision*test_recall)/(test_precision+test_recall)
training_precision
test_precision
training_recall
test_recall
training_Fscore
test_Fscore


# ----------------------------- Part (d) -----------------------------

# Training the model on Lag1 variable

logit1 <- glm(Direction ~ Lag1,data=training_data, family="binomial")
summary(logit1)
test_model<- predict(logit1,test_data,type = "response")
training_model_dir <- rep("Down",985)
training_model_dir[test_model > 0.5] <-"Up" 
test_model_dir <- rep("Down",104)
test_model_dir[test_model > 0.5] <-"Up" 
training_conf_mat <- table(training_model_dir,training_dir)
test_conf_mat <- table(test_model_dir,test_dir)
confusionMatrix(training_conf_mat)
confusionMatrix(test_conf_mat)
training_precision<-training_conf_mat[2,2]/sum(training_conf_mat[2,1:2])
test_precision1<-test_conf_mat[2,2]/sum(test_conf_mat[2,1:2])
training_recall<-training_conf_mat[2,2]/sum(training_conf_mat[1:2,2])
test_recall1<-test_conf_mat[2,2]/sum(test_conf_mat[1:2,2])
training_Fscore<-(2*training_precision*training_recall)/(training_precision+training_recall)
test_Fscore<-(2*test_precision1*test_recall1)/(test_precision1+test_recall1)
training_precision
test_precision1
training_recall
test_recall1
training_Fscore
test_Fscore


# Training the model on Lag2 variable
logit2 <- glm(Direction ~ Lag2,data=training_data, family="binomial")
summary(logit2)
test_model<- predict(logit2,test_data,type = "response")
training_model_dir <- rep("Down",985)
training_model_dir[test_model > 0.5] <-"Up" 
test_model_dir <- rep("Down",104)
test_model_dir[test_model > 0.5] <-"Up" 
training_conf_mat <- table(training_model_dir,training_dir)
test_conf_mat <- table(test_model_dir,test_dir)
confusionMatrix(training_conf_mat)
confusionMatrix(test_conf_mat)
training_precision<-training_conf_mat[2,2]/sum(training_conf_mat[2,1:2])
test_precision2<-test_conf_mat[2,2]/sum(test_conf_mat[2,1:2])
training_recall<-training_conf_mat[2,2]/sum(training_conf_mat[1:2,2])
test_recall2<-test_conf_mat[2,2]/sum(test_conf_mat[1:2,2])
training_Fscore<-(2*training_precision*training_recall)/(training_precision+training_recall)
test_Fscore<-(2*test_precision2*test_recall2)/(test_precision2+test_recall2)
training_precision
test_precision2
training_recall
test_recall2
training_Fscore
test_Fscore

# Training the model on Lag3 variable
logit3 <- glm(Direction ~ Lag3, data=training_data, family="binomial")
summary(logit3)
test_model<- predict(logit3,test_data,type = "response")
training_model_dir <- rep("Down",985)
training_model_dir[test_model > 0.5] <-"Up" 
test_model_dir <- rep("Down",104)
test_model_dir[test_model > 0.5] <-"Up" 
u<-union(training_model_dir,training_dir)
training_conf_mat<- table(factor(training_model_dir,u),factor(training_dir,u))
confusionMatrix(training_conf_mat)
u<-union(test_model_dir,test_dir)
test_conf_mat<- table(factor(test_model_dir,u),factor(test_dir,u))
confusionMatrix(test_conf_mat)
training_precision<-training_conf_mat[1,1]/sum(training_conf_mat[1,1:2])
test_precision3<-test_conf_mat[1,1]/sum(test_conf_mat[1,1:2])
training_recall<-training_conf_mat[1,1]/sum(training_conf_mat[1:2,1])
test_recall3<-test_conf_mat[1,1]/sum(test_conf_mat[1:2,1])
training_Fscore<-(2*training_precision*training_recall)/(training_precision+training_recall)
test_Fscore<-(2*test_precision3*test_recall3)/(test_precision3+test_recall3)
training_precision
test_precision3
training_recall
test_recall3
training_Fscore
test_Fscore


# Training the model on Lag4 variable
logit4 <- glm(Direction ~ Lag4, data=training_data, family="binomial")
summary(logit4)
test_model<- predict(logit4,test_data,type = "response")
training_model_dir <- rep("Down",985)
training_model_dir[test_model > 0.5] <-"Up" 
test_model_dir <- rep("Down",104)
test_model_dir[test_model > 0.5] <-"Up" 
u<-union(training_model_dir,training_dir)
training_conf_mat<- table(factor(training_model_dir,u),factor(training_dir,u))
confusionMatrix(training_conf_mat)
u<-union(test_model_dir,test_dir)
test_conf_mat<- table(factor(test_model_dir,u),factor(test_dir,u))
confusionMatrix(test_conf_mat)
training_precision<-training_conf_mat[1,1]/sum(training_conf_mat[1,1:2])
test_precision4<-test_conf_mat[1,1]/sum(test_conf_mat[1,1:2])
training_recall<-training_conf_mat[1,1]/sum(training_conf_mat[1:2,1])
test_recall4<-test_conf_mat[1,1]/sum(test_conf_mat[1:2,1])
training_Fscore<-(2*training_precision*training_recall)/(training_precision+training_recall)
test_Fscore<-(2*test_precision4*test_recall4)/(test_precision4+test_recall4)
training_precision
test_precision4
training_recall
test_recall4
training_Fscore
test_Fscore


# Training the model on Lag5 variable
logit5 <- glm(Direction ~ Lag5,data=training_data, family="binomial")
summary(logit5)
test_model<- predict(logit5,test_data,type = "response")
training_model_dir <- rep("Down",985)
training_model_dir[test_model > 0.5] <-"Up" 
test_model_dir <- rep("Down",104)
test_model_dir[test_model > 0.5] <-"Up" 
training_conf_mat <- table(training_model_dir,training_dir)
test_conf_mat <- table(test_model_dir,test_dir)
confusionMatrix(training_conf_mat)
confusionMatrix(test_conf_mat)
training_precision<-training_conf_mat[2,2]/sum(training_conf_mat[2,1:2])
test_precision5<-test_conf_mat[2,2]/sum(test_conf_mat[2,1:2])
training_recall<-training_conf_mat[2,2]/sum(training_conf_mat[1:2,2])
test_recall5<-test_conf_mat[2,2]/sum(test_conf_mat[1:2,2])
training_Fscore<-(2*training_precision*training_recall)/(training_precision+training_recall)
test_Fscore<-(2*test_precision5*test_recall5)/(test_precision5+test_recall5)
training_precision
test_precision5
training_recall
test_recall5
training_Fscore
test_Fscore


# ----------------------------- Part (e) and (f) -----------------------------

# Getting ROC and PR for the overall model
pred<- predict(logit, Weekly, type='response')
pred<- prediction(pred, Weekly$Direction)
roc1<- performance(pred,"tpr","fpr")
prc1<- performance(pred,"prec","rec")
auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
prc1 <- pr.curve(test_recall,test_precision)
prc1
auc

# Getting ROC and PR for the Lag1 variable model  
pred<- predict(logit1, Weekly, type='response')
pred<- prediction(pred, Weekly$Direction)
roc2<- performance(pred,"tpr","fpr")
prc2<- performance(pred,"prec","rec")
auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
prc1 <- pr.curve(test_recall1,test_precision1)
prc1
auc

# Getting ROC and PR for the Lag2 variable model
pred<- predict(logit2, Weekly, type='response')
pred<- prediction(pred, Weekly$Direction)
roc3<- performance(pred,"tpr","fpr")
prc3<- performance(pred,"prec","rec")
auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
prc1 <- pr.curve(test_recall2,test_precision2)
prc1
auc

# Getting ROC and PR for the Lag3 variable model
pred<- predict(logit3, Weekly, type='response')
pred<- prediction(pred, Weekly$Direction)
roc4<- performance(pred,"tpr","fpr")
prc4<- performance(pred,"prec","rec")
auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
prc1 <- pr.curve(test_recall3,test_precision3)
prc1
auc

# Getting ROC and PR for the Lag4 variable model
pred<- predict(logit4, Weekly, type='response')
pred<- prediction(pred, Weekly$Direction)
roc5<- performance(pred,"tpr","fpr")
prc5<- performance(pred,"prec","rec")
auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
prc1 <- pr.curve(test_recall4,test_precision4)
prc1
auc

# Getting ROC and PR for the Lag5 variable model
pred<- predict(logit5, Weekly, type='response')
pred<- prediction(pred, Weekly$Direction)
roc6<- performance(pred,"tpr","fpr")
prc6<- performance(pred,"prec","rec")
auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,4)
prc1 <- pr.curve(test_recall5,test_precision5)
prc1
auc

# Plotting all ROC curves in 1 graph
plot(roc1,col="Green")
abline(a=0,b=1)
plot(roc2,col="Violet",add="True")
plot(roc3,col="Brown",add="True")
plot(roc4,col="Orange",add="True")
plot(roc5,col="Yellow",add="True")
plot(roc6,col="Blue",add="True")

# Plotting all PR curves in 1 graph
plot(prc1,col="Green")
plot(prc2,col="Violet")
plot(prc3,col="Brown",add="True")
plot(prc4,col="Orange",add="True")
plot(prc5,col="Yellow",add="True")
plot(prc6,col="Blue",add="True")

library(Matrix)
require(Matrix)