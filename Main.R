########################################################
### Final Project: Predicting Churn Rate of Telecommunication Customers
########################################################
### Team:27 Ngoc Pham, Saim Shujaat, Tiansheng Xu, Ikenna Ugwu, Niti Doshi
########################################################

library(glmnet)
library(keras)
#install_keras()
library(readr)
library(tidyverse)
library(dplyr)
use_implementation("keras")
library(magrittr)
library(lattice)
library(grid)
library(DMwR)
library('curl')
library("pROC")
library(ROCR)
library(keras)
library(caTools)
library(ragtop)
source("DataAnalyticsFunctions.R")
library(rpart)
library(caret)
library(randomForest)
library(partykit)
library(gbm)
library(corrplot)
library(pROC)
library(RODBC)
library(sqldf)
# read data into R
setwd("C:/Users/31221/Desktop/Term2/data science  for BUSINESS/FinalProject")
data <- read.csv("dec520Q-10B-team27-FinalProject_data.csv")
test = sqldf("select count(*) from data")
test

#summary(data)
# Cleaning up the data
#converting categorical variables to factors
data$new_cell <- as.factor(data$new_cell)
data$crclscod <- as.factor(data$crclscod)
data$asl_flag <- as.factor(data$asl_flag)
data$prizm_social_one <- as.factor(data$prizm_social_one)
data$area <- as.factor(data$area)
data$dualband <- as.factor(data$dualband)
data$refurb_new <- as.factor(data$refurb_new)
data$hnd_webcap <- as.factor(data$hnd_webcap)
data$dwlltype <- as.factor(data$dwlltype)
data$marital <- as.factor(data$marital)
data$infobase <- as.factor(data$infobase)
data$HHstatin <- as.factor(data$HHstatin)
data$dwllsize <- as.factor(data$dwllsize)
data$ethnic <- as.factor(data$ethnic)
data$kid0_2 <- as.factor(data$kid0_2)
data$kid3_5 <- as.factor(data$kid3_5)
data$kid6_10 <- as.factor(data$kid6_10)
data$kid11_15 <- as.factor(data$kid11_15)
data$kid16_17 <- as.factor(data$kid16_17)
data$creditcd <- as.factor(data$creditcd)

missing_means <- data[which(is.na(data$da_Mean)),]
data1 <- data[-which(is.na(data$da_Mean)),]

#making the missing values in truck and rv column to 2, so that we can identify them separately
data_temp <- data1
data_temp <- data_temp[-which(is.na(data_temp$truck)),]
data_temp$hnd_webcap[is.na(data_temp$hnd_webcap)] <- "UNKW"
data_temp$eqpdays <- ifelse(is.na(data_temp$eqpdays), 0, data_temp$eqpdays)
data_temp$phones <- ifelse(is.na(data_temp$phones), 1, data_temp$phones)
data_temp$models <- ifelse(is.na(data_temp$models), 1, data_temp$models)
data_temp$lor <- NULL
data_temp$numbcars <- NULL
#summary(data_temp)

data_temp$income <- NULL

missing_change <- data_temp[which(is.na(data_temp$change_mou) & is.na(data_temp$change_rev)),]
data_use <- data_temp[-which(is.na(data_temp$change_mou) & is.na(data_temp$change_rev)),]


data_use$prizm_social_one <- NULL
data_use <- data_use[-which(is.na(data_use$hnd_price)),]
data_use <- data_use[-which(is.na(data_use$avg6qty)),]
data_use$infobase <- NULL
data_use$Customer_ID <- NULL
data_use$dwlltype <- NULL
data_use$ownrent <- NULL
data_use$HHstatin <- NULL
data_use$dwllsize <- NULL
data_use$adults <- NULL
data_use = unique(data_use)


##########################
#Exploratory Data Analysis (EDA)
## checking for unbalanced data: values of 0 and 1 for churn are pretty balanced 
summary(data_use$churn)
## EDA: hnd_price vs churn. there is a relationship between hnd_price vs churn
ggplot(data_use, aes(x=hnd_price, y=phones)) + geom_point(aes(color= churn))
ggplot(data_use, aes(x=churn, y=hnd_price)) +
  geom_boxplot(aes(x=as.factor(churn), y=hnd_price), fill="white", color="navyblue")+
  ggtitle("churn vs handset price")

#Churn plotted aginst months first and then handset price
data_use2 <- data_use
data_use2$churn <- as.factor(data_use2$churn)
data_use2$forgntvl <- as.factor(data_use2$forgntvl)
ggplot(data_use2, aes(x=months, fill=churn))+geom_density(alpha=0.4)
ggplot(data_use2, aes(x=hnd_price, fill = churn))+geom_density(alpha=0.4)

#Churn vs Eqpdays Code
plot(factor(churn) ~ eqpdays, data=data_use, col=c(8,2), ylab="Churn Rate") 

###############################
#Run LASSO to select variables
###############################
names(data_use)
Mx<- model.matrix(churn ~ ., data=data_use)[,-1]
My<- data_use$churn

cv.lasso <- cv.glmnet(Mx, My, family = "binomial", alpha = 1, type.measure = "deviance", nfolds = 10)
#plot(cv.lasso)

cv.lasso$lambda.min
cv.lasso$lambda.1se
#coef(cv.lasso, cv.lasso$lambda.min)
#coef(cv.lasso, cv.lasso$lambda.1se)

lasso.model <- glmnet(Mx, My, family = "binomial",
                      lambda = cv.lasso$lambda.1se)
#lasso.model <- glmnet(Mx, My, family = "binomial",
#                      lambda = lambda.theory)
plot(lasso.model, label = TRUE)
#summary(lasso.model)
support(lasso.model$beta)
length(support(lasso.model$beta))
selection <- colnames(Mx)[support(lasso.model$beta)]
#selection

data_after_lasso <- data.frame(Mx[,selection],My)

######################################################
#Run PCA focusing on the importance of each variable
######################################################
pca.full <- prcomp(data_after_lasso, scale=TRUE)
### Lets plot the variance that each component explains
plot(pca.full,main="PCA: Variance Explained by Factors")
mtext(side=1, "Factors",  line=1, font=2)
loadings <- pca.full$rotation[,1:4]
v<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:27],1]
loadingfit <- lapply(1:27, function(k) ( t(v[1:k])%*%v[1:k] - 3/4 )^2)
v[1:which.min(loadingfit)]


###############################
#Spliting data into train and test
###############################

set.seed(123)
split = sample.split(data_after_lasso, SplitRatio = 0.7)
training_set = subset(data_after_lasso, split == TRUE)
test_set = subset(data_after_lasso, split == FALSE)


########Prepare for Neutral Network Model
x.holdout<- model.matrix(My ~ ., data=test_set)[,-1]
y.holdout<- test_set$My == 1

x.data<- model.matrix(My ~ ., data=training_set)[,-1]
y.data<- training_set$My == 1

#rescale (to be between 0 and 1)
x_train <- x.data %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_train <- as.numeric(y.data)
x_test <- x.holdout %*% diag(1/apply(x.data, 2, function(x) max(x, na.rm = TRUE)))
y_test <- as.numeric(y.holdout) 

#rescale (unit variance and zero mean)
mean <- apply(x.data,2,mean)
std <- apply(x.data,2,sd)
x_train <- scale(x.data,center = mean, scale = std)
y_train <- as.numeric(y.data)
x_test <- scale(x.holdout,center = mean, scale = std)
y_test <- as.numeric(y.holdout) 

#num.inputs <- ncol(x_test)
#num.inputs

reticulate::py_available()
reticulate::import("keras.models")
Module(keras.models)
reticulate::py_config()

#######################################################
#Neural network model
#######################################################
model <- keras_model_sequential() %>% 
  layer_dense(units=64, kernel_regularizer = regularizer_l2(0.001), activation="relu",input_shape = c(ncol(x_train))) %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(units=64, kernel_regularizer = regularizer_l2(0.001), activation="relu") %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(units=64, kernel_regularizer = regularizer_l2(0.001), activation="relu") %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(units=32, kernel_regularizer = regularizer_l2(0.001), activation="relu") %>%
  layer_dropout(rate=0.3) %>%
  layer_dense(units=1,activation="sigmoid")

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


history <- model %>% fit(
  x_train, y_train, 
  epochs = 50, batch_size = 256, 
  validation_split = 0.3
)

#predict the result using train dataset and get the accuracy
results.NN4 <- model %>% evaluate(x_train,y_train)
results.NN4

#predict the result using the test dataset and get the accuracy
results.NN4 <- model %>% evaluate(x_test,y_test)
results.NN4

#get the predictions
pred.NN1 <- model%>% predict(x_test)
pred.NN1





########################### k fold cross validation
### create a vector of fold memberships (random order)
nfold <- 5
n <- nrow(training_set)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### create an empty dataframe of results
OOS <- data.frame(logistic=rep(NA,nfold),cart=rep(NA,nfold),rf=rep(NA,nfold) ,null=rep(NA,nfold))  


### Use a for loop to run through the nfold trails to get accuracy
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit the logistic regression, cart and null model
  model.logistic <-glm(My~., data=training_set, subset=train,family="binomial")
  model.cart <- rpart(My~., data=training_set, subset=train)
  £¬model.rf <- randomForest(My~.,data=training_set, subset=train,na.action = na.omit,mtry=1, ntry=200)
  model.null <-glm(My~1, data=training_set, subset=train,family="binomial")
  
  ## calculate Accuracy
  #install.packages("caret")
  #library(caret)
  emp_pred <- training_set[-train,]
  # Logistic
  emp_pred$problog <- predict(model.logistic, newdata=emp_pred, type="response")
  emp_pred$pred_log <- ifelse(emp_pred$problog >= 0.5, 1, 0)
  cm_log <- confusionMatrix(as.factor(emp_pred$pred_log), as.factor(emp_pred$My))
  overall_log <- cm_log$overall
  OOS$logistic[k] <- overall_log['Accuracy']
  
  # Cart
  emp_pred$pred_cart <- predict(model.cart, newdata=emp_pred, type="vector")
  emp_pred$pred_cart <- ifelse(emp_pred$pred_cart >= 0.5, 1, 0)
  cm_cart <- confusionMatrix(as.factor(emp_pred$pred_cart), as.factor(emp_pred$My))
  overall_cart <- cm_cart$overall
  OOS$cart[k] <- overall_cart['Accuracy']
  
  #RF
  emp_pred$pred_rf <- predict(model.rf, newdata=emp_pred, type="response")
  emp_pred$pred_rf <- ifelse(emp_pred$pred_rf >= 0.5, 1, 0)
  cm_rf <- confusionMatrix(as.factor(emp_pred$pred_rf), as.factor(emp_pred$My))
  overall_rf <- cm_rf$overall
  OOS$rf[k] <- overall_rf['Accuracy']
  
  
  
  type#Null
  emp_pred$probnull <- predict(model.null, newdata=emp_pred, type="response")
  emp_pred$pred_null <- ifelse(emp_pred$probnull >= 0.5, 1, 0)
  cm_null <- confusionMatrix(as.factor(emp_pred$pred_null), as.factor(emp_pred$My))
  overall_null <- cm_null$overall
  OOS$null[k] <- overall_null['Accuracy']
  
  
  #Null Model guess
  sum(data_after_lasso$My[train]=="Yes")/length(train)
  
  ## We will loop this 5 times 
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

### Lets list the mean of the results stored in the dataframe OOS
### we have nfold values in OOS for each model, this computes the mean of them)
colMeans(OOS)
m.OOS <- as.matrix(OOS)

#### Plot the result for k-fold cross validation
rownames(m.OOS) <- c(1:nfold)
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=0),
        ylab= bquote( "Accuracy " ), ylim= c(0.0,0.7), xlab="Fold", names.arg = c(1:5))


###Use a for loop to run through the nfold trails to get FPR
###FPR
### create a vector of fold memberships (random order)
nfold <- 5
n <- nrow(training_set) 
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### create an empty dataframe of results
OOS <- data.frame(logistic=rep(NA,nfold),cart=rep(NA,nfold),rf=rep(NA,nfold) ,null=rep(NA,nfold))  

### Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit the logistic regression, cart and null model
  model.logistic <-glm(My~., data=training_set, subset=train,family="binomial")
  model.cart <- rpart(My~., data=training_set, subset=train)
  model.rf <- randomForest(My~.,data=training_set, subset=train,na.action = na.omit,mtry=1, ntry=200)
  model.null <-glm(My~1, data=training_set, subset=train,family="binomial")
  
  ## calculate Accuracy
  #install.packages("caret")
  #library(caret)
  emp_pred <- training_set[-train,]
  # Logistic
  emp_pred$problog <- predict(model.logistic, newdata=emp_pred, type="response")
  emp_pred$pred_log <- ifelse(emp_pred$problog >= 0.5, 1, 0)
  cm_log <- confusionMatrix(as.factor(emp_pred$pred_log), as.factor(emp_pred$My))
  OOS$logistic[k] <- cm_log$table[2,1]/(cm_log$table[2,1]+cm_log$table[1,1])
  
  # Cart
  emp_pred$pred_cart <- predict(model.cart, newdata=emp_pred, type="vector")
  emp_pred$pred_cart <- ifelse(emp_pred$pred_cart >= 0.5, 1, 0)
  cm_cart <- confusionMatrix(as.factor(emp_pred$pred_cart), as.factor(emp_pred$My))
  OOS$cart[k] <- cm_cart$table[2,1]/(cm_cart$table[2,1]+cm_cart$table[1,1])
  
  #RF
  emp_pred$pred_rf <- predict(model.rf, newdata=emp_pred, type="response")
  emp_pred$pred_rf <- ifelse(emp_pred$pred_rf >= 0.5, 1, 0)
  cm_rf <- confusionMatrix(as.factor(emp_pred$pred_rf), as.factor(emp_pred$My))
  OOS$rf[k] <- cm_rf$table[2,1]/(cm_rf$table[2,1]+cm_rf$table[1,1])
  
  #Null
  emp_pred$probnull <- predict(model.null, newdata=emp_pred, type="response")
  emp_pred$pred_null <- ifelse(emp_pred$probnull >= 0.5, 1, 0)
  cm_null <- confusionMatrix(as.factor(emp_pred$pred_null), as.factor(emp_pred$My))
  OOS$null[k] <- cm_null$table[2,1]/(cm_null$table[2,1]+cm_null$table[1,1])

  
  #Null Model guess
  sum(data_after_lasso$My[train]=="Yes")/length(train)
  
  ## We will loop this 5 times 
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

### Lets list the mean of the results stored in the dataframe OOS
### we have nfold values in OOS for each model, this computes the mean of them)
colMeans(OOS)
m.OOS <- as.matrix(OOS)
#plot the result
rownames(m.OOS) <- c(1:nfold)
m.OOS
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=1),
        ylab= bquote( "FPR" ), ylim= c(0.0,1.1), xlab="Fold", names.arg = c(1:20), col =c('blue','cornflowerblue','skyblue','azure'))


###Use a for loop to run through the nfold trails to get TPR
###TPR
### create a vector of fold memberships (random order)
nfold <- 5
n <- nrow(training_set)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### create an empty dataframe of results
OOS <- data.frame(logistic=rep(NA,nfold),cart=rep(NA,nfold),rf=rep(NA,nfold),null=rep(NA,nfold))  

### Use a for loop to run through the nfold trails
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  
  ## fit the logistic regression, cart and null model
  model.logistic <-glm(My~., data=training_set, subset=train,family="binomial")
  model.cart <- rpart(My~., data=training_set, subset=train)
  model.rf <- randomForest(My~.,data=training_set, subset=train,na.action = na.omit,mtry=1, ntry=200)
   model.null <-glm(My~1, data=training_set, subset=train,family="binomial")
  
  ## calculate Accuracy
  #install.packages("caret")
  #library(caret)
  emp_pred <- data_after_lasso[-train,]
  # Logistic
  emp_pred$problog <- predict(model.logistic, newdata=emp_pred, type="response")
  emp_pred$pred_log <- ifelse(emp_pred$problog >= 0.5, 1, 0)
  cm_log <- confusionMatrix(as.factor(emp_pred$pred_log), as.factor(emp_pred$My))
  OOS$logistic[k] <- cm_log$table[2,2]/(cm_log$table[1,2]+cm_log$table[2,2])
  
  # Cart
  emp_pred$pred_cart <- predict(model.cart, newdata=emp_pred, type="vector")
  emp_pred$pred_cart <- ifelse(emp_pred$pred_cart >= 0.5, 1, 0)
  cm_cart <- confusionMatrix(as.factor(emp_pred$pred_cart), as.factor(emp_pred$My))
  OOS$cart[k] <- cm_cart$table[2,2]/(cm_cart$table[1,2]+cm_cart$table[2,2])
  
  #RF
  emp_pred$pred_rf <- predict(model.rf, newdata=emp_pred, type="response")
  emp_pred$pred_rf <- ifelse(emp_pred$pred_rf >= 0.5, 1, 0)
  cm_rf <- confusionMatrix(as.factor(emp_pred$pred_rf), as.factor(emp_pred$My))
  OOS$rf[k] <- cm_rf$table[2,2]/(cm_rf$table[1,2]+cm_rf$table[2,2])
 
  
  #Null
  emp_pred$probnull <- predict(model.null, newdata=emp_pred, type="response")
  emp_pred$pred_null <- ifelse(emp_pred$probnull >= 0.5, 1, 0)
  cm_null <- confusionMatrix(as.factor(emp_pred$pred_null), as.factor(emp_pred$My))
  OOS$null[k] <- cm_null$table[2,2]/(cm_null$table[1,2]+cm_null$table[2,2])
  
  
  #Null Model guess
  sum(data_after_lasso$My[train]=="Yes")/length(train)
  
  ## We will loop this 5 times 
  ## this will print the progress (iteration that finished)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

### Lets list the mean of the results stored in the dataframe OOS
### we have nfold values in OOS for each model, this computes the mean of them)
colMeans(OOS)
m.OOS <- as.matrix(OOS)

rownames(m.OOS) <- c(1:nfold)
m.OOS
barplot(t(as.matrix(OOS)), beside=TRUE, legend=TRUE, args.legend=c(xjust=1, yjust=0),
        ylab= bquote( "TPR" ), ylim= c(0.0,1.1), xlab="Fold", names.arg = c(1:20), col =c('blue','cornflowerblue','skyblue','azure'))


###########Get the OOS accuracy, FPR, TPR for each model
#####Logistic regression
emp_pred = test_set
model.logistic <-glm(My~., data=training_set, family="binomial")
emp_pred$problog <- predict(model.logistic, newdata=emp_pred, type="response")
emp_pred$pred_log <- ifelse(emp_pred$problog >= 0.5, 1, 0)
cm_log <- confusionMatrix(as.factor(emp_pred$pred_log), as.factor(emp_pred$My))
cm_log$overall["Accuracy"]
cm_log$table[2,1]/(cm_log$table[2,1]+cm_log$table[1,1])
cm_log$table[2,2]/(cm_log$table[1,2]+cm_log$table[2,2])

#####CART
model.cart <- rpart(My~., data=training_set)
emp_pred$pred_cart <- predict(model.cart, newdata=emp_pred, type="vector")
emp_pred$pred_cart <- ifelse(emp_pred$pred_cart >= 0.5, 1, 0)
cm_cart <- confusionMatrix(as.factor(emp_pred$pred_cart), as.factor(emp_pred$My))
cm_cart$overall["Accuracy"]
cm_cart$table[2,1]/(cm_cart$table[2,1]+cm_cart$table[1,1])
cm_cart$table[2,2]/(cm_cart$table[1,2]+cm_cart$table[2,2])

#####Randomforest
model.rf <- randomForest(My~.,data=training_set,na.action = na.omit,mtry=1, ntry=200)
emp_pred$pred_rf <- predict(model.rf, newdata=emp_pred, type="response")
emp_pred$pred_rf <- ifelse(emp_pred$pred_rf >= 0.5, 1, 0)
cm_rf <- confusionMatrix(as.factor(emp_pred$pred_rf), as.factor(emp_pred$My))
cm_rf$overall["Accuracy"]
cm_rf$table[2,1]/(cm_rf$table[2,1]+cm_rf$table[1,1])
cm_rf$table[2,2]/(cm_rf$table[1,2]+cm_rf$table[2,2])

#####Neural network
emp_pred$pred_nn=pred.NN1
emp_pred$pred_nn<- ifelse(emp_pred$pred_nn >= 0.5, 1, 0)
cm_nn <- confusionMatrix(as.factor(emp_pred$pred_nn), as.factor(emp_pred$My))
cm_nn$overall["Accuracy"]
cm_nn$table[2,1]/(cm_nn$table[2,1]+cm_nn$table[1,1])
cm_nn$table[2,2]/(cm_nn$table[1,2]+cm_nn$table[2,2])

###############################
#MODEL BUILDING - GRADIENT BOOSTING MACHINE
###############################
#That's our final model
###############################
#For reproducibility
set.seed(123)

#Train GBM model
gbm.fit3 <- gbm(
  formula = My ~ .,
  distribution = "gaussian",
  data = training_set,
  n.trees = 7000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

#Find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit3$cv.error)
#Get MSE and compute RMSE
sqrt(gbm.fit3$cv.error[min_MSE])
#Plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit3, method = "cv")
test_set$pred <- predict(gbm.fit3, n.trees = gbm.fit3$n.trees, test_set)
#Results
caret::RMSE(test_set$pred, test_set$My)
test_set$pred_churn <- ifelse(test_set$pred >= 0.5, 1, 0)
test_set$actual_churn <- ifelse(test_set$My == TRUE, 1, 0)
confusionMatrix(factor(test_set$actual_churn), factor(test_set$pred_churn))
auc(test_set$actual_churn, test_set$pred_churn)



