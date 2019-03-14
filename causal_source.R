
##################################
CausalLinear<-function(y,d,x){
  num.features <- ncol(x)
  num.n <- nrow(x)
  ### Step 1
  ## we set penalty level using the theoretical choice
  supp1 <- which.max(abs(cor(x,y)))
  res <- glm(y~x[,supp1])
  w <-sd(res$residuals)
  lambda.theory <- 2*w*sqrt(log(num.features/0.01)/num.n)
  ## call Lasso 
  lassoTheory <- glmnet(x,y,lambda = lambda.theory)
  ## get the support
  supp1 <- support(lassoTheory$beta)
  ### Step 1 selected
  length(supp1)
  colnames(x[,supp1])
  ### controls
  ###
  ### Step 2
  w <-sd(d)
  lambda.theory <- 2*w*sqrt(log(num.features/0.05)/num.n)
  lassoTheory <- glmnet(x,d,lambda = lambda.theory)
  supp2<-support(lassoTheory$beta)
  ### Step 2 selected
  length(supp2)
  ### controls
  colnames(x[,supp2])
  ###
  ### Step 3
  inthemodel <- unique(c(supp1,supp2)) # unique grabs union
  selectdata <- cBind(d,x[,inthemodel]) 
  selectdata <- as.data.frame(as.matrix(selectdata)) # make it a data.frame
  dim(selectdata) ## p about half n
  
  ## run a a linear regression of Y on d and all selected
  causal_glm <- glm(y~., data=selectdata)
  ## The theory actually says that the standard SE calc for gamma is correct!
  ## despite the model selection
  summary(causal_glm)$coef["d",]
}
##################################
CausalLogistic<-function(y, dd, xx){
  x <- cbind(dd, xx)
  i <- 1
  num.n <- length(y)
  num.features <- ncol(x)
  num.pos <- sum(y)
  w <- sqrt( (num.pos/num.n)*(1-(num.pos/num.n)) )
  
  lambda.theory <- w*qnorm(1-0.05/num.features)/sqrt(num.n)
  lassoTheory <- glmnet(x,y, family="binomial",lambda = lambda.theory) # allow intercept
  supp1 <- support(lassoTheory$beta) # intercept is not here
  if ( length(supp1)==0 ){
    supp1 <- which.max(abs(cor(x,y)))
  }
  ####
  #prob <- predict(lassoTheory,newx=as.matrix(x), type="response")
  postLassoLogistic <- glm(y~., data=as.data.frame(as.matrix(x[,supp1])), family= "binomial")
  prob <- predict(postLassoLogistic,newx=as.matrix(x[,supp1]), type="response")
  
  weight <- sqrt(prob*(1-prob))
  
  weighted_x <- diag(weight[1:num.n])%*%x
  
  lambda_base <- min( qnorm(1-0.05/num.features)/sqrt(num.n), ComputeLambdaLasso(weighted_x,2000,1-0.05) )
  ### Steps 2 and 3 are performed adaptive to each variable in "indices"
  ### k was initialized outside t-loop
    ## call Lasso 
    w <-sd(weighted_x[,i])
    w_x_i <- weighted_x[,i]
    weighted_x[,i] <- weight
    lambda.theory <- w * lambda_base 
    # recall that we need a weighted intercept
    lassoTheory <- glmnet(weighted_x, w_x_i,lambda = lambda.theory, intercept=FALSE)
    ###check intercept on support selection
    supp2 <- sort(union(support(lassoTheory$beta),i)) # at least the intercept 
    postLasso <- glm(w_x_i ~ 0 + weighted_x[,supp2])
    #ortho_new <- predict(lassoTheory,newx=as.matrix(weighted_x[,-i]), type="response")/weight
    
    z_i <- x[,i] - (predict(postLasso,newx=as.matrix(weighted_x[,supp2]), type="response")/weight)
    
    ### Step 3 (Putting both set of selected variables together)
    selectdata <- x[,sort(unique(c(i,supp1,supp2)))]  # unique grabs union
    where_i <- which(sort(unique(c(i,supp1,supp2)))==i)
    selectdata <- as.data.frame(as.matrix(selectdata)) # make it a data.frame
    ## run a Logistic regression of Y on d and all selected
    causal_glm <- glm(y~., data=selectdata, family= "binomial")
    
    estimates<-summary(causal_glm)$coef[1+where_i,]
      
}

ComputeLambdaLasso <- function(x,rep,conf){
  num.n <- nrow(x)
  pos_sd <- which(sd(x)>0)
  x_scale <- scale(x[,pos_sd],center=FALSE,scale = TRUE)
  xi  <- matrix(rnorm(num.n*rep),num.n,rep)  
  score <- apply( abs(t(x_scale)%*%xi)/num.n, 2, max)
  return (quantile(score, conf,names=FALSE))
  
}