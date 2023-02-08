#DenModel is true or false
#QuanModel is true or false
#Pol is deterministic or random
#a is optional when DenModel is FALSE
nips <- function(Data_train, Data_val, Data_test,a,quantiles, type, side, alpha, wthigh, wtlow){
  
  Xtrain <- as.data.frame(Data_train$X)
  Ytrain <- Data_train$Y
  OutputFit <- QuantileReg(Ytrain,Xtrain,outfun,outparams,quantiles)
  Yhat_test <- OutputFit(as.data.frame(Data_test$X))
  
  Yhat_val <- OutputFit(as.data.frame(Data_val$X))
  Yscore <- conformalScore(Data_val$Y, Yhat_val, type, side)
  
  #weight functions
  #conditional density for Y|X,A
  #a is a parameter to control the density shift
  n <- length(Data_val$X)
  ntest <- length(Data_test$X)
  
  M <- 500
  #weights as eq(7) in Tauqq2022nips
  wt <- list()
  for(i in 1:n){
    Ds <- unlist(lapply(rep(Data_val$X[i],M), MulActPolicy,epsilon=0.2))
    Dmean <- Data_val$X[i]*Ds
    
    Ts <- unlist(lapply(rep(Data_val$X[i],M), MulActPolicy,epsilon=0.3))
    Tmean <- Data_val$X[i]*Ts
    
    wt[i] <- mean(dnorm(Data_val$Y[i],Dmean,1)+runif(M,0,a))/mean(dnorm(Data_val$Y[i],Tmean,1)+runif(M,0,a))
  }
  
  wt_test <- list()
  for(i in 1:ntest){
    Ds <- unlist(lapply(rep(Data_test$X[i],M), MulActPolicy,epsilon=0.2))
    Dmean <- Data_test$X[i]*Ds
    
    Ts <- unlist(lapply(rep(Data_test$X[i],M), MulActPolicy,epsilon=0.3))
    Tmean <- Data_test$X[i]*Ts
    
    wt_test[i] <- mean(dnorm(Data_test$Y[i],Dmean,1)+runif(M,0,a))/mean(dnorm(Data_test$Y[i],Tmean,1)+runif(M,0,a))
  }
  wt <- unlist(wt)
  wt_test <- unlist(wt_test)
  PredInt<- predict.conformalSplit(Yhat_test,Yscore,wt,wt_test,alpha,wthigh,wtlow,type,side)
  
  Ytest <- Data_test$Y
  ind <- (Ytest<(PredInt[,2]))&(Ytest>(PredInt[,1]))
  #sum(ind)/ntest
  CovProb <- sum(ind)/ntest
  AveLeng <- mean(PredInt[,2]-PredInt[,1])
  return(list(CovProb = CovProb,AveLeng=AveLeng))
  
}