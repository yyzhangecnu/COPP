######################################
###### 2022.4.13 zhang yingying ######

#X,Y,T: X is a matrix, Y is a vector, T is a binary variable 0/1 (behavior policy)
#Deta: is the parameter vector for target policy
#Xtest: is the dataframe for test data

#naive method: select D=T
#trainprop: proportion of sample for training, including propensity score estimation and regression estimation
#psfun: propensity score estimation method("Boosting", "RF", "Logistic", "PenLogistic")
#psparams: parameters for psfun
#outfun: regression estimation method("quantRF", "quantBoosting", "quantBART")
#outparams: parameters for outfun
#quantiles: 0.05,0.95 quantile levels for regression prediction for "CQR"
#type: conformal quantile regression("CQR") and mean regression("mean")
#side: "two", "above" and "below" confidence interval with quantile regression 
#alpha: confidence level
#wthigh/wtlow: upper and lower bounds for weights


Naive_TS <- function(X1,X2,Y,T1,T2,Deta1,Deta2,X1test,trainprop,psfun,psparams,outfun,outparameters,quantiles,type,side,weighted,alpha,wthigh,wtlow){
  
  n <- length(Y)
  #split samples for training and calibration
  YY <- Y
  YY[!T1] <- NA
  T <- as.numeric(!is.na(YY))
  res <- SampleSplit(YY,trainprop)
  #res$trainid all training data
  #res$trainid1 all training data with T=1
  #!!!need to select T=1 first and then select trainid1
  #res$trainid0 all training data with T=0
  
  #random sample D1 and D2
  tp_D1 <- exp(cbind(1,X1)%*%Deta1)/(1+exp(cbind(1,X1)%*%Deta1))
  tp_D2 <- exp(cbind(1,X2)%*%Deta2)/(1+exp(cbind(1,X2)%*%Deta2))
  
  #select T1=D1, T2=D2
  D1 <- rbinom(n = n, size = 1, prob=tp_D1)
  D2 <- rbinom(n = n, size = 1, prob=tp_D2)
  O <- (D1==T1)&(D2==T2)
  Nts <- sum(O)
  
  
  #train quantile regression
  inds <- which(O[res$trainid] == 1)
  X1train <- X1[res$trainid, ,drop=FALSE]
  X1train <- X1train[inds, ,drop=FALSE]
  Ytrain <- Y[res$trainid]
  Ytrain <- Ytrain[inds]
  OutputFit <- QuantileReg(Ytrain,X1train,outfun,outparams,quantiles)
  Yhat_test <- OutputFit(X1test)
  
  #data for validation
  inds <- which(O[-res$trainid] == 1)
  X1val <- X1[-res$trainid, ,drop=FALSE]
  X1val <- X1val[inds, ,drop=FALSE]
  Yval <- Y[-res$trainid]
  Yval <- Yval[inds]
  Yhat_val <- OutputFit(X1val)
  
  Yscore <- conformalScore(Yval, Yhat_val, type, side)
  #construct the confidence interval by weighted CP
  wt <- rep(1,length(Yval))
  wt_test <- rep(1,ntest)
  PredInt<- predict.conformalSplit(Yhat_test, Yscore,wt,wt_test,alpha,wthigh,wtlow,type,side)
  
  lefts <- PredInt[,1]
  rights <- PredInt[,2]
  Lens <- rights-lefts
  
  return(list(lefts=lefts, rights=rights, Lens=Lens, Nts=Nts))
}
