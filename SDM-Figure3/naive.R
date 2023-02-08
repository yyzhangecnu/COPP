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


Naive <- function(X,Y,T,Deta,Xtest,trainprop,psfun,psparams,outfun,outparams,quantiles,type,side,alpha,wthigh,wtlow){
  
  n <- length(Y)
  #split samples for training and calibration
  YY <- Y
  YY[!T] <- NA
  T <- as.numeric(!is.na(YY))
  res <- SampleSplit(YY,trainprop)
  #res$trainid: index for all training
  #res$trainid1: index for all training with T=1
  #!!!need to select T=1 first and then select trainid1
  #res$trainid0: index for all training with T=0
  
  #estimate the propensity score
  Xtrain <- X[res$trainid, ,drop=FALSE]
  Ttrain <- T[res$trainid]
  PSFit_T <- PropScore(Ttrain,Xtrain,psfun, psparams)
  
  #estimate the propensity score for binary A
  tp_T <- PSFit_T(X)
  tp_D <- exp(cbind(1,X)%*%Deta)/(1+exp(cbind(1,X)%*%Deta))
  ps_T_test <- PSFit_T(Xtest)
  
  #random sample from binary varibale(D)
  D <- rbinom(n = n, size = 1, prob=tp_D)
  O <- I(T==D)
  Nts <- sum(O)
  #quantile regression fitting after selecting A=T
  X1 <- X[res$trainid, ,drop=FALSE]
  Y1 <- Y[res$trainid]
  #select O=1 from training data
  ind <- which(O[res$trainid] == 1)
  Xtrain <- X1[ind, ,drop=FALSE]
  Ytrain <- Y1[ind]
  OutputFit <- QuantileReg(Ytrain,Xtrain,outfun,outparams,quantiles)
  Yhat_test <- OutputFit(Xtest)
    
  #data for validation
  X0 <- X[-res$trainid, ,drop=FALSE]
  Y0 <- Y[-res$trainid]
  ind <- which(O[-res$trainid] == 1)
  Xval <- X0[ind, ,drop=FALSE]
  Yval <- Y0[ind]
  ps_T_val <- PSFit_T(as.data.frame(Xval))
  Yhat_val <- OutputFit(Xval)
      
  Yscore <- conformalScore(Yval, Yhat_val, type, side)
  #construct the confidence interval by weighted CP
  wt <- rep(1,length(ps_T_val))
  wt_test <- rep(1,length(ps_T_test))
  PredInt<- predict.conformalSplit(Yhat_test, Yscore,wt,wt_test,alpha,wthigh,wtlow,type,side)
 
  lefts <- PredInt[,1]
  rights <- PredInt[,2]
  Lens <- rights-lefts
  
  return(list(lefts=lefts, rights=rights, Lens=Lens, Nts=Nts))
}
