######################################
#X,Y,T: X is a matrix, Y is a vector, T is a binary variable 0/1 (behavior policy)
#Deta: is the parameter vector for target policy
#Xtest: is the dataframe for test data
#method: "Sweight"(single weighted,split-and-sample in paper) B=1, "Dweight"(double weighted) B=1, "BootSweight" i.e. B=100, "BootDweight" i.e. B=100 
#B: resampling times for bootstrap
#trainprop: proportion of sample for training, including propensity score estimation and regression estimation
#psfun: propensity score estimation method("Boosting", "RF", "Logistic", "PenLogistic")
#psparams: parameters for psfun
#outfun: regression estimation method("quantRF", "quantBoosting", "quantBART")
#outparams: parameters for outfun
#quantiles: 0.05,0.95 quantile levels for regression prediction for "CQR"
#type: conformal quantile regression("CQR") and mean regression("mean")
#side: "two", "above" and "below" confidence interval with quantile regression 
#weighted: "TRUE"(estimated weights), "FALSE"(unweighted CQR) or user-specified weights vector for weighted conformal prediction
#alpha: confidence level
#wthigh/wtlow: upper and lower bounds for weights


ConformalCI <- function(X,Y,T,Deta,Xtest,method,B=1,trainprop,psfun,psparams,outfun,outparams,quantiles,type,side,weighted,alpha,wthigh,wtlow){
  
  
  YBleft <- NULL
  YBright <- NULL
  Nts <- NULL
  #random sample from binary varibale(A) B times
  for (b in 1:B) {
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
  
  #estimate weights for testing data
  ps_T_test <- PSFit_T(Xtest)
  Xtest <- as.matrix(Xtest)
  ps_D_test <- exp(cbind(1,Xtest)%*%Deta)/(1+exp(cbind(1,Xtest)%*%Deta))
  ps_test <- ps_T_test*(1-ps_T_test)/(ps_T_test*(1-ps_D_test)+ps_D_test*(1-ps_T_test)) 
  
  #estimate the propensity score for binary A
  tp_T <- PSFit_T(X)
  tp_D <- exp(cbind(1,X)%*%Deta)/(1+exp(cbind(1,X)%*%Deta))
  tp_A <- tp_D*(1-tp_T)/(tp_D*(1-tp_T)+tp_T*(1-tp_D))
  

    A <- rbinom(n = n, size = 1, prob=tp_A)
    O <- I(T==A)
    Nts[b] <- sum(O)
    #quantile regression fitting after selecting A=T
    X1 <- X[res$trainid, ,drop=FALSE]
    Y1 <- Y[res$trainid]
    #select O=1 from training data
    ind <- which(O[res$trainid] == 1)
    Xtrain <- X1[ind, ,drop=FALSE]
    Ytrain <- Y1[ind]
    OutputFit <- QuantileReg(Ytrain,Xtrain,outfun,outparams,quantiles)
    Yhat_test <- OutputFit(Xtest)
    
    if(method == "Sweight" | method == "BootSweight"){
      #data for validation
      X0 <- X[-res$trainid, ,drop=FALSE]
      Y0 <- Y[-res$trainid]
      ind <- which(O[-res$trainid] == 1)
      Xval <- X0[ind, ,drop=FALSE]
      Yval <- Y0[ind]
      ps_T_val <- PSFit_T(as.data.frame(Xval))
      Yhat_val <- OutputFit(Xval)
      
      Xval <- as.matrix(Xval)
      ps_D_val <- exp(cbind(1,Xval)%*%Deta)/(1+exp(cbind(1,Xval)%*%Deta))
      ps_val <- ps_T_val*(1-ps_T_val)/(ps_T_val*(1-ps_D_val)+ps_D_val*(1-ps_T_val)) 
    
      }else if(method == "Dweight" | method== "BootDweight"){
      #regression for validation
      Xval <- X[-res$trainid, ,drop=FALSE]
      Yval <- Y[-res$trainid]
      ps_T_val <- PSFit_T(as.data.frame(Xval))
      Yhat_val <- OutputFit(Xval)
      
      Xval <- as.matrix(Xval)
      ps_D_val <- exp(cbind(1,Xval)%*%Deta)/(1+exp(cbind(1,Xval)%*%Deta))
      ps_val <- ps_T_val*(1-ps_T_val)/(ps_T_val*(1-ps_D_val)+ps_D_val*(1-ps_T_val)) 
      ps_A_val <- ps_D_val*(1-ps_T_val)/(ps_D_val*(1-ps_T_val)+ps_T_val*(1-ps_D_val))
      ps_A_val[which(T[-res$trainid]==0)] <- 1-ps_A_val[which(T[-res$trainid]==0)]
      ps_val <- ps_val/ps_A_val
    }

    Yscore <- conformalScore(Yval, Yhat_val, type, side)
    #construct the confidence interval by weighted CP
    if(length(weighted)>1){
      wt <- weighted[1:n]
      if(method == "Sweight" | method == "BootSweight"){
        wt <- wt[-res$trainid]
        ind <- which(O[-res$trainid] == 1)
        wt <- wt[ind]
      }else if(method == "Dweight" | method == "BootDweight"){
        wt <- wt[-res$trainid]
        }
      wt_test <- weighted[-(1:n)]
      PredInt<- predict.conformalSplit(Yhat_test, Yscore,wt,wt_test,alpha,wthigh,wtlow,type,side)
    }else if(weighted==TRUE){
      wt <- 1/ps_val
      wt_test <- 1/ps_test
      PredInt<- predict.conformalSplit(Yhat_test, Yscore,wt,wt_test,alpha,wthigh,wtlow,type,side)
    }else if(weighted==FALSE){
      wt <- rep(1,length(ps_val))
      wt_test <- rep(1,length(ps_test))
      PredInt<- predict.conformalSplit(Yhat_test, Yscore,wt,wt_test,alpha,wthigh,wtlow,type,side)
    }
    
    YBleft <- rbind(YBleft,PredInt[,1])
    YBright <- rbind(YBright,PredInt[,2])
  } 
  
  #if bootstrap used, combine multiple CI to a single one
  #otherwise, a single CI is produced
  lefts <- NULL
  rights <- NULL
  Lens <- NULL
  if((B>1) & (method == "BootSweight" | method == "BootDweight")){
    
    ntest <- nrow(Xtest)
    for (j in 1:ntest) {
      
      Ys <- c(YBleft[,j],YBright[,j])
      Ws <- c(rep(1,nrow(YBleft)),rep(1,nrow(YBright)))
      Ss <- c(rep(1,nrow(YBleft)),rep(0,nrow(YBright)))
      #tol <- B*(1-gamma)
      tol <- B/2
      
      ind <- order(Ys)
      Ys <- Ys[ind]
      Ws <- Ws[ind]
      Ss <- Ss[ind]
      count <- 0
      leftpoints <- NULL
      rightpoints <- NULL
      
      for (i in 1:length(Ys)) {
        if(Ss[i]==1){
          count <- count+Ws[i]
          if((count>tol)&(count-Ws[i]<=tol)){
            leftpoints <- c(leftpoints,Ys[i])
          }
        }else if(Ss[i]==0){
          if((count>tol)&(count-Ws[i]<=tol)){
            rightpoints <- c(rightpoints,Ys[i])
          }
          count <- count-Ws[i]
        }
      }
      lefts[[j]] <- leftpoints
      rights[[j]] <- rightpoints
      Lens[j] <- sum(rightpoints-leftpoints)
    }
  }else if((B==1) & (method == "Sweight" | method == "Dweight")){
    for (j in 1:ntest) {
    lefts[[j]] <- YBleft[j]
    rights[[j]] <- YBright[j]
    Lens[j] <- YBright[j]-YBleft[j]
    }
  }
  
  return(list(lefts=lefts, rights=rights, Lens=Lens, Nts=Nts))
}
  