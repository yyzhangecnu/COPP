######################################
###### 2022.4.13 zhang yingying ######

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


ConformalCI_MAS <- function(Data_train,Data_val,Data_test,method,B=1,trainprop,psfun,psparams,outfun,outparams,quantiles,type,side,weighted,alpha,wthigh,wtlow){
  
  
  YBleft <- NULL
  YBright <- NULL
  Nts <- NULL
  #random sample from binary varibale(A) B times
  for (b in 1:B) {
 
  #estimate the propensity score
  #randomforeset for multi classification
  # fit <- randomForest::randomForest(Ttrain~Xtrain,ntree=1000)
  # Xtrain <- Data_test$X
  # res <- predict(fit, newdata = Xtrain, type = "prob")
  # res <- as.matrix(res)
  # PSFit_T <- res
  tp_T <- do.call(rbind,lapply(Data_train$X,MulActProb,epsilon=0.3))
  tp_D <- do.call(rbind,lapply(Data_train$X,MulActProb,epsilon=0.2))
  temp <- tp_D/tp_T
  tp_A <- t(apply(temp,1,function(x) x/sum(x)))
  Atrain <- apply(tp_A,1,function(x) sample(1:4,1,prob=x))
  O <- I(Data_train$A==Atrain)
  Nts[b] <- sum(O)
  
  #quantile regression fitting after selecting A=T
  #select O=1 from training data
  ind <- which(O == 1)
  Xtrain <- as.data.frame(Data_train$X[ind,drop=FALSE])
  Ytrain <- Data_train$Y[ind]
  OutputFit <- QuantileReg(Ytrain,Xtrain,outfun,outparams,quantiles)
  Yhat_test <- OutputFit(as.data.frame(Data_test$X))
  
  #estimate weights for testing data
  tp_T_test <- do.call(rbind,lapply(Data_test$X,MulActProb,epsilon=0.3))
  tp_D_test <- do.call(rbind,lapply(Data_test$X,MulActProb,epsilon=0.2))
  temp <- tp_D_test/tp_T_test
  tp_A_test <- t(apply(temp,1,function(x) x/sum(x)))
  ps_test <- apply(tp_A_test*tp_T_test,1,sum)
  
  #estimate weights for validation data
  tp_T_val <- do.call(rbind,lapply(Data_val$X,MulActProb,epsilon=0.3))
  tp_D_val <- do.call(rbind,lapply(Data_val$X,MulActProb,epsilon=0.2))
  temp <- tp_D_val/tp_T_val
  tp_A_val <- t(apply(temp,1,function(x) x/sum(x)))
  A_val <- apply(tp_A_val,1,function(x) sample(1:4,1,prob=x))
  ps_val <- apply(tp_A_val*tp_T_val,1,sum)
  O_val <- I(Data_val$A==A_val)
    if(method == "Sweight" | method == "BootSweight"){
      #data for validation
      ind <- which(O_val == 1)
      Xval <- Data_val$X[ind,drop=FALSE]
      Yval <- Data_val$Y[ind]
      
      Yhat_val <- OutputFit(as.data.frame(Xval))
      ps_val <- ps_val[ind]
      }else if(method == "Dweight" | method== "BootDweight"){
      #regression for validation
      Yval <- Data_val$Y
      Yhat_val <- OutputFit(as.data.frame(Data_val$X))
      ind <- Data_val$A
      ps_A_val <- apply(cbind(tp_A_val,ind),1,function(x) x[(x[5])])
      ps_val <- ps_val/ps_A_val
    }

    Yscore <- conformalScore(Yval, Yhat_val, type, side)
    #construct the confidence interval by weighted CP
    if(weighted==TRUE){
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
  
  Xtest <- Data_test$X
  #if bootstrap used, combine multiple CI to a single one
  #otherwise, a single CI is produced
  lefts <- NULL
  rights <- NULL
  Lens <- NULL
  if((B>1) & (method == "BootSweight" | method == "BootDweight")){
    
    ntest <- length(Xtest)
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
  