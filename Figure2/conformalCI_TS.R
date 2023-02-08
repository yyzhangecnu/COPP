ConformalCI_TS <- function(X1,X2,Y,T1,T2,Deta1,Deta2,X1test,method,B,trainprop,psfun,psparams,outfun,outparameters,quantiles,type,side,weighted,alpha,wthigh,wtlow){
  
  YBleft <- NULL
  YBright <- NULL
  Nts <- NULL
  #random sample from binary varibale(A) B times
  for (b in 1:B) {
  
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
    
    #estimate the propensity score
    X1train <- X1[res$trainid, ,drop=FALSE]
    T1train <- T1[res$trainid]
    PSFit_T1 <- PropScore(T1train,X1train,psfun, psparams)
    
    X2train <- X2[res$trainid, ,drop=FALSE]
    T2train <- T2[res$trainid]
    PSFit_T2 <- PropScore(T2train,X2train,psfun, psparams)
    
    #random sample A1 and A2
    tp_D1 <- exp(cbind(1,X1)%*%Deta1)/(1+exp(cbind(1,X1)%*%Deta1))
    tp_D2 <- exp(cbind(1,X2)%*%Deta2)/(1+exp(cbind(1,X2)%*%Deta2))
    tp_T1 <- PSFit_T1(X1)
    tp_T2 <- PSFit_T2(X2)
    
    tp_A1 <- tp_D1*(1-tp_T1)/(tp_D1*(1-tp_T1)+tp_T1*(1-tp_D1))
    A1 <- rbinom(n = n, size = 1, prob=tp_A1)
    tp_A2 <- tp_D2*(1-tp_T2)/(tp_D2*(1-tp_T2)+tp_T2*(1-tp_D2))
    A2 <- rbinom(n = n, size = 1, prob=tp_A2)
    O <- (A1==T1)&(A2==T2)
    
    #train ps for O
    Otrain <- O[res$trainid]
    X1train <- X1[res$trainid, ,drop=FALSE]
    PSFit_O <- PropScore(Otrain,X1train,psfun, psparams)
    
    #train quantile regression
    inds <- which(O[res$trainid] == 1)
    X1train <- X1[res$trainid, ,drop=FALSE]
    X1train <- X1train[inds, ,drop=FALSE]
    Ytrain <- Y[res$trainid]
    Ytrain <- Ytrain[inds]
    OutputFit <- QuantileReg(Ytrain,X1train,outfun,outparams,quantiles)
    Yhat_test <- OutputFit(X1test)
    
    if(method == "Sweight" | method == "BootSweight"){
    inds <- which(O[-res$trainid] == 1)
    X1val <- X1[-res$trainid, ,drop=FALSE]
    X1val <- X1val[inds, ,drop=FALSE]
    Yval <- Y[-res$trainid]
    Yval <- Yval[inds]
    
    Yhat_val <- OutputFit(X1val)
    
    X1val <- as.data.frame(X1val)
    ps_val <- PSFit_O(X1val)
    ps_test <- PSFit_O(X1test)
    
    }else if(method == "Dweight" | method== "BootDweight"){
      X1val <- X1[-res$trainid, ,drop=FALSE]
      Yval <- Y[-res$trainid]
      Yhat_val <- OutputFit(X1val)
      
      #weights for validation set
      X1val <- as.data.frame(X1val)
      ps_val <- PSFit_O(X1val)
      ps_test <- PSFit_O(X1test)
      
      T1val <- T1[-res$trainid]
      T2val <- T2[-res$trainid]
      tp_A1_val <- tp_A1[-res$trainid]
      tp_A2_val <- tp_A2[-res$trainid]
      
      tp_A1_val[which(T1val==0)] <- 1-tp_A1_val[which(T1val==0)]
      tp_A2_val[which(T2val==0)] <- 1-tp_A2_val[which(T2val==0)]
      ps_select <- tp_A1_val*tp_A2_val
      
      ps_val <- ps_val/ps_select
    
      }
    
    #conformity score
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
    
    ntest <- nrow(X1test)
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