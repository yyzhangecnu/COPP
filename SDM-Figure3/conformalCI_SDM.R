ConformalCI_SDM <- function(Data,H,Deta,Data_test,method,B,trainprop,psfun,psparams,outfun,outparameters,quantiles,type,side,weighted,alpha,wthigh,wtlow){
  
  YBleft <- NULL
  YBright <- NULL
  Nts <- NULL
  #random sample from binary varibale(A) B times
  for (b in 1:B) {
    
    n <- length(Data$Y_T[[H]])
    Y <- Data$Y_T[[H]]
    #split samples for training and calibration
    YY <- Y
    T1 <- Data$A_T[[1]]
    YY[!T1] <- NA
    T1 <- as.numeric(!is.na(YY))
    res <- SampleSplit(YY,trainprop)
    #res$trainid all training data
    #res$trainid1 all training data with T=1
    #!!!need to select T=1 first and then select trainid1
    #res$trainid0 all training data with T=0
    
    #estimate the propensity score
    PSFit <- list()
    for (t in 1:H) {
      Xt <- Data$X_T[[t]]
      Xt <- Xt[res$trainid,drop=FALSE]
      
      Tt <- Data$A_T[[t]]
      Tt <- Tt[res$trainid]
      PSFit[[t]] <- PropScore(Tt,Xt,psfun, psparams)
    }
    
    #random sample At
    O_T <- list()
    O <- (rep(1,n)==rep(1,n))
    tp_A <- list()
    for (t in 1:H) {
      tp_D <- exp(cbind(1,Data$X_T[[t]])%*%Deta)/(1+exp(cbind(1,Data$X_T[[t]])%*%Deta))
      tp_T <- PSFit[[t]](Data$X_T[[t]])
      
      tp_A[[t]] <- tp_D*(1-tp_T)/(tp_D*(1-tp_T)+tp_T*(1-tp_D))
      At <- rbinom(n = n, size = 1, prob=tp_A[[t]])
      O_T[[t]] <- (At==Data$A_T[[t]])
      O <- O&O_T[[t]]
    }

    #train ps for O
    Ot <- O[res$trainid]
    Xt <- Data$X_T[[1]]
    Xt <- Xt[res$trainid,drop=FALSE]
    PSFit_O <- PropScore(Ot,Xt,psfun, psparams)
    
    #train quantile regression
    inds <- which(O[res$trainid] == 1)
    Xt <- Data$X_T[[1]]
    Xt <- Xt[res$trainid,drop=FALSE]
    Xt <- Xt[inds,drop=FALSE]
    Xt <- as.data.frame(Xt)
    
    Yt <- Data$Y_T[[H]]
    Yt <- Y[res$trainid]
    Yt <- Yt[inds]
    OutputFit <- QuantileReg(Yt,Xt,outfun,outparams,quantiles)
    Xtest <- as.data.frame(Data_test$X_T[[1]])
    Yhat_test <- OutputFit(Xtest)
    
    if(method == "Sweight" | method == "BootSweight"){
      inds <- which(O[-res$trainid] == 1)
      Xtval <- Data$X_T[[1]]
      Xtval <- Xtval[-res$trainid,drop=FALSE]
      Xtval <- Xtval[inds,drop=FALSE]
      Xtval <- as.data.frame(Xtval)
      
      Ytval <- Data$Y_T[[H]]
      Ytval <- Ytval[-res$trainid]
      Ytval <- Ytval[inds]
      
      Yhat_val <- OutputFit(Xtval)
      
      Xtval <- as.data.frame(Xtval)
      ps_val <- PSFit_O(Xtval)
      ps_test <- PSFit_O(Xtest)
      
    }else if(method == "Dweight" | method== "BootDweight"){
      Xtval <- Data$X_T[[1]]
      Xtval <- Xtval[-res$trainid,drop=FALSE]
      Xtval <- as.data.frame(Xtval)
      
      Ytval <- Data$Y_T[[H]]
      Ytval <- Ytval[-res$trainid]
      
      Yhat_val <- OutputFit(Xtval)
      
      #weights for validation set
      Xtval <- as.data.frame(Xtval)
      ps_val <- PSFit_O(Xtval)
      ps_test <- PSFit_O(Xtest)
      
      for (t in 1:H) {
       Tval <- Data$A_T[[t]][-res$trainid]
       tp_A_val <- tp_A[[t]][-res$trainid]
       tp_A_val[which(Tval==0)] <- 1-tp_A_val[which(Tval==0)]
       ps_val <- ps_val/tp_A_val
      }
    }
    
    #conformity score
    Yval <- Ytval
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