#QuanModel is true or false
#Pol is deterministic or random
#a is optional when QuanModel is FALSE
AOPM <- function(X, T, Y, Xtest, Ytest, Teta, Deta, QuanModel,a, Pol,quantiles, type, side, alpha, wthigh, wtlow){
  
  if(Pol == "deterministic"){
    tp_T <- exp(cbind(1,X)%*%Teta)/(1+exp(cbind(1,X)%*%Teta))
    tp_D <- I(cbind(1,X)%*%Deta>0)
    tp_A <- tp_D*(1-tp_T)/(tp_D*(1-tp_T)+tp_T*(1-tp_D))
    tp_T_test <- exp(cbind(1,Xtest)%*%Teta)/(1+exp(cbind(1,Xtest)%*%Teta))
    tp_D_test <- I(cbind(1,Xtest)%*%Deta>0)
    tp_A_test <- tp_D_test*(1-tp_T_test)/(tp_D_test*(1-tp_T_test)+tp_T_test*(1-tp_D_test))
  }else if(Pol == "random"){
    tp_T <- exp(cbind(1,X)%*%Teta)/(1+exp(cbind(1,X)%*%Teta))
    tp_D <- exp(cbind(1,X)%*%Deta)/(1+exp(cbind(1,X)%*%Deta))
    tp_A <- tp_D*(1-tp_T)/(tp_D*(1-tp_T)+tp_T*(1-tp_D))
    tp_T_test <- exp(cbind(1,Xtest)%*%Teta)/(1+exp(cbind(1,Xtest)%*%Teta))
    tp_D_test <- exp(cbind(1,Xtest)%*%Deta)/(1+exp(cbind(1,Xtest)%*%Deta))
    tp_A_test <- tp_D_test*(1-tp_T_test)/(tp_D_test*(1-tp_T_test)+tp_T_test*(1-tp_D_test))
    }

  #Yhat for quantile estimation at lower and upper quantiles
  n <- nrow(X)
  ntest <- nrow(Xtest)
  if(QuanModel == "TRUE"){
  Yhat <- matrix(0,n,2)
  Yhat_test <- matrix(0,ntest,2)
  }else if(QuanModel == "FALSE"){
    Yhat <- matrix(runif(2*n,0,a),n,2)
    Yhat_test <- matrix(runif(2*ntest,0,a),ntest,2)
  }
  
  #nselect <- length(ind)
  #generate true monte carlo quantile of Y^{D}|X
  #compute the nonconformity score function
  for (i in 1:n) {
    Drep <- rbinom(n = 10000, size = 1, prob=tp_D[i])
    M1 <- 1+X[i,1]-X[i,2]+X[i,3]^{3}+exp(X[i,4])
    M2 <- (3-5*X[i,1]+2*X[i,2]-3*X[i,3]+X[i,4])
    M3 <- (1+X[i,1]+X[i,2]+X[i,3]+X[i,4])
    Err <- rnorm(10000)
    Yrep <- M1+Drep*M2+(1+Drep)*M3*Err
    Yhat[i,] <- Yhat[i,]+quantile(Yrep,probs = quantiles)
  }
  
  A <- rbinom(n = n, size = 1, prob=tp_A)
  ind <- which(A==T)
  Yscore <- conformalScore(Y[ind], Yhat[ind,], type, side)
  
  #generate true monte carlo quantile of Y^{T}|X for test data
  for (i in 1:ntest) {
    Dtestrep <- rbinom(n = 10000, size = 1, prob=tp_D_test[i])
    M1 <- 1+Xtest[i,1]-Xtest[i,2]+Xtest[i,3]^{3}+exp(Xtest[i,4])
    M2 <- (3-5*Xtest[i,1]+2*Xtest[i,2]-3*Xtest[i,3]+Xtest[i,4])
    M3 <- (1+Xtest[i,1]+Xtest[i,2]+Xtest[i,3]+Xtest[i,4])
    Err <- rnorm(10000)
    Ytestrep <- M1+Dtestrep*M2+(1+Dtestrep)*M3*Err
    Yhat_test[i,] <- Yhat_test[i,]+quantile(Ytestrep,probs = quantiles)
  }
  
  #weight functions
  if(Pol == "deterministic"){
    wt <- 1/(tp_A*tp_T+(1-tp_A)*(1-tp_T))
    wt <- wt[ind]
    wt_test <- 1/(tp_A_test*tp_T_test+(1-tp_A_test)*(1-tp_T_test))
  }else if(Pol == "random"){
    wt <- (tp_D*(1-tp_T)+tp_T*(1-tp_D))/(tp_T*(1-tp_T))
    wt <- wt[ind]
    wt_test <- (tp_D_test*(1-tp_T_test)+tp_T_test*(1-tp_D_test))/(tp_T_test*(1-tp_T_test))
  }
  
  PredInt<- predict.conformalSplit(Yhat_test, Yscore,wt,wt_test,alpha,wthigh,wtlow,type,side)
  ind <- (Ytest<(PredInt[,2]))&(Ytest>(PredInt[,1]))
  sum(ind)/ntest
  CovProb <- sum(ind)/ntest
  AveLeng <- mean(PredInt[,2]-PredInt[,1])
  return(list(CovProb = CovProb,AveLeng=AveLeng))
  
}