rm(list=ls())
source("genY.R")
source("conformal_utils.R")
source("AOPM.R")

# Generate data
n <- 500
d <- 4
quantiles <- c(0.05,0.95)
type <- "CQR"
side <- "two"
alpha <- 0.1
wthigh <- 20
wtlow <- 0.05
CovProb <- list()
AveLeng <- list()

Teta_ran <- c(-0.5,-0.5,-0.5,-0.5,-0.5)
Deta_ran <- c(-0.5,1,1,-1,-1)

Teta_det <- c(-0.5,-0.5,-0.5,-0.5,-0.5)
Deta_det <- c(0,-1,-1,1,1)

CovProb_ran_M1 <- rep(0,100)
CovProb_ran_M0 <- rep(0,100)
CovProb_det_M1 <- rep(0,100)
CovProb_det_M0 <- rep(0,100)

AveLeng_ran_M1 <- rep(0,100)
AveLeng_ran_M0 <- rep(0,100)
AveLeng_det_M1 <- rep(0,100)
AveLeng_det_M0 <- rep(0,100)

for (s in 1:100) {
  
  QuanModel <- "TRUE"
  Pol <- "random"
  #generate test data
  if(Pol == "random"){
    #generate data
    X <- matrix(runif(n * d), nrow = n, ncol = d)
    tp_T <- exp(cbind(1,X)%*%Teta_ran)/(1+exp(cbind(1,X)%*%Teta_ran))
    T <-rbinom(n = n, size = 1, prob=tp_T)
    Y <- genY_singlestage(X,T)
    
    ntest <- 10000
    Xtest <- matrix(runif(ntest * d), nrow = ntest, ncol = d)
    tp_D_test <- exp(cbind(1,Xtest)%*%Deta_ran)/(1+exp(cbind(1,Xtest)%*%Deta_ran))
    Dtest <- rbinom(n = ntest, size = 1, prob=tp_D_test)
    Ytest <- genY_singlestage(Xtest,Dtest)
  }else if(Pol == "deterministic"){
    #generate data
    X <- matrix(runif(n * d), nrow = n, ncol = d)
    tp_T <- exp(cbind(1,X)%*%Teta_det)/(1+exp(cbind(1,X)%*%Teta_det))
    T <-rbinom(n = n, size = 1, prob=tp_T)
    Y <- genY_singlestage(X,T)
    
    ntest <- 10000
    Xtest <- matrix(runif(ntest * d), nrow = ntest, ncol = d)
    tp_D_test <- I(cbind(1,Xtest)%*%Deta_det>0)
    Dtest <- rbinom(n = ntest, size = 1, prob=tp_D_test)
    Ytest <- genY_singlestage(Xtest,Dtest)
  }
  if(Pol == "random"){
    Teta = Teta_ran
    Deta = Deta_ran
  }else if(Pol == "deterministic"){
    Teta = Teta_det
    Deta = Deta_det
  }
  Direct_ran_M1 <- AOPM(X, T, Y, Xtest, Ytest,Teta, Deta, QuanModel, a, Pol,quantiles, type, side, alpha, wthigh, wtlow)
  CovProb_ran_M1[s] <- Direct_ran_M1$CovProb
  AveLeng_ran_M1[s] <- Direct_ran_M1$AveLeng
  
  QuanModel <- "TRUE"
  Pol <- "deterministic"
  if(Pol == "random"){
    #generate data
    X <- matrix(runif(n * d), nrow = n, ncol = d)
    tp_T <- exp(cbind(1,X)%*%Teta_ran)/(1+exp(cbind(1,X)%*%Teta_ran))
    T <-rbinom(n = n, size = 1, prob=tp_T)
    Y <- genY_singlestage(X,T)
    
    ntest <- 10000
    Xtest <- matrix(runif(ntest * d), nrow = ntest, ncol = d)
    tp_D_test <- exp(cbind(1,Xtest)%*%Deta_ran)/(1+exp(cbind(1,Xtest)%*%Deta_ran))
    Dtest <- rbinom(n = ntest, size = 1, prob=tp_D_test)
    Ytest <- genY_singlestage(Xtest,Dtest)
  }else if(Pol == "deterministic"){
    #generate data
    X <- matrix(runif(n * d), nrow = n, ncol = d)
    tp_T <- exp(cbind(1,X)%*%Teta_det)/(1+exp(cbind(1,X)%*%Teta_det))
    T <-rbinom(n = n, size = 1, prob=tp_T)
    Y <- genY_singlestage(X,T)
    
    ntest <- 10000
    Xtest <- matrix(runif(ntest * d), nrow = ntest, ncol = d)
    tp_D_test <- I(cbind(1,Xtest)%*%Deta_det>0)
    Dtest <- rbinom(n = ntest, size = 1, prob=tp_D_test)
    Ytest <- genY_singlestage(Xtest,Dtest)
  }
  if(Pol == "random"){
    Teta = Teta_ran
    Deta = Deta_ran
  }else if(Pol == "deterministic"){
    Teta = Teta_det
    Deta = Deta_det
  }
  Direct_det_M1 <- AOPM(X, T, Y, Xtest, Ytest, Teta, Deta, QuanModel, a, Pol,quantiles, type, side, alpha, wthigh, wtlow)
  CovProb_det_M1[s] <- Direct_det_M1$CovProb
  AveLeng_det_M1[s] <- Direct_det_M1$AveLeng
  
  a <- 1
  QuanModel <- "FALSE"
  Pol <- "random"
  if(Pol == "random"){
    #generate data
    X <- matrix(runif(n * d), nrow = n, ncol = d)
    tp_T <- exp(cbind(1,X)%*%Teta_ran)/(1+exp(cbind(1,X)%*%Teta_ran))
    T <-rbinom(n = n, size = 1, prob=tp_T)
    Y <- genY_singlestage(X,T)
    
    ntest <- 10000
    Xtest <- matrix(runif(ntest * d), nrow = ntest, ncol = d)
    tp_D_test <- exp(cbind(1,Xtest)%*%Deta_ran)/(1+exp(cbind(1,Xtest)%*%Deta_ran))
    Dtest <- rbinom(n = ntest, size = 1, prob=tp_D_test)
    Ytest <- genY_singlestage(Xtest,Dtest)
  }else if(Pol == "deterministic"){
    #generate data
    X <- matrix(runif(n * d), nrow = n, ncol = d)
    tp_T <- exp(cbind(1,X)%*%Teta_det)/(1+exp(cbind(1,X)%*%Teta_det))
    T <-rbinom(n = n, size = 1, prob=tp_T)
    Y <- genY_singlestage(X,T)
    
    ntest <- 10000
    Xtest <- matrix(runif(ntest * d), nrow = ntest, ncol = d)
    tp_D_test <- I(cbind(1,Xtest)%*%Deta_det>0)
    Dtest <- rbinom(n = ntest, size = 1, prob=tp_D_test)
    Ytest <- genY_singlestage(Xtest,Dtest)
  }
  if(Pol == "random"){
    Teta = Teta_ran
    Deta = Deta_ran
  }else if(Pol == "deterministic"){
    Teta = Teta_det
    Deta = Deta_det
  }
  Direct_ran_M0 <- AOPM(X, T, Y, Xtest, Ytest, Teta, Deta, QuanModel, a, Pol,quantiles, type, side, alpha, wthigh, wtlow)
  CovProb_ran_M0[s] <- Direct_ran_M0$CovProb
  AveLeng_ran_M0[s] <- Direct_ran_M0$AveLeng
  
  a <- 1
  QuanModel <- "FALSE"
  Pol <- "deterministic"
  if(Pol == "random"){
    #generate data
    X <- matrix(runif(n * d), nrow = n, ncol = d)
    tp_T <- exp(cbind(1,X)%*%Teta_ran)/(1+exp(cbind(1,X)%*%Teta_ran))
    T <-rbinom(n = n, size = 1, prob=tp_T)
    Y <- genY_singlestage(X,T)
    
    ntest <- 10000
    Xtest <- matrix(runif(ntest * d), nrow = ntest, ncol = d)
    tp_D_test <- exp(cbind(1,Xtest)%*%Deta_ran)/(1+exp(cbind(1,Xtest)%*%Deta_ran))
    Dtest <- rbinom(n = ntest, size = 1, prob=tp_D_test)
    Ytest <- genY_singlestage(Xtest,Dtest)
  }else if(Pol == "deterministic"){
    #generate data
    X <- matrix(runif(n * d), nrow = n, ncol = d)
    tp_T <- exp(cbind(1,X)%*%Teta_det)/(1+exp(cbind(1,X)%*%Teta_det))
    T <-rbinom(n = n, size = 1, prob=tp_T)
    Y <- genY_singlestage(X,T)
    
    ntest <- 10000
    Xtest <- matrix(runif(ntest * d), nrow = ntest, ncol = d)
    tp_D_test <- I(cbind(1,Xtest)%*%Deta_det>0)
    Dtest <- rbinom(n = ntest, size = 1, prob=tp_D_test)
    Ytest <- genY_singlestage(Xtest,Dtest)
  }
  if(Pol == "random"){
    Teta = Teta_ran
    Deta = Deta_ran
  }else if(Pol == "deterministic"){
    Teta = Teta_det
    Deta = Deta_det
  }
  Direct_det_M0 <- AOPM(X, T, Y, Xtest, Ytest, Teta, Deta, QuanModel, a, Pol,quantiles, type, side, alpha, wthigh, wtlow)
  CovProb_det_M0[s] <- Direct_det_M0$CovProb
  AveLeng_det_M0[s] <- Direct_det_M0$AveLeng
  
  print(CovProb_ran_M1[s])
  print(CovProb_det_M1[s])
  print(CovProb_ran_M0[s])
  print(CovProb_det_M0[s])
  print(s)
}


