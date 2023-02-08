rm(list=ls())
source("genY.R")
source("samplesplit.R")
source("conformal_learners.R")
source("propensityscore.R")
source("quantilereg.R")
source("conformal_utils.R")
source("naive_TS.R") 

# Generate data
n <- 2000
d <- 1
trainprop <- 0.75
quantiles <- c(0.05,0.95)
psfun <- Logistic
psparams <- NULL
outfun <- quantRF
outparams <- NULL

type <- "CQR"
side <- "two"
alpha <- 0.1
wthigh <- 20
wtlow <- 0.05
Deta1 <- c(-0.5,0.5)
Deta2 <- c(-1,0.5)

Teta1 <- c(-0.5,1)
Teta2 <- c(-0.5,-1)

CovProb <- list()
AveLeng <- list()
Nt <- list()

for (s in 1:100) {
  
  X1 <- runif(n)
  tp_T1 <- exp(cbind(1,X1)%*%Teta1)/(1+exp(cbind(1,X1)%*%Teta1))
  T1 <-rbinom(n = n, size = 1, prob=tp_T1)
  X2 <- unlist(lapply(X1, function(x) runif(1,min = x,max = x+1)))
  tp_T2 <- exp(cbind(1,X2)%*%Teta2)/(1+exp(cbind(1,X2)%*%Teta2))
  T2 <- rbinom(n = n, size = 1, prob=tp_T2)
  
  Y <- genY_twostage(X1,X2,T1,T2)
  
  #generate test data
  ntest <- 10000
  X1test <- runif(ntest)
  tp_D1_test <- exp(cbind(1,X1test)%*%Deta1)/(1+exp(cbind(1,X1test)%*%Deta1))
  D1test <-rbinom(n = ntest, size = 1, prob=tp_D1_test)
  X2test <- unlist(lapply(X1test, function(x) runif(1,min = x,max = x+1)))
  tp_D2_test <- exp(cbind(1,X2test)%*%Deta2)/(1+exp(cbind(1,X2test)%*%Deta2))
  D2test <-rbinom(n = ntest, size = 1, prob=tp_D2_test)
  Ytest <- genY_twostage(X1test,X2test,D1test,D2test)
  
  
  weighted <-TRUE
  X1 <- matrix(X1,ncol = 1)
  X2 <- matrix(X2,ncol = 1)
  X1test <- as.data.frame(X1test)
  
  out <- Naive_TS(X1,X2,Y,T1,T2,Deta1,Deta2,X1test,trainprop,psfun,psparams,outfun,outparameters,quantiles,type,side,weighted,alpha,wthigh,wtlow)
  #summary for single split
  out$Nts
  
  ind <- (Ytest<(out$rights))&(Ytest>(out$lefts))
  CovProb[s] <- sum(ind)/ntest
  AveLeng[s] <- mean(out$Lens)
  #ALOrac[s] <- mean((1+Dtest)*(cbind(1,as.matrix(Xtest))%*%rep(1,5))*(qnorm(1-alpha/2,0,1)-qnorm(alpha/2,0,1)))
  
  
  print(CovProb[s])
  print(AveLeng[s])
  print(s)
}

save.image("TS_low_naive2.Rdata")




