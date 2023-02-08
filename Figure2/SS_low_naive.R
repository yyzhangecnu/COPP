rm(list=ls())
source("genY.R")
source("samplesplit.R")
source("conformal_learners.R")
source("propensityscore.R")
source("quantilereg.R")
source("conformal_utils.R")
source("conformalCI.R")
source("naive.R")

# Generate data
n <- 2000
d <- 4
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
method <- "BootSweight" #Sweight, BootSweight, Dweight, BootDweight
B <- 100 #B=1 for Sweight or Dweight, B>1 for BootSweight or BootDweight
CovProb <- list()
AveLeng <- list()
ALOrac <- list()
Nt <- list()

Teta <- c(-0.5,-0.5,-0.5,-0.5,-0.5)
Deta <- c(-0.5,1,1,-1,-1)

for (s in 1:100) {
  
  #generate data
  X <- matrix(runif(n * d), nrow = n, ncol = d)
  tp_T <- exp(cbind(1,X)%*%Teta)/(1+exp(cbind(1,X)%*%Teta))
  T <-rbinom(n = n, size = 1, prob=tp_T)
  Y <- genY_singlestage(X,T)
  
  #generate test data
  ntest <- 10000
  Xtest <- matrix(runif(ntest * d), nrow = ntest, ncol = d)
  tp_D_test <- exp(cbind(1,Xtest)%*%Deta)/(1+exp(cbind(1,Xtest)%*%Deta))
  Dtest <- rbinom(n = ntest, size = 1, prob=tp_D_test)
  Ytest <- genY_singlestage(Xtest,Dtest)
  
  Xtest <- as.data.frame(Xtest)
  
  out <- Naive(X,Y,T,Deta,Xtest,trainprop,psfun,psparams,outfun,outparams,quantiles,type,side,alpha,wthigh,wtlow)

 
  #summary for single split
  ind <- (Ytest<(out$rights))&(Ytest>(out$lefts))
  CovProb[s] <- sum(ind)/ntest
  AveLeng[s] <- mean(out$Lens)
  ALOrac[s] <- mean((1+Dtest)*(cbind(1,as.matrix(Xtest))%*%rep(1,5))*(qnorm(1-alpha/2,0,1)-qnorm(alpha/2,0,1)))
  
  
  print(CovProb[s])
  print(AveLeng[s])
  print(s)
}

save.image("SS_low_Naive.Rdata")
