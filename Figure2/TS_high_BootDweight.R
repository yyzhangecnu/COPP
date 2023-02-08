rm(list=ls())
source("genY.R")
source("samplesplit.R")
source("conformal_learners.R")
source("propensityscore.R")
source("quantilereg.R")
source("conformal_utils.R")
source("conformalCI_TS.R") 

# Generate data
n <- 2000
d <- 100
trainprop <- 0.75
quantiles <- c(0.05,0.95)
psfun <- PenLogistic
psparams <- NULL
outfun <- quantRF
outparams <- NULL
method <- "BootDweight"
B <- 50
type <- "CQR"
side <- "two"
alpha <- 0.1
wthigh <- 20
wtlow <- 0.05
Deta1 <- c(-0.5,0.5,rep(0,99))
Deta2 <- c(-1,0.5,rep(0,99))

Teta1 <- c(-0.5,1,rep(0,99))
Teta2 <- c(-0.5,-1,rep(0,99))

CovProb <- list()
AveLeng <- list()
Nt <- list()

for (s in 1:100) {
  
  X1 <- matrix(runif(n*d),ncol = d)
  tp_T1 <- exp(cbind(1,X1)%*%Teta1)/(1+exp(cbind(1,X1)%*%Teta1))
  T1 <-rbinom(n = n, size = 1, prob=tp_T1)
  X2 <- unlist(lapply(X1[,1], function(x) runif(1,min = x,max = x+1)))
  X2 <- cbind(X2,matrix(runif(n*(d-1)),ncol = (d-1)))
  tp_T2 <- exp(cbind(1,X2)%*%Teta2)/(1+exp(cbind(1,X2)%*%Teta2))
  T2 <- rbinom(n = n, size = 1, prob=tp_T2)
  
  Y <- genY_twostage(X1[,1],X2[,1],T1,T2)
  
  #generate test data
  ntest <- 10000
  X1test <- matrix(runif(ntest*d),ncol = d)
  tp_D1_test <- exp(cbind(1,X1test)%*%Deta1)/(1+exp(cbind(1,X1test)%*%Deta1))
  D1test <-rbinom(n = ntest, size = 1, prob=tp_D1_test)
  X2test <- unlist(lapply(X1test[,1], function(x) runif(1,min = x,max = x+1)))
  X2test <- cbind(X2test,matrix(runif(ntest*(d-1)),ncol = (d-1)))
  tp_D2_test <- exp(cbind(1,X2test)%*%Deta2)/(1+exp(cbind(1,X2test)%*%Deta2))
  D2test <-rbinom(n = ntest, size = 1, prob=tp_D2_test)
  Ytest <- genY_twostage(X1test[,1],X2test[,1],D1test,D2test)
  
  
  weighted <-TRUE
  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  X1test <- as.data.frame(X1test)
  out <- ConformalCI_TS(X1,X2,Y,T1,T2,Deta1,Deta2,X1test,method,B,trainprop,psfun,psparams,outfun,outparameters,quantiles,type,side,weighted,alpha,wthigh,wtlow)
  if((B==1) & (method == "Sweight" | method == "Dweight")){
    
    #summary for single split
    ind <- (Ytest<unlist(out$rights))&(Ytest>unlist(out$lefts))
    CovProb[s] <- sum(ind)/ntest
    AveLeng[s] <- mean(out$Lens)
    #ALOrac[s] <- mean((1+Dtest)*(cbind(1,as.matrix(Xtest))%*%rep(1,5))*(qnorm(1-alpha/2,0,1)-qnorm(alpha/2,0,1)))
  }else if((B>1) & (method == "BootSweight" | method == "BootDweight")){
    
    #summary for multiple split
    ind <- NULL
    for(k in 1:ntest){
      NInt <- length(out$lefts[k])
      flag <- unlist(lapply(seq(1:NInt), function(x) return((out$lefts[[k]][x]<Ytest[k])&(Ytest[k]<out$rights[[k]][x]))))
      ind[k] <- (sum(flag)>0)
    }
    sum(ind)/ntest
    CovProb[s] <- sum(ind)/ntest
    AveLeng[s] <- mean(out$Lens)
    #ALOrac[s] <- mean((1+Dtest)*(1+Xtest[,1]+Xtest[,2]+Xtest[,3]+Xtest[,4])*(qnorm(1-alpha/2,0,1)-qnorm(alpha/2,0,1)))
  }
  
  print(CovProb[s])
  print(AveLeng[s])
  print(s)
}

save.image("TS_high_BootDweight2.Rdata")




