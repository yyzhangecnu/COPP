rm(list=ls())
source("genY.R")
source("samplesplit.R")
source("conformal_learners.R")
source("propensityscore.R")
source("quantilereg.R")
source("conformal_utils.R")
source("conformalCI_SDM.R") 

# Generate data
n <- 2000
d <- 1
H <- 3
Teta <- c(-0.5,1)
delta <- 0.1

trainprop <- 0.75
quantiles <- c(0.05,0.95)
psfun <- Logistic
psparams <- NULL
outfun <- quantRF
outparams <- NULL
method <- "BootSweight"
B <- 10
type <- "CQR"
side <- "two"
alpha <- 0.1
wthigh <- 20
wtlow <- 0.05

Deta <- c(-0.5,0.5)

CovProb <- list()
AveLeng <- list()
Nt <- list()

for (s in 1:100) {
  
  Data <- genY_SDM(H,Teta,delta,n)
  
  #generate test data
  ntest <- 3000
  Data_test <- genY_SDM(H,Deta,delta,ntest)
  
  weighted <-TRUE
  out <- ConformalCI_SDM(Data,H,Deta,Data_test,method,B,trainprop,psfun,psparams,outfun,outparameters,quantiles,type,side,weighted,alpha,wthigh,wtlow)
  Ytest <- Data_test$Y_T[[H]]
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

save.image("SDM_H3_BootSweight.Rdata")
mean(unlist(CovProb))
median(unlist(CovProb))



