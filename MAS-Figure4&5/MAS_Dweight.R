rm(list=ls())
source("genY.R")
source("samplesplit.R")
source("conformal_learners.R")
source("propensityscore.R")
source("quantilereg.R")
source("conformal_utils.R")
source("conformalCI_MAS_expr.R")

# Generate data
n <- 2000
d <- 1

trainprop <- 0.75
quantiles <- c(0.05,0.95)
psfun <- RF
psparams <- NULL
outfun <- quantRF
outparams <- NULL
type <- "CQR"
side <- "two"
alpha <- 0.1
wthigh <- 20
wtlow <- 0.05
method <- "BootSweight" #Sweight, BootSweight, Dweight, BootDweight
B <- 10 #B=1 for Sweight or Dweight, B>1 for BootSweight or BootDweight

CovProb <- list()
AveLeng <- list()
ALOrac <- list()
Nt <- list()


for (s in 1:100) {
  
  #generate data
  Data_train <- genY_MAS(n*trainprop,epsilon=0.3)
  Data_val <- genY_MAS(n*(1-trainprop),epsilon=0.3)
  #generate test data
  ntest <- 3000
  Data_test <- genY_MAS(ntest,epsilon=0.2)
  
  weighted <- TRUE
  out <- ConformalCI_MAS_expr(Data_train,Data_val,Data_test,method,B,trainprop,psfun,psparams,outfun,outparams,quantiles,type,side,weighted,alpha,wthigh,wtlow)
  
  Ytest <- Data_test$Y
  if((B==1) & (method == "Sweight" | method == "Dweight")){
    
  #summary for single split
  ind <- (Ytest<unlist(out$rights))&(Ytest>unlist(out$lefts))
  CovProb[s] <- sum(ind)/ntest
  AveLeng[s] <- mean(out$Lens)
  #ALOrac[s] <- mean((1+Dtest)*(1+Xtest[,1]+Xtest[,2]+Xtest[,3]+Xtest[,4])*(qnorm(1-alpha/2,0,1)-qnorm(alpha/2,0,1)))
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
mean(unlist(CovProb))
median(unlist(CovProb))

save.image("MAS_BootSweight_expr.Rdata")
