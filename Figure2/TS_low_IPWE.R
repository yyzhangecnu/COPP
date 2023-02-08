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

IPW_CP <- vector("list",length = 1)
IPW_AL <- vector("list",length = 1)

AIPW_CP <- vector("list",length = 1)
AIPW_AL <- vector("list",length = 1)


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
  
  
  tp_D1 <- exp(cbind(1,X1)%*%Deta1)/(1+exp(cbind(1,X1)%*%Deta1))
  tp_D2 <- exp(cbind(1,X2)%*%Deta2)/(1+exp(cbind(1,X2)%*%Deta2))
  tp_A1 <- tp_D1*(1-tp_T1)/(tp_D1*(1-tp_T1)+tp_T1*(1-tp_D1)) 
  A1 <-rbinom(n = n, size = 1, prob=tp_A1)
  tp_A2 <- tp_D2*(1-tp_T2)/(tp_D2*(1-tp_T2)+tp_T2*(1-tp_D2)) 
  A2 <-rbinom(n = n, size = 1, prob=tp_A2)
  
  O = (I(A1==T1))&(I(A2==T2))
  tp_O1 <- tp_T1*(1-tp_T1)/(tp_D1*(1-tp_T1)+tp_T1*(1-tp_D1))
  tp_O2 <- tp_T2*(1-tp_T2)/(tp_D2*(1-tp_T2)+tp_T2*(1-tp_D2))
  tp_O <- tp_O1*tp_O2
  
  X1 <- matrix(X1,ncol = 1)
  OutputFit <- QuantileReg(Y[which(O==1)],X1[which(O==1), ,drop=FALSE],outfun,outparams,quantiles=0.5)
  X1 <- as.data.frame(X1)
  Yhat <- OutputFit(X1) 
  #AIPW_band <- c(0.0001,0.0003,0.0005,0.0007,0.001)
  
  X1 <- as.matrix(X1,ncol = 1)
  AIPW_band <- 10^(-8)
  for (k in 1:length(AIPW_band)) {
    b <- AIPW_band[k]
    ker <- function(x){
      out  <- apply(X1, 1, function(y) exp(-(t(x-y))%*%(x-y)/(2*b)))
      out <- out/mean(out)
      return(out)
    }
    AIPW_IN <- rep(0,ntest)
    AIPW_LE <- rep(0,ntest)
    for (j in 1:ntest) {
      kers  <- ker(X1test[j])
      temp <- ((Y*O)/tp_O-(O-tp_O)*Yhat/tp_O)*kers
      Left <- mean(temp)-qnorm(0.95,0,1)*(sd(temp)/sqrt(n))
      Right <- mean(temp)+qnorm(0.95,0,1)*(sd(temp)/sqrt(n))
      AIPW_IN[j] <- (Ytest[j]>Left)&(Ytest[j]<Right)
      AIPW_LE[j] <- qnorm(0.95,0,1)*(sd(temp)/sqrt(n))*2
    }
    AIPW_CP[[k]] <- c(AIPW_CP[[k]],mean(AIPW_IN))
    AIPW_AL[[k]] <- c(AIPW_AL[[k]],mean(AIPW_LE))
    print(k)
  }
  
  
  #IPW_band <- c(0.001,0.003,0.005,0.007,0.01)
  X1 <- as.matrix(X1,ncol = 1)
  IPW_band <- 10^(-5)
  for (k in 1:length(AIPW_band)) {
    b <- IPW_band[k]
    ker <- function(x){
      out  <- apply(X1, 1, function(y) exp(-(t(x-y))%*%(x-y)/(2*b)))
      out <- out/mean(out)
      return(out)
    }
    IPW_IN <- rep(0,ntest)
    IPW_LE <- rep(0,ntest)
    for (j in 1:ntest) {
      kers  <- ker(X1test[j])
      temp <- ((Y*O)/tp_O)*kers
      Left <- mean(temp)-qnorm(0.95,0,1)*(sd(temp)/sqrt(n))
      Right <- mean(temp)+qnorm(0.95,0,1)*(sd(temp)/sqrt(n))
      IPW_IN[j] <- (Ytest[j]>Left)&(Ytest[j]<Right)
      IPW_LE[j] <- qnorm(0.95,0,1)*(sd(temp)/sqrt(n))*2
    }
    IPW_CP[[k]] <- c(IPW_CP[[k]],mean(IPW_IN))
    IPW_AL[[k]] <- c(IPW_AL[[k]],mean(IPW_LE))
    print(k)
  }
  time_e <- proc.time()
  #time_e-time_b
  
  print(AIPW_CP[[1]][s])
  print(AIPW_AL[[1]][s])
  print(IPW_CP[[1]][s])
  print(IPW_AL[[1]][s])
  print(s)
}

save.image("TS_low_IPWE.Rdata")




