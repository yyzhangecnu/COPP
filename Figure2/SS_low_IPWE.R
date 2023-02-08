rm(list=ls())
source("genY.R")
source("samplesplit.R")
source("conformal_learners.R")
source("propensityscore.R")
source("quantilereg.R")
source("conformal_utils.R")
source("conformalCI.R")

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

IPW_CP <- vector("list",length = 1)
IPW_AL <- vector("list",length = 1)

AIPW_CP <- vector("list",length = 1)
AIPW_AL <- vector("list",length = 1)



for (s in 1:100) {
  
  time_b <- proc.time()
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
  
  # Xtest <- as.data.frame(Xtest)
  # weighted <- TRUE
  # out <- ConformalCI(X,Y,T,Deta,Xtest,method,B,trainprop,psfun,psparams,outfun,outparams,quantiles,type,side,weighted,alpha,wthigh,wtlow)
  
  tp_D <- exp(cbind(1,X)%*%Deta)/(1+exp(cbind(1,X)%*%Deta))
  tp_A <- tp_D*(1-tp_T)/(tp_D*(1-tp_T)+tp_T*(1-tp_D)) 
  A <-rbinom(n = n, size = 1, prob=tp_A)
  
  O = I(A==T)
  tp_O <- tp_T*(1-tp_T)/(tp_D*(1-tp_T)+tp_T*(1-tp_D))
  
  # B <- 1000
  # Boot <- rep(0,B)
  # for(b in 1:B){
  #   ind <- sample(n, n,replace = TRUE)
  #   Boot[b] <- mean(temp[ind])
  # }
  OutputFit <- QuantileReg(Y[which(O==1)],X[which(O==1),],outfun,outparams,quantiles=0.5)
  Yhat <- OutputFit(X) 
  #AIPW_band <- c(0.0001,0.0003,0.0005,0.0007,0.001)
  
  AIPW_band <- 0.0001
  for (k in 1:length(AIPW_band)) {
    b <- AIPW_band[k]
    ker <- function(x){
      out  <- apply(X, 1, function(y) exp(-(t(x-y))%*%(x-y)/(2*b)))
      out <- out/mean(out)
      return(out)
    }
    AIPW_IN <- rep(0,ntest)
    AIPW_LE <- rep(0,ntest)
    for (j in 1:ntest) {
      kers  <- ker(Xtest[j, ])
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
  
  IPW_band <- 0.005
  for (k in 1:length(AIPW_band)) {
    b <- IPW_band[k]
    ker <- function(x){
      out  <- apply(X, 1, function(y) exp(-(t(x-y))%*%(x-y)/(2*b)))
      out <- out/mean(out)
      return(out)
    }
    IPW_IN <- rep(0,ntest)
    IPW_LE <- rep(0,ntest)
    for (j in 1:ntest) {
      kers  <- ker(Xtest[j, ])
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

save.image("SS_low_IPWE.Rdata")
