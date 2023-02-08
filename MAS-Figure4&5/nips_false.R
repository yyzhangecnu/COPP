rm(list=ls())
source("genY.R")
source("samplesplit.R")
source("conformal_learners.R")
source("propensityscore.R")
source("quantilereg.R")
source("conformal_utils.R")
source("nips.R")

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
method <- "Sweight" #Sweight, BootSweight, Dweight, BootDweight
B <- 1 #B=1 for Sweight or Dweight, B>1 for BootSweight or BootDweight

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
  out <-nips(Data_train, Data_val, Data_test,a=0.1,quantiles, type, side, alpha, wthigh, wtlow)

  CovProb[s] <- out$CovProb
  AveLeng[s] <- out$AveLeng
  print(CovProb[s])
  print(AveLeng[s])
  print(s)
}
mean(unlist(CovProb))
median(unlist(CovProb))

save.image("nips_false.Rdata")
