#################################################
#first use all training data with T=1 and T=0
#to training propensity score and predict propensity
#score for testing data points


## Convert a valid psfun string to the function
str_psfun <- function(method){
  if (method == "RF"){
    if (!requireNamespace("randomForest")){
      stop("randomForest package needs to be installed")
    }
    return(RF)
  } else if (method == "Boosting"){
    if (!requireNamespace("gbm")){
      stop("gbm package needs to be installed")
    }
    return(Boosting)
  } else {
    stop(paste0(method, " is not supported. Please input a valid string or a function that meets the minimal requirements described in the man page"))
  }
}


#psfun: only Boosting and RF are supported
PropScore <- function(Ttrain,Xtrain,psfun, psparams){
  
  if (is.null(psfun)){
    psfun <- Boosting
  } else if (is.character(psfun)){
    psfun <- str_psfun(psfun[1])
  } 
  
  psparams0 <- psparams
  psparams <- c(list(Y = Ttrain, X = Xtrain), psparams0)
  PropScorefun <- function(X){
    ps <- do.call(psfun, c(list(Xtest = X), psparams))
    ps
  }
  PropScorefun
}