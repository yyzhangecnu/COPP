#################################################
#first use all training data with T=1 for 
#quantile regression prediction


## Convert a valid outfun string to the function
#quantRF, quantBoosting, quantBART are supported
str_outfun <- function(method){
  if (method == "RF"){
    if (!requireNamespace("randomForest")){
      stop("randomForest package needs to be installed")
    }
    return(RF)
  } else if (method == "quantRF"){
    if (!requireNamespace("grf")){
      stop("grf package needs to be installed")
    }
    return(quantRF)
  } else if (method == "Boosting"){
    if (!requireNamespace("gbm")){
      stop("gbm package needs to be installed")
    }
    return(Boosting)
  } else if (method == "quantBoosting"){
    if (!requireNamespace("gbm")){
      stop("gbm package needs to be installed")
    }
    return(quantBoosting)
  } else if (method == "BART"){
    if (!requireNamespace("bartMachine")){
      stop("bartMachine package needs to be installed")
    }
    return(BART)
  } else if (method == "quantBART"){
    if (!requireNamespace("bartMachine")){
      stop("bartMachine package needs to be installed")
    }
    return(quantBART)
  } else {
    stop(paste0(method, " is not supported. Please input a valid string or a function that meets the minimal requirements described in the man page"))
  }
}

QuantileReg <- function(Ytrain,Xtrain,outfun,outparams=NULL,quantiles){
  outparams <- c(outparams, list(quantiles = quantiles))
  outparams <- c(list(Y = Ytrain, X = Xtrain), outparams)
  
  Ymodel <- function(X){
    fit <- do.call(outfun, c(outparams, list(Xtest = X)))
    fit
  }
  Ymodel
}






