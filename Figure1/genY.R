genY <- function(X){
  n <- nrow(X)
  2 / (1 + exp(-12 * (X[, 1] - 0.5))) * 2 / (1 + exp(-12 * (X[, 2] - 0.5))) + rnorm(n)
}

genY_singlestage <- function(X,T){
  n <- length(T)
  Y <- 1+X[,1]-X[,2]+X[,3]^{3}+exp(X[,4])+T*(3-5*X[,1]+2*X[,2]-3*X[,3]+X[,4])+(1+T)*(1+X[,1]+X[,2]+X[,3]+X[,4])*rnorm(n)
  return(Y)
}

genY_twostage <- function(X1,X2,T1,T2){
  n <- length(T1)
  Y <- 1+X1+T1*(1-3*(X1-0.2)^{2})+X2+T2*(1-5*(X2-0.4)^{2})+(1+0.5*T1-T1*X1+0.5*T2-T2*X2)*rnorm(n,0,sd=0.4)
  return(Y)
  }