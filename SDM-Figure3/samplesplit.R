############################################
#split samples to training and validation
#for both T=1 and T=0
SampleSplit <- function(Y,trainprop){
  T <- as.numeric(!is.na(Y))
  inds1 <- which(T == 1)
  inds0 <- which(T == 0)
  n1 <- length(inds1)
  n0 <- length(inds0)
  trainid1 <- sample(n1, floor(n1 * trainprop))
  trainid0 <- sample(n0, floor(n0 * trainprop))
  trainid <- c(inds1[trainid1], inds0[trainid0])
  return(list(trainid=trainid, trainid1=trainid1,trainid0=trainid0))
}