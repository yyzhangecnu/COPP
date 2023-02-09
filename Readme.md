# Conformal Off-Policy Prediction

This repository is the official implementation of of [Conformal Off-policy Prediction](https://arxiv.org/pdf/2206.06711.pdf).  It includes the toy example 
in Section 3.2 and the synthetic data analysis in Section 5.


### Requirements: 
The experiments is run in R 4.0.3. It requires R packages "grf", "gbm", "bartMachine","randomForest","glmnet","ggplot2"

### Training:
1. Toy example is implemented in the file "Figure1". 
    `DirectM.R`:                the function for direct method, we can choose true/false model, deterministic/random policy
    SS_low_DirectM.R:   run 100 repetitions for Direct method

    SamplingM.R:                the function for subsampling method,we can choose true/false model, deterministic/random policy
    SS_low_SamplingM.R:    run 100 repetitions for Subsampling method

    AOPM.R:                the function for COPP method,we can choose true/false model, deterministic/random policy
    SS_low_AOPM.R:   run 100 repetitions for COPP method

    conformal_utils.R:  functions for conformal prediction
    genY.R:   data generation

    All training setups in SS_low_DirectM.R,  SS_low_SamplingM.R, SS_low_AOPM.R are the same with the paper.

2.  Synthetic data analysis is implemented in the file "Figure2".

    conformal_learners.R:           prediction models such as quantile random forest        
    conformal_utils.R:                 functions for weighted conformal prediction, construct CI
    conformalCI.R:                      the main function COPP for single-stage decision making, Algorithm 1 in main paper
    conformalCI_TS.R:                 the main function COPP for multi-stage decision making, Alogorithm 1 in supp
    genY.R:                                  data generation 
    propensityscore.R:                prediction models for propensity score such as  logistic regression
    quantilereg.R:                        convert a valid outfun string to the function
    samplesplit.R:                        split samples randomly as training and calibration sets
   
    SS_high_BootDweight.R:         high dim,single-stage, implement 100 repetitions of the COPP_IS_MS method(B=100, method="BootDweight"), COPP_IS(B=1, method="Dweight")
    SS_high_BootSweight.R:          high dim,single-stage,  implement 100 repetitions of  the COPP_MS method(B=100, method="BootSweight"), COPP(B=1, method="Sweight")
    SS_high_naive.R:                      high dim,single-stage,  implement 100 repetitions of  the Subsampling-based method

    SS_low_BootDweight.R:          low dim,single-stage,  implement 100 repetitions of  COPP_IS_MS method(B=100, method="BootDweight"), COPP_IS(B=1, method="Dweight")
    SS_low_BootSweight.R:           low dim,single-stage,  implement 100 repetitions of  COPP_MS method(B=100, method="BootSweight"), COPP(B=1, method="Sweight")
    SS_low_naive.R:                       low dim,single-stage,  implement 100 repetitions of  Subsampling-based method
    SS_low_IPWE.R:                       low dim,single-stage,  implement 100 repetitions of  IS and DR method

    TS_XXX_XXX.R is the same as SS but for two-stage setting

### Results：
All results have been described in our paper. The boxplots in the paper are summarized by the 100 repetitions in the
summary.R in "Figure1" and summary_high.R, summary_low.R in "Figure2".

Contact: yyzhang@fem.ecnu.edu.cn
