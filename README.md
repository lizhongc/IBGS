# IBGS
 Here is a brief introduction to the iterated block Gibbs sampler algorithm (IBGS).

## Requirement
 This package requires package "doParallel" to run and only works under non-windows environment.
 
## Models
 IBGS works for the generalized linear models containing linear, logistic, and poisson.
 
## Model selection criteria
 IBGS uses AIC, AICc, BIC and exBIC to evaluate the performance of candidate models.
 
## Prediction
 IBGS can use a model averaging scheme based on the smooth-SIC weights.
