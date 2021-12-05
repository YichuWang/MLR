# MLR

<!-- badges: start -->
  [![R-CMD-check](https://github.com/YichuWang/MLR/workflows/R-CMD-check/badge.svg)](https://github.com/YichuWang/MLR/actions)
  
  [![codecov](https://codecov.io/gh/YichuWang/MLR/branch/main/graph/badge.svg?token=3L3EFF5LAL)](https://codecov.io/gh/YichuWang/MLR)
<!-- badges: end -->
## Introduction
### Multiple linear regression 
Multiple linear regression(MLR), also known simply as multiple regression, is a statistical technique that uses several explanatory variables to predict the
outcome of a response variable. The goal of multiple linear regression is to model the linear relationship between the explanatory (independent) variables 
and response (dependent) variables. In essence, multiple regression is the extension of ordinary least-squares (OLS) regression because it involves more than 
one explanatory variable.

### package "lm"
The package "lm" in R is used to fit linear models. It can be used to carry out regression, single stratum analysis of variance and analysis of covariance
(although aov may provide a more convenient interface for these).
However, it can't output the result directly without package "summary".
Also, sometimes we are interested in the fitted value, the important hat matrix, and the value of SSE or SSY(especially to see how they change when adding 
more predictors. And we might only need F test to decide whether reject the null hypothesis.

### package "MLR"
MLR package is easier to fit a linear regression model with or without intercept and when you just want to :
1. Find the estimated y. 
2. Find the value of SSE and/or SSY.
3. Find the value of r square and/or adjusted r square.
4. Find the t and/or f statistics and p values.
5. Find the hat matrix.

## Installation
Install
Install MLR package with vignettes from github:
```{r}
devtools::install_github("YichuWang/MLR",build_vignettes = T)
```
Load package by:
```{r}
library(MLR)
```

Use the following code to find more details about the "MLR" package:
```{r}
browseVignettes("MLR")
```
