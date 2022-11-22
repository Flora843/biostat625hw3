
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LRM

<!-- badges: start -->

[![R-CMD-check](https://github.com/Flora843/biostat625hw3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Flora843/biostat625hw3/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/Flora843/biostat625hw3/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Flora843/biostat625hw3?branch=main)
<!-- badges: end -->

## Introduction

### *What is “LRM”?*

This R package helps users to fit a linear regression model. It
calculate some coefficients and carry out some important tests of the
existing `stats::lm` function. Besides, it can calculate the confidence
interval of estimated beta. However, it can’t fit a model without an
intercept properly.

### *What users can obtain with “LRM”?*

Here are some results that the users can obtain using `LRM` package:

-   Get the point estimates and standard errors of $\hat{\beta}$
-   Obtain the $t$-statistics and the corresponding p value.
-   Get the confidence interval of $\hat{\beta}$
-   Get the $R^2$ and adjusted-$R^2$.
-   Obtain the $F$-statistics

## Installation

You can install the development version of LRM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Flora843/biostat625hw3")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(LRM)
LRM(mpg~wt+drat,data=mtcars)$LRM_coefs
#>              est_beta std_error     t_test      p_value
#> intercept   30.290370 7.3178783  4.1392285 2.477121e-04
#> predictor_1 -4.782890 0.7970353 -6.0008515 1.224127e-06
#> predictor_2  1.442491 1.4585676  0.9889776 3.303310e-01
LRM(mpg~wt+drat,data=mtcars)$F.statistics
#> $value
#>          [,1]
#> [1,] 46.14331
#> 
#> $numdf
#> [1] 2
#> 
#> $dendf
#> [1] 29
LRM(mpg~wt+drat,data=mtcars)$R.squared
#> $R_squared
#>          [,1]
#> [1,] 0.760897
#> 
#> $Adjusted.R.squared
#>           [,1]
#> [1,] 0.7444071
LRM(mpg~wt+drat,data=mtcars)$Confidence.Interval
#>       CI_lower  CI_upper
#> [1,] 15.323629 45.257112
#> [2,] -6.413010 -3.152770
#> [3,] -1.540615  4.425596
```
