#'LRM
#'
#'The LRM function will fit a linear regression model based on given data set. It shows the summary of coefficients, confidence interval and covariance table of estimated parameters of the model.
#'The function may not explore interaction between predictors and it cannot fit a model without an intercept properly.
#'The function can fit data with NA values.
#'
#'@param formula It's similar to "formula" in the R function lm(): a symbolic description of the model to be fitted.
#'
#'@param data A data frame, containing both the predictors of interests and outcome variables.
#'
#'@param alpha A number represents the significance level, used to calculate confidence interval of estimated parameters (alpha=0.05 by default).
#'
#'@return The function returns the Coefficients' matrix with the estimated value, Standard error, t-statistics and p-value of each coefficient. It also automatically print the F statistics, R squared and adjusted R squared. Besides, it also returns the covariance matrix and confidence interval table
#'
#'@examples
#'LRM(mpg~wt+drat,data=mtcars,alpha=0.01)
#'LRM(mpg~wt,data=mtcars,alpha=0.05)
#'
#'@import statsL bench
#'@export
#'LRM

LRM<-function (formula, data, alpha=0.05){
  ## Processing formula input
  mf<-match.call(expand.dots = FALSE)
  m<-match(c("formula", "data"),
             names(mf), 0L)
  mf<-mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  ## remove incomplete cases
  YX=na.omit(mf)
  ## design matrix
  Y<-YX[,1]
  n<-nrow(YX)
  intercept=rep(1,n)
  X=data.matrix(cbind(intercept,YX[,2:ncol(YX)]))
  p<-ncol(X)
  ## add predictors' name
  predictor_name <- rep("0", p)
  predictor_name[1]= "intercept"
  for (i in 2:p) {
    predictor_name[i]=colnames(mf)[i]
  }
  colnames(X)=predictor_name
  ## check if the design matrix is invertible
  XI=try(solve(t(X)%*%X),silent=T)
  if(!is.matrix(XI)){
    print("The X matrix is invertible, failed to build LRM")
    return(-1)
  }
  if((n-p)==0){
    print(" NO F statistics")
    return(-1)
  }

  ## beta_hat
  betahat<-XI %*% t(X) %*% Y
  H<-X %*% XI %*% t(X)
  Yhat<-H %*% Y
  rsd<-Y-Yhat

  ## Standard errors of beta_hat
  sigma_square_hat<- (t(rsd) %*% rsd)/(n-p)
  var_beta_hat <- diag(as.numeric(sigma_square_hat)*as.matrix(XI))
  se_beta_hat<-sqrt(var_beta_hat)
  ##T statistics and p value
  T_stat<-betahat/se_beta_hat
  t_pvalue<-2*pt(-abs(T_stat),df=n-1)
  ## SSY SSE SSR
  In=diag(n)
  AY=In-matrix(1,n,n)/n
  SSY=t(Y) %*% AY %*% Y
  AR=H-matrix(1,n,n)/n
  SSR=t(Y) %*% AR %*% Y
  AE= In - H
  SSE=t(Y) %*% AE %*% Y
  ##R squared
  R_sqaure <-1-SSE/SSY
  ad_R_square <- 1-(SSE/(n-p))/(SSY/(n-1))
  ## F Statistics
  F_stat=(SSR/(p-1))/(SSE/(n-p))
  F_pvalue=1-pf(F_stat,df1=p-1,df2=n-p)
  ##MSE
  MSE=SSE/(n-p)
  ## covariance matrix
  cov_matrix<-as.numeric(sigma_square_hat)*as.matrix(XI)

  ## calculate confidence interval of estimated coefficients

  t <- qt(p=1-alpha/2, df=(n - p))
  betahat_CIu<-betahat+t*se_beta_hat
  betahat_CId<-betahat-t*se_beta_hat

  ##output
  coefs_table<-cbind(est_beta = c(betahat),std_error = c(se_beta_hat),t_test=c(T_stat),p_value=c(t_pvalue))
  output<- list(LRM_coefs = coefs_table)
  output$covariance.matrix=cov_matrix
  R.squared <- list(R_squared = R_sqaure, Adjusted.R.squared = ad_R_square)
  output$R.squared<-R.squared
  F.statistics <- list(value=F_stat, numdf=p-1,dendf=n-p)
  output$F.statistics <- F.statistics
  CI_table<- cbind(CI_lower=c(betahat_CId), CI_upper=c(betahat_CIu))
  output$Confidence.Interval<-CI_table
  return(output)
}

