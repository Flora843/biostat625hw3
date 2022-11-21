

LRM<-function (formula, data){
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
  if (ncol(YX)==2){
    X=data.matrix(cbind(intercept,YX[,2]))
  }
  else if (ncol(YX)>2){
    X=data.matrix(cbind(intercept,YX[,2:ncol(YX)]))
  }
  p<-ncol(X)
  ## check if the design matrix is invertible
  XI=try(solve(t(X)%*%X),silent=T)
  if(!is.matrix(XI)){
    print("The X matrix is invertible, failed to build LRM")
    return(-1)
  }
  if((n-p)==0){
    print("Failed to do F statistics")
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
  ##R_square
  R_sqaure <-1-SSE/SSY
  ad_R_square <- 1-(SSE/(n-p))/(SSY/(n-1))
  ## F Statistics
  F_stat = (SSR/(p-1))/(SSE/(n-p))
  F_pvalue = 1-pf(F_stat,df1=p-1,df2=n-p)
  ##MSE
  MSE=SSE/(n-p)
  ##output
  coefs_table<-cbind(est_beta = c(betahat),std_error = c(se_beta_hat),t_test=c(T_stat),p_value=c(t_pvalue))
  output<- list(LRM_coefs = coefs_table)
  R.squared <- list(R_squared = R_sqaure, Adjusted.R.squared = ad_R_square)
  output$R.squared<-R.squared
  F.statistics <- list(value=F_stat, numdf=p-1,dendf=n-p)
  output$F.statistics <- F.statistics
  return(output)


}

