
LRM<-function (formula, data, subset, weights, na.action, method = "qr",
               model = TRUE, x = FALSE, y = FALSE, qr = TRUE){
  ## Processing formula input
  ret.x <- x
  ret.y <- y
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action",
               "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  ## remove incomplete cases
  YX=na.omit(mf)
  #design matrix
  Y<-YX[,1]
  n<-nrow(YX)
  intercept=rep(1,n)
  X<-data.matrix(cbind(intercept,YX[,2:ncol(YX)]))
  p<-ncol(X)
  ## beta_hat
  betahat<-solve(t(X) %*% X) %*% t(X) %*% Y
  H<-X %*% solve(t(X) %*% X) %*% t(X)
  Yhat<-H %*% Y
  rsd<-Y-Yhat
  ## Standard errors of beta_hat
  sigma_square<- (t(rsd) %*% rsd)/ (n - p)
  var.beta_hat <- c(sigma_square)*diag(solve(t(X) %*% X))
  se.beta_hat<-sqrt(var.beta_hat)
  ##T statistics and p value
  T_stat<-betahat/se.beta_hat
  t.pvalue<-2*pt(-abs(T_stat),df=n-1)
  ## SSY SSE SSR
  In=diag(n)
  AY=In-matrix(1,n,n)/n
  SSY=t(Y) %*% AY %*% Y
  AR=H-matrix(1,n,n)/n
  SSR=t(Y) %*% AR %*% Y
  AE= diag(n) - H
  SSE=t(Y) %*% AE %*% Y
  ##R_square
  R_sqaure <-1-SSE/SSY
  ad.R_square <- 1-(SSE/(n-p))/(SSY/(n-1))
  ## F Statistics
  F_stat = (SSR/(p-1))/(SSE/(n-p))
  F.pvalue = 1-pf(F_stat,df1=p-1,df2=n-p)
  #MSE
  MSE=SSE/(n-p)
  ##output
  coefs.table<-cbind(est_beta = c(betahat),std_error = c(se.beta_hat),t.test=c(T_stat),p.value=c(t.pvalue))
  output<- list(LRM.coefs = coefs.table)
  R.squared <- list(R.squared = R_sqaure, Adjusted.R.squared = ad.R_square)
  output$R.squared<-R.squared
  F.statistics <- list(value=F_stat, numdf=p-1,dendf=n-p)
  output$F.statistics <- F.statistics
  return(output)
}

