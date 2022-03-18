ttest <- function(m1, sd1, n1, m2, sd2, n2) {
  diff <- m1 - m2
  df1 <- n1-1
  df2 <- n2-1
  var1 <- sd1^2
  var2 <- sd2^2
  pooled.var <- (df1*var1 + df2*var2) / (df1 + df2)
  
  se <- sqrt(pooled.var*((n1 + n2)/(n1*n2)))
  
  d <- (diff / sqrt(pooled.var))
  t <- diff / se
  df <- n1+n2-2
  p <- 2*(1-pt(abs(t),df=df))
  
  CI.lower <- diff - qt(.975, df)*se
  CI.upper <- diff + qt(.975, df)*se
          
          return(list(t=t, df=df, p=p, d = d, se = se, pooled.var = pooled.var, diff=diff, CI.lower = CI.lower, CI.upper = CI.upper))
          
}