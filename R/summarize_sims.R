summarize_sims<-function(res, RD, n){
  Bias_mean <- RD - mean(res[,1], na.rm = T)
  Bias_sd <- sd(RD - res[,1], na.rm = T)
  MSE <- mean((res[,1] - RD)^2 + res[,4], na.rm = T )
  Coverage <- mean( res[,2] < RD & RD < res[,3], na.rm = T )
  
  all_res<-c(N=n, RD=RD, Bias_mean=Bias_mean, 
             Bias_sd=Bias_sd, MSE=MSE, Coverage=Coverage)
  
  return(all_res)
}