boot_freq_RD<-function(boot_iter=100, d, n, N_gcomp,
                       misspecified=misspecified, freq_mod){
  boot_RD <- numeric(length=boot_iter)
  
  for(i in 1:boot_iter){
    boot_id<-sample(x = 1:n, size = n, replace = T)
    
    boot_d<-list(X_0=d$X_0[boot_id], L_1=d$L_1[boot_id], 
                 X_1=d$X_1[boot_id], Y=d$Y[boot_id], N=d$N[boot_id])
    
    boot_RD[i]<-freq_est(data = boot_d, N_gcomp = N_gcomp,
                         misspecified=misspecified, freq_mod=freq_mod)
  }
  
  res<-c(RD_hat = mean(boot_RD, na.rm=T),
         boot_lwr = quantile(boot_RD, probs = .025, na.rm = T),
         boot_upr = quantile(boot_RD, probs = .975, na.rm = T),
         boot_V = var(boot_RD,na.rm = T))
  return(res)
}