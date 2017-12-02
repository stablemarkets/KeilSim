freq_est <- function(data, N_gcomp, freq_mod, 
                     misspecified=misspecified){
  
  formula_l<-freq_mod$formula_l
  formula_y<-freq_mod$formula_y
  
  glm_l <- glm(data = data, 
               formula = formula_l, family=binomial('logit'))
  glm_y <- glm(data = data, 
               formula = formula_y, family=binomial('logit'))
  
  freq_res <- list(bl_0=glm_l$coefficients['(Intercept)'],
                   bl_1=glm_l$coefficients['X_0'],
                   by_0=glm_y$coefficients['(Intercept)'],
                   by_1=glm_y$coefficients['X_0'],
                   by_2=glm_y$coefficients['X_1'])
  
  if(!misspecified){
    freq_res <- c(freq_res, by_3=glm_y$coefficients['L_1'])
  }
  
  Y11_hat<-gcomp_iter(freq_res, N_gcomp, interv = list(X_0=1, X_1=1),
                      misspecified = misspecified)
  Y00_hat<-gcomp_iter(freq_res, N_gcomp, interv = list(X_0=0, X_1=0),
                      misspecified = misspecified)
  
  RD_hat<-mean(Y11_hat - Y00_hat, na.rm=T)
  
  freq_gcomp_res<-c(RD_hat=RD_hat)
  
  return(freq_gcomp_res)
}