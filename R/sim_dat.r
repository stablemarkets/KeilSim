sim_dat<-function(true_params, n){
  
  # hard coded true values from Keil's papers. Do not change.
  U<-runif(n = n, min = .4, max = .5)
  
  px0 <- .5 
  X_0<-rbinom(n = n, size = 1, prob = px0 ) 
  
  logit_pl1 <- true_params$bl_0 + true_params$bl_1*X_0 + U
  L_1<-rbinom(n = n, size = 1, inv.logit( logit_pl1 ) )
  
  logit_px1 <- true_params$bx_0 + true_params$bx_1*X_0 + true_params$bx_2*L_1
  X_1<-rbinom(n = n, size = 1, inv.logit( logit_px1 ) )
  
  py <- true_params$by_0 + U  + true_params$by_1*X_0 + true_params$by_2*X_1
  Y<-rbinom(n = n, size = 1, py  ) 
  
  d<-list(X_0=X_0, L_1=L_1, X_1=X_1, Y=Y, N=n)
  return(d)
}