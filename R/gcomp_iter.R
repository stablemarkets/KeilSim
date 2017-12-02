gcomp_iter<- function(params, n, interv, misspecified){
  X_0<-interv$X_0
  L_1<-rbinom(n = n, size = 1, inv.logit( params$bl_0 + params$bl_1*X_0  ) )
  X_1<-interv$X_1
  
  if(misspecified){
    logit_py<-params$by_0 + params$by_1*X_0 + params$by_2*X_1
  }else{
    logit_py<-params$by_0 + params$by_1*X_0 + params$by_2*X_1 + params$by_3*L_1   
  }
  
  Y<-rbinom(n = n, size = 1, inv.logit( logit_py ) ) 
  return(Y)
}