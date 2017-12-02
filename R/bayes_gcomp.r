bayes_gcomp<-function(post_dist, N_draws, N_gcomp,
                      misspecified=misspecified){
  
  RD_post_dist <- numeric(length=N_draws)
  for(draw in 1:N_draws){
    
    draw_params<-list(bl_0= post_dist$bl_0[draw], 
                      bl_1= post_dist$bl_vec[draw], 
                      by_0= post_dist$by_0[draw], 
                      by_1= post_dist$by_vec[draw,1], 
                      by_2= post_dist$by_vec[draw,2])
    
    if(!misspecified){
      draw_params <- c(draw_params, by_3 = post_dist$by_vec[draw,3])
    }
    
    post_Y11<-gcomp_iter(draw_params, N_gcomp, interv = list(X_0=1, X_1=1),
                         misspecified=misspecified)
    post_Y00<-gcomp_iter(draw_params, N_gcomp, interv = list(X_0=0, X_1=0),
                         misspecified=misspecified)
    
    RD_post_dist[draw]<-mean(post_Y11 - post_Y00)
  }
  
  res<-c(post_mean = mean(RD_post_dist, na.rm = T),
         post_lwr = quantile(RD_post_dist, probs = .025, na.rm = T),
         post_upr = quantile(RD_post_dist, probs = .975, na.rm = T),
         V = var(RD_post_dist, na.rm = T))
  
  return(res)
}