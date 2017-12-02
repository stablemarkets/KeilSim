error_handle<-function(func_args){
  is.integer<-function(x) x %% 1 == 0
  
  if( ! (is.integer(func_args$n) & func_args$n>1  ) ){ 
    stop("ERROR: n must be an integer greater than 1.")
  }
  if( ! (is.numeric(func_args$RD)  & is.finite(func_args$RD) ) ){ 
    stop("ERROR: RD must be numeric and finite.")
  }
  if( ! (is.integer(func_args$mcmc_iter) & 
         func_args$mcmc_iter > func_args$warmup_iter) & func_args$mcmc_iter>0  ){
    stop("ERROR: mcmc_iter must be positive integer greater than warmup_iter")
  }else if(func_args$mcmc_iter<10000){
    warning("Warning (non-fatal): >=10000 iterations recommended")
  }
  if( ! (is.integer(func_args$warmup_iter) & 
         func_args$mcmc_iter > func_args$warmup_iter & func_args$warmup_iter>0)  ){
    stop("ERROR: warmup_iter must be a positive integer less than mcmc_iter")
  }else if(func_args$warmup_iter<1000){
    warning('Warning (non-fatal): warmup >=1000 recommended')
  }
  if( ! (is.integer(func_args$N_gcomp) & func_args$N_gcomp>0)  ){
    stop("ERROR: N_gcomp must be interger greater than 0.")
  }else if(func_args$N_gcomp<func_args$n){
    warning("Warning (non-fatal): N_gcom >= n recommended")
  }
  if( !(is.integer(func_args$boot_iter) & func_args$boot_iter>0 ) ){
    stop("ERROR: boot_iter must be interger greater than 0.")
  }else if(func_args$boot_iter<100){
    warning("Warning (non-fatal): at least 100 bootstrap samples recommended.")
  }
  if( !(is.logical(func_args$output_all)) ){ 
    stop("ERROR: output_all must be either TRUE or FALSE") 
  }
  if( !(is.logical(func_args$misspecified)) ){ 
    stop("ERROR: misspecified must be either TRUE or FALSE") 
  }
  if( !(is.logical(func_args$parallel)) ){ 
    stop("ERROR: parallel must be either TRUE or FALSE") 
  }else if(func_args$parallel==FALSE & !is.null(func_args$ncores) ){
    stop("ERROR: if parallel is FALSE then ncores must be NULL")
  }else if(func_args$parallel==TRUE & is.null(func_args$ncores)){
    stop("ERROR: if parallel==TRUE then ncores cannot be null")
  }
  if(!is.null(func_args$ncores)){
    if( !(is.integer(func_args$ncores) & func_args$ncores>1 & func_args$parallel ) ){ 
      stop(c("ERROR: ncores must only be specified if parallel==TRUE",
             "   1 < ncores <= max cores ",
             " find max cores using detectCores() "))
    }
  }
}