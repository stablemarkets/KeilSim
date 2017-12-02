#' Function for running simulations comparing bayesian and frequentist g-formulas.
#'
#' Simulates causal effects under setting detailed in section 4.2 of Keil et al. 2017 paper "A Bayesian approach to the g-formula". Simulation can be conducted under varying sample sizes, n, varying true risk difference values, RD, and either under a correct model, misspecified=FALSE, or misspecified model, misspecified=TRUE.
#' @param n scalar, positive interger greater than 1. This is the sample size in each simulate dataset.
#' @param RD scalar, numeric values. This represents the true risk difference under which data are simulated.
#' @param N_sims scalar, positive interger for number of simulated
#'    data sets to use.
#' @param mcmc_iter scalar, positive interger for total number of MCMC draws to
#'    take from the posterior when performing Bayesian g-compuation. Should have
#'    at least 1000 posterior draws after a sufficient warm-up period.
#' @param warmup_iter scalar, positive interger for number of warm-up
#'    (aka burn-in) draws when performing Bayesian g-computation. Must be < mcmc_iter.
#'    The total number of draws used is mcmc_iter - warmup_iter.
#'    Recommended to use at least 1000 warm-up.
#' @param N_gcomp scalar, positive interger for number of MCMC iterations to use
#'     when performing the integral involved in g-computation. See Details.
#' @param boot_iter number of nonparametric bootstrap resamples to use when
#'     performing frequentist g-computation. See Details.
#' @param output_all logical (TRUE/FALSE). If TRUE, outputs estimate for each
#'     simulated dataset. If FALSE, just outputs summary statistics across all
#'     simulated datasets.
#' @param misspecified logical (TRUE/FALSE). If TRUE, both frequentist and
#'     Bayesian g-computation is performing without adjusting for confounding.
#'     If FALSE, both models correctly adjust for confounding.
#' @param parallel logical (TRUE/FALSE). If TRUE, parallel processing is used to
#'     perform N_sims simulates in parallel. If FALSE, only single core is used.
#'     See Details.
#' @param ncores scalar, positive interger for number of cores to use if
#'     parallel==TRUE. Must be between 1 and max cores. Use snow::detectCores()
#'     to find the maximum available cores on your machine.
#' @details keilsim() performs the simulation described in Keil et al 2017 for
#'     desired simulation settings. In each iteration, we perform a Bayesian
#'     g-computation and a frequentist g-computation. Bayesian models
#'     are estimated using STAN (which in turn back-ends to C++) in the back end.
#'     R's glm() is used to estimate frequentist models. Both g-computations use
#'     MCMC to evaluate the integral involved in g-computation.
#'     The number of MCMC iterations to use in this integration is gcomp_iter.
#'     Since the data generation process involves only two time periods and
#'     binary treatments and confounders, we only need about 100 g-computation
#'     interations to accurately estimate the integral. For the frequentist
#'     method, an interval estimate for the causal effect is calculated using
#'     nonparametric bootstrap. boot_iter resamples with replacement are used.
#'     Percentiles of the sampling distribution are used to form intervals.
#'     The function includes an option to run the N_sims simulations in parallel.
#'     This is enabled for both Macs and PCs by implementing doParallel, however
#'     **reproducibility is NOT gauranteed when running in parallel** since set.seed()
#'     is not respected. set.seed() is respected only when not running in parallel.
#' @export
#' @import Rcpp rstan boot doParallel snow

keilsim <- function(n=20, RD=0, N_sims=1000, mcmc_iter=10000, warmup_iter=9900,
                    N_gcomp=1000, boot_iter=100, output_all=FALSE,
                    misspecified=FALSE, parallel=FALSE, ncores=NULL){

  # error handling: check for invalid parameter inputs #
  func_args<-mget(names(formals()),sys.frame(sys.nframe()))
  error_handle(func_args)

  ########################### Simulatio Parameters #############################

  ### these are NOT user-modifiable. This function was made to replicate
  ### and extend simulations based on the true parameters used by Keil, so
  ### these truths aren't modifiable.
  if(misspecified){
    bayes_mod<-stan_model(model_code = misspecified_bayes_mod())
    freq_mod<-list(formula_l= L_1 ~ X_0,
                   formula_y = Y ~ X_0 + X_1) # don't condition on L_1
  }else{
    bayes_mod<-stan_model(model_code = correct_bayes_mod())
    freq_mod<-list(formula_l=  L_1 ~ X_0,
                   formula_y = Y ~ X_0 + X_1 + L_1) # condition on L_1
  }

  true_params<-list(bl_0= -1, bl_1=1,
                    bx_0= -1, bx_1=1, bx_2=1,
                    by_0=  0, by_1=.5*RD, by_2=.5*RD)

  pars<-c("bl_0","by_0",'bl_vec', 'by_vec')

  # get platform
  N_draws <- mcmc_iter - warmup_iter

  bayes_res_all<-matrix(NA, nrow=N_sims, ncol=4)
  freq_res_all<-matrix(NA, nrow=N_sims, ncol=4)

  ########################### Run Simulation    ################################
  if(parallel){
    #clus<-snow::makeCluster(ncores)
    registerDoParallel(cores = ncores)

    r<-foreach(i=1:N_sims,
               .export = c(ls(environment()),ls(.GlobalEnv)),
               .packages = pckgs,
               .combine = 'c') %dopar% {

                 d <- sim_dat(true_params, n)

                 stan_res <- sampling(object = bayes_mod, data = d, pars = pars,
                                      chains=1, iter=mcmc_iter, warmup=warmup_iter)
                 post_dist <- extract(stan_res, pars)

                 bayes_res_all <- bayes_gcomp(post_dist = post_dist,
                                              N_draws = N_draws, N_gcomp=N_gcomp,
                                              misspecified = misspecified)

                 freq_res_all <- boot_freq_RD(freq_mod=freq_mod, boot_iter = boot_iter,
                                              n=n, d=d, N_gcomp=N_gcomp,
                                              misspecified = misspecified)
                 list(bayes_res_all, freq_res_all)

               }
    closeAllConnections()

    bayes_res_all<-do.call(rbind, r[seq(1,2*N_sims-1,2)])
    freq_res_all<-do.call(rbind, r[seq(2,2*N_sims,2)])
  }else{
    for(i in 1:N_sims){
      d <- sim_dat(true_params, n)

      stan_res <- sampling(object = bayes_mod, data = d, pars = pars,
                           chains=1, iter=mcmc_iter, warmup=warmup_iter)
      post_dist <- extract(stan_res, pars)

      bayes_res_all[i,] <- bayes_gcomp(post_dist = post_dist,
                                       N_draws = N_draws, N_gcomp=N_gcomp,
                                       misspecified = misspecified)

      freq_res_all[i,] <- boot_freq_RD(freq_mod=freq_mod, boot_iter = boot_iter,
                                       n=n, d=d, N_gcomp=N_gcomp,
                                       misspecified = misspecified)
    }
  }

  ########################### Summarize Results ################################

  sum_stats_bayes<-summarize_sims(res = bayes_res_all, RD = RD, n)
  sum_stats_freq<-summarize_sims(res = freq_res_all, RD = RD, n)

  sum_stack<-rbind(sum_stats_freq, sum_stats_bayes)
  sum_stack<-cbind(sum_stack,
                   MSE_Ratio = sum_stack[,'MSE']/sum_stats_freq['MSE'] )

  ########################### Output Results    ################################
  if(output_all){
    res<-list(bayes_sims=bayes_res_all, freq_sim=freq_res_all,
              sum_stats = sum_stack)
  }else{
    res<-list(sum_stats=sum_stack)
  }

  return(res)
}
