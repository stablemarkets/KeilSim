correct_bayes_mod<-function(){
  code<-c("
data {
  int<lower=0> N;
    int<lower=0,upper=1> Y[N];
    row_vector[N] X_0;
    row_vector[N] X_1;
    int<lower=0,upper=1> L_1[N];
}

parameters {
real bl_0;
real by_0;
vector[1] bl_vec;
vector[3] by_vec;
}

model {
// priors
bl_0 ~ normal(log(.5), 1000);
by_0 ~ normal(log(.5), 1000);
bl_vec ~ normal(0, 3);
by_vec ~ normal(0, 3);

// likelihood
for(i in 1:N){
L_1[i] ~ bernoulli_logit(bl_0 + X_0[i]*bl_vec[1]);
Y[i] ~ bernoulli_logit(by_0 + by_vec[1]*X_0[i] + by_vec[2]*X_1[i] + by_vec[3]*L_1[i]);
}
}")
  return(code)
}


misspecified_bayes_mod<-function(){
  code<-c("
data {
  int<lower=0> N;
          int<lower=0,upper=1> Y[N];
          row_vector[N] X_0;
          row_vector[N] X_1;
          int<lower=0,upper=1> L_1[N];
}

parameters {
real bl_0;
real by_0;
vector[1] bl_vec;
vector[2] by_vec;
}

model {
// priors
bl_0 ~ normal(log(.5), 1000);
by_0 ~ normal(log(.5), 1000);
bl_vec ~ normal(0, 3);
by_vec ~ normal(0, 3);

// likelihood
for(i in 1:N){
L_1[i] ~ bernoulli_logit(bl_0 + X_0[i]*bl_vec[1]);
Y[i] ~ bernoulli_logit(by_0 + by_vec[1]*X_0[i] + by_vec[2]*X_1[i]);
}
}")
  return(code)
}