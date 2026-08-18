data{
  int<lower=1> N;
  array[N] int<lower=0,upper=1> y;
  array[N] int<lower=0,upper=1> trt;
  array[N] int<lower=0,upper=1> adolescent;
  int<lower=1> J_country;
  array[N] int<lower=1,upper=J_country> country;
  int<lower=1> J_site;
  array[N] int<lower=1,upper=J_site> site;
  int<lower=1> J_group;
  array[N] int<lower=1,upper=J_group> group;
  int<lower=1> P;
  matrix[N,P] X;
}
parameters{
  // Fixed effects
  //----------------------------------------------------------
  real alpha;                  // intercept
  real beta_trt;               // intervention effect
  real beta_age;               // adolescent effect
  vector[P] beta;              // HIV + smear + cavity

  // Country random intercepts
  //----------------------------------------------------------
//  vector[J_country] u_country;
  real<lower=0> sigma_country;

  // Site random intercepts
  //----------------------------------------------------------
//  vector[J_site] u_site;
  real<lower=0> sigma_site;

  // Lineage-group random intercepts
  //----------------------------------------------------------
//  vector[J_group] u_group;
  real<lower=0> sigma_group;

  // Random treatment effect by country
  //----------------------------------------------------------
//  vector[J_country] b_country;
  real<lower=0> sigma_country_trt;

  // Random treatment effect by lineage group
  //----------------------------------------------------------
//  vector[J_group] b_group;
  real<lower=0> sigma_group_trt;

vector[J_country] z_country;
vector[J_site] z_site;
vector[J_group] z_group;

vector[J_country] z_country_trt;
vector[J_group] z_group_trt;
}
transformed parameters{
  vector[J_country] u_country = sigma_country * z_country;
  vector[J_site] u_site = sigma_site * z_site;
  vector[J_group] u_group = sigma_group * z_group;
  vector[J_country] b_country =
      sigma_country_trt * z_country_trt;
  vector[J_group] b_group =
      sigma_group_trt * z_group_trt;
}
model{
  // PRIORS
  //----------------------------------------------------------
  alpha ~ normal(0, 5);

  // Fixed effects
  //----------------------------------------------------------
  beta_trt ~ normal(0, 1);
  beta_age ~ normal(0, 1);
  beta ~ normal(0, 1);

  // Random-effect standard deviations
  //----------------------------------------------------------
  sigma_country ~ exponential(1);
  sigma_site ~ exponential(1);
  sigma_group ~ exponential(1);
  sigma_country_trt ~ exponential(1);
  sigma_group_trt ~ exponential(1);

  // Random intercepts
  //----------------------------------------------------------
  z_country ~ std_normal();
  z_site ~ std_normal();
  z_group ~ std_normal();

  z_country_trt ~ std_normal();
  z_group_trt ~ std_normal();

  //----------------------------------------------------------
  // Likelihood
  //----------------------------------------------------------
for(i in 1:N){

  real eta;

  eta =
      alpha
    + beta_trt * trt[i]
    + beta_age * adolescent[i]
    + dot_product(X[i], beta)

    + u_country[country[i]]
    + u_site[site[i]]
    + u_group[group[i]]

    + b_country[country[i]] * trt[i]
    + b_group[group[i]] * trt[i];;

  y[i] ~ bernoulli_logit(eta);

}
}
generated quantities{
// Overall
real RD;
real OR;
// Country-specific
vector[J_country] RD_country;
vector[J_country] OR_country;
// Lineage-group-specific
vector[J_group] RD_group;
vector[J_group] OR_group;
// Optional
matrix[J_country,J_group] RD_country_group;
matrix[J_country,J_group] OR_country_group;

  //----------------------------------------------------------
  // Overall treatment effect
  //----------------------------------------------------------

  OR = exp(beta_trt);
  {
    real risk_control = 0;
    real risk_treatment = 0;
    for(i in 1:N){
      real eta0;
      real eta1;
      eta0 =
        alpha
      + beta_age * adolescent[i]
      + dot_product(X[i], beta)
      + u_country[country[i]]
      + u_site[site[i]]
      + u_group[group[i]];
    eta1 =
        eta0
      + beta_trt
      + b_country[country[i]]
      + b_group[group[i]];
      risk_control += inv_logit(eta0);
      risk_treatment += inv_logit(eta1);
    }
    risk_control /= N;
    risk_treatment /= N;
    RD = risk_treatment - risk_control;
  }

  // Country-specific effects
  //----------------------------------------------------------
  for(c in 1:J_country){
    real eta0;
    real eta1;
    eta0 =
        alpha
      + u_country[c];
    eta1 =
        eta0
      + beta_trt
      + b_country[c];
    OR_country[c] = exp(beta_trt + b_country[c]);
    RD_country[c] =
        inv_logit(eta1)
      - inv_logit(eta0);
  }

  // Lineage-group-specific effects
  //----------------------------------------------------------
  for(g in 1:J_group){
    real eta0;
    real eta1;
    eta0 =
        alpha
      + u_group[g];
    eta1 =
        eta0
      + beta_trt
      + u_group[g];
    OR_group[g] =
        exp(beta_trt + u_group[g]);
    RD_group[g] =
        inv_logit(eta1)
      - inv_logit(eta0);
  }
  
  // Country×Group matrix
  //----------------------------------------------------------
  for(c in 1:J_country){
  for(g in 1:J_group){
    real eta0;
    real eta1;
    eta0 =
        alpha
      + u_country[c]
      + u_group[g];
    eta1 =
        eta0
      + beta_trt
      + b_country[c]
      + b_group[g];
    RD_country_group[c,g] =
        inv_logit(eta1)
      - inv_logit(eta0);
    OR_country_group[c,g] =
        exp(beta_trt +
            b_country[c] +
            b_group[g]);
  }
}
}
