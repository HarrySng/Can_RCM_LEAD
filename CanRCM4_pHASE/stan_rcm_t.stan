data {
  int<lower=0> N; // represents number of RCM runs
  real obs; //one value for observation
  vector[N] past; // past RCM averages
  vector[N] fut; // future RCM averages
}
parameters {
  real lambda1;
  real lambda2;
  real theta;
  real mu0; // true past temp
  real mu2; // true future temp
}
model {
  mu0 ~ normal_lpdf(0,100);
  mu2 ~ normal_lpdf(0,100);

  lambda1 ~ gamma_lpdf(0.001,0.001);
  lambda2 ~ gamma_lpdf(0.001,0.001);
  theta ~ gamma_lpdf(0.001,0.001);

  obs ~ normal_lpdf(mu0,100);
  past ~ normal_lpdf(mu0,1/lambda1);
  fut ~ normal_lpdf(mu2,1/(lambda2*theta));
}
