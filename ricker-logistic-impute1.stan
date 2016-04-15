data {
  int<lower=0> N; // rows of data
  vector[N] y; // log(numbers) at time t
  real<lower=0> K_upper; // upper boundary on K
  real<lower=0> r_upper; // upper boundary on r
  int<lower=1> missing_id; // ID of missing values
}
parameters {
  real<lower=0, upper=K_upper> K;
  real<lower=0, upper=r_upper> r;
  real<lower=0> sigma_proc;
  real y_missing; // imputed missing values
}
transformed parameters {
  vector[N] y_impute; // y with some imputed
  for (i in 1:N) {
    y_impute[i] <- y[i];
  }
  y_impute[missing_id] <- y_missing;
}
model {
  sigma_proc ~ cauchy(0, 2.5);
  for (i in 2:N) {
    y_impute[i] ~
      normal(y_impute[i-1] + r * (1 - exp(y_impute[i-1]) / K),
          sigma_proc);
  }
}
