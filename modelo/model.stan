data {
  int<lower = 1> modelo_n_encuestas;
  int<lower = 0,upper = modelo_n_encuestas> modelo_priors[6];
}
parameters {
  simplex[6] theta;
}
model {
  target += dirichlet_lpdf(theta | rep_vector(2, 6));
  
  target += multinomial_lpmf(modelo_priors | theta);
}
generated quantities{
  int pred[6] = multinomial_rng(theta, 100);
}