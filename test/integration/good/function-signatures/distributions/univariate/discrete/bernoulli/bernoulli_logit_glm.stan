transformed data {
  int N = 2;
  int M = 3;
  int d_y = 1;
  int d_y_a[N] = {1, 0};
  matrix[N, M] d_x_m = [[1, 2, 3], [4, 5, 6]];
  row_vector[M] d_x_rv = [1, 2, 3];
  vector[M] d_beta_v = [1, 2, 3]';
  real d_alpha = 3;
  vector[N] d_alpha_v = [0.5, 0.6]';
  real transformed_data_real;
  transformed_data_real = bernoulli_logit_glm_lpmf(d_y_a| d_x_m, d_alpha, d_beta_v);
  transformed_data_real = bernoulli_logit_glm_lpmf(d_y_a| d_x_m, d_alpha_v, d_beta_v);
  transformed_data_real = bernoulli_logit_glm_lpmf(d_y| d_x_m, d_alpha, d_beta_v);
  transformed_data_real = bernoulli_logit_glm_lpmf(d_y| d_x_m, d_alpha_v, d_beta_v);
  transformed_data_real = bernoulli_logit_glm_lpmf(d_y_a| d_x_rv, d_alpha, d_beta_v);
  transformed_data_real = bernoulli_logit_glm_lpmf(d_y_a| d_x_rv, d_alpha_v, d_beta_v);
}
parameters {
  matrix[N, M] p_x_m;
  row_vector[M] p_x_rv;
  vector[M] p_beta_v;
  real p_alpha;
  vector[N] p_alpha_v;
  real y_p;
}
transformed parameters {
  real transformed_param_real;
  transformed_param_real = bernoulli_logit_glm_lpmf(d_y_a| p_x_m, p_alpha, p_beta_v);
  transformed_param_real = bernoulli_logit_glm_lpmf(d_y_a| p_x_m, p_alpha_v, p_beta_v);
  transformed_param_real = bernoulli_logit_glm_lpmf(d_y| p_x_m, p_alpha, p_beta_v);
  transformed_param_real = bernoulli_logit_glm_lpmf(d_y| p_x_m, p_alpha_v, p_beta_v);
  transformed_param_real = bernoulli_logit_glm_lpmf(d_y_a| p_x_rv, p_alpha, p_beta_v);
  transformed_param_real = bernoulli_logit_glm_lpmf(d_y_a| p_x_rv, p_alpha_v, p_beta_v);
}
model {
  y_p ~ normal(0, 1);
}
generated quantities {
  int y_gen_ar[N];
  y_gen_ar = bernoulli_logit_glm_rng(d_x_m, d_alpha, d_beta_v);
  y_gen_ar = bernoulli_logit_glm_rng(d_x_m, d_alpha_v, d_beta_v);
  y_gen_ar = bernoulli_logit_glm_rng(d_x_rv, d_alpha, d_beta_v);
  y_gen_ar = bernoulli_logit_glm_rng(d_x_rv, d_alpha_v, d_beta_v);
}

