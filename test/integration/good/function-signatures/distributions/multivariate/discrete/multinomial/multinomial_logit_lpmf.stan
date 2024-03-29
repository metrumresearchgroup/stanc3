data {
  int d_int;
  array[d_int] int d_int_array;
  vector[d_int] d_vector;
}
transformed data {
  real transformed_data_real;
  transformed_data_real = multinomial_logit_lpmf(d_int_array | d_vector);
}
parameters {
  vector[d_int] p_vector;
  real y_p;
}
transformed parameters {
  real transformed_param_real;
  transformed_param_real = multinomial_logit_lpmf(d_int_array | d_vector);
  transformed_param_real = multinomial_logit_lpmf(d_int_array | p_vector);
}
model {
  y_p ~ normal(0, 1);
}

