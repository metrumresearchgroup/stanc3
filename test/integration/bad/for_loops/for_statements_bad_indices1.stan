functions {
  int foo(int a) {
    while (1) {
      array[2] int vs;
      for (v in vs) v[2] = 3;
    }
    return 0;
  }
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
