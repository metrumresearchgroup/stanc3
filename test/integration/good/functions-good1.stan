/**
 * Tests for 0, 1, 2 arity functions void and non-void returns
 * and calls.
 */
functions {
  real foo0() {
    return 0.0;
  }
  real foo1(real x) {
    return 1.0;
  }
  real foo2(real x, real y) {
    return 2.0;
  }
  
  real foo0_lp() {
    return 0.0;
  }
  real foo1_lp(real x) {
    return 1.0;
  }
  real foo2_lp(real x, real y) {
    return 2.0;
  }
  
  real foo0_rng() {
    return 0.0;
  }
  real foo1_rng(real x) {
    return 2.0 * x;
  }
  real foo2_rng(real x, real y) {
    return x * y - 2.0;
  }
  
  real foo0_lpmf(int y) {
    return -5;
  }
  real foo1_lpdf(real x) {
    return -exp(x * x);
  }
  real foo2_lpdf(real x, real y) {
    real diff;
    diff = x - y;
    return -exp(diff * diff);
  }
}
data {
  array[6] int<lower=0> N;
}
transformed data {
  real a;
  real b;
  real c;
  // double, int arguments
  a = foo0();
  b = foo1(N[6]);
  c = foo2(a, b);
  c = foo2(a, 1);
  c = foo2(2, b);
  c = foo2(2, 3);
  
  a = foo0_lpmf(2 | );
  b = foo1_lpdf(N[6] | );
  c = foo2_lpdf(a | b);
  c = foo2_lpdf(a | 1);
  c = foo2_lpdf(2 | b);
  c = foo2_lpdf(2 | 3);
  
  a = foo0_rng();
  b = foo1_rng(a);
  b = foo1_rng(1);
  c = foo2_rng(a, b);
  c = foo2_rng(1, b);
  c = foo2_rng(a, 2);
  c = foo2_rng(1, 2);
}
parameters {
  array[10] real<lower=0, upper=1> theta;
}
transformed parameters {
  real phi1;
  real phi2;
  real phi3;
  // var, double and mixed instantiations
  phi1 = foo0();
  phi2 = foo1(1);
  phi2 = foo1(2.0);
  phi2 = foo1(phi1);
  
  phi3 = foo2(1.0, 2.0);
  phi3 = foo2(1.0, 2);
  phi3 = foo2(1, 2.0);
  phi3 = foo2(1, 2);
  
  phi3 = foo2(phi1, 2.0);
  phi3 = foo2(phi1, 2);
  
  phi3 = foo2(1.0, phi2);
  phi3 = foo2(1, phi2);
  
  phi3 = foo2(phi1, phi2);
  
  //   phi1 = foo0_lp();
  //   phi2 = foo1_lp(1);
  //   phi2 = foo1_lp(2.0);
  //   phi2 = foo1_lp(phi1);
  
  //   phi3 = foo2_lp(1.0,2.0);
  //   phi3 = foo2_lp(1.0,2);
  //   phi3 = foo2_lp(1,2.0);
  //   phi3 = foo2_lp(1,2);
  
  //   phi3 = foo2_lp(phi1,2.0);
  //   phi3 = foo2_lp(phi1,2);
  
  //   phi3 = foo2_lp(1.0,phi2);
  //   phi3 = foo2_lp(1,phi2);
  
  //   phi3 = foo2_lp(phi1,phi2);
  
  phi1 = foo0_lpmf(3 | );
  
  phi2 = foo1_lpdf(1 | );
  phi2 = foo1_lpdf(2.0 | );
  phi2 = foo1_lpdf(phi1 | );
  
  phi3 = foo2_lpdf(1.0 | 2.0);
  phi3 = foo2_lpdf(1.0 | 2);
  phi3 = foo2_lpdf(1 | 2.0);
  phi3 = foo2_lpdf(1 | 2);
  
  phi3 = foo2_lpdf(phi1 | 2.0);
  phi3 = foo2_lpdf(phi1 | 2);
  
  phi3 = foo2_lpdf(1.0 | phi2);
  phi3 = foo2_lpdf(1 | phi2);
  
  phi3 = foo2_lpdf(phi1 | phi2);
}
model {
  real psi1;
  real psi2;
  real psi3;
  
  psi1 = foo0();
  psi2 = foo1(1);
  psi2 = foo1(2.0);
  psi2 = foo1(psi1);
  
  psi3 = foo2(1.0, 2.0);
  psi3 = foo2(1.0, 2);
  psi3 = foo2(1, 2.0);
  psi3 = foo2(1, 2);
  
  psi3 = foo2(psi1, 2.0);
  psi3 = foo2(psi1, 2);
  
  psi3 = foo2(1.0, psi2);
  psi3 = foo2(1, psi2);
  
  psi3 = foo2(psi1, psi2);
  
  psi1 = foo0_lp();
  psi2 = foo1_lp(1);
  psi2 = foo1_lp(2.0);
  psi2 = foo1_lp(psi1);
  
  psi3 = foo2_lp(1.0, 2.0);
  psi3 = foo2_lp(1.0, 2);
  psi3 = foo2_lp(1, 2.0);
  psi3 = foo2_lp(1, 2);
  
  psi3 = foo2_lp(psi1, 2.0);
  psi3 = foo2_lp(psi1, 2);
  
  psi3 = foo2_lp(1.0, psi2);
  psi3 = foo2_lp(1, psi2);
  
  psi3 = foo2_lp(psi1, psi2);
  
  psi1 = foo0_lpmf(3 | );
  psi2 = foo1_lpdf(1 | );
  psi2 = foo1_lpdf(2.0 | );
  psi2 = foo1_lpdf(psi1 | );
  
  psi3 = foo2_lpdf(1.0 | 2.0);
  psi3 = foo2_lpdf(1.0 | 2);
  psi3 = foo2_lpdf(1 | 2.0);
  psi3 = foo2_lpdf(1 | 2);
  
  psi3 = foo2_lpdf(psi1 | 2.0);
  psi3 = foo2_lpdf(psi1 | 2);
  
  psi3 = foo2_lpdf(1.0 | psi2);
  psi3 = foo2_lpdf(1 | psi2);
  
  psi3 = foo2_lpdf(psi1 | psi2);
  
  // use _log as sampling statements
  theta[1] ~ foo1();
  theta[2] ~ foo2(theta[1]);
}
generated quantities {
  real x;
  real y;
  real z;
  // double instantiations
  x = foo0();
  y = foo1(x);
  y = foo1(1);
  z = foo2(x, y);
  z = foo2(1, y);
  z = foo2(x, 2);
  z = foo2(1, 2);
  
  x = foo0_lpmf(3 | );
  y = foo1_lpdf(x | );
  y = foo1_lpdf(1 | );
  z = foo2_lpdf(x | y);
  z = foo2_lpdf(1 | y);
  z = foo2_lpdf(x | 2);
  z = foo2_lpdf(1 | 2);
  
  x = foo0_rng();
  y = foo1_rng(x);
  y = foo1_rng(1);
  z = foo2_rng(x, y);
  z = foo2_rng(1, y);
  z = foo2_rng(x, 2);
  z = foo2_rng(1, 2);
}

