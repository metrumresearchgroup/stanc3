data{
  int<lower = 1> nt;  // number of events
  int<lower = 1> nObs;  // number of observation
  int<lower = 1> iObs[nObs];  // index of observation
  
  // NONMEM data
  int<lower = 1> cmt[nt];
  int evid[nt];
  int addl[nt];
  int ss[nt];
  real amt[nt];
  real time[nt];
  real rate[nt];
  real ii[nt];
}

parameters{
  real<lower = 0> CL;
  real<lower = 0> V1;
  real<lower = 0> ka;
  real<lower = 0> sigma;
}

transformed parameters{
  real theta[3];  // ODE parameters
  row_vector<lower = 0>[nt] cHat;
  matrix<lower = 0>[2, nt] x;

  theta[1] = CL;
  theta[3] = V1;
  theta[5] = ka;

  x = pmx_solve_onecpt(time, amt, rate, ii, evid, cmt, addl, ss, theta);

  cHat = x[2, :] ./ V1; // we're interested in the amount in the second compartment
}

model{
}
