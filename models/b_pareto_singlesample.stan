functions{
  real paretocustom_lpdf(real x, real lambda, real xmin, real xmax){
    if(lambda != -1)
    return(log((lambda+1) / ( xmax^(lambda+1) - xmin^(lambda+1))) + lambda*log(x));
    else
    return(log(log(xmin) - log(xmax)) + lambda*log(x));
  }
}
    
data {
	int<lower=0> N;
	vector <lower = 0>[N] x;
	vector <lower = 0>[N] xmin;
	vector <lower = 0>[N] xmax;
	// vector <lower = 0>[N] counts;
}

parameters {
	real lambda;
}


model {
	lambda ~ normal(-2, 1);
	for (i in 1:N){
	  x[i] ~ paretocustom(lambda, xmin[i], xmax[i]);
	  }
}

