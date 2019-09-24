data {
	int<lower=1> N; // ## number of observed records (decadal average of 60 years(1951-2010)).
	int<lower=1> DCD; // ## number of model records (decadal average of 150 years(1951-2100)).
	int<lower=1> MDL; // ## Number of models.
	vector[N] t_obs; // ## Vector of decadal averages of observed tmp.
	vector[N] p_obs; // ## Vector of decadal averages of observed pcp.
	matrix[DCD,MDL] t_mdl; // ## Matrix of decadal averages of model tmp (Decades in rows, Models in columns).
	matrix[DCD,MDL] p_mdl; // ## Matrix of decadal averages of model tmp (Decades in rows, Models in columns).
}

parameters {

	real<lower=0> t_eta; // ## Scale of observed tmp. Scale cannot be negative so lower bound.
	real<lower=0> p_eta; // ## Scale of observed pcp. Scale cannot be negative so lower bound.
	
	
	real t_alpha; // ## Intercept of Tmp~Time linear trend. Theoretically corresponds to temperature at the beginning of the first decade.
	real<lower=0> t_beta; // ## Slope of Tmp~Time linear trend. We know the trend is positive so lower bound of 0.
	real<lower=0> t_gma; // ## Slope of Tmp~Time linear trend after elbow in 2000 decade.
	
	
	real p_alpha; // ## Intercept of Pr~Time linear trend. Theoretically corresponds to precipitation at the beginning of the first decade.
	real p_beta; // ## Slope of Pr~Time linear trend. Note: Trend may not be positive so no lower bound.
	real p_gma; // ## Slope of Pr~Time linear trend after elbow in 2000 decade. Note: Trend may not be positive so no lower bound.
	
	
	vector[MDL] d_t; // ## Model specific bias term for temperature.
	real a_t; // ## Hyper-parameter - Mean of model specific temperature bias.
	real<lower=0> lmd_t; // ## Hyper-parameter - Scale of model specific temperature bias. Scale cannot be negative so lower bound.
	
	
	vector[MDL] d_p; // ## Model specific bias term for precipitation.
	real a_p; // ## Hyper-parameter - Mean of model specific precipitation bias.
	real<lower=0> lmd_p; // ## Hyper-parameter - Scale of model specific precipitation bias. Scale cannot be negative so lower bound.
	
	
	vector<lower=0>[MDL] t_absm; // ## Scale of modeled temperature for each model. Scale cannot be negative so lower bound.
	// ## Mean is not a hyper-parameter as the model misbehaves under such loose assumptions. Centered at 0.
	real<lower=0> b_absm_t; // ## Hyper-parameter - Scale of the scale of model temperature. Scale cannot be negative so lower bound.
	
	
	vector<lower=0>[MDL] p_absm; // ## Scale of modeled precipitation for each model. Scale cannot be negative so lower bound.
	// ## Mean is not a hyper-parameter as the model misbehaves under such loose assumptions. Centered at 0.
	real<lower=0> b_absm_p; // ## Hyper-parameter - Scale of the scale of model precipitation. Scale cannot be negative so lower bound.
	
	
	real beta_xo; // ## Correlation between observed temperature and precipitation.
	real beta_o; // ## Hyper-parameter - Mean of correlation between temperature and precipitation. Both observation and model.
	
	vector<lower=0>[MDL] beta_x;// ## Correlation between modeled temperature and precipitation.
	real<lower=0> lmd_b; // ## Hyper-parameter Scale of modeled correlation. Scale cannot be negative so lower bound.
	
	// ## Both observed and modeled correlation have common mean but different scales because we expect correlations to be not drastically different.
}

transformed parameters {

	vector[DCD] t_mu; // ## The true mean temperature of each decade.
	vector[DCD] p_mu; // ## The true mean precipitation of each decade.
	
	for (i in 1:DCD) { // ## Iterate over each decade.
		if (i <= N) { // ## For the first six decades.
			t_mu[i] = t_alpha + t_beta*i; // ## No elbow.
			p_mu[i] = p_alpha + p_beta*i; // ## No elbow.
		} else { # For decades after 2000.
			t_mu[i] = t_alpha + t_beta*i + t_gma*(i-N); // ## Introduce elbow.
			p_mu[i] = p_alpha + p_beta*i + p_gma*(i-N); // ## Introduce elbow.
		}
	}
}

model {

	t_eta ~ normal_lpdf(1,0.5); // ## Prior for scale of observed temperature data.
	p_eta ~ normal_lpdf(1,0.5); // ## Prior for scale of observed precipitation data.
	
	t_alpha ~ normal_lpdf(10,5); // ## Prior for intercept of Tmp~Time linear trend.
	t_beta ~ normal_lpdf(0,5); // ## Prior for slope of Tmp~Time linear trend.
	t_gma ~ normal_lpdf(0,5); // ## Prior for slope of additional elbow for Tmp~Time linear trend.
	
	p_alpha ~ normal_lpdf(0,5); // ## Prior for intercept of Pr~Time linear trend.
	p_beta ~ normal_lpdf(0,5); // ## Prior for slope of Pr~Time linear trend.
	p_gma ~ normal_lpdf(0,5); // ## Prior for slope of additional elbow for Pr~Time linear trend.
	
	a_t ~ normal_lpdf(0,5); // ## Hyperprior - Prior of mean of temperature bias.
	lmd_t ~ cauchy(0,1); // ## Hyperprior - Prior of variance of temperature bias.
	
	a_p ~ normal_lpdf(0,5); // ## Hyperprior - Prior of mean of precipitation bias.
	lmd_p ~ cauchy(0,1); // ## Hyperprior - Prior of variance of precipitation bias.
	
	b_absm_t ~ cauchy(0,1); // ## Hyperprior - Prior of variance of model temperature variance.
	b_absm_p ~ cauchy(0,1); // ## Hyperprior - Prior of variance of model precipitation variance.
	
	beta_xo ~ normal_lpdf(beta_o, 0.1); // ## Slope of linear trend between observed pr~tmp. The scale is fixed based on observed record (Tebaldi, 2009: P88)
	beta_o ~ normal_lpdf(0,1); // ## Hyperprior - Prior of mean of linear trend between both observed and model pr~tmp
	lmd_b ~ cauchy(0,1); // ## Hyperprior - Prior of variance of linear trend between model pr~tmp
	
	for (i in 1:MDL) { // ## Iterate over each model
		t_absm[i] ~ cauchy(0,b_absm_t); // ## Sample scale of each model's temperature from hyper-parameters. Here the mean is not a hyperparameter. Cauchy on cauchy range goes very high. Assumption is unrealistic.

		d_t[i] ~ normal_lpdf(a_t,lmd_t); // ## Sample bias of each model's temperature from hyper-parameters.
		
		p_absm[i] ~ cauchy(0,b_absm_p);  // ## Sample scale of each model's precipitation from hyper-parameters. Here the mean is not a hyperparameter. Cauchy on cauchy range goes very high. Assumption is unrealistic.

		d_p[i] ~ normal_lpdf(a_p,lmd_p); // ## Sample bias of each model's precipitation from hyper-parameters.
		
		beta_x[i] ~ normal_lpdf(beta_o, lmd_b); // ## Sample correlation of temperature and precipitation for each model from hyper-parameters.
		
		for (k in 1:DCD) { // ## Iterate over each decade.
			if (k <= N) { // ## For first six decades with no elbow.
				t_obs[k] ~ normal_lpdf(t_mu[k], t_eta); // ## Sample observed temperature and accept/reject the whole chain.
				p_obs[k] ~ normal_lpdf(p_mu[k] + beta_xo*(t_obs[k] - t_mu[k]), p_eta); // ## Sample observed precipitation and accept/reject the whole chain.
				
				t_mdl[k,i] ~ normal_lpdf((t_mu[k] + d_t[i]), t_absm[i]); // ## Sample model temperature and accept/reject the whole chain.
				p_mdl[k,i] ~ normal_lpdf((p_mu[k] + beta_x*(t_mdl[k,i] - t_mu[k] - d_t[i]) + d_p[i]), p_absm[i]); // ## Sample model precipitation and accept/reject the whole chain.
				
			} else { // ## For decades after 2000.
				t_mdl[k,i] ~ normal_lpdf((t_mu[k] + d_t[i]), t_absm[i]); // ## Sample model temperature and accept/reject the whole chain.
				p_mdl[k,i] ~ normal_lpdf((p_mu[k] + beta_x*(t_mdl[k,i] - t_mu[k] - d_t[i]) + d_p[i]), p_absm[i]); // ## Sample model precipitation and accept/reject the whole chain.
			}
		}
	}
}
