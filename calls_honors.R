callers = read.csv(file="G:\\Machine Learning\\bayes_mcmc\\callers.csv")

head(callers)

library("rjags")

mod_string = " model {
    for (i in 1:length(calls)) {
        numcalls[i] ~ dnorm(lam[i])
        lam[i] = int + b[1] * calls + b[2] * days_active + b[3] * age + b[4] * isgroup2
    }
    int ~ dnorm(0.0, 1.0/1e6)
    b[1] ~ dpois(lam)
    lam ~ dgamma(alpha, beta)
    alpha = mu^2 / sig^2
    beta = mu / sig^2

    mu ~ dgamma(2.0, 1.0/5.0)
    sig ~ dexp(1.0)

    for (i in 2:4){
        b[i] ~ dnorm(0.0, 1.0/1e6)
    }
} "

set.seed(102)

data_jags = as.list(callers)

params = c("int", "b")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                        variable.names=params,
                        n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
autocorr.plot(mod_sim)
effectiveSize(mod_sim)

## compute DIC
dic = dic.samples(mod, n.iter=1e3)

