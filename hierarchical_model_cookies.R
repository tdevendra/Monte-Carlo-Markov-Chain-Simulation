dat = read.table(file="G:\\Machine Learning\\bayes_mcmc\\cookies.txt", header=TRUE)
head(dat)
table(dat$location)
hist(dat$chips)
boxplot(chips ~ location, data=dat)

set.seed(112)
n_sim = 500
alpha_pri = rexp(n_sim, rate=1.0/2.0)
beta_pri = rexp(n_sim, rate=5.0)
mu_pri = alpha_pri/beta_pri
sig_pri = sqrt(alpha_pri/beta_pri^2)
summary(mu_pri)
summary(sig_pri)
lam_pri = rgamma(n=n_sim, shape=alpha_pri, rate=beta_pri)
summary(lam_pri)
(lam_pri = rgamma(n=5, shape=alpha_pri[1:5], rate=beta_pri[1:5]))
(y_pri = rpois(n=150, lambda=rep(lam_pri, each=30)))


library("rjags")

mod_string = " model {
for (i in 1:length(chips)) {
  chips[i] ~ dpois(lam[location[i]])
}

for (j in 1:max(location)) {
  lam[j] ~ dgamma(alpha, beta)
}

alpha = mu^2 / sig^2
beta = mu / sig^2

mu ~ dgamma(2.0, 1.0/5.0)
sig ~ dexp(1.0)

} "

set.seed(113)

data_jags = as.list(dat)

params = c("lam", "mu", "sig")

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


## observation level residuals
(pm_params = colMeans(mod_csim))

yhat = rep(pm_params[1:5], each=30)
resid = dat$chips - yhat
plot(resid)

plot(jitter(yhat), resid)

var(resid[yhat<7])

var(resid[yhat>11])


## location level residuals
lam_resid = pm_params[1:5] - pm_params["mu"]
plot(lam_resid)
abline(h=0, lty=2)

summary(mod_sim)

(n_sim = nrow(mod_csim))
lam_pred = rgamma(n=n_sim, shape=mod_csim[,"mu"]^2/mod_csim[,"sig"]^2, 
                  rate=mod_csim[,"mu"]/mod_csim[,"sig"]^2)
hist(lam_pred)

mean(lam_pred > 15)
y_pred = rpois(n=n_sim, lambda=lam_pred)
hist(y_pred)


# Prob of More than 15 chips per cookie
mean(y_pred > 15)
hist(dat$chips)


# Ques: Posterior prob of next cookie produ. in Loc 1 will have less than 7 chips
# Location 1
y_pred1 = rpois(n=n_sim, lambda=mod_csim[,"lam[1]"])
hist(y_pred1)
mean(y_pred1 < 7)


