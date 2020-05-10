library("rjags")
library(gapminder)
data(gapminder)
head(gapminder)
table(gapminder$country)
hist(gapminder$lifeExp)
boxplot(lifeExp ~ country, data=gapminder)

# boxplot(lifeExp ~ country, data=gapminder[gapminder$country == 'United States', ])
# boxplot(lifeExp ~ country, data=gapminder[gapminder$country == 'Senegal', ])

head(gapminder)
str(gapminder)
pairs(gapminder)


hist(gapminder$pop)
hist(gapminder$gdpPercap)

hist(gapminder$lifeExp)

# Since both population and gdpPercap looks
# to be right skewed we can check using log values
# instead

gapminder$logpop = log(gapminder$pop)
gapminder$loggdp = log(gapminder$gdpPercap)

plot(lifeExp ~ logpop, data=gapminder)
plot(lifeExp ~ loggdp, data=gapminder)

# Lets do MCMC modelling
#gdat = na.omit(gapminder)
library("rjags")

gmod1_string = " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b0[country[i]] + b1[country[i]]*log_pop[i] + b2[country[i]]*log_gdp[i] + b3[country[i]]*year[i]
    }
    # prior
    for(j in 1:max(country)){
        b0[j] ~ dnorm(mu0,prec0)
        b1[j] ~ dnorm(mu1,prec1)
        b2[j] ~ dnorm(mu2,prec2)
	  b3[j] ~ dnorm(mu2,prec3)
   }
   mu0 ~ dnorm(0.0,1.0/1.0e6)
   mu1 ~ dnorm(0.0,1.0/1.0e6)
   mu2 ~ dnorm(0.0,1.0/1.0e6)
   mu3 ~ dnorm(0.0,1.0/1.0e6)

   prec0 ~ dgamma(1/2.0, 1*10.0/2.0)
   prec1 ~ dgamma(1/2.0, 1*10.0/2.0)
   prec2 ~ dgamma(1/2.0, 1*10.0/2.0)
   prec3 ~ dgamma(1/2.0, 1*10.0/2.0)

   tau0 = sqrt(1.0/prec0)
   tau1 = sqrt(1.0/prec1)
   tau2 = sqrt(1.0/prec2)
   tau3 = sqrt(1.0/prec3)

   prec ~ dgamma(5/2.0, 5*10.0/2.0)
   sig = sqrt(1.0/prec)
} "

set.seed(72)
gdata1_jags = list(y=gapminder$lifeExp,
			n=nrow(gapminder),
			country=as.numeric(factor(gapminder$country)),
			year=gapminder$year,
              	log_pop=gapminder$logpop,
			log_gdp=gapminder$loggdp)

gparams1 = c("b0", "b1", "b2", "b3", "sig", "tau0", "tau1", "tau2", "tau3")

gmod1 = jags.model(textConnection(gmod1_string), data=gdata1_jags, n.chains=5)
update(gmod1, 1000) # burn-in

gmod1_sim = coda.samples(model=gmod1,
                        variable.names=gparams1,
                        n.iter=200000, thin=200)

gmod1_csim = do.call(rbind, gmod1_sim) # combine multiple chains


# Lets do convergence diagnostics
#plot(gmod1_sim)
gelman.diag(gmod1_sim)
#autocorr.diag(gmod1_sim)
#autocorr.plot(gmod1_sim)
#effectiveSize(gmod1_sim)

dic.samples(gmod1_sim, n.iter=1e3)

summary(gmod1_sim)


# Back to log transformed values
#X = cbind(rep(1.0, gdata1_jags$n), gdata1_jags$lifeExp)
#head(X)

 # posterior mean

#yhat1 = drop(X %*% pm_params1[1:2])
#resid1 = gdata1_jags$y - yhat1
#plot(resid1) # against data index
#plot(yhat1, resid1) # against predicted values
#qqnorm(resid1) # checking normality of residuals
#plot(predict(lmod), resid(lmod)) # to compare with reference linear model
#rownames(gapminder)[order(resid1, decreasing=TRUE)[1:142]] 
# which countries have the largest positive residuals?

dic.samples(gmod1, n.iter=1e3)


# X2 = cbind(rep(1.0, gdata1_jags$n), gdata1_jags$log_pop, gdata1_jags$log_gdp)

usx = gapminder[gapminder$country == 'United States', ]
usgdata1_jags = list(y=usx$lifeExp,
			n=nrow(usx),
			country=as.numeric(factor(usx$country)),
			continent=as.numeric(factor(usx$continent)),
			year=usx$year,
              	log_pop=usx$logpop,
			log_gdp=usx$loggdp)

usxd = cbind(rep(1.0, usgdata1_jags$n), usgdata1_jags$log_pop, usgdata1_jags$log_gdp, usgdata1_jags$year)
(pm_params1 = colMeans(gmod1_csim))
yhat2 = drop(usxd %*% c(pm_params1[135], pm_params1[135+142], pm_params1[135+142+142], pm_params1[135+142+142+142]))
hist(yhat2)
mean(yhat2<50)
hist(usgdata1_jags$y)

resid2 = usgdata1_jags$y - yhat2
plot(resid2)


########################

X2 = cbind(rep(1.0, gdata1_jags$n), gdata1_jags$log_pop, gdata1_jags$log_gdp, gdata1_jags$year)
yhat = drop(X2 %*% c(pm_params1[1:142][135], pm_params1[144:146]))
hist(yhat)
mean(yhat<50)

resid2 = usgdata1_jags$y - yhat2
plot(resid2)



##########################

xv1 = c(1.0, 19.5, 10.7, 2007)
yh1 = drop(xv1 %*% c(pm_params1[135], pm_params1[135+142], pm_params1[135+142+142], pm_params1[135+142+142+142]))
yz = rnorm(1.0, mean=yh1)
yz

resid2 = usgdata1_jags$y - yhat2
plot(resid2)





yhat3 = drop(X2 %*% c(pm_params1[111], pm_params1[144:145]))
hist(yhat3)
mean(yhat3<50)


y_pred1 = rnorm(n=500, mean=gmod1_csim[,"a[112]"])
hist(y_pred1)
mean(y_pred1 < 50)


