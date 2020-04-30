library('bayesm')
library('dplyr')
library('tidyr')
library('ggplot2')
library('stringr')
library('Metrics')
library('rjags')
library('reshape2')
library(plotly)
library('gapminder')

data('cheese')
head(cheese)

# adding additional columns
#cheese2 = tidyr::separate(cheese, c("RETAILER"), sep="(.*?)\\s\\—\\s(.*)",
#				into=c("LOCATION","KEY_ACCOUNT"),
#				remove=F)
cheese2 = cheese %>% extract(RETAILER, c("LOCATION", "KEY_ACCOUNT"), "(.*?)\\s\\-\\s(.*)")
cheese2$KEY_ACCOUNT = factor(cheese2$KEY_ACCOUNT)


# Data exploration
cheese %>%
group_by(RETAILER) %>%
summarise(n=n())

range(cheese$PRICE)

range(cheese$DISP)

# Histogram of sales volume
hist(cheese$VOLUME, 50, main="Sales volume distribution", xlab="")

# comparison of random sample of stores
retailers = unique(cheese$RETAILER)
ret_sample = sample(retailers, 5)
sel_df = cheese[cheese$RETAILER %in% ret_sample, ]
sel_df$RETAILER = factor(sel_df$RETAILER)
sel_df$RETAILER2 = str_wrap(sel_df$RETAILER, width = 10)
ggplot(sel_df, aes(RETAILER2,VOLUME)) +
geom_boxplot() + coord_flip() + xlab("Retailer") + ylab("Volume")

# number of Key Accounts
unique(cheese2$KEY_ACCOUNT)
unique(cheese2$LOCATION)

KA = cheese2 %>%
group_by(KEY_ACCOUNT,LOCATION) %>%
summarise(VOLUME=sum(VOLUME)) %>%
group_by(KEY_ACCOUNT) %>%
summarise("Number of stores"=n(), Volume=sum(VOLUME)) %>%
arrange(-Volume) %>%
mutate("Volume share" =round(Volume / sum(Volume) *100,2),
"Cumulative share" = cumsum(`Volume share`))

KA_top = KA[KA$n > 1,]
head(KA, 10)

# number of Locations
Location = cheese2 %>%
group_by(LOCATION,KEY_ACCOUNT) %>%
summarise(VOLUME=sum(VOLUME)) %>%
group_by(LOCATION) %>%
summarise("Number of stores"=n(), Volume=sum(VOLUME)) %>%
arrange(-Volume) %>%
mutate("Volume share" =round(Volume / sum(Volume) *100,2),
"Cumulative share" = cumsum(`Volume share`))
anova_KA = aov(VOLUME~KEY_ACCOUNT, data=cheese2)
summary(anova_KA)

anova_LOC = aov(VOLUME~LOCATION, data=cheese2)
summary(anova_LOC)

# price exploration
hist(cheese2$PRICE, 50, main="Selling price distribution", xlab="")

# advertising exploration
hist(cheese2$DISP, 50, main="Display Advertising distribution", xlab="")

# pairwise distribution
cheese2$logVolume = log(cheese2$VOLUME)
pairs(cheese2[c("logVolume","PRICE","DISP")])

## split by train and test ############
set.seed(123)
train_id = NULL
cheese2$row_id = 1:dim(cheese2)[1]
train_id = vector()
for(KA in unique(cheese2$KEY_ACCOUNT)){

# KA = unique(cheese2$KEY_ACCOUNT)[1]
cur_id = cheese2[cheese2$KEY_ACCOUNT == KA, 'row_id']
cur_sample = cur_id[1:(length(cur_id)-1)]

if(length(train_id)==0){
train_id = cur_sample
}else{
train_id = c(train_id,cur_sample)
}
}
train <- cheese2[train_id, ]
test <- cheese2[-train_id, ]
dim(train)
dim(test)

#Modeling ######################
# Simple regression model
smp_size <- floor(0.8 * nrow(cheese2))

# reference model
mod_lm = lm(log(VOLUME)~PRICE+DISP, data=cheese2)
summary(mod_lm)
par(mfrow=c(1,2))
plot(mod_lm)
par(mfrow=c(1,1))
exp(-0.39283)
exp(0.54045)

# test model prediction
test$pred = exp(predict(mod_lm,test))
par(mfrow=c(1,1))
plot(test$VOLUME, test$pred, xlim=c(0,10000), ylim=c(0,10000))
mape(test$VOLUME, test$pred)

## Bayesian hierarchical model ##################
## Model 1: at Key Account level
mod_string1 = "model{

# likelihood
for(i in 1:length(y)){
y[i] ~ dnorm(nu[i], prec)
nu[i] = b0[KA[i]]+b1[KA[i]]*PRICE[i]+b2[KA[i]]*DISP[i]
}

# prior
for(j in 1:max(KA)){
b0[j] ~ dnorm(mu0,prec0)
b1[j] ~ dnorm(mu1,prec1)
b2[j] ~ dnorm(mu2,prec2)
}
mu0 ~ dnorm(0.0,1.0/1.0e6)
mu1 ~ dnorm(0.0,1.0/1.0e6)
mu2 ~ dnorm(0.0,1.0/1.0e6)
prec0 ~ dgamma(1/2.0, 1*10.0/2.0)
prec1 ~ dgamma(1/2.0, 1*10.0/2.0)
prec2 ~ dgamma(1/2.0, 1*10.0/2.0)
tau0 = sqrt(1.0/prec0)
tau1 = sqrt(1.0/prec1)
tau2 = sqrt(1.0/prec2)
prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig = sqrt(1.0/prec)
}"

set.seed(116)

data_jags1 = list(y=log(train$VOLUME), KA=as.numeric(train$KEY_ACCOUNT),
PRICE=train$PRICE,
DISP=train$DISP)

params1 = c("b0","b1","b2","mu0","mu1","mu2","sig","tau0","tau1","tau2")

mod1 = jags.model(textConnection(mod_string1),data=data_jags1,n.chains=3)
update(mod1,1e3)

mod_sim1 = coda.samples(model=mod1,
variable.names = params1,
n.iter=30e3)


# convergence diagnostics
# plot(mod_sim)
gelman.diag(mod_sim1)
effectiveSize(mod_sim1)
dic1 = dic.samples(mod1, n.iter=1e3)
summary(mod_sim1)

mod_csim1 = as.mcmc(do.call(rbind, mod_sim1))
pred_coef1 = apply(mod_csim1, 2, mean)
pred_coef1[c("mu0","mu1","mu2")]
exp(pred_coef1["mu1"])
exp(pred_coef1["mu2"])

# predict train data
train$pred1 = 0
for(i in 1:length(train$VOLUME)){

# i = 2
KA = as.numeric(train$KEY_ACCOUNT)[i]
train$pred1[i] = exp(pred_coef1[paste0("b0[",KA,"]")] +
pred_coef1[paste0("b1[",KA,"]")]*train$PRICE[i] +
pred_coef1[paste0("b2[",KA,"]")]*train$DISP[i] )
}
par(mfrow=c(1,1))
hist(train$pred1, breaks = 40)
plot(train$VOLUME, train$pred1)
mape(train$VOLUME, train$pred1)

# predict test data
test$pred1 = 0
for(i in 1:length(test$VOLUME)){

# i = 2
KA = as.numeric(test$KEY_ACCOUNT)[i]
test$pred1[i] = exp(pred_coef1[paste0("b0[",KA,"]")] +
pred_coef1[paste0("b1[",KA,"]")]*test$PRICE[i] +
pred_coef1[paste0("b2[",KA,"]")]*test$DISP[i] )
}
plot(test$VOLUME, test$pred1)
mape(test$VOLUME, test$pred1)



## Model 2: at store level
mod_string2 = "model{

#likelihood
for(i in 1:length(y)){
y[i] ~ dnorm(nu[i], prec)
nu[i] = b0[LOC[i]]+b1[LOC[i]]*PRICE[i]+b2[LOC[i]]*DISP[i]
}

# prior
for(j in 1:max(LOC)){
b0[j] ~ dnorm(mu0,prec0)
b1[j] ~ dnorm(mu1,prec1)
b2[j] ~ dnorm(mu2,prec2)
}
mu0 ~ dnorm(0.0,1.0/1.0e6)
mu1 ~ dnorm(0.0,1.0/1.0e6)
mu2 ~ dnorm(0.0,1.0/1.0e6)
prec0 ~ dgamma(1/2.0, 1*10.0/2.0)
prec1 ~ dgamma(1/2.0, 1*10.0/2.0)
prec2 ~ dgamma(1/2.0, 1*10.0/2.0)
tau0 = sqrt(1.0/prec0)
tau1 = sqrt(1.0/prec1)
tau2 = sqrt(1.0/prec2)
prec ~ dgamma(5/2.0, 5*10.0/2.0)
sig = sqrt(1.0/prec)
}"
set.seed(116)
data_jags2 = list(y=log(train$VOLUME), LOC=as.numeric(factor(train$LOCATION)), 
PRICE=train$PRICE,
DISP=train$DISP)

params2 = c("b0","b1","b2","mu0","mu1","mu2","sig","tau0","tau1","tau2")
mod2 = jags.model(textConnection(mod_string2),data=data_jags2,n.chains=3)
update(mod2,1e3)
mod_sim2 = coda.samples(model=mod2,
variable.names = params2,
n.iter=50e3)

#convergence diagnostics
# plot(mod_sim)
gelman.diag(mod_sim2)
effectiveSize(mod_sim2)
dic2 = dic.samples(mod2, n.iter=1e3)
summary(mod_sim2)
dic1
dic2
mod_csim2 = as.mcmc(do.call(rbind, mod_sim2))
pred_coef2 = apply(mod_csim2, 2, mean)
pred_coef2[c("mu0","mu1","mu2")]
exp(pred_coef2["mu1"])
exp(pred_coef2["mu2"])

# predict train data
train$pred2 = 0
for(i in 1:length(train$VOLUME)){

# i = 2
r = as.numeric(factor(train$LOCATION))[i]
train$pred2[i] = exp(pred_coef2[paste0("b0[",r,"]")] +
pred_coef2[paste0("b1[",r,"]")]*train$PRICE[i] +
pred_coef2[paste0("b2[",r,"]")]*train$DISP[i] )
}
par(mfrow=c(1,1))
hist(train$pred2, breaks = 40)
plot(train$VOLUME, train$pred2)
mae(train$VOLUME, train$pred2)
mae(train$VOLUME, train$pred1)
dic2

# predict test data
test$pred2 = 0
for(i in 1:length(test$VOLUME)){

# i = 2
r = as.numeric(factor(test$LOCATION))[i]
test$pred2[i] = exp(pred_coef2[paste0("b0[",r,"]")] +
pred_coef2[paste0("b1[",r,"]")]*test$PRICE[i] +
pred_coef2[paste0("b2[",r,"]")]*test$DISP[i] )
}
plot(test$VOLUME, test$pred2, xlab="Actual", ylab="Predicted", main="Model output vs. actual volume \non out-of-sample data")
mape(test$VOLUME, test$pred)
mape(test$VOLUME, test$pred1)
mape(test$VOLUME, test$pred2)

#### Analysis of coeficients
coef_df = data.frame(pred_coef2)
coef_df$coef_str = row.names(coef_df)
coef_df$coef = gsub('(.+)\\[.*$', '\\1\\2', coef_df$coef_str)
coef_df$ret_code = as.numeric( gsub("^.*?\\[(.*)\\]$", '\\1', coef_df$coef_str))
#coef_df$ret_code = as.numeric( gsub('.+\\[([0–9]+)(?:-([0–9]+))?\\].*$', '\\1\\2', coef_df$coef_str) )
retailers = levels(cheese$RETAILER)
coef_df$retailer = sapply(coef_df$ret_code, FUN=function(x){retailers[coef_df$ret_code[x]] })
head(coef_df)

coef_df2 = dcast(data = coef_df[c("retailer","coef","pred_coef2")],
formula = retailer~coef,fun.aggregate=sum)
head(coef_df2,2)

## histograms for coeficients
hist(coef_df2$b0,30,main="Intercept distribution (b0)",xlab="")
hist(coef_df2$b1,30,main="Price sensitivity distribution (b1)",xlab="")
hist(coef_df2$b2,30,main="Ad effectiveness distribution (b2)",xlab="")
coef_df2$mu1
coef_df2$mu2
exp(10.31895)
exp(-0.799909)
exp(1.160073)
mean(cheese2$PRICE)
mean(cheese2$DISP)

# scatterplot
par(mfrow=c(1,1))
plot(coef_df2$b1, coef_df2$b2, xlab="Price coefficient", ylab="Ad coefficient",
main="Stores clusterisation")
abline(v=coef_df2$mu1[89], lty=2)
abline(h=coef_df2$mu2[89], lty=2)

# segmenting stores based on coeficients

## Group 1: high sensitivity & low ad effectiveness
grp1 = coef_df2[(coef_df2$b1 < coef_df2$mu1[89]) & (coef_df2$b2 < coef_df2$mu2[89]), ]
dim(grp1)

## Group 2: high sensitivity & moderate ad effectiveness
grp2 = coef_df2[(coef_df2$b1 < coef_df2$mu1[89]) & (coef_df2$b2 > coef_df2$mu2[89]), ]
dim(grp2)

## Group 3: high sensitivity & moderate ad effectiveness
grp2 = coef_df2[(coef_df2$b1 < coef_df2$mu1[89]) & (coef_df2$b2 > coef_df2$mu2[89]), ]
dim(grp2)

# bubble chart
p <- plot_ly(coef_df2, x = ~b1, y = ~b2, text = ~retailer, type = 'scatter', mode = 'markers',
marker = list(size = ~b0, opacity = 0.5)) %>%
layout(title = 'Price sensitivity and ad efficiency by retailers',
xaxis = list(showgrid = FALSE),
yaxis = list(showgrid = FALSE))

# breaking
coef_dist = dist(coef_df2[1:88, c('b1','b2')])
hc = hclust( coef_dist, method="complete")
plot(hc)
coef_df2$group =factor(c(cutree(hc, k = 5),NA))
p <- gapminder %>%
ggplot(data=coef_df2[1:88,], mapping=aes(b1, b2, size = b0,text=retailer,color=group)) +
geom_point(alpha=0.5) +
theme_bw()+
geom_vline(xintercept=coef_df2$mu1[89]) +
geom_hline(yintercept=coef_df2$mu2[89]) +
ggtitle("Clusterization of stores") +
xlab("price sensitivity") +
ylab("ad response")
ggplotly(p)


