### Problem 1 ###

# Observations 1.433, 0.524, 0.384, 4.515, 1.852, 0.429
# part a
lik<-function(lam) prod(dexp(c(1.433,0.524,0.384,4.515,1.852,0.429), rate=lam))
nlik<-function(lam) -lik(lam)
optim(par=1, nlik)

# part b
Xsum <- (1/6)*sum(1.433,0.524,0.384,4.515,1.852,0.429)
print(Xsum)

### Problem 2 ###


mu<-98.6
s<-9.4
se<-s/(75^.5)
test<-qt(0.95,mu)
lower<- mu-se*test


### Problem 3 ###

library(multtest) # load golub dataset

gol.fac <- factor(golub.cl, levels=0:1, labels = c("ALL", "AML"))


# Line 32-93 are for part a

# mean ALL
Zyxin_ALL<-golub[2124,gol.fac=="ALL"]
n<-length(Zyxin_ALL)
nboot<-1000
boot.xbar <- rep(NA, nboot)

for (i in 1:nboot) {
  
  data.star <- Zyxin_ALL[sample(1:n,replace=TRUE)] #random sample n observations
  
  boot.xbar[i]<-mean(data.star)   #i-th bootstraped mean, save in boot.xbar
  
}

quantile(boot.xbar,c(0.025,0.975))


# variance ALL
boot.vbar <- rep(NA, nboot)

for (i in 1:nboot) {
  
  data.star <- Zyxin_ALL[sample(1:n,replace=TRUE)] #random sample n observations
  
  boot.vbar[i]<-var(data.star)   #i-th bootstraped var, save in boot.vbar
  
}

quantile(boot.vbar,c(0.025,0.975))



# mean AML
Zyxin_AML<-golub[2124,gol.fac=="AML"]
n<-length(Zyxin_AML)
nboot<-1000
boot.xbar <- rep(NA, nboot)

for (i in 1:nboot) {
  
  data.star <- Zyxin_AML[sample(1:n,replace=TRUE)] #random sample n observations
  
  boot.xbar[i]<-mean(data.star)   #i-th bootstraped mean, save in boot.xbar
  
}

quantile(boot.xbar,c(0.025,0.975))



# variance AML
boot.vbar<-rep(NA, nboot)

for (i in 1:nboot) {
  
  data.star <- Zyxin_AML[sample(1:n,replace=TRUE)] #random sample n observations
  
  boot.vbar[i]<-var(data.star)   #i-th bootstraped var, save in boot.vbar
  
}

quantile(boot.vbar,c(0.025,0.975))

# part b

Zyxin_ALL.mu <- mean(Zyxin_ALL)
Zyxin_ALL.sd <- sd(Zyxin_ALL)

Zyxin_AML.mu <- mean(Zyxin_AML)
Zyxin_AML.sd <- sd(Zyxin_AML)


### ALL mean 

test <- qnorm(0.975, mean = Zyxin_ALL.mu, sd = Zyxin_ALL.sd)
lower <- Zyxin_ALL.mu - Zyxin_ALL.sd*test
upper <- Zyxin_ALL.mu + Zyxin_ALL.sd*test


### AML mean

test <- qnorm(0.975, mean = Zyxin_AML.mu, sd = Zyxin_AML.sd)
lower <- Zyxin_AML.mu - Zyxin_AML.sd*test
upper <- Zyxin_AML.mu + Zyxin_AML.sd*test

# part c


### ALL Median

Zyxin_ALL<-golub[2124,gol.fac=="ALL"]
n<-length(Zyxin_ALL)
nboot<-1000
boot.xbar <- rep(NA, nboot)

for (i in 1:nboot) {
  
  data.star <- Zyxin_ALL[sample(1:n,replace=TRUE)] #random sample n observations
  
  boot.xbar[i]<-median(data.star)   #i-th bootstraped mean, save in boot.xbar
  
}

quantile(boot.xbar,c(0.025,0.975))


### AML Median

Zyxin_AML<-golub[2124,gol.fac=="AML"]
n<-length(Zyxin_AML)
nboot<-1000
boot.xbar <- rep(NA, nboot)

for (i in 1:nboot) {
  
  data.star <- Zyxin_AML[sample(1:n,replace=TRUE)] #random sample n observations
  
  boot.xbar[i]<-median(data.star)   #i-th bootstraped mean, save in boot.xbar
  
}

quantile(boot.xbar,c(0.025,0.975))



### Problem 4 ###


MCsim<- function(nsim, lambda) {
  
  cov1<-cov2<-rep(NA,nsim) # create empty matrices to store data
  
  for (i in 1:nsim) {
    
    x= rpois(50, lambda = lambda) # The question says Poisson
    
    xbar = mean(x) #find mean of x
    
    Xsd = sd(x) # find sd of x
    
    CI1<- c(xbar + qt(0.05,sqrt(xbar/50)), xbar + qt(0.95,sqrt(xbar/50)))
    
    CI2<- c(49/qchisq(0.95,49), 49/qchisq(0.05,49))
    
    cov1[i]<-(CI1[1]<lambda)&(lambda<CI1[2])
    
    cov2[i]<-(CI2[1]<lambda)&(lambda<CI2[2])
  
  }
  
  print(paste("When lambda=", lambda, ": coverage for first CI is", mean(cov1), ", coverage for second CI is", mean(cov2), ".")) # Just to keep your output presentable
  
}

MCsim(1000,0.1)