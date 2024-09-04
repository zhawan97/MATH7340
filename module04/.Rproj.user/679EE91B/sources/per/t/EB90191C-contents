
### Problem 1 ###

# pnorm(probability, mean = u, sd = sqrt(variance)/sqrt(size))

purine <- pnorm(20, mean = 14, sd = sqrt(4.2)/sqrt(100)) - pnorm(15, mean = 14, sd = sqrt(4.2)/sqrt(100))

### Problem 2 ###

# load packages with require()

require(mvtnorm) # get pdf for dmvnorm()

nsim= 100
mu <- c(7,12)
sig <- matrix(c(3,3,3,7), nrow = 2)
  
XmeanLess.sim<- rep(NA,nsim) # Define an empty matrix to store data

for (i in 1:nsim) {
  data.sim<-rmvnorm(2, mean=mu, sigma=sig)
  mean.sim<-apply(data.sim, 1, mean) #Find the mean of data.sim
  Xmean<-mean.sim[1]
  Ymean<-mean.sim[2]
  
  XmeanLess.sim[i] <- (Xmean+0.5<Ymean) # logical statement, read the question to see why I wrote this
}

mean(XmeanLess.sim) # Mean of all the MC simulations

mean(XmeanLess.sim) + c(-1,1)*1.96*sqrt(var(XmeanLess.sim)/10000) # confidence interval

### Problem 3 ###

# Use linear combination due to independence

x1 <- rchisq(10000,8)
x2 <- rgamma(10000,1,2)
x3 <- rt(10000,5)
y <- sqrt(x1)*x2+(4*x3^2)
mean(y)


### Problem 4 ###

n = 1000


an <- sqrt(2*log(n)) - 0.5*(log(log(n))+log(4*pi))*(2*log(n))^(-1/2)
bn <- (2*log(n))^(-1/2)
x1 <- seq(min(x), max(x), length = 1000)
f.x <- exp(-x1)*exp(-x1)*exp(-x1)
x2 <- seq(min(x), max(x), length = 1000)
fun <- dnorm(x2, mean = mean(x), sd = sd(x))

for (i in 1:n) {
  max_r <- max(rnorm(1000))
  normalize[i] <- (max_r-an)/bn
}

scatter.smooth(normalize)
lines(fun, col="red", lwd=2)
lines(f.x, col="green", lwd=2)
