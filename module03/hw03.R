### Problem 1 ###

# P(X = 1)
f.x <- function(x) (3^x*exp(-3))/factorial(x)
X_range = c(0,1,2)
f_x <- function (x) f.x(x)*(x %in% X_range)
print(f_x(1))

# P(-3 < X < 5)
print(sum(f_x(X_range)*(-3<X_range & X_range<5)))


### normal distribution dnorm(x, mean = mu, sd = theta) ###
### cumulutaive distribution pnorm(x, mean = mu, sd = theta) ###
### binomial distribution choose(n,p)*p^x*(1-p)^n-x ###
### pdf of binomial distribution dbinom(x,size=n,p=p) ###
### CDf for binomial distribution pbinom(x,size=n,p=p) ###

### Problem 3 ###

set.Y <- c(0, 1, 2)
print(sum(dbinom(set.Y, 3, 0.25)))
E_y <- 3 * 0.25
Var_y <- (3*0.25)*(1-0.25)
print(E_y)
print(Var_y)

### Problem 4 ###

x1 <- integrate(function(x) dchisq(x,5), lower = 2, upper = 5)
print(x1) # Answer for part A
E_x <- integrate(function(x) dchisq(x,5), lower = 2, upper = 5)
print(E_x)  # Answer for part B
VarE <- integrate(function(x) x*dchisq(x,5), lower = 2, upper = 5)
print(VarE)  # Answer for part C
monte <- rchisq(100000,5)
monte_mean <- mean((2<monte) & (monte<5))
print(monte_mean) # Answer for part D. The answer is very similar to part A so I would say that it agrees.

### Problem 6 ###

zyxin <- integrate(function(x) dnorm(x,mean=1.6,sd=0.4), lower = 1, upper = 1.6)
print(zyxin) # Answer for part A
m_zyxin <- rnorm(500000,mean=1.6,sd=0.4)
zyxin_mean <- mean((1<m_zyxin) & (m_zyxin<1.6))
print(zyxin_mean) # Answer for part B
bin_zyxin <- dbinom(2,size=5,p=zyxin_mean) # Use mean from monte carlo method as p value
print(bin_zyxin) # Answer for part C

### Problem 7 ###

pdf_EF <- 10/(10-2)
print(pdf_EF) # mean for both variables
VarX <- 2*10^2*(1+10-2)/((10-2)^2)*(10-4) 
print(VarX) # Variance for X
VarY <- 2*10^2*(12+10-2)/(12*(10-2)^2)*(10-4)
print(VarY) # Varaince for Y

E_F <- integrate(function(x) x*df(x,df1=1,df2=10), lower = 0, upper = Inf)$value
print(E_F) # Mean 
VarX.F <- integrate(function(x) (x-E_F)^2*df(x,df1 = 1,df2 = 10), lower = 0, upper = Inf)$value
print(VarX.F) # Variance
varY.F <- integrate(function(x) (x-E_F)^2*df(x,df1 = 12,df2 = 10), lower = 0, upper = Inf)$value


