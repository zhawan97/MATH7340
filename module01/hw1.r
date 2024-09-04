
### Problem 02 ###


lst <- c(1:1000)
tot <- sum((lst^2))

### Problem 03 ###


age <- c(80,25,33,64,48,92,17,44,3,31)
age
ageByMonth <- age*12
ageByMonth
ageSum <-sum(age)
ageSum
youngest <- 99999
iter <- c(1,2,3,4,5,6,7,8,9,10)
for (i in iter) {
  if (age[i] < youngest) {
    youngest <- age[i]
  }
}
youngest
oldest <- 1
for (i in iter) {
  if (age[i] > oldest) {
    oldest <- age[i]
  }
}
oldest
ageSquareRoot <- sqrt(age)
ageSquareRoot


### Problem 04 ###


X <- numeric(30) 
for (k in 1:30) {
  X[k] <- 3 * k
}

print(X)

Y <- numeric(30)
for (i in 1:30) {
  Y[i] <- 0
}

print(Y)

for (k in 1:30) {
  if (k < 20) {
    Y[k] <- sin(2 * k)
  } else if (k >= 20) {
    integrand <- function(t) sqrt(t)
    integrate(integrand, lower = 0, upper = k)
  }
}

print(Y)
