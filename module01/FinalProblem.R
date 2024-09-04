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
  