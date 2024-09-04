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
