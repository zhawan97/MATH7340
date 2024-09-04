library(multtest) # load golub dataset
data(golub)
gol.fac <- factor(golub.cl, levels=0:1, labels = c("ALL", "AML"))

### Problem 1 ###

# Fetch relevant gene data

H4j.ALL <- golub[2972,gol.fac=="ALL"]
H4j.AML <- golub[2972,gol.fac=="AML"]
APS.ALL <- golub[2989,gol.fac=="ALL"]
APS.AML <- golub[2989,gol.fac=="AML"]

#part a
t.test(H4j.ALL,mu=-0.9,alternative = "greater")
#part b
t.test(H4j.ALL,H4j.AML) 
#part c
t.test(APS.ALL,H4j.ALL,alternative = "greater")
#part d
prop.test(x=10,n=27,p=0.5,alternative = "less",correct = FALSE)
#part e
a=c(10,8)
b=c(27,10)
prop.test(x=a,n=b,alternative = "two.sided")

### Problem 2 ###

### Problem 3 ###

### Problem 4 ###

n <- 30
nsim <- 10000
x0.sim<-matrix(NA, ncol=n, nrow=n.hyp)
p.fdr<-p.bon<-p.sim<-matrix(NA, nrow=nsim, ncol=n.hyp)
n.fdisc<-n.disc<-rep(NA,nsim)
for (i in 1:nsim) { #i-th simulation
  

}