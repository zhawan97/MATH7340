library(multtest)
data(golub)


### Problemn 1 ###

grep("GRO2", golub.gnames[,2])
grep("GRO3", golub.gnames[,2])
x <- golub[2714,] # GRO2
y <- golub[2715,] # GRO3 
plot(x,y,xlab=golub.gnames[2714,3],ylab=golub.gnames[2715,3])

# part a
cor(x,y)

# part b
cor.test(x,y,conf.level = 0.90)

# part c
nboot <- 10000
boot.cor <- matrix(0,nrow = nboot,ncol = 1)
data <- cbind(x,y)
for (i in 1:nboot) {
  dat.star<-data[sample(1:nrow(data), replace=TRUE), ]
  boot.cor[i,] <- cor(dat.star[,1], dat.star[,2])
}
quantile(boot.cor[,1], c(0.05,0.95))


### Porblem 2 ###

grep("Zyxin", golub.gnames[,2]) # Row 2124
z <- golub[2124,]

# part a
cor.values <- apply(golub, 1, function(x) cor.test(z,x)$estimate)
sum(cor.values< -0.5)

# part b



# part c
pvalues <- apply(golub, 1, function(x) cor.test(z,x)$p.value)
p.fdr <- p.adjust(p=pvalues, method = 'fdr')
sum(p.fdr<0.05)


### Problem 3 ###

GRO2 <- golub[2714,]
GRO3 <- golub[2715,]
reg.fit <- lm(GRO3 ~ GRO2)

# part a
summary(reg.fit)

# part b
predict(reg.fit,newdata = data.frame(GRO2=0),interval = "prediction", level = 0.8)

# part c
qqnorm(resid(reg.fit))
qqline(resid(reg.fit))
plot(reg.fit,whcih=1)
shapiro.test(resid(reg.fit))


### Problem 4 ###

#part a
stack.reg <- lm(stack.loss ~ stack.x, data = stackloss)
summary(stack.reg)

# part b
newd <- data.frame(Air.Flow=60, Water.Temp = 20, Acid.Conc = 90)
predict(stack.reg, newdata = newd,interval = "prediction", level = 0.9)
predict(stack.reg, newdata = newd,interval = "confidence", level = 0.9)