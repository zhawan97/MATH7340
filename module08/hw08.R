library(ALL)
data(ALL)

### Problem 1 ### 

Bcells <- ALL[,ALL$BT%in%c("B","B1","B2","B3","B4")]
y <- exprs(Bcells)["109_at",]

# part a
anova(lm(y ~ Bcells$BT))

# part b
summary(lm(y ~ Bcells$BT - 1))

# part c
pairwise.t.test(y,Bcells$BT,p.adjust.methods = 'fdr')

# part d
shapiro.test(residuals(lm(y ~ Bcells$BT)))

### Problem 2 ###

y1 <- exprs(Bcells)
ktest <- apply(y1, 1, function(x) {kruskal.test(x ~ Bcells$BT)})

# part a
pvalues <- apply(y1, 1, function(x) kruskal.test(x ~ Bcells$BT)$p.value)
p.fdr <- p.adjust(p=pvalues, method = 'fdr')
sum(p.fdr<0.05)

# part b
names(sort(p.fdr)[1:5])

### Problem 3 ###

ALL_BS <- ALL[,which(ALL$BT%in%c("B1","B2","B3","B4")& ALL$sex%in%c("M","F"))]
y2 <- exprs(ALL_BS)['38555_at',]

# part a 
Bcells2 <- ALL_BS$BT
sex <- ALL_BS$sex
anova(lm(y2 ~ Bcells2*sex))

# part b
shapiro.test(residuals(lm(y2 ~ Bcells2*sex)))