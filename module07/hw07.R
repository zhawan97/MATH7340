library(multtest) # load golub dataset
data(golub)
gol.fac <- factor(golub.cl, levels=0:1, labels = c("ALL", "AML"))
library(p.adjust)
library(gtools)

### Problem 1 ###

all <- golub[,gol.fac == "ALL"]
aml <- golub[,gol.fac == "AML"]


diff <- all - aml
# binom.test(x=sum(diff>0), n=length(diff), p=0.5, alternative="greater")

wilcox.test(x=all,y=aml,paired = F,alternative = "greater")


### Problem 2 ###

pvals <- rep(NA,length(aml))

for (i in length(aml)) {
  results <- shapiro.test(aml[i])
  results.pvals <- results$p.value
  p.fdr <- p.adjust(results.pvals,method = "fdr")
}

sum(p.fdr<0.05)


### Problem 3 ###

wilcox.test(golub[1391,gol.fac=="ALL"],golub[1042,gol.fac=="ALL"])

### Problem 4 ###

data("UCBAdmissions")

t.test()


### Problem 5 ###

gene1 <- golub[1834,]
all<- gol.fac=="ALL"
aml<- gol.fac=="AML"

testTable<- matrix(c(sum(gene1[all]>1), sum(gene1[all]<=1), sum(gene1[aml]>1), sum(gene1[aml]<=1)), nrow=2, dimnames=list("Gene"=c(">1","<=1"), "Disease"=c("ALL","AML")))

fisher.test(var(testTable))