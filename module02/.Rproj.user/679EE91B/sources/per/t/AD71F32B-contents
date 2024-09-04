# Problem 1

library(multtest)
data(golub)
gol.fac <- factor(golub.cl, levels=0:1, labels=c("ALL", "AML"))
meanALL <- apply(golub[,gol.fac=="ALL"], 1, mean)

meanALL_vector <- as.vector(meanALL)
largestALL <- order(meanALL_vector, decreasing = TRUE)[1:3] # Get largest mean expression value for ALL

### Get names of genes for ALL patients ###

golub.gnames[largestALL[1],]
golub.gnames[largestALL[2],]
golub.gnames[largestALL[3],]

meanAML <- apply(golub[,gol.fac=="AML"], 1, mean)

meanAML_vector <- as.vector(meanAML)
largestAML <- order(meanAML_vector, decreasing = TRUE)[1:3] # Get largest mean expression value for AML

### Get names of genes for AML patients ###

golub.gnames[largestAML[1],]
golub.gnames[largestAML[2],]
golub.gnames[largestAML[3],]


# Problem 2

AMLlst <- golub[,gol.fac=="AML"]
AML5 <- c(AMLlst[1,1],AMLlst[2,1],AMLlst[3,1],AMLlst[4,1],AMLlst[5,1]) # initialize data
golubdf = data.frame(AML5)
write.table(golubdf,"AML5.csv")

ALLlst <- golub[,gol.fac=="ALL"]
ALL5 <- c(ALLlst[1,1],ALLlst[2,1],ALLlst[3,1],ALLlst[4,1],ALLlst[5,1]) # initialize data
golubdf = data.frame(ALL5)
write.table(golubdf,"ALL5.csv")

row_indices <- c(100:200)
for (i in 1:length(row_indices)) {
  patient1_vec <- c(patient1_vec, golub[i,1])
}

standard_deviation_patient1 <- sd(patient1_vec)
standard_deviation_patient1

standard_deviation_all <- apply(golub, 1, sd)
threshold_vec <- c()
for (i in 1:length(standard_deviation_all)) {
  if (standard_deviation_all[i] > 1) {
    threshold_vec <- c(threshold_vec, standard_deviation_all[i])
  }
}
length(threshold_vec)  # number of genes with standard deviation greater than 1

plot(x,y,xlab="NUCLEAR PORE COMPLEX PROTEIN NUP214",ylab="PHOSPHATIDYLSERINE SYNTHASE I")


# Problem 3

matrix_data <- exprs(ALL[,ALL$BT=="B1"])
hist(matrix_data)           # histogram
apply(matrix_data, 1, mean) # mean gene expressions
B1vector <- as.vector(matrix_data)
largestB1 <- order(B1vector, decreasing = TRUE)[1:3] # 3 largest gene expressions

# Problem 4

data(trees)
girth <- trees[,1]
height <- trees[,2]
volume <- trees[,3]
plot(girth,height)
plot(girth,height,xlab="Girth",ylab="Height",col="blue",pch="+")
points(girth,volume)
points(girth,volume,col="black",pch="o",ylim=c(0,90))
plot(girth,height,xlab="Girth",ylab="Height/Volume",col="blue",pch="+",ylim=c(0,90))
points(girth,volume,col="black",pch="o")