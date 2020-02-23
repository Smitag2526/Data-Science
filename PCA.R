
install.packages("gdata")
library(gdata)

PCA <- read.csv("D:/R Excel Sessions/Day 4/PCA.csv")

head(PCA)
nrow(PCA)

pca <- princomp(PCA[,2:7],cor=T,scores = T,covmat = NULL)

summary(pca)

pca$scores
pca$loadings

plot(pca$scores[,1:2],col="blue",pch=18,cex=0.3,lwd=3)
text(pca$scores[,1:2],labels = c(1:25),cex = 1)
