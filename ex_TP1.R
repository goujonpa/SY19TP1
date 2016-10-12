

# Paul GOUJON
# UTC
# SY19 - TP1


# install the regression package
install.packages("FNN");
library("FNN")

# package for calculating mean square error
install.packages("hydroGOF")
library("hydroGOF")

# clear workspace
rm(list=ls())

# read dataset
data = read.table("C:/Users/goujonpa/Desktop/SY19/TP1/data/prostate.data");

# checked if the 32nd individual is still wrong, OK.

# x is data
x = data[,1:9]

# z is labels 
z = data[,9:10]

# sx is s	caled data (mean 0) and variance 96 ?
sx = as.data.frame(scale(x))

# Q2 ======================

# plot centered dataframe
pdf("./plots/centered_initial_data.pdf");
plot(sx, main="Scaled initial dataset");
dev.off();
# seems described by lcavol, lweigth and age. Not much by the others

# heatmap ?
correlation = cor(sx);
pdf("./plots/component_cor.pdf");
image(x=1:ncol(correlation), y=1:nrow(correlation), z=t(as.matrix(1-correlation)), axes=F, xlab="", ylab="", main="Prostate dataset components correlation");
axis(2, at=1:nrow(correlation), labels=colnames(correlation), col="white", las=1, cex.axis=0.8);
axis(1, at=1:ncol(correlation), labels=colnames(correlation), col="white", las=2, cex.axis=0.8);
dev.off();
# seems correlated to lcavol, lcp and svi

# Q3 ======================

# Divide dataset into train and test samples
Xtrain = sx[which(z[,2] == T),]
Xtst = sx[which(z[,2] == F),]
ztrain = z[which(z[,2] == T),]
ztst = z[which(z[,2] == F),]

# initialises the mse result matrix
errors = matrix(nrow=nrow(ztrain), ncol=1);

# for k in number of possible neighbours
# make a prediction and calculate the mse
for (k in 1:nrow(ztrain)) {
	# doc knn.reg : https://cran.r-project.org/web/packages/FNN/FNN.pdf
	pred = knn.reg(Xtrain, Xtst, ztrain[,1], k = k)
	zpred = pred$pred;

	# doc mse : https://www.rforge.net/doc/packages/hydroGOF/mse.html
	errors[k] = mse(as.vector(zpred), as.vector(ztst[,1]))
}

pdf("./plots/mseQ3.pdf");
plot(errors, ylab="MSE", xlab="Number of neighbours", main="Mean Square Error on the test example");
dev.off();

