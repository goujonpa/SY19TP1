# Paul GOUJON
# UTC - SY19
# TP1 Exercise 2

# Bias - variance trade-off study

# Y = 1 + 5X^2 + epsilon
# X ~ unif(0,1)
# epsilon ~ normal(0, 0.25)
# regression fonction : f(x) = 1 + 5x2

n = 50  # size of the training set
x0 = 0.5    # we want to study that trade-off on a fixed x
kmax = 40   # is the max number of nearest neighbours
sig = 0.5   # is the sd for the normal law
Ey0 = 1 + 5*x0^2    # esperance of the y at x = x0

# init
y0 = rep(0, n)  # vector of y0 for the training examples
# matrix of the predictions using the training samples with k varying from 1 to kmax
yhat = matrix(0, n, kmax)   

for (i in 1:n) {
    x = runif(n) 
    # vector of n training samples 
    y = 1 + 5 * x ^ 2 + sig * rnorm(n)
    
    d = abs(x - x0)    # distance between x and x0
    sorting = sort(d, index.return=T)   # get the order of the x in function of their distance from x0
    
    # y0 generation
    y0[i] = Ey0 + sig * rnorm(n)
    
    # for k neighbours from 1 to kmax
    # estimation of y is the mean of the values of the knn
    for (k in 1:kmax) {
        yhat[i, k] = mean(y[sorting$ix[1:k]])
    }
}

error = rep(0, kmax)
bias2 = rep(0, kmax)
variance = rep(0, kmax)

for (k in 1:kmax) {
    error[k] = mean((y - yhat[,k])^2)
    bias2[k] = (mean(yhat[,k] - Ey0))^2
    variance[k] = var(yhat[,k])
}

plot(1:kmax, error, type="l", ylim=range(error, bias2, variance))
lines(1:kmax, bias2, lty=2)
lines(1:kmax, variance, lty=2)
lines(1:kmax, variance+bias2+sig^2, lty=3)



