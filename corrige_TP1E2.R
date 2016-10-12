# Corrigé de la partie 2 du TD 1 (k ppv)

prostate<-read.table('/Users/Thierry/Documents/R/Data/SY19/prostate.data',header = TRUE)  # changer le chemin d'accès aux données

#--- Résumés
summary(prostate)
plot(prostate)

plot(prostate$lcavol,prostate$lpsa)
plot(prostate$lweight,prostate$lpsa)
plot(prostate$age,data$lpsa)
boxplot(lpsa~svi,data=prostate)

# k-ppv

library('FNN')

data<-prostate[,c('lcavol','lweight','age','lbph','lpsa','train')]
x.app<-scale(data[data$train==T,1:4])
y.app<-data[data$train==T,5]
x.tst<-scale(data[data$train==F,1:4])
y.tst<-data[data$train==F,5]

MSE<-rep(0,15)
for(k in 1:15){
    reg<-knn.reg(train=x.app, test = x.tst, y=y.app, k = k)
    MSE[k]<-mean((y.tst-reg$pred)^2)
}
plot(1:15,MSE,type='b',xlab='k',ylab='MSE')       

reg<-knn.reg(train=x.app, test = x.tst, y=y.app, k = 6)
plot(y.tst,reg$pred,xlab='y',ylab='prediction')
abline(0,1)

# Corrigé de la partie 3 du TD 1 (biais-variance)

# Exemple de génération des données
sig<- 0.5
n<-50
x<-runif(n)
y<-1+5*x^2+sig*rnorm(n)
plot(x,y)


x0<-0.5
Ey0<-1+5*x0^2
Kmax<-40

N<-10000 # on génère 10000 ensembles d'apprentissage
yhat<-matrix(0,N,Kmax)
y0<-rep(0,N)
for(i in 1:N){
    x<-runif(n)
    y<-1+5*x^2+sig*rnorm(n)
    d<-abs(x-0.5)
    ds<-sort(d,index.return=TRUE)
    y0[i]<-Ey0+sig*rnorm(1)
    for(K in 1:Kmax){
        yhat[i,K]<-mean(y[ds$ix[1:K]]) # Prédiction à partir de l'ensemble d'apprentissage i, avec K voisins
    }
}


error<-rep(0,K)
biais2<-rep(0,K)
variance<-rep(0,K)

for(K in 1:Kmax){
    error[K]<-mean((yhat[,K]-y0)^2)   # MSE
    biais2[K]<-(mean(yhat[,K])-Ey0)^2 # biais^2
    variance[K]<-var(yhat[,K])        # variance
}


plot(1:Kmax,error,type="l",ylim=range(error,biais2,variance))
lines(1:Kmax,biais2,lty=2)
lines(1:Kmax,variance,lty=3)
lines(1:Kmax,biais2+variance+sig^2,lty=4,col="red")