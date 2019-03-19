#q1
magnes <- read.delim("C:/Users/XIACHE/Desktop/DATA/magnes.csv")
ma=magnes
summary(ma)
head(ma)
attach(ma) 
ma$Time2=ma$Time*ma$Time
model1<-lm(Magnesium~Time+Time2+Treatment,data=ma)
summary(model1)

T1=ma[which(ma$Treatment==1),]
T1
T2=ma[which(ma$Treatment==2),]
T2
attach(T1)
attach(T2)
mT1<-lm(Magnesium~Time+Time2,data=T1)
summary(mT1)
mT2<-lm(Magnesium~Time+Time2,data=T2)
summary(mT2)
mT1$coef
mT2$coef

b=c(2.108,2.138,0.0017,-0.175,-0.0139,0.0119)
X1=model.matrix(mT1)
X2=model.matrix(mT2)
X=matrix(data=rep(0,336),nrow=56,ncol=6)
X[1:28,1]=X1[1:28,1]
X[1:28,3]=X1[1:28,2]
X[1:28,5]=X1[1:28,3]
X[29:56,2]=X2[1:28,1]
X[29:56,4]=X2[1:28,2]
X[29:56,6]=X2[1:28,3]
X
C=rbind(c(0,0,1,-1,0,0),c(0,0,0,0,1,-1))
res=c(mT1$residuals,mT2$residuals)
sig2=sum(res^2)/(dim(magnes)[1]-2)
V=C%*%solve(t(X)%*%X)%*%t(C)
#T2 test #statistic
d=(t(C%*%b)%*%solve(V)%*%C%*%b)%/%sig2 
d
#critical value
cv <- 2*qf(.95, 2, dim(magnes)[1]-2) 
cv


#q2
air <- read.csv("C:/Users/XIACHE/Desktop/DATA/air.csv")
head(air)
R=cor(air)
R
eva=eigen(R)$values
eve=eigen(R)$vectors
cumsum(eva)/sum(eva)
m=3
Lam = eve[, 1:m]%*%diag(sqrt(eva[1:m]))
Lam
Psi = diag(R-Lam%*%t(Lam))
Psi
fit<-factanal(air,3,rotation="none")
print(fit,digits=3,sort=TRUE)
fit$loading
fit$scores

fit2<-factanal(air,3,rotation="varimax",scores="regression")
print(fit2,digits=3,sort=TRUE)
fit2$loading
fit2$scores

fit3<-factanal(air,3,rotation="varimax",scores="Bartlett")
fit3$scores


#q3
stock <- read.csv("C:/Users/XIACHE/Desktop/DATA/stock.csv")
R=cor(stock)
R
eva=eigen(R)$values
eva
eve=eigen(R)$vectors
eve
eva/sum(eva)
cumsum(eva)/sum(eva)
head(stock)
attach(stock)
pca=princomp(~Allied.Chemical+Du.Pont+Union.Carbide+Exxon+Texaco, cor=TRUE, data=stock)
summary(pca)
pca$loadings
screeplot(pca)
plot(pca, type="lines")

#q4
love <- read.csv("C:/Users/XIACHE/Desktop/DATA/love.dat", sep="")
attach(love)
S1=cov(love[,1:4])
S2=cov(love[,5:8])
colMeans(love)

n1=30; n2=30 #sample sizes
n = n1+n2 #total sample size
g =2; p=4 #no. of groups g and no. of variables p
Sp =((n1-1)*S1+(n2-1)*S2)/(n-g) #pooled variance
# Sig test
Lam = det(S1)^(n1-1)*det(S2)^(n2-1)/det(Sp)^(n-g) #Bartlett¡¯s Lambda
Lam
M = -2*log(Lam) #Box¡¯s M quantity
M
u = (1/(n1-1)+1/(n2-1)-1/(n-g))*(2*p^2+3*p-1)/(6*(p+1)*(g-1))
u
#computes the u quantity
C = (1-u)*M #C is approx chisq with df = p*(p+1)*(g-1)/2
C
qchisq(0.95, p*(p+1)*(g-1)/2) #critical value from chisquare distribution

#Parallel test
C=cbind(c(-1,0,0),c(1,-1,0),c(0,1,-1),c(0,0,1))
C
mu1h=c(3.9,3.967,4.333,4.4)
mu2h=c(3.833,4.1,4.633,4.533)
demu=mu1h-mu2h
T2=t(demu)%*%t(C)%*%solve(((1/n1)+(1/n2))*C%*%Sp%*%t(C))%*%C%*%demu
T2
cv=((p-1)*(n1+n2-2))/(n1+n2-p)*qf(0.95, p-1, n1+n2-p)
cv
# coincident test
C1=c(1,1,1,1)
T21=t(demu)%*%C1%*%solve(((1/n1)+(1/n2))%*%t(C1)%*%Sp%*%C1)%*%t(C1)%*%demu
cv=qf(0.95,1,n1+n2-2)
cv


