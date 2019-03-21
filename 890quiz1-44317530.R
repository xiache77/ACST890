#Question 1
#define C as coupon
#F as face value
#y as interest rate which has length n 
#n as the total number of coupon

price<-function(C,F,n,y){
  cc=0
  for(j in 1:n){
  cc=cc+(C*exp(-y[j]*j))
  }
  P=cc+F*exp(-y[n]*n)
  P
}
#test the fuction
#y=rep(1.05,4)  
#C=40
#F=1000
#n=4
#price(C,F,n,y)

#Question 3
#read the dataset
dataset<- read.csv("C:/Users/XIACHE/Desktop/DATA/singapore.economy.csv")
View(dataset)
#exclude NA
dataset<-na.omit(dataset)
#processing time format
dataset$time<-(dataset$time-floor(dataset$time))*12/100+floor(dataset$time)
dataset$time
attach(dataset)
#plot time vs GDP
plot(time,gdp,xlab="Time", ylab="GDP(%)",main="Singapore GDP growth" )
#calculae mean and sd of GDP by periods
gmean<-tapply(gdp,period,mean)
gmean
gsd<-tapply(gdp,period,sd)
gsd
stat.table<-data.frame(gmean,gsd)
stat.table
# scatterplot for every pair of variables except time and period
pairs(dataset[3:8])
#simple linear regression with gdp~exp
lm1<-lm(gdp~exp,data=dataset)
summary(lm1)
#multiple regression
lm2<-lm(gdp~exp+epg+hpr+oil+gdpus+crd,data=dataset)
summary(lm2)
#calculate the 5% quatile of gdp
quan<-quantile(gdp,probs=c(0.05))
quan
#group the gdp
state<-ifelse(gdp>quan,"normal","crisis")
state<-factor(state)
state
dataset$state<-state
head(dataset)
#divide dataset by time
detach(dataset)
train<-subset(dataset,time<2007)
tail(train)
attach(train)
#logistic regression model
glm<-glm(state~bci,data=train,family=binomial)
summary(glm)
#confusion matrix
predict<-predict(glm,type='response')
table(train$state,predict>0.5)
detach(train)

