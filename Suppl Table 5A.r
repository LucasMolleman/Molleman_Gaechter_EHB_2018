#set path and read data

#this code analyses 'frequency-based learning'

#setwd('')
data<-read.table('data_decisions.txt', header=TRUE, sep='\t')
library('lme4')

data$uniqueSes<-paste(substr(data$country,1,2), substr(data$city,1,3), data$session, sep='')

data<-subset(data, data$treatment==2)
data<-subset(data, data$obsN==4)
data$obsA<- data$obsB

#data<-subset(data, data$period==2)
data$prevPay<-data$prevPay/100

#after defecting
m1<-glmer( decision ~ obsA*country + period +  (1 | uniqueSes/playerNr), family='binomial', data=subset(data, data$prevDec==0), control=glmerControl(optimizer="bobyqa"))
summary(m1)

#after cooperating
m2<-glmer( decision ~ obsA*country + period +  (1 | uniqueSes/playerNr), family='binomial', data=subset(data, data$prevDec==1))
summary(m2)
