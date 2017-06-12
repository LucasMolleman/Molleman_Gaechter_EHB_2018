#set path and read data
#setwd('')

#this code analyses 'payoff-based social learning'

data<-read.table('data_decisions.txt', header=TRUE, sep='\t')
library('lme4')

data$uniqueSes<-paste(substr(data$country,1,2), substr(data$city,1,3), data$session, sep='')

data<-subset(data, data$treatment==2)
data<-subset(data, data$obsN==2)
data$obsA<- data$obsB


#after defecting
a<-subset(data, data$prevDec==0)
a$dif<-a$obsPayB-a$prevPay
m1<-glmer( decision ~ dif * country + period + (1 | uniqueSes/playerNr), family='binomial', data=a)
summary(m1)

#after cooperating
a<-subset(data, data$prevDec==1)
a$dif<-a$obsPayA-a$prevPay
m2<-glmer( decision ~ dif * country + period + (1 | uniqueSes/playerNr), family='binomial', data=a)
summary(m2)
