#set path and read data
#setwd('')
#this code analyses 'individual learning'

#logistic models fitted to decisions NOT preceded by observing social information

data<-read.table('data_decisions.txt', header=TRUE, sep='\t')
library('lme4')

data$uniqueSes<-paste(substr(data$country,1,2), substr(data$city,1,3), data$session, sep='')

# only look at social dilemma
data<-subset(data, data$treatment==2)
# only look at those cases where no social information was requested
data<-subset(data, data$reqInfo==0)

#data<-subset(data, data$period==2)
data$prevPay<-data$prevPay/100

# after defecting
m1<-glmer( decision ~ prevPay*country + period +  (1 | uniqueSes/playerNr), family='binomial', data=subset(data, data$prevDec==0))
summary(m1)

# after cooperating
m2<-glmer( decision ~ prevPay*country + period +  (1 | uniqueSes/playerNr), family='binomial', data=subset(data, data$prevDec==1))
summary(m2)


