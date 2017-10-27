#set path and read data
#setwd('')

data<-read.table('data_decisions.txt', header=TRUE, sep='\t')
data<-subset(data, data$period>1)
data<-subset(data, data$treatment<4)
# prepare some identifyers for sessions and participants for the model error structure
data$uniqueSes<-paste(substr(data$country,1,2), substr(data$city,1,3), data$session, sep='')
data$uniqueSes<-factor(data$uniqueSes)
data$uniquePlayerNr<-factor(data$playerNr);
library('lme4')
data$prevPay<-data$prevPay/100;

data$treat<-factor(data$treatment)
data$treat<-relevel(data$treat, ref="2")

#Model 1 (all data)
m1<-glmer(reqInfo ~ treat * country + period + prevPay + (1|uniqueSes/uniquePlayerNr), family='binomial',control=glmerControl(optimizer="bobyqa"),  data=data)
summary(m1)


#Model 2 (only period 2)
m2<-glmer(reqInfo ~ treat * country + (1|uniqueSes/uniquePlayerNr), family='binomial',control=glmerControl(optimizer="bobyqa"),  data=subset(data, data$period==2))
summary(m2)


#Model 3 (only China)
m3<-glmer(reqInfo ~ treat * city + period + prevPay + (1|uniqueSes/uniquePlayerNr), control=glmerControl(optimizer="bobyqa"), family='binomial', data=subset(data, data$country=='China'))
summary(m3)

#Model 4 (only UK)
m4<-glmer(reqInfo ~ treat * city + period + prevPay + (1|uniqueSes/uniquePlayerNr), control=glmerControl(optimizer="bobyqa"), family='binomial', data=subset(data, data$country=='UK'))
summary(m4)
