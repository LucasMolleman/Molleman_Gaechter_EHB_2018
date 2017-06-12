#set path and read data
#setwd('')
data<-read.table('data_decisions.txt', header=TRUE, sep='\t')
data<-subset(data, data$period>1)

# prepare some identifyers for sessions and participants for the model error structure
data$uniqueSes<-paste(substr(data$country,1,2), substr(data$city,1,3), data$session, sep='')
data$uniqueSes<-factor(data$uniqueSes)
data$uniquePlayerNr<-factor(data$playerNr);
library('lme4')

#adjust this for each treatment separately
data<-subset(data, data$treatment==2)
data$prevPay<-data$prevPay/100;



#Model 2
m2<-glmer(reqInf#Model 1
m1<-glmer(reqInfo ~ country + period + prevPay + (1|uniqueSes/uniquePlayerNr), family='binomial', data=data)
summary(m1)
o ~ country + period + prevDec + (1|uniqueSes/uniquePlayerNr), family='binomial', data=data)
summary(m2)

#Model 3
m3<-glmer(reqInfo ~ city + period + prevPay + (1|uniqueSes/uniquePlayerNr), family='binomial', data=data)
summary(m3)
library('multcomp')
m1contrast<-glht(m3, linfct = mcp(city = "Tukey"))
summary(m3)

#Model 4
m4<-glmer(reqInfo ~ city + period + prevPay + (1|uniqueSes/uniquePlayerNr), family='binomial', data=subset(data, data$country=='China'))
summary(m4)

#Model 5
m5<-glmer(reqInfo ~ city + period + prevPay + (1|uniqueSes/uniquePlayerNr), family='binomial', data=subset(data, data$country=='UK'))
summary(m5)


#Additional model (only period 2; no feedback in this block yet)
m1a<-glmer(reqInfo ~ country + period + prevPay + (1|uniqueSes/uniquePlayerNr), family='binomial', data=subset(data, data$period==2))
summary(m1a)

