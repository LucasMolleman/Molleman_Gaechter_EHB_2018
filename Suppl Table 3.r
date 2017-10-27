#set path and read data
#setwd('')
data<-read.table('data_decisions.txt', header=TRUE, sep='\t')
data<-subset(data, data$period>1)

# prepare some identifyers for sessions and participants for the model error structure
data$uniqueSes<-paste(substr(data$country,1,2), substr(data$city,1,3), data$session, sep='')
data$uniqueSes<-factor(data$uniqueSes)
data$uniquePlayerNr<-factor(data$playerNr);#paste(data$uniqueSes, data$playerNr, sep='_')
library('lme4')
data$treat<-factor(data$treatment)
data$treat<-relevel(data$treat, ref="2")

data$prevPay<-data$prevPay/100;

# only consider those cases where social information was requested
data<-subset(data, data$reqInfo==1)

#calculate for each decision p (number of peer decisions observed), k (the number of payoffs observed), and n (the total number of peers observed in that period)

data$p<-0; #obs behav
data$k<-0; #obs pay
data$n<-0; #obs n

for (i in 1:nrow(data)){
	data$k[i]<-data$pay1[i] + data$pay2[i] + data$pay3[i] + data$pay4[i] + data$pay5[i]
	data$p[i]<-data$dec1[i] + data$dec2[i] + data$dec3[i] + data$dec4[i] + data$dec5[i]
	
	data$n[i]<-max(data$pay1[i],data$dec1[i]) + max(data$pay2[i],data$dec2[i]) + max(data$pay3[i],data$dec3[i]) + max(data$pay4[i],data$dec4[i]) + max(data$pay5[i],data$dec5[i])
}

# only consider those cases where any peer was observed
data<-subset(data, data$obsN>0)

#calculate 'f', the fraction of peer requests that included payoffs. this is the dependent variable (i.e. the 'success rate') in the binomial regression models. We take 'n' as the 'weights' (i.e. the 'trials')
data$f<-data$k/data$n


#Model 1
m1<-glmer(f ~ country * treat + period + prevPay + (1|uniqueSes/uniquePlayerNr), control=glmerControl(optimizer="bobyqa"),family='binomial', weights=n, data=data)
summary(m1)

#Model 2
m2<-glmer(f ~ country * treat + (1|uniqueSes/uniquePlayerNr), control=glmerControl(optimizer="bobyqa"),family='binomial', weights=n, data=subset(data, data$period==2))
summary(m2)

#Model 3 (only China)
m3<-glmer(f ~ treat * city + period + prevPay + (1|uniqueSes/uniquePlayerNr), control=glmerControl(optimizer="bobyqa"), family='binomial', data=subset(data, data$country=='China'))
summary(m3)


#Model 4 (only UK)
m4<-glmer(f ~ treat * city + period + prevPay + (1|uniqueSes/uniquePlayerNr), control=glmerControl(optimizer="bobyqa"), family='binomial', data=subset(data, data$country=='UK'))
summary(m4)
