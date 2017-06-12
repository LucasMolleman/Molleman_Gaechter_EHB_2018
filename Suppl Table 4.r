#set path and read data
#setwd('')
data<-read.table('data_subjects.txt', header=TRUE, sep='\t')
library('lme4')
#library('lmerTest')
data$types<-factor(data$FGFtype)
data$FGFtype<-relevel(data$types, 'DD')
data$gender<-factor(data$gender)


###### RELIANCE ON PAYOFFS ####

# Model 1
m1<-glm(SDpayInfo ~ country + age + gender + siblings+  knownParticipants + religious +
SWbenevolence + SWuniversalism + SWselfdirect + SWstimulation + SWhedonism + SWachievement + 
SWpower + SWsecurity + SWtradition + riskTaking + FGFtype, family='binomial', data=data)
summary(m1)

#Model 2
m2<-glm(SDpayInfo ~ country + age + gender + siblings+  knownParticipants + religious, family='binomial', data=data)
summary(m2)




#Model 3
m3<-glm(SDpayInfo ~ IOS + age + gender + siblings+  knownParticipants + religious, family='binomial', data=data)
summary(m3)
summary(m3)$coefficients

#Model 4
m4<-glm(SDpayInfo ~ SWconformity + age + gender + siblings+  knownParticipants + religious, family='binomial', data=data)
summary(m4)

summary(m4)$coefficients
