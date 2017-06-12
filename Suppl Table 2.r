#set path and read data
#setwd('')
data<-read.table('data_subjects.txt', header=TRUE, sep='\t')
library('lme4')
#library('lmerTest')
data$types<-factor(data$FGFtype)
data$FGFtype<-relevel(data$types, 'DD')
data$gender<-factor(data$gender)

# Model 1
m1<-glm(SDreq ~ country + age + gender + siblings+  knownParticipants + religious +
SWbenevolence + SWuniversalism + SWselfdirect + SWstimulation + SWhedonism + SWachievement + 
SWpower + SWsecurity + SWtradition + riskTaking + FGFtype, family='binomial', data=data)
summary(m1)

#Model 2
m2<-glm(SDreq ~ country + age + gender + siblings+  knownParticipants + religious, family='binomial', data=data)
summary(m2)

#Model 3
m3<-glm(SDreq ~ IOS + age + gender + siblings+  knownParticipants + religious, family='binomial', data=data)
summary(m3)

#Model 4
m4<-glm(SDreq ~ SWconformity + age + gender + siblings+  knownParticipants + religious, family='binomial', data=data)
summary(m4)
