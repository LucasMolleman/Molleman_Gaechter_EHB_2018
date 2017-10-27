#set path and read data
#setwd('')
data<-read.table('data_subjects.txt', header=TRUE, sep='\t')
library('lme4')
#library('lmerTest')
data$types<-factor(data$FGFtype)
data$FGFtype<-relevel(data$types, 'DD')
data$gender<-factor(data$gender)

data$treat<-factor(data$treatment)
data$treat<-relevel(data$treat, ref='2')

data$uniquePlayer<-paste(substr(data$country,1,2), substr(data$city,1,3), data$session, data$playerNr, sep='')


# SOCIAL DILEMMA
# Model 1a
m1a<-glm(SDreq ~ country + IOS + SWconformity + age + gender + siblings+  knownParticipants + religious +
SWbenevolence + SWuniversalism + SWselfdirect + SWstimulation + SWhedonism + SWachievement + 
SWpower + SWsecurity + SWtradition + riskTaking + FGFtype, family='binomial', data=data)
summary(m1a)

# Model 1b
m1b<-glm(SDreq ~ country + IOS + SWconformity + age + gender, family='binomial', data=data)
summary(m1b)


# COORDINATION GAME
# Model 2a
m2a<-glm(COreq ~ country + IOS + SWconformity + age + gender + siblings+  knownParticipants + religious +
SWbenevolence + SWuniversalism + SWselfdirect + SWstimulation + SWhedonism + SWachievement + 
SWpower + SWsecurity + SWtradition + riskTaking + FGFtype, family='binomial', data=data)
summary(m2a)

# Model 2b
m2b<-glm(COreq ~ country + IOS + SWconformity + age + gender, family='binomial', data=data)
summary(m2b)

# COORDINATION GAME
# Model 2a
m3a<-glm(BCreq ~ country + IOS + SWconformity + age + gender + siblings+  knownParticipants + religious +
SWbenevolence + SWuniversalism + SWselfdirect + SWstimulation + SWhedonism + SWachievement + 
SWpower + SWsecurity + SWtradition + riskTaking + FGFtype, family='binomial', data=data)
summary(m3a)

# Model 3b
m3b<-glm(BCreq ~ country + IOS + SWconformity + age + gender, family='binomial', data=data)
summary(m3b)

