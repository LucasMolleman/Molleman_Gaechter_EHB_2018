#set path and read data

#this code analyses 'frequency-based learning'

#setwd('')
data<-read.table('data_decisions.txt', header=TRUE, sep='\t')
library('lme4')

data$uniqueSes<-paste(substr(data$country,1,2), substr(data$city,1,3), data$session, sep='')

data$uniquePlayer<-paste(data$uniqueSes, data$playerNr, sep='_')
mat<-matrix(nrow=0, ncol=8)

parts<-c()
N<-c()

for (treat in c(2,3,1)){
	a<-subset(data, data$treatment==treat)
	a<-subset(a, a$obsN==4)
	a$obsA<- a$obsB

	#after defecting
	m1<-glmer( decision ~ obsA*country + period +  (1 | uniqueSes/playerNr), family='binomial', data=subset(a, a$prevDec==0), control=glmerControl(optimizer="bobyqa"))

	#after cooperating
	m2<-glmer( decision ~ obsA*country + period +  (1 | uniqueSes/playerNr), family='binomial', data=subset(a, a$prevDec==1), control=glmerControl(optimizer="bobyqa"))
	mat<-rbind(mat, cbind(summary(m1)$coefficients, summary(m2)$coefficients))
	
	parts<-c(parts, length(unique(subset(a, a$prevDec==0)$uniquePlayer)))
	parts<-c(parts, length(unique(subset(a, a$prevDec==1)$uniquePlayer)))
	
	N<-c(N, nrow(subset(a, a$prevDec==0)))
	N<-c(N, nrow(subset(a, a$prevDec==1)))
	
	
	
	}
mat

N
parts

N
