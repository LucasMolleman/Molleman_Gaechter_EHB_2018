# Supplementary analysis: Experience weighted attraction model based on McElreath et al 2005 (Evol. Hum. Behav.), adapted

data<-read.table('data_decisions.txt', header=TRUE, sep='\t')
# prepare some identifyers for sessions and participants for the model error structure
data$uniqueSes<-paste(substr(data$country,1,2), substr(data$city,1,3), data$session, sep='')
data$uniqueSes<-factor(data$uniqueSes)
data$uniquePlayerNr<-factor(paste(data$uniqueSes, data$playerNr, sep='_'));

#normalise payoff data
newData<-matrix(nrow=0, ncol=ncol(data))
for (tr in 1:3){
	a<-subset(data, data$treatment==tr)
	b<-subset(a, a$period==1)
	m<-mean(b$payoff)
	s<-sd(b$payoff)
	a$payoff<-(a$payoff-m)/s
	
	a$obsPayA<-(a$obsPayA - m) / s
	a$obsPayB<-(a$obsPayB - m) / s
	
	
	newData<-rbind(newData, a)
}
data<-newData
# load player data
aNew<-matrix(nrow=0, ncol=ncol(data)+1)
logLikker<- function(v) {
	lambda<-v[1]; # decision accuracy (inverse noise)
	phi<-v[2]; # weight of new payoff information
	gamm<-v[3]; # weight on social info

	if (lambda < 0 || lambda > 20 || gamm < 0 || gamm > 1 || phi < 0 || phi > 1 ) return (1000000);

	for (ind in unique(optimData$uniquePlayerNr)){
		a<-subset(optimData, optimData$uniquePlayerNr==ind)

		p<-c()
		for (bl in 1:4){ # loop over blocks
			b<-subset(a, (1+(a$block-1)%%4)==bl)
			wA<-priors[treat,bl];
			wB<-priors[treat,bl];
			pBlock<-0.5; # probability of playing B in period 1
			for (per in 2:5){
				# reinforcement of the previously chosen option based on payoffs
				if (b$prevDec[per]==0) wA<-(1-phi)*wA + phi * b$prevPay[per]
				if (b$prevDec[per]==1) wB<-(1-phi)*wB + phi * b$prevPay[per]
				
				# prob of choice
				payoffComponent_indiv<- exp(lambda * wB) / (exp(lambda * wA) + exp(lambda * wB))
				# if no social info has been requested, this is all the participant learned
				Prob1<-payoffComponent_indiv

				# in case social information was requested as well
				if (b$reqInfo[per]==1) {
					# learn from peer payoffs. update the expected value of an option for which more info is available
					if (!is.na(b$obsPayA[per])) {
						wA<-(1-phi)*wA + phi * b$obsPayA[per]
						}
					if (!is.na(b$obsPayB[per])){
						wB<-(1-phi)*wB + phi * b$obsPayB[per]
					}
					
					# this is individual AND social payoff information COMBINED
					payoffComponent<-exp(lambda * wB) / (exp(lambda * wA) + exp(lambda * wB))
					
					# learn from frequencies of behaviour component

					P<- b$obsB[per]/b$obsN[per]
					frequencyComponent<- P^f / (P^f + (1-P)^f)

					# combine everything into one probability
					Prob1<-(1-gamm)*payoffComponent + gamm*frequencyComponent
				}
				pBlock<-c(pBlock, Prob1)
			}
			p<-c(p, pBlock)
		}
		a$p<-p
		
		aNew<-rbind(aNew,a)
	}
	
	aNew$ProbCorrect <- ifelse(aNew$decision == 1,  aNew$p, 1-aNew$p)
	aNew$ProbCorrect <- ifelse(aNew$ProbCorrect < 0.00001, 0.00001, aNew$ProbCorrect) # remove close to zero for log transform
	aNew$loglikelihood <- log(aNew$ProbCorrect)
	
	LL <- sum(aNew$loglikelihood, na.rm = TRUE)
	G2 <- -2*LL # we are finding the minimal G squared, alternative is -LL because we 
	return (G2)
}

priors<-rbind(c(0,0,0,0),
			c(0,0,0,0),
			c(0,0,0,0));
			
# start close to optimal values
initialValues<-rbind(c(0.1, 0.35, 0.15),
				c(0.08, 0.45, 0.25),
				c(0.14, 0.41, 0.35))

parMat<-matrix(0,nrow=15, ncol=9)
rowCnt<-1;
for (treat in 1:3){
	a0<-subset(data, data$treatment==treat)
	for (f in c(1/4,1/2,1,2,4)){
		print(paste('processing treatment', treat, ', conformity ', f))
		flush.console();
		
		countrCnt<-1;
		for (countr in c('China', 'UK')){
			optimData<-subset(a0, a0$country==countr)
			
			print(paste('currently processing data from', countr))
			flush.console();
					
			initialParameterValues<-initialValues[treat,]
			
			m<-optim(initialParameterValues, fn = logLikker, method = c("Nelder-Mead"), control=c(maxit=500))
			parMat[rowCnt, 1]<-f
			xrange<-2+(countrCnt-1)*4
			parMat[rowCnt, xrange:(xrange+2)]<-round(m$par,3)
			parMat[rowCnt, xrange+3]<-round(m$value,3);
		
			print(paste('finished. values:', round(m$par,3), round(m$value)))
			
			countrCnt<-countrCnt+1;
		}
		print(f)
		flush.console();
		rowCnt<-rowCnt+1;
	}
}

parMat<-data.frame(parMat)
names(parMat)<-c('f', 'lambda_CH', 'phi_CH', 'gamma_CH', 'LL_CH','lambda_UK', 'phi_UK', 'gamma_UK', 'LL_UK')
parMat
