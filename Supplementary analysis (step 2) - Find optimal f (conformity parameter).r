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
	f<-v[1]; # decision accuracy (inverse noise)

	if (f < 0 || f > 10) return (1000000);

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
			
				
lambdaV<-rbind(	c(0.092,0.080,0.134),
				c(0.111,0.091,0.180))# decision accuracy (inverse noise)
phiV<-rbind(	c(0.336,0.484,0.440),
				c(0.385,0.448,0.415)) # weight of new payoff information
gammV<-rbind(	c(0.134,0.280,0.402),
				c(0.196,0.247,0.536)) # weight on social info
				
				
parMat<-matrix(0,nrow=3, ncol=4)
rowCnt<-1;
for (treat in 1:3){
	a0<-subset(data, data$treatment==treat)

	print(paste('processing treatment', treat))
	flush.console();
	
	countrCnt<-1;
	for (countr in c('China', 'UK')){
		optimData<-subset(a0, a0$country==countr)
		
		print(paste('currently processing data from', countr))
		flush.console();
				
		lambda<-lambdaV[countrCnt,treat]
		phi<-phiV[countrCnt,treat]
		gamm<-gammV[countrCnt,treat]
		
		m<-optim(c(1), fn = logLikker, method = c("L-BFGS-B"), control=c(maxit=500))

		x<-1+(countrCnt-1)*2
		parMat[rowCnt, x]<-round(m$par,3)
		parMat[rowCnt, x+1]<-round(m$value,3);
	
		print(paste('finished. values:', round(m$par,3)))
		print(paste('LL =',round(m$value,3)))
		
		countrCnt<-countrCnt+1;
	}
	rowCnt<-rowCnt+1;

}

parMat<-data.frame(parMat)
names(parMat)<-c('f_CH','LL_CH','f_UK', 'LL_UK')
parMat
