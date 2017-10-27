#set path and read data
#setwd('')
data<-read.table('data_decisions.txt', header=TRUE, sep='\t')
source('nonlinear fitting function.r')
# only in periods >1, social information was available
data<-subset(data, data$period>1)

# keep device size
w<-dev.size()[1]; h<-dev.size()[2]
dev.off();
dev.new(width=w, height=h)
plot.new()
par(las=1, cex.axis=1.5, cex.lab=1.5, lend=1)

#responses in period 2 only
#data<-subset(a, a$period==2)

# only take those instances where 4 peer decisions were observed
a0<-subset(data, data$obsN==4)
cols<-c('#8B88FF', '#FF9C00', '#7BB31A')

# which treatment to plot? (1=best choice; 2=social dilemma; 3=coordination)
xTreatPos<-1;
for (treat in c(2,3,1)){
	a<-subset(a0, a0$treatment==treat)

	#### FREQUENCY BASED LEARNING ####
	aspect<-1; 

	#set up the plot with axes
	countrCnt<-1; # a counter for setting the colours for each country
	x1<-0.1+0.9*(xTreatPos-1)/3; x2<-0.1+0.9*(xTreatPos-0.1)/3;
	y1<- 0.63; y2<-1;
	par(plt=c(x1,x2,y1,y2), new=T, mgp=c(4, 1, 0))
	plot.new()	
	plot(0, type='n', xlim=c(-0.2, 4.2), ylim=c(0,1), axes=F,
		xlab='', ylab='')
	axis(1, at=0:4, lwd.ticks=3);
	if (treat==2) axis(2, at=0:5/5)
	else axis(2, at=0:5/5, labels=F)
	arrows(-0.5, 0.5, 4.2, 0.5, code=0, lty=3)

	# loop through countries and plot
	for (countr in unique(a$country)){
		b<-subset(a, a$country==countr)
		for (prDec in 0:1){ # plot by previous decision
			c<-subset(b, b$prevDec==prDec)
			fracB<-rep(0,4)
			x<-rep(0,4)
			for (otherB in 0:4){
				d<-subset(c, c$obsB==otherB)
				fracB[otherB+1]<-mean(d$decision)
				x[otherB+1]<-length(d$decision)
			}

			# add dots to graph
			if (countr=='China') points(0:4, fracB, pch=16+prDec, col=adjustcolor(cols[treat],alpha=0.7), cex=x^0.2)
			else points(0:4, fracB, col=adjustcolor('black', alpha=0.5), pch=16+prDec, cex=x^0.2)
					
			# calculate logistic regression coefficients and draw line through dots
			m1<-glm(decision ~ obsB, family='binomial', data=c)
			a1<-summary(m1)$coef[2]
			b1<-summary(m1)$coef[1]
			
			bestModel<-optim(c(1,0.5,0.5), fn = estimateD, method = c("Nelder-Mead"), control=c(maxit=10000))
			
		
			x<- 0:400/100
			D<-bestModel$par[1]
			interc<-bestModel$par[2]
			maxSlope<-bestModel$par[3]
			
			f<-x^D / (x^D + (1-x)^D)
			y<- interc + maxSlope * f
			y<- exp(a1*x+b1)/(1+exp(a1*x+b1))
			
			if (countr=='China') lines(x,y, col=cols[treat], lty=2-prDec, lwd=2)
			else lines(x,y, col='black', lty=2-prDec, lwd=2)
		}
	}
	xTreatPos<-xTreatPos+1; # counter for positioning of panel
}
#### PAYOFF BASED LEARNING #####
par(new=T)

# only consider those cases where 2 peers were observed
a0<-subset(data, data$obsN==2)
aspect<-2;

# which treatment to plot? (1=best choice; 2=social dilemma; 3=coordination)
xTreatPos<-1;
for (treat in c(2,3,1)){

	a<-subset(a0, a0$treatment==treat)
	#set up the plot with axes
	cntryCnt<-1;
	x1<-0.1+0.9*(xTreatPos-1)/3; x2<-0.1+0.9*(xTreatPos-0.1)/3;
	y1<- 0.08; y2<-0.45;
	par(plt=c(x1,x2,y1,y2), new=T)
	plot.new()	
	plot(0, type='n', xlim=c(-95, 95), ylim=c(0,1), axes=F,
		xlab='', ylab='')
	axis(1, at=-9:9*10, labels=FALSE);
	axis(1, at=-1:1*50, lwd.ticks=3);
#	axis(1, at=-2:2*50, lwd.ticks=3, labels=FALSE);
	
	if (xTreatPos==1) axis(2, at=0:5/5)
	axis(2, at=0:5/5, labels=F)

	arrows(0,-1,0,1, code=0, lty=3)
	arrows(-150, 0.5, 150,0.5, code=0, lty=3)

	# loop through the countries
	for (countr in unique(a$country)){
		b<-subset(a, a$country==countr)
		for (prDec in 0:1){# plot by previous decision
			c<-subset(b, b$prevDec==prDec)
			if (prDec==0) {
				c$payoffDifference<- c$obsPayB - c$prevPay
			}
			if (prDec==1) {
				c$payoffDifference<- c$obsPayA - c$prevPay
			}
					
			# calculate y-position for each cohort
			fracB<-rep(0,11); l<-rep(0,11)
			cnt<-1;
			for (payDif in -5:5){
				d<-subset(c, floor(c$payoffDifference / 20) == payDif)
				if (payDif==-5) d<-subset(c, floor(c$payoffDifference / 20) <= payDif)
				if (payDif==5) d<-subset(c, floor(c$payoffDifference / 20) >= payDif)
				fracB[cnt]<-mean(d$decision)
				l[cnt]<-length(d$decision)
				cnt<-cnt+1
			}
			
			for (i in 1:length(fracB)){
				x1<- (i-6)*20+10;
				if (countr=='China') points(x1, fracB[i], col=adjustcolor(cols[treat], alpha=0.7), pch=16+prDec, cex=(l[i]^0.2))
				else points(x1, fracB[i], col=adjustcolor('black', alpha=0.5), pch=16+prDec, cex=(l[i]^0.2))
			}

			# calculate logistic regression coefficients and draw line through dots
			m1<-glm(decision ~ payoffDifference, family='binomial', data=c)
			a1<-summary(m1)$coef[2]
			b1<-summary(m1)$coef[1]
			
			x<--150:150
			y<- exp(a1*x+b1)/(1+exp(a1*x+b1))
			
			if (countr=='China') lines(x,y, col=cols[treat], lty=2-prDec, lwd=2)
			else lines(x,y, col='black', lty=2-prDec, lwd=2)
					
		}
	}
	
	xTreatPos<-xTreatPos+1; # counter for positioning of panel
}
