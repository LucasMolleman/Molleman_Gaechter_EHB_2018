#set path and read data
#setwd('')
data<-read.table('data_decisions.txt', header=TRUE, sep='\t')

# only in periods >1, social information was available
data<-subset(data, data$period>1)

# keep device size
w<-dev.size()[1]; h<-dev.size()[2]
dev.off();
dev.new(width=w, height=h)
plot.new()
par(las=1, cex.axis=1.5, cex.lab=1.5, family='serif', lend=1)

#responses in period 2 only
#data<-subset(a, a$period==2)

# only take those instances where 4 peer decisions were observed
a<-subset(data, data$obsN==4)
cols<-c('#8B88FF', '#FF9C00', '#7BB31A')

# which treatment to plot? (1=best choice; 2=social dilemma; 3=coordination)
treat<-2;
a<-subset(a, a$treatment==treat)

#### FREQUENCY BASED LEARNING ####
aspect<-1; 

#set up the plot with axes
countrCnt<-1; # a counter for setting the colours for each country
x1<-0.1+0.9*(aspect-1)/2; x2<-0.1+0.9*(aspect-0.1)/2-0.02;
y1<- 0.22; y2<-0.95;
par(plt=c(x1,x2,y1,y2), new=T, mgp=c(4, 1, 0))
plot.new()	
plot(0, type='n', xlim=c(-0.2, 4.5), ylim=c(0,1), axes=F,
	xlab='Number of cooperators observed \n(out of four)', ylab='Cooperation rate')
axis(1, at=0:4);
axis(2, at=0:5/5)
else {axis(2, at=0:5/5, labels=F);}
arrows(-0.2, 0.5, 4.2, 0.5, code=0, lty=3)

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
		
		x<- 0:400/100
		y<- exp(a1*x+b1)/(1+exp(a1*x+b1))
		if (countr=='China') lines(x,y, col=cols[treat], lty=2-prDec, lwd=2)
		else lines(x,y, col='black', lty=2-prDec, lwd=2)
	}
}

#### PAYOFF BASED LEARNING #####
par(new=T)

# only consider those cases where 2 peers were observed
a<-subset(data, data$obsN==2)
aspect<-2;

a<-subset(a, a$treatment==treat)
#set up the plot with axes
cntryCnt<-1;
x1<-0.1+0.9*(aspect-1)/2; x2<-0.1+0.9*(aspect-0.1)/2-0.02;
y1<- 0.25; y2<-0.95;
par(plt=c(x1,x2,y1,y2), new=T)
plot.new()	
plot(0, type='n', xlim=c(-120, 120), ylim=c(0,1), axes=F,
	xlab='Payoff of opposite behaviour \nminus own payoff', ylab='')
axis(1, at=-15:15*10, labels=FALSE);
axis(1, at=-3:3*50, lwd.ticks=3);
axis(2, at=0:5/5, labels=F)

arrows(0,0,0,1, code=0, lty=3)
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
		fracB<-rep(0,17); l<-rep(0,17)
		cnt<-1;
		for (payDif in -8:8){
			d<-subset(c, floor(c$payoffDifference / 20) == payDif)
			fracB[cnt]<-mean(d$decision)
			l[cnt]<-length(d$decision)
			cnt<-cnt+1
		}
		
		for (i in 1:length(fracB)){
			x1<- (i-9)*20+10;
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

