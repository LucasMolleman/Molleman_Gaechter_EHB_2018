#set path and read data (per-participant summary data)
#setwd('')
a<-read.table('data_subjects.txt', header=TRUE, sep='\t')

# keep device size
w<-dev.size()[1]; h<-dev.size()[2]
dev.off();
dev.new(width=w, height=h)
plot.new()
par(las=1, cex.axis=1.5, cex.lab=1.5, family='serif', lend=1)

# set line types for cities
ltys<-c(2,1,2,1)

# the graph shows two aspects of social learning: (1) reliance on social info, and (2) reliance on peer payoffs
for (aspect in 1:2)
{
	x1<-0.05+0.9*(aspect-0.9)/2; x2<-0.05+0.9*aspect/2-0.02;
	y1<- 0.18; y2<-0.95;
	par(plt=c(x1,x2,y1,y2), new=T)
	plot.new()
	
	# create empty panels
	# reliance on social information
	if (aspect==1) {
		plot(0, type='n', xlim=c(0,1), ylim=c(0,1), axes=F, xlab='Information request rate', ylab='Cumulative frequency')
		axis(1, at=0:5/5);
		axis(2, at=0:5/5)
	}
	# reliance on peer payoffs
	else {
		plot(0, type='n', xlim=c(0,1), ylim=c(0,1), axes=F,
		xlab='Fraction of requests including payoffs', ylab='')
		axis(1, at=0:5/5);
		axis(2, at=0:5/5, labels=F);
	}
	
	cityCnt<-1; # counter for plotting line type
	#plot reliance on payoffs per city
	for (cit in unique(a$city)){
		b<-subset(a, a$city==cit)
		
		if (aspect==1) srt<-sort(b$COreq) # request rate (=reliance on social info)
		if (aspect==2) srt<-sort(b$COpayInfo) # rate of requests for payoffs (=reliance on peer payoffs)
		
		# for cumulative distribution, loop through all in steps of 1/200 (clunky code)
		freq<-rep(0,201)
		for (i in 0:200){
			i1<-i/200;
			for (j in 1:length(srt)){
				if (srt[j]>=(i/200) && srt[j] < (i+1)/200) freq[i+1]<-freq[i+1]+1;
			}
		}
		# create cumulative frequencies for y-coordinates in graph
		freqSum<-sum(freq)
		freq<-freq/freqSum;
		cumFreq<-c();
		for (i in 1:length(freq)){
			cumFreq[i]<-sum(freq[1:i])
		}

		# draw lines for each city
		if (cit=='Norwich' || cit=='Nottingham') lines(c(0,0:200/200), c(0,cumFreq), col=adjustcolor('black', alpha=0.6), lwd=1, lty=ltys[cityCnt])
		else lines(c(0,0:200/200), c(0,cumFreq), col=adjustcolor('#7BB31A', alpha=1), lwd=1, lty=ltys[cityCnt])
	
		cityCnt<-cityCnt+1;
	}

	# add country averages (code has same structure as above)
	for (countr in c('China', 'UK')){
		b<-subset(a, a$country==countr)

		if (aspect==1) srt<-sort(b$COreq)
		if (aspect==2) srt<-sort(b$COpayInfo)
		
		freq<-rep(0,201)
		for (i in 0:200){
			i1<-i/200;
			for (j in 1:length(srt))
			{
				if (srt[j]>=(i/200) && srt[j] < (i+1)/200) freq[i+1]<-freq[i+1]+1;
			}
		}
		
		freqSum<-sum(freq)
		freq<-freq/freqSum;
		cumFreq<-c();
		for (i in 1:length(freq)){
			cumFreq[i]<-sum(freq[1:i])
		}
		
		if (countr=='UK') lines(c(0,0:200/200), c(0,cumFreq), col='grey30', lwd=3, lty=1)
		else lines(c(0,0:200/200), c(0,cumFreq), col=adjustcolor('#7BB31A', alpha=0.5), lwd=3, lty=1)
	}
}
