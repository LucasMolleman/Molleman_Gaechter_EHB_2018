#set path and read data (per-participant summary data)
#setwd('')
a<-read.table('data_subjects.txt', header=TRUE, sep='\t')

# keep device size
w<-dev.size()[1]; h<-dev.size()[2]
dev.off();
dev.new(width=w, height=h)
plot.new()
par(las=1, cex.axis=1.5, cex.lab=1.5, lend=1)

# set line types for cities
ltys<-c(2,1,2,1)

# colours for each interaction setting
cols<-c('#8B88FF', '#FF9C00', '#7BB31A')


# the graph shows two aspects of social learning: (1) reliance on social info, and (2) reliance on peer payoffs
xTreatPos<-1;
for (setting in c(2,3,1)){
	for (aspect in 1:2){

		x1<-0.1+0.9*(xTreatPos-1)/3; x2<-0.1+0.9*(xTreatPos-0.1)/3;
#		y1<- 0.05 + 0.9*(2.1-aspect)/2; y2<-0.05+ 0.9*(3-aspect)/2;
		if (aspect==1) {y1<- 0.63; y2<-1;}
		if (aspect==2)	{y1<- 0.08; y2<-0.45;}
		
		par(plt=c(x1,x2,y1,y2), new=T)
		plot.new()
		
		# create empty panels
		# reliance on social information
		if (setting==2) {
			plot(0, type='n', xlim=c(0,1), ylim=c(0,1), axes=F, xlab='', ylab='')
			axis(1, at=0:10/10, labels=F)
			axis(1, at=0:2/2, lwd.ticks=2)
			axis(2, at=0:5/5)
		}
		# reliance on peer payoffs
		else {
			plot(0, type='n', xlim=c(0,1), ylim=c(0,1), axes=F,
			xlab='', ylab='')
			if (setting==1) axis(1, at=0:10/10, labels=F)
			else axis(1, at=0:10/10, labels=F)
			
			axis(1, at=0:2/2, lwd.ticks=2)

			axis(2, at=0:5/5, labels=F);
		}
		
		
		cityCnt<-1; # counter for plotting line type
		#plot reliance on payoffs per city
		for (cit in unique(a$city)){
			b<-subset(a, a$city==cit)
			
			if (setting==1){
				if (aspect==1) srt<-sort(b$BCreq) # request rate (=reliance on social info)
				if (aspect==2) srt<-sort(b$BCpayInfo) # rate of requests for payoffs (=reliance on peer payoffs)
			}
			if (setting==2){
				if (aspect==1) srt<-sort(b$SDreq) # request rate (=reliance on social info)
				if (aspect==2) srt<-sort(b$SDpayInfo) # rate of requests for payoffs (=reliance on peer payoffs)
			}
			if (setting==3){
				if (aspect==1) srt<-sort(b$COreq) # request rate (=reliance on social info)
				if (aspect==2) srt<-sort(b$COpayInfo) # rate of requests for payoffs (=reliance on peer payoffs)
			}
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
			else lines(c(0,0:200/200), c(0,cumFreq), col=adjustcolor(cols[setting], alpha=1), lwd=1, lty=ltys[cityCnt])
		
			cityCnt<-cityCnt+1;
		}

		# add country averages (code has same structure as above)
		for (countr in c('China', 'UK')){
			b<-subset(a, a$country==countr)

			if (setting==1){
				if (aspect==1) srt<-sort(b$BCreq) # request rate (=reliance on social info)
				if (aspect==2) srt<-sort(b$BCpayInfo) # rate of requests for payoffs (=reliance on peer payoffs)
			}
			if (setting==2){
				if (aspect==1) srt<-sort(b$SDreq) # request rate (=reliance on social info)
				if (aspect==2) srt<-sort(b$SDpayInfo) # rate of requests for payoffs (=reliance on peer payoffs)
			}
			if (setting==3){
				if (aspect==1) srt<-sort(b$COreq) # request rate (=reliance on social info)
				if (aspect==2) srt<-sort(b$COpayInfo) # rate of requests for payoffs (=reliance on peer payoffs)
			}
			
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
			else lines(c(0,0:200/200), c(0,cumFreq), col=adjustcolor(cols[setting], alpha=0.5), lwd=3, lty=1)
		}
	}
	xTreatPos<-xTreatPos+1
}
