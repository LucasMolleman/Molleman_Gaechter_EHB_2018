# this script reproduces the simple comparisons based on individuals' summary statistics. The numbers that are reported in the main text. The more elaborate GLMMs are found in separate scripts.

#set path and read data (per-participant summary data)
#setwd('')
a<-read.table('data_subjects.txt', header=TRUE, sep='\t')

#### SOCIAL INFORMATION USE IN THE SOCIAL DILEMMA ####

# set up a function to output the relevant social learning statistics
stats<-function(x,y){

	MWtest1<-wilcox.test(x, y) # choose order of vectors (reference category; lowest U)
	U1<-MWtest1$statistic
	MWtest2<-wilcox.test(y, x)
	U2<-MWtest2$statistic
	U<-min(U1,U2)
	
	P<-round(MWtest1$p.value, 3);
	
	n1<-length(na.omit(x)); n2<-length(na.omit(y))
	effSize<-1-(2*U)/(n1*n2)
	effSize<-round(effSize,3)

	xMean<-round(mean(x, na.rm=TRUE),3); xMedian<-round(median(x, na.rm=TRUE),3)
	yMean<-round(mean(y, na.rm=TRUE), 3); yMedian<-round(median(y, na.rm=TRUE),3)
	
	v<-c(xMean, xMedian, yMean, yMedian, U, n1, n2, P, effSize)
	return (v)
}

# SOCIAL DILEMMA
# reliance on social information
tab<-matrix(0, nrow=0, ncol=11)
x<-subset(a, a$country=='China')$SDreq; y<-subset(a, a$country=='UK')$SDreq;
tab<-rbind(tab, c('SDrrCH', 'SDrrUK', stats(x,y)))

x<-subset(a, a$city=='Shanghai')$SDreq; y<-subset(a, a$city=='Ningbo')$SDreq;
tab<-rbind(tab, c('SDrrSha', 'SDrrNin', stats(x,y)))

x<-subset(a, a$city=='Nottingham')$SDreq; y<-subset(a, a$city=='Norwich')$SDreq;
tab<-rbind(tab, c('SDrrNot', 'SDrrNor', stats(x,y)))


# reliance on peer payoffs
x<-subset(a, a$country=='China')$SDpayInfo; y<-subset(a, a$country=='UK')$SDpayInfo;
tab<-rbind(tab, c('SDpayCH', 'SDpayUK', stats(x,y)))

x<-subset(a, a$city=='Shanghai')$SDpayInfo; y<-subset(a, a$city=='Ningbo')$SDpayInfo;
tab<-rbind(tab, c('SDpaySha', 'SDpayNin', stats(x,y)))

x<-subset(a, a$city=='Nottingham')$SDpayInfo; y<-subset(a, a$city=='Norwich')$SDpayInfo;
tab<-rbind(tab, c('SDpayNot', 'SDpayNor', stats(x,y)))

# COORDINATION GAME
# reliance on social information
x<-subset(a, a$country=='China')$COreq; y<-subset(a, a$country=='UK')$COreq;
tab<-rbind(tab, c('COrrCH', 'COrrUK', stats(x,y)))

x<-subset(a, a$city=='Shanghai')$COreq; y<-subset(a, a$city=='Ningbo')$COreq;
tab<-rbind(tab, c('COrrSha', 'COrrNin', stats(x,y)))

x<-subset(a, a$city=='Nottingham')$COreq; y<-subset(a, a$city=='Norwich')$COreq;
tab<-rbind(tab, c('COrrNot', 'COrrNor', stats(x,y)))


# reliance on peer payoffs
x<-subset(a, a$country=='China')$COpayInfo; y<-subset(a, a$country=='UK')$COpayInfo;
tab<-rbind(tab, c('COpayCH', 'COpayUK', stats(x,y)))

x<-subset(a, a$city=='Shanghai')$COpayInfo; y<-subset(a, a$city=='Ningbo')$COpayInfo;
tab<-rbind(tab, c('COpaySha', 'COpayNin', stats(x,y)))

x<-subset(a, a$city=='Nottingham')$COpayInfo; y<-subset(a, a$city=='Norwich')$COpayInfo;
tab<-rbind(tab, c('COpayNot', 'COpayNor', stats(x,y)))

# BEST CHOICE SETTING
# reliance on social information
x<-subset(a, a$country=='China')$BCreq; y<-subset(a, a$country=='UK')$BCreq;
tab<-rbind(tab, c('BCrrCH', 'BCrrUK', stats(x,y)))

x<-subset(a, a$city=='Shanghai')$BCreq; y<-subset(a, a$city=='Ningbo')$BCreq;
tab<-rbind(tab, c('BCrrSha', 'BCrrNin', stats(x,y)))

x<-subset(a, a$city=='Nottingham')$BCreq; y<-subset(a, a$city=='Norwich')$BCreq;
tab<-rbind(tab, c('BCrrNot', 'BCrrNor', stats(x,y)))


# reliance on peer payoffs
x<-subset(a, a$country=='China')$BCpayInfo; y<-subset(a, a$country=='UK')$BCpayInfo;
tab<-rbind(tab, c('BCpayCH', 'BCpayUK', stats(x,y)))

x<-subset(a, a$city=='Shanghai')$BCpayInfo; y<-subset(a, a$city=='Ningbo')$BCpayInfo;
tab<-rbind(tab, c('BCpaySha', 'BCpayNin', stats(x,y)))

x<-subset(a, a$city=='Nottingham')$BCpayInfo; y<-subset(a, a$city=='Norwich')$BCpayInfo;
tab<-rbind(tab, c('BCpayNot', 'BCpayNor', stats(x,y)))



tab<-data.frame(tab)
names(tab)<-c('var1', 'var2', 'mean1', 'median1', 'mean2', 'median2', 'U', 'n1', 'n2', 'P', 'r')
tab

