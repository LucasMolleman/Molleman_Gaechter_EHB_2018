#setwd('D:\\Nottingham projects\\Social learning across cultures\\data\\')

#set path and read data (per-participant summary data)
#setwd('')
data<-read.table('data_decisions.txt', header=TRUE, sep='\t')

data$uniqueGroup<-paste(data$city, data$session, data$block, data$groupNr, sep='_')
data$uniqueSes<-paste(data$city, data$session, sep='_')

# device settings
gameNames<-c('Best choice', 'Social dilemma', 'Coordination')
w<-dev.size()[1]; h<-dev.size()[2]
dev.off();
dev.new(width=w, height=h)
plot.new()
par(las=1, cex.axis=1.5, cex.lab=1.5, lend=1)

# colours for each type of information request
cols<-c('#E64A45', '#4DB3B3', '#F2C249')

# custom function to add arrows to the bars (pointing in the expected direction of change)
myArrow <- function(x1,y1,x2,y2,lty,lwd,cols){
	if (y2 < y1){		# arrow points up
		arrows(x2,y2,x2+0.1,y2+0.02, code=0, lty=lty, lwd=lwd,col=cols)
		arrows(x2,y2,x2-0.1,y2+0.02, code=0, lty=lty, lwd=lwd,col=cols)
	}
	else{				# arrow points down
		arrows(x2,y2,x2+0.1,y2-0.02, code=0, lty=lty, lwd=lwd,col=cols)
		arrows(x2,y2,x2-0.1,y2-0.02, code=0, lty=lty, lwd=lwd,col=cols)
	}
}

# create colours and store in matrix 'colMat'
library('colorRamps')
BCpal <- colorRampPalette(c("grey99", "#8B88FF"), space = "rgb")
SDpal <- colorRampPalette(c("#FF9C00", "grey99"), space = "rgb")
COpal <- colorRampPalette(c("#547913", "grey99", "#7BB31A"), space = "rgb")
colMat<-matrix(nrow=3, ncol=6)
for (i in 1:6) colMat[1,i]<- BCpal(6)[i]
for (i in 1:6) colMat[2,i]<- SDpal(6)[i]
for (i in 1:6) colMat[3,i]<- COpal(6)[i]

xTreatPos<-1;
#loop through the interaction settings and plot group states
for(treat in c(2,3,1)){
	#panel settings (make empty plot)
	x1<-0.1+0.9*(xTreatPos-1)/3; x2<-0.1+0.9*xTreatPos/3-0.02;
	y1<- 0.57; y2<-1;
	par(plt=c(x1,x2,y1,y2), new=T)
	plot.new()	
	plot(0, type='n', xlim=c(0.5,5.5), ylim=c(0,1.1), axes=F,
		xlab='', ylab='')
	axis(1, at=c(1:5), labels=FALSE);
	if (treat==2) {axis(2, at=0:5/5)}
	else {axis(2, at=0:5/5, labels=F);}	
	
	a<-subset(data, data$treatment==treat)

	for (cntr in unique(a$country)){ # calculate group states by country
		grMat<-matrix(0, nrow=6, ncol=5) # for storing group states
		b<-subset(a, a$country==cntr)
		for (ses in unique(b$uniqueSes)){
			d<-subset(b, b$uniqueSes==ses)
			for (exp1 in unique(d$block)){
				e<-subset(d, d$block==exp1)
				for (gr in unique(e$uniqueGroup)){
					f<-subset(e, e$uniqueGroup==gr)
					for (per in 1:5)
					{
						g<-subset(f, f$period==per)
						x<-1+sum(g$decision)
						grMat[x,per]<-grMat[x,per]+1;
					}
				}
			}
		}
		colSum<-sum(grMat[,1])
		grMat<-grMat/colSum
		grMat<-rbind(rep(0,5),grMat) # grMat contains the y-coordinates for the bars
		
		# plot the bars with the group states using the grMat matrix
		for (i in 1:6){
			for (j in 1:5){
				y0<-sum(grMat[1:i, j])
				y1<-sum(grMat[1:(i+1), j])
				if (cntr=='China') rect(j - 0.37, y0, j-0.02, y1, col=colMat[treat, i])
				else {
					rect(j + 0.02, y0, j+0.37, y1, col=colMat[treat, i])
					rect(j + 0.02, y0, j+0.37, y1, col='black', density=30)
					rect(j + 0.02, y0, j+0.37, y1, col=NA)
				}				
			}
		}
		# add the arrows to the bars with the expected direction of change
		for (i in 1:6){
			for (j in 1:5){
				y0<-sum(grMat[1:i, j])
				y1<-sum(grMat[1:(i+1), j])
				if (cntr=='China') j1<- j-0.21
				else j1<- j + 0.21
				
				if (abs(y0 - y1)>0.01){
					# arrows for expected direction of change
					if (i < 6){
						if (treat == 1) {
							 myArrow(j1, max(0,y1 - 0.02), j1, y1+0.02, 1, 2, 'black')
						}
						if (treat == 2) {
							myArrow(j1, min(y1 + 0.02,1), j1, y1 - 0.02, 1, 2, 'black')
						}
						if (treat == 3) {
							if (i < 4) myArrow(j1, y1 + 0.02, j1, y1-0.02, 1, 2, 'black')
							else myArrow(j1, y1 - 0.02, j1, y1+0.02, 1, 2, 'black')
						}		
					}
				}
			}	
		}
	}
	par(new=F)
	xTreatPos<-xTreatPos+1;
}

grMat

xTreatPos<-1;
# loop through the three interaction settings and plot INFORMATION REQUESTS
for (treat in c(2,3,1)){
	# create empty plot panels
	a<-subset(data, data$treatment==treat)
	a<-subset(a, a$period>1)
	x1<-0.1+0.9*(xTreatPos-1)/3; x2<-0.1+0.9*xTreatPos/3-0.02;
	y1<- 0.08; y2<-0.51;
	par(plt=c(x1,x2,y1,y2), new=T)
	plot.new()
	
	plot(0, type='n', xlim=c(0.5,5.5), ylim=c(0,1), axes=F,
		xlab='', ylab='')
	axis(1, at=c(1:5));
	if (treat==2) {axis(2, at=0:5/5)}
	else {axis(2, at=0:5/5, labels=F);}

	# calculate the frequencies of each type of information request
	for (cntr in unique(a$country)){
		a0<-subset(a, a$country==cntr)
		infoMat<-matrix(0,nrow=5, ncol=20)
		for (seas in 2:5){ # do the calculations for each period after period 1
			b<-subset(a0, a0$period==seas)
			
			# store each of of the types in a variable
			fracPayOnly<-0;
			fracDecOnly<-0;
			fracBoth<-0;
			fracNone<-0;
			
			totPeers<-0;
			totDecs<-0;
			totPays<-0;
			totBoth<-0;
			
			for (i in 1:length(b$decision)){	
				for (j in 1:5){ # loop through each of the 5 players in the group
					x<-10+j;	#decision of peer
					y<-15+j;    #payoff of peer
					if (b[i,x]==1 || b[i,y]==1) totPeers<-totPeers+1;
					if (b[i,x]==1 && b[i,y]==0) totDecs<-totDecs+1;
					if (b[i,x]==0 && b[i,y]==1) totPays<-totPays+1;
					if (b[i,x]==1 && b[i,y]==1) totBoth<-totBoth+1;
				}
			}
			
			# calculate the fractions of each request type
			fDecs<-totDecs/totPeers
			fPays<-totPays/totPeers
			fBoth<-totBoth/totPeers
			
			cumDist<-c(0,fDecs,fDecs+fBoth,fDecs+fPays+fBoth)
			
			# scale the bar with the total rate of requests for this period
			totSize<-mean(b$reqInfo)
			cumDist<-cumDist*totSize
			
			#plot the bar
			for (k in 1:3){
				if (cntr=='China') rect(seas-0.8, cumDist[k], seas-0.42, cumDist[k+1], col=cols[k])
				else {
					rect(seas-0.38, cumDist[k], seas, cumDist[k+1], col=cols[k])
					rect(seas-0.38, cumDist[k], seas, cumDist[k+1], col='black', density=30)
					rect(seas-0.38, cumDist[k], seas, cumDist[k+1], col=NA)
				}
			}
		}
	}	
	xTreatPos<-xTreatPos+1;
}