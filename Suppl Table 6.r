#set path and read data (per-participant summary data)
#setwd('')
a<-read.table('data_subjects.txt', header=TRUE, sep='\t')

# correlations between settings (Table S6)

uk<-subset(a, a$country=='UK')
ch<-subset(a, a$country=='China')


# reliance on social info per country
cor(uk$BCreq, uk$SDreq)
cor(uk$BCreq, uk$COreq)
cor(uk$SDreq, uk$COreq)

cor(ch$BCreq, ch$SDreq)
cor(ch$BCreq,  ch$COreq)
cor(ch$SDreq,  ch$COreq)

# reliance on payoffs per country

cor(uk$BCpayInfo, uk$SDpayInfo, use='complete.obs')
cor(uk$BCpayInfo, uk$COpayInfo, use='complete.obs')
cor(uk$SDpayInfo, uk$COpayInfo, use='complete.obs')

cor(ch$BCpayInfo, ch$SDpayInfo, use='complete.obs')
cor(ch$BCpayInfo,  ch$COpayInfo, use='complete.obs')
cor(ch$SDpayInfo,  ch$COpayInfo, use='complete.obs')
