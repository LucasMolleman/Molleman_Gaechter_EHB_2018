#set path and read data (per-participant summary data)
#setwd('')
a<-read.table('data_subjects.txt', header=TRUE, sep='\t')

#Basic checks re 'collectivism'
# Schwartz' conformity
ch<-subset(a, a$country=='China')$SWconformity
uk<-subset(a, a$country=='UK')$SWconformity
t.test(ch, uk)

# Inclusion of the other in the self (Aron et al 1992)
ch<-subset(a, a$country=='China')$IOS
uk<-subset(a, a$country=='UK')$IOS
t.test(ch, uk)
