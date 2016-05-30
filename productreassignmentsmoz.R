#distribution of bugs across topics for ones assigned which were not in fixed category
library(lda)
library(ggplot2)
library(reshape2)
library(tm)
library(SnowballC)
library(RMySQL)
library(stringr)
drv=dbDriver("MySQL")
con=dbConnect(drv,dbname='bug_report',user='root')
stat0="select report_id,timestamp,what from updates where prod_name in ('Firefox','Thunderbird','Bugzilla','Core') and attribute=\"product\" order by report_id,timestamp;"
table0=dbGetQuery(con,stat0)
stat1="select report_id,topic from topicsmoz order by report_id;"
table1=dbGetQuery(con,stat1)
tableprod<-data.frame(report_id=integer(),prod1=character(),prod2=character(),topic=integer(),stringsAsFactors=FALSE)
i<-1
k<-1
while(i<=nrow(table1))
{
  tableprod[i,1]=table1[i,1]
  tableprod[i,2]=table0[k,3]
  while(k<nrow(table0) && table0[k+1,1]==tableprod[i,1])
  {
    k<-k+1
  }
  tableprod[i,3]=table0[k,3]
  tableprod[i,4]=table1[i,2]
  i<-i+1
  k<-k+1
}
tableprod1<-subset(tableprod,prod1!=prod2)
tableprod1<-subset(tableprod1,select=c(prod2,topic))
tableprod1$topic<-factor(tableprod1$topic)
ggplot(tableprod1,aes(x=topic,fill=prod2))+geom_bar(position='dodge')+facet_wrap(~prod2,scales='free')
