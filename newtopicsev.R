library(lda)
library(ggplot2)
library(reshape2)
library(tm)
library(SnowballC)
library(RMySQL)
library(stringr)
drv=dbDriver("MySQL")
con=dbConnect(drv,dbname='bug_report',user='root')
stat1="select report_id,what from updates where prod_name=\"Bugzilla\" and attribute=\"severity\" order by report_id,timestamp;"
table1=dbGetQuery(con,stat1)
stat2="select report_id,topic from topicsmoz where prod_name=\"Bugzilla\" order by report_id;"
table2=dbGetQuery(con,stat2)
tablesev<-data.frame(report_id=integer(),severity=character(),topic=integer(),stringsAsFactors=FALSE)
i<-1
k<-1
while(i<=nrow(table2))
{
  tablesev[i,1]=table2[i,1]
  while(k<nrow(table1) && table1[k+1,1]==tablesev[i,1])
  {
    k<-k+1
  }
  tablesev[i,2]=table1[k,2]
  tablesev[i,3]=table2[i,2]
  i<-i+1
  k<-k+1
}
tablesev$topic<-factor(tablesev$topic)
ggplot(tablesev[,2:3],aes(x=topic))+geom_bar(aes(fill=severity,stat="count"),position=position_dodge())