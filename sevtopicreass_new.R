library(lda)
library(ggplot2)
library(reshape2)
library(tm)
library(SnowballC)
library(RMySQL)
library(stringr)
drv=dbDriver("MySQL")
con=dbConnect(drv,dbname='bug_report',user='root')
stat1="select prod_name,report_id,what from updates where prod_name in ('Firefox','Bugzilla','Thunderbird','Core') and attribute=\"severity\" order by prod_name,report_id,timestamp;"
table1=dbGetQuery(con,stat1)
stat2="select prod_name,report_id,topic from topicsmoz order by prod_name,report_id;"
table2=dbGetQuery(con,stat2)
tablesev<-data.frame(prod_name=character(),report_id=integer(),severity1=character(),severity2=character(),topic=integer(),stringsAsFactors=FALSE)
i<-1
k<-1
while(i<=nrow(table2))
{
  tablesev[i,1]=table2[i,1]
  tablesev[i,2]=table2[i,2]
  tablesev[i,3]=table1[k,3]
  while(table1[k+1,1]==tablesev[i,1] && table1[k+1,2]==tablesev[i,2])
  {
    k<-k+1
  }
  tablesev[i,4]=table1[k,3]
  tablesev[i,5]=table2[i,3]
  i<-i+1
  k<-k+1
}
tablesev1<-subset(tablesev,severity1=="normal")
tablesev1<-subset(tablesev1,severity2!="normal")
tablesev1$topic<-factor(tablesev1$topic)
ggplot(tablesev1[,4:5],aes(x=severity2))+geom_bar(aes(fill=topic,stat="count"))
