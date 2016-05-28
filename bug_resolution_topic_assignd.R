library(lda)
library(ggplot2)
library(reshape2)
library(tm)
library(SnowballC)
library(RMySQL)
library(stringr)
drv=dbDriver("MySQL")
con=dbConnect(drv,dbname='new_bug_report',user='root')
stat0="select *from bug_sever;"
table0=dbGetQuery(con,stat0)
stat1="select l.* from bug_sever l inner join ( select report_id,what, max(timestamp) as latest from bug_sever group by report_id) r on l.timestamp = r.latest and l.report_id = r.report_id order by timestamp asc;"
table1=dbGetQuery(con,stat1)
stat2="select l.* from bug_sever l inner join ( select report_id,what, min(timestamp) as latest from bug_sever group by report_id) r on l.timestamp = r.latest and l.report_id = r.report_id order by timestamp asc;"
table2=dbGetQuery(con,stat2)
stat3="select l.* from bug_sev l inner join ( select report_id,what, max(timestamp) as latest from bug_sev group by report_id) r on l.timestamp = r.latest and l.report_id = r.report_id order by timestamp asc;"
table3=dbGetQuery(con,stat3)
stat4="select * from Bugzilla_topics order by report_id;"
table4=dbGetQuery(con,stat4)
tab<-data.frame(report_id=integer(),timefor_resolution=integer(),severity=character(),topic=integer(),year=integer(),stringsAsFactors=FALSE)
i<-1
k<-1
while(i<4617)
{
  tab[i,1]=table2[i,1]
  tab[i,3]=table3[i,3]
  tab[i,2]=(table1[i,2])-(table2[i,2])
  if(table4[k,1]==tab[i,1])
  {
    tab[i,4]=table4[k,2]
    k<-k+1
  }else
  {
    tab[i,4]=NA
  }
  tab[i,5]=strtoi(substr(as.Date(as.POSIXct(table2[i,2],origin="1970-01-01")),1,4))
  i=i+1
}
tab<-subset(tab,!is.na(topic))
c<-1
tab1<-data.frame(severity=character(),no_of_bug=integer(),average_time=numeric(),year=integer(),stringsAsFactors=FALSE)
for(y in unique(tab$year))
{
  ctn=c(0,0,0,0,0,0,0,0,0,0)
  avg=c(0,0,0,0,0,0,0,0,0,0)
  i<-1
  while(i<=nrow(subset(tab,year==y)))
  {
    if(tab[i,4]==1)
    {
      ctn[1]=ctn[1]+1
      avg[1]=avg[1]+tab[i,2]
      
    }
    if(tab[i,4]==2)
    {
      ctn[2]=ctn[2]+1
      avg[2]=avg[2]+tab[i,2]
      
    }
    if(tab[i,4]==3)
    {
      ctn[3]=ctn[3]+1
      avg[3]=avg[3]+tab[i,2]
      
    }
    if(tab[i,4]==4)
    {
      ctn[4]=ctn[4]+1
      avg[4]=avg[4]+tab[i,2]
      
    }
    if(tab[i,4]==5)
    {
      ctn[5]=ctn[5]+1
      avg[5]=avg[5]+tab[i,2]
      
    }
    if(tab[i,4]==6)
    {
      ctn[6]=ctn[6]+1
      avg[6]=avg[6]+tab[i,2]
      
    }
    if(tab[i,4]==7)
    {
      ctn[7]=ctn[7]+1
      avg[7]=avg[7]+tab[i,2]
      
    }
    if(tab[i,4]==8)
    {
      ctn[8]=ctn[8]+1
      avg[8]=avg[8]+tab[i,2]
      
    }
    if(tab[i,4]==9)
    {
      ctn[9]=ctn[9]+1
      avg[9]=avg[9]+tab[i,2]
      
    }
    if(tab[i,4]==10)
    {
      ctn[10]=ctn[10]+1
      avg[10]=avg[10]+tab[i,2]
      
    }
    i=i+1
  }
  for(i in 1:10)
  {
    avg[i]=avg[i]/ctn[i]
  }
  #the no_of_bugs in each severity
  
  top<-c('topic1','topic2','topic3','topic4','topic5','topic6','topic7','topic8','topic9','topic10')
  j<-1
  while(j<=10)
  {
    tab1[c,1]=top[j]
    tab1[c,2]=ctn[j]
    tab1[c,3]=avg[j]
    tab1[c,4]=y
    j<-j+1
    c<-c+1
  }  
}
ggplot(tab1,aes(x=year,y=average_time))+geom_line(aes(color=severity))


