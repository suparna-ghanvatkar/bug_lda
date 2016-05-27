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
tab<-data.frame(report_id=integer(),timefor_resolution=integer(),severity=character(),year=integer(),stringsAsFactors=FALSE)
i<-1
while(i<4617)
{
  tab[i,1]=table2[i,1]
  tab[i,3]=table3[i,3]
  tab[i,2]=(table1[i,2])-(table2[i,2])
  tab[i,4]=strtoi(substr(as.Date(as.POSIXct(table2[i,2],origin="1970-01-01")),1,4))
  tab
  i=i+1
}
c<-1
tab1<-data.frame(severity=character(),no_of_bug=integer(),average_time=numeric(),year=integer(),stringsAsFactors=FALSE)
for(y in unique(tab$year))
{
  ctn<-0
  ctt<-0
  ctc<-0
  ctmi<-0
  ctmj<-0
  ctb<-0
  avgn<-0
  avgc<-0
  avgb<-0
  avgmi<-0
  avgmj<-0
  avgt<-0
  i<-1
  while(i<=nrow(subset(tab,year==y)))
  {
    if(tab[i,3]=="normal")
    {
      ctn=ctn+1
      avgn=avgn+tab[i,2]
      
    }
    if(tab[i,3]=="critical")
    {
      ctc=ctc+1
      avgc=avgc+tab[i,2]
    }
    if(tab[i,3]=="trivial")
    {
      ctt=ctt+1
      avgt=avgt+tab[i,2]
    }
    if(tab[i,3]=="minor")
    {
      ctmi=ctmi+1
      avgmi=avgmi+tab[i,2]
    } 
    if(tab[i,3]=="major")
    {
      ctmj=ctmj+1
      avgmj=avgmj+tab[i,2]
    } 
    if(tab[i,3]=="blocker")
    {
      ctb=ctb+1
      avgb=avgb+tab[i,2]
    } 
    i=i+1
  }
  avgt=avgt/ctt
  avgn=avgn/ctn
  avgmi=avgmi/ctmi
  avgmj=avgmj/ctmj
  avgc=avgc/ctc
  avgb=avgb/ctb
  #the no_of_bugs in each severity
  
  sev<-c("trivial","minor","normal","major","critical","blocker")
  k=list(ctt,ctmi,ctn,ctmj,ctc,ctb)
  a=list(avgt,avgmi,avgn,avgmj,avgc,avgb)
  j<-1
  while(j<=6)
  {
    tab1[c,1]=sev[j]
    tab1[c,2]=k[j]
    tab1[c,3]=a[j]
    tab1[c,4]=y
    j<-j+1
    c<-c+1
  }  
}
ggplot(tab1,aes(x=year,y=average_time,color=severity))+geom_line()


