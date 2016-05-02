library(lda)
library(ggplot2)
library(reshape2)
library(tm)
library(SnowballC)
library(RMySQL)
library(stringr)
drv=dbDriver("MySQL")
con=dbConnect(drv,dbname='bug_report',user='root')
stat="select Bugzilla_severity.report_id,Bugzilla_severity.timestamp,Bugzilla_severity.what as 'severity',Bugzilla_desc.what as 'desc' from Bugzilla_severity left join Bugzilla_desc on (Bugzilla_desc.report_id=Bugzilla_severity.report_id and Bugzilla_desc.timestamp=Bugzilla_severity.timestamp);"
table1=dbGetQuery(con,stat)
sw <- c(stopwords("english"),"na","NA","content","will")
desc<-table1["desc"]
docu=""
i<-1
while(i<=5616)
{
  temp<-desc[i,1]
  #if(is.na(temp) && i>1)
  #{  temp<-desc[i-1,1]
  #   docu[[i-1]]<-NA
  #}
  temp<-lapply(temp,tolower)
  temp<-lapply(temp,removePunctuation)
  temp<-lapply(temp,removeNumbers)
  temp<-unlist(temp)
  temp<-removeWords(temp,sw)
  temp<-strsplit(temp," ")
  temp<-lapply(temp,stemDocument)
  j<-2
  while(j<=length(temp[[1]]))
  {
    temp[[1]][1]<-paste(temp[[1]][1],temp[[1]][j])
    j<-j+1
  }
  temp<-temp[[1]][1]
  desc[i,1]<-relist(temp,desc[i,1])
  docu[[i]]<-desc[i,1]
  i<-i+1
}
Table<-table1
Table['desc']<-docu
docu<-subset(docu,docu!="NA")
Table<-subset(Table,desc!="NA")
docs<-lexicalize(docu)
vocab=docs$vocab
docs=docs$documents
K<-10
result<-lda.collapsed.gibbs.sampler(docs,K,vocab,800,0.15,0.01,compute.log.likelihood=TRUE)
top.words <- top.topic.words(result$topics, 20, by.score=TRUE)
#View(result$document_sums)
#View(subset(Table,desc!="NA"))
i<-1
table<-data.frame(report_id=integer(),timestamp=integer(),severity=character(),topic=integer(),stringsAsFactors=FALSE)
while(i<=length(docu))
{
  table[i,1]<-Table[i,1]
  table[i,2]<-Table[i,2]
  table[i,3]<-Table[i,3]
  table[i,4]<-which.max(result$document_sums[,i])
  i<-i+1
}
dist<-data.frame(severity=character(),t1=integer(),t2=integer(),t3=integer(),t4=integer(),t5=integer(),t6=integer(),t7=integer(),t8=integer(),t9=integer(),t10=integer(),stringsAsFactors=FALSE)
i<-1
sev<-c("trivial","minor","normal","major","critical","blocker")
while(i<=6)
{
  dist[i,1]<-sev[i]
  dist[i,2:11]<-tabulate(subset(table,severity==sev[i])$topic, nbins = 10)
  i<-i+1
}
