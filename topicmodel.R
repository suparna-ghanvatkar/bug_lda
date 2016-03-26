library(lda)
library(ggplot2)
library(reshape2)
library(tm)
library(SnowballC)
library(RMySQL)
library(stringr)
drv=dbDriver("MySQL")
con=dbConnect(drv,dbname='bug_report',user='root')
stat="select report_id,timestamp,what from updates where prod_name='Bugzilla' and attribute='short_desc';"
table1=dbGetQuery(con,stat)
sw <- c(stopwords("english"),"na","NA","content","will")
desc<-table1["what"]
docu=""
i<-1
while(i<=5709)
{
  temp<-desc[i,1]
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
docs<-lexicalize(docu)
vocab=docs$vocab
docs=docs$documents
K<-10
result<-lda.collapsed.gibbs.sampler(docs,K,vocab,800,0.15,0.01,compute.log.likelihood=TRUE)
top.words <- top.topic.words(result$topics, 20, by.score=TRUE)
