library(lda)
library(ggplot2)
library(reshape2)
library(tm)
library(SnowballC)
library(RMySQL)
drv=dbDriver("MySQL")
con=dbConnect(drv,dbname='bug_report',user='root')
stat="select report_id,timestamp,what from updates where prod_name='Bugzilla' and attribute='short_desc';"
table1=dbGetQuery(con,stat)
sw <- c(stopwords("english"),"na","NA","content")
table1<-lapply(table1,tolower)
desc<-unlist(table1["what"])
i<-1
while(i<=length(table1))
{
  desc[i][[1]]<-removeWords(desc[i][[1]],sw)
  i<-i+1;
}
desc<-relist(desc,table1["what"])
table1["what"]<-desc
View(table1)
docs<-lexicalize(desc)
vocab=docs$vocab
docs=docs$documents
K<-10
result<-lda.collapsed.gibbs.sampler(docs,K,vocab,500,0.1,0.01,compute.log.likelihood=TRUE)
top.words <- top.topic.words(result$topics, 10, by.score=TRUE)