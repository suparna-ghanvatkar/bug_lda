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
while(i<=5709)
{
  desc[i][[1]]<-removeWords(desc[i][[1]],sw)
  i<-i+1;
}
desc<-relist(desc,table1["what"])
table1["what"]<-desc
View(table1)
#create a character vector for dictionary
dict<-""
dict<-paste(dict,desc["what"])
dict<-strsplit(dict," ")
dict<-unlist(dict)
j<-1
while(j<=length(dict))
{
  if(dict[j]=="")
  {
    dict<-dict[-j]
  }
  else
  {
    j<-j+1 
  }
}
dictCorp<-Corpus(VectorSource(dict))
Data<-rep(list(NA),5709)
#stem completion----modify here
stemCompletion_mod <- function(x,dict=dictCorp) {
  stripWhitespace(paste(stemCompletion(unlist(strsplit(as.character(x)," ")),dictionary=dict, type="prevalent"),sep="", collapse=" "))
}
desc<-unlist(table1["what"])
i<-1
while(i<=5709)
{
  temp<-strsplit(desc[i][[1]]," ")
  temp<-lapply(temp,stemDocument)
  temp<-unlist(temp)
  j<-2
  while(j<=length(temp))
  {
    temp[1]<-paste(temp[1],temp[j])
    j<-j+1
  }
  temp<-temp[1]
  temp<-unlist(temp)
  desc[i][[1]]<-temp
  i<-i+1
}
desc<-relist(desc,table1["what"])
#Now LDA
docs<-lexicalize(desc)
vocab=docs$vocab
docs=docs$documents
K<-10
result<-lda.collapsed.gibbs.sampler(docs,K,vocab,500,0.1,0.01,compute.log.likelihood=TRUE)
top.words <- top.topic.words(result$topics, 10, by.score=TRUE)