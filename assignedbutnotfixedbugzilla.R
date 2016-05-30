#distribution of bugs across topics for ones assigned which were not in fixed category
library(lda)
library(ggplot2)
library(reshape2)
library(tm)
library(SnowballC)
library(RMySQL)
library(stringr)
drv=dbDriver("MySQL")
con=dbConnect(drv,dbname='new_bug_report',user='root')
stat0="select * from (select report_id,s.what as resolution from (select report_id from Bugzilla_updates where attribute=\"assigned_to\" and what is not NULL) l natural join (select report_id,what from Bugzilla_updates where attribute=\"resolution\" and what!=\"FIXED\") s) res natural join Bugzilla_topics;"
table0=dbGetQuery(con,stat0)
table0$topic<-factor(table0$topic)
ggplot(table0,aes(x=topic))+geom_bar(aes(fill=resolution),position=position_dodge(),stat='bin')