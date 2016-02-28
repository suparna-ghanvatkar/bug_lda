library(RMySQL)
drv=dbDriver("MySQL")
con=dbConnect(drv,dbname='bug_report',user='root')
table1=dbGetQuery(con,statement="select prod_name,count(distinct a.report_id) as reports, count(distinct a.what) as components from (select report_id,what,prod_name from updates where attribute='component') as a group by prod_name;")
View(table1)
attr=list('assigned_to', 'bug_status', 'cc', 'component', 'op_sys', 'priority', 'product', 'resolution', 'severity', 'short_desc', 'version')
i<-1
while(i<=11)
{
  stat="select report_id,timestamp,what from updates where prod_name='Bugzilla' and attribute='"
  statemnt<-paste(stat,attr[i],"';",sep="")
  tab=dbGetQuery(con,statemnt)
  file<-paste(attr[i],".csv",sep="")
  write.csv(tab,file)
  i<-i+1
}