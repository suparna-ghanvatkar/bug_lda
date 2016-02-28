# -*- coding: utf-8 -*-
"""
Created on Thu Sep  3 23:47:13 2015

"""

import xml.etree.ElementTree as ET
import MySQLdb as mdb
conn=mdb.connect('localhost','root',db='bug_report')
cur=conn.cursor()
product_list=['./mozilla/Bugzilla','./mozilla/Core','./mozilla/Firefox','./mozilla/Thunderbird','./eclipse/CDT','./eclipse/JDT', './eclipse/PDE', './eclipse/Platform']
attribute_list=['assigned_to', 'bug_status', 'cc', 'component', 'op_sys', 'priority', 'product', 'resolution', 'severity', 'short_desc', 'version']
obj={}
'''
Adding into products table

for prod in product_list:
    cur.execute("""INSERT INTO products (prod_name,category) VALUES (%s,%s);""",(prod.split('/')[2],prod.split('/')[1]))
conn.commit()

for prod in product_list:
    for i in attribute_list:
        filenm=prod+'/'+i+'.xml'
        tree = ET.parse(filenm)
        root = tree.getroot()

        for x in root:
            obj.clear()
            obj['report_id']=int(x.attrib['id'])
            for t in x:
                for child in t:
                    if child.tag=='when':
                        obj[child.tag]=int(child.text)
                    else:
                        obj[child.tag]=child.text
                print obj
                if obj['what']==None:
                    cur.execute("""INSERT INTO updates ( report_id, timestamp, attribute, what,prod_name ) VALUES (%s,%s,%s,NULL,%s);""",(str(obj['report_id']),str(obj['when']), i, prod.split('/')[2]))
                else:
                    cur.execute("""INSERT INTO updates ( report_id, timestamp,attribute,  what, prod_name ) VALUES (%s,%s,%s,%s,%s);""",(str(obj['report_id']),str(obj['when']),i,str(obj['what']), prod.split('/')[2]))
        conn.commit()  



Now insert into reports table
'''
obj.clear()
for prod in product_list:
    filenm=prod+'/reports.xml'
    tree = ET.parse(filenm)
    root = tree.getroot()
    for x in root:
        obj.clear()
        obj['report_id']=int(x.attrib['id'])
        for child in x:
            obj[child.tag]=child.text
        print obj
        cur.execute("""INSERT INTO reports ( report_id, prod_name, open_time, reporter ) VALUES (%s,%s,%s,%s);""",(str(obj['report_id']),prod.split('/')[2],str(obj['opening_time']),str(obj['reporter'])))
            
conn.commit()          
